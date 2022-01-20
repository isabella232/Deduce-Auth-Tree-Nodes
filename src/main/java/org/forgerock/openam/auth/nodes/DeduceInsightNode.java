/*
 * The contents of this file are subject to the terms of the Common Development and
 * Distribution License (the License). You may not use this file except in compliance with the
 * License.
 *
 * You can obtain a copy of the License at legal/CDDLv1.0.txt. See the License for the
 * specific language governing permission and limitations under the License.
 *
 * When distributing Covered Software, include this CDDL Header Notice in each file and include
 * the License file at legal/CDDLv1.0.txt. If applicable, add the following below the CDDL
 * Header, with the fields enclosed by brackets [] replaced by your own identifying
 * information: "Portions copyright [year] [name of copyright owner]".
 *
 * Copyright 2017-2018 ForgeRock AS.
 */


package org.forgerock.openam.auth.nodes;

import static org.forgerock.http.protocol.Responses.noopExceptionFunction;
import static org.forgerock.json.JsonValue.field;
import static org.forgerock.json.JsonValue.json;
import static org.forgerock.json.JsonValue.object;
import static org.forgerock.util.CloseSilentlyFunction.closeSilently;
import static org.forgerock.util.Closeables.closeSilentlyAsync;

import java.net.URISyntaxException;
import java.security.AccessController;
import java.util.Set;

import javax.inject.Inject;

import org.forgerock.http.handler.HttpClientHandler;
import org.forgerock.http.protocol.Request;
import org.forgerock.http.protocol.Response;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.NodeState;
import org.forgerock.openam.auth.node.api.SharedStateConstants;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.openam.core.CoreWrapper;
import org.forgerock.services.context.RootContext;
import org.forgerock.util.Function;
import org.forgerock.util.promise.Promise;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.assistedinject.Assisted;
import com.iplanet.sso.SSOException;
import com.sun.identity.authentication.util.ISAuthConstants;
import com.sun.identity.idm.AMIdentity;
import com.sun.identity.idm.IdRepoException;
import com.sun.identity.idm.IdUtils;
import com.sun.identity.security.AdminTokenAction;
import com.sun.identity.sm.SMSException;
import com.sun.identity.sm.ServiceConfig;

@Node.Metadata(outcomeProvider = AbstractDecisionNode.OutcomeProvider.class,
        configClass = DeduceInsightNode.Config.class, tags = {"risk"})
public class DeduceInsightNode extends AbstractDecisionNode {

    private final Logger logger = LoggerFactory.getLogger(DeduceInsightNode.class);
    private final Config config;
    private final CoreWrapper coreWrapper;
    private final HttpClientHandler clientHandler;

    /**
     * Configuration for the node.
     */
    public interface Config {
        /**
         * Site ID provided by Deduce during onboarding.
         */
        @Attribute(order = 100)
        default String site() {
            return "";
        }

        /**
         * API key provided by Deduce during onboarding.
         */
        @Attribute(order = 200)
        char[] apiKey();

        /**
         * A contextual input to describe the request
         */
        @Attribute(order = 300)
        default String action() {return "auth.success.password";}

        /**
         * Set as true to prevent queries from influencing a user’s profile.
         */
        @Attribute(order = 400)
        default boolean test() {
            return false;
        }


        @Attribute(order = 500)
        default String endpoint() {
            return "https://api.deducesecurity.com/insights";
        }
    }


    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to obtain instances of other classes
     * from the plugin.
     *
     * @param config The service config.
     */
    @Inject
    public DeduceInsightNode(@Assisted Config config, CoreWrapper coreWrapper,
                             HttpClientHandler client) {
        this.config = config;
        this.coreWrapper = coreWrapper;
        this.clientHandler = client;
    }

    static Function<Response, JsonValue, NodeProcessException> mapToJsonValue() {
        return response -> {
            try {
                if (!response.getStatus().isSuccessful()) {
                    throw response.getCause();
                }
                return json(response.getEntity().getJson());
            } catch (Exception e) {
                throw new NodeProcessException("Unable to process request. " + response.getEntity().toString(), e);
            }
        };
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {

        NodeState state = context.getStateFor(this);
        if (!state.isDefined(SharedStateConstants.USERNAME) || !state.isDefined(SharedStateConstants.REALM)) {
            logger.error(
                    "Deduce Insight Node requires both the realm and username populated in sharedState to proceed");
            return goTo(false).build();
        }
        Request request;
        try {
            request = new Request().setUri(config.endpoint());
        } catch (URISyntaxException e) {
            throw new NodeProcessException(e);
        }

        request.setMethod("POST");

        request.setEntity(createInsightsBody(context, getIdentityFromSearchAlias(
                state.get(SharedStateConstants.USERNAME).asString(),
                state.get(SharedStateConstants.REALM).asString())));
        Promise<Void, NodeProcessException> deduceResponse = clientHandler.handle(new RootContext(), request)
                                                                          .thenAlways(closeSilentlyAsync(request))
                                                                          .then(closeSilently(mapToJsonValue()),
                                                                                noopExceptionFunction())
                                                                          .then(storeResponse(state));
        try {
            deduceResponse.getOrThrow();
        } catch (Exception e) {
            logger.error("Exception when making insight requests");
            return goTo(false).build();
        }
        return goTo(true).build();
    }

    private JsonValue createInsightsBody(TreeContext context, AMIdentity user) {
        JsonValue requestBody = json(object(
                field("site", config.site()),
                field("apikey", String.valueOf(config.apiKey())),
                field("ip", context.request.clientIp),
                field("user_agent", context.request.headers.get("User-Agent").get(0)),
                field("action", config.action()),
                field("test", config.test()
                )));

        return addUserAttributes(user, requestBody);

    }

    private JsonValue addUserAttributes(AMIdentity user, JsonValue requestBody) {
        try {
            Set<String> mail = user.getAttribute("mail");
            if (!mail.isEmpty()) {
                requestBody.add("email", mail.iterator().next());
            }
        } catch (IdRepoException | SSOException e) {
            logger.error("Unable to lookup mail attribute");
        }

        try {
            Set<String> mail = user.getAttribute("telephoneNumber");
            if (!mail.isEmpty()) {
                requestBody.add("phone", mail.iterator().next());
            }
        } catch (IdRepoException | SSOException e) {
            logger.error("Unable to lookup telephoneNumber attribute");
        }

        return requestBody;
    }

    private AMIdentity getIdentityFromSearchAlias(String username, String realm) {
        ServiceConfig serviceConfig = null;
        try {
            serviceConfig = coreWrapper.getServiceConfigManager(ISAuthConstants.AUTH_SERVICE_NAME,
                                                                AccessController.doPrivileged(
                                                                        AdminTokenAction.getInstance()))
                                       .getOrganizationConfig(realm, null);
        } catch (SSOException | SMSException e) {
            logger.error("Unable to get the " + ISAuthConstants.AUTH_SERVICE_NAME + " service");
        }

        return IdUtils.getIdentity(username, realm, serviceConfig.getAttributes().get(ISAuthConstants.AUTH_ALIAS_ATTR));
    }

    private Function<JsonValue, Void, NodeProcessException> storeResponse(NodeState state) {
        return response -> {
            state.putShared("deduceResponse", response);
            return null;
        };
    }

}
