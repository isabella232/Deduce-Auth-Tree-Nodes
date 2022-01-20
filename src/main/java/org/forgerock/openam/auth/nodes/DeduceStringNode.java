package org.forgerock.openam.auth.nodes;


import static org.forgerock.openam.auth.nodes.DeduceHelper.getFieldValue;

import java.util.List;
import java.util.ResourceBundle;

import javax.inject.Inject;

import org.apache.commons.lang.StringUtils;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.InputState;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.util.i18n.PreferredLocales;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;
import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = DeduceStringNode.DeduceStringOutcomeProvider.class, configClass =
        DeduceStringNode.Config.class, tags = {"risk"})
public class DeduceStringNode extends AbstractDecisionNode {

    private static final String BUNDLE = "org/forgerock/openam/auth/nodes/DeduceStringNode";

    private final Config config;
    private final Logger logger = LoggerFactory.getLogger(DeduceStringNode.class);


    /**
     * Configuration for the node.
     */
    public interface Config {

        @Attribute(order = 100)
        String field();

        @Attribute(order = 200)
        String string();

    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to obtain instances of other classes
     * from the plugin.
     *
     * @param config The service config.
     */
    @Inject
    public DeduceStringNode(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) {
        JsonValue value;
        String string;
        try {
            value = getFieldValue(context.getStateFor(this), config.field());
            string = value.asString();
        } catch (Exception e) {
            logger.error(e.getMessage());
            return Action.goTo(DeduceStringOutcome.EXCEPTION.name()).build();
        }
        if (!(null == string) && StringUtils.equals(value.asString(), config.string())) {
            return Action.goTo(DeduceStringOutcome.TRUE.name()).build();
        }
        return Action.goTo(DeduceStringOutcome.FALSE.name()).build();
    }

    /**
     * The possible outcomes for the DeduceStringNode.
     */
    private enum DeduceStringOutcome {
        TRUE,
        FALSE,
        EXCEPTION
    }

    /**
     * Defines the possible outcomes from this DeduceStringNode
     */
    public static class DeduceStringOutcomeProvider implements org.forgerock.openam.auth.node.api.OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            ResourceBundle bundle = locales.getBundleInPreferredLocale(BUNDLE,
                                                                       DeduceStringNode.class.getClassLoader());
            return ImmutableList.of(
                    new Outcome(DeduceStringOutcome.TRUE.name(),
                                bundle.getString("trueOutcome")),
                    new Outcome(DeduceStringOutcome.FALSE.name(), bundle.getString("falseOutcome")),
                    new Outcome(DeduceStringOutcome.EXCEPTION.name(), bundle.getString("exceptionOutcome")));
        }
    }

    @Override
    public InputState[] getInputs() {
        return new InputState[]{new InputState(DeduceHelper.INSIGHT_RESPONSE, true)};
    }
}
