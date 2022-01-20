package org.forgerock.openam.auth.nodes;


import static org.forgerock.openam.auth.nodes.DeduceHelper.getFieldValue;

import java.util.List;
import java.util.ResourceBundle;

import javax.inject.Inject;

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

@Node.Metadata(outcomeProvider = DeduceBooleanNode.DeduceBooleanOutcomeProvider.class, configClass =
        DeduceBooleanNode.Config.class, tags = {"risk"})
public class DeduceBooleanNode extends AbstractDecisionNode {

    private static final String BUNDLE = "org/forgerock/openam/auth/nodes/DeduceBooleanNode";
    private final Config config;
    private final Logger logger = LoggerFactory.getLogger(DeduceBooleanNode.class);


    /**
     * Configuration for the node.
     */
    public interface Config {

        @Attribute(order = 100)
        String field();

    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to obtain instances of other classes
     * from the plugin.
     *
     * @param config The service config.
     */
    @Inject
    public DeduceBooleanNode(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) {
        JsonValue value;
        Boolean bool;
        try {
            value = getFieldValue(context.getStateFor(this), config.field());
            bool = value.asBoolean();
        } catch (Exception e) {
            logger.error(e.getMessage());
            return Action.goTo(DeduceBooleanOutcome.EXCEPTION.name()).build();
        }
        if (!(null == bool) && bool) {
            return Action.goTo(DeduceBooleanOutcome.TRUE.name()).build();
        }
        return Action.goTo(DeduceBooleanOutcome.FALSE.name()).build();
    }

    @Override
    public InputState[] getInputs() {
        return new InputState[]{new InputState(DeduceHelper.INSIGHT_RESPONSE, true)};
    }


    /**
     * The possible outcomes for the DeduceBooleanNode.
     */
    private enum DeduceBooleanOutcome {
        TRUE,
        FALSE,
        EXCEPTION
    }

    /**
     * Defines the possible outcomes from this DeduceBooleanNode
     */
    public static class DeduceBooleanOutcomeProvider implements org.forgerock.openam.auth.node.api.OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            ResourceBundle bundle = locales.getBundleInPreferredLocale(BUNDLE,
                                                                       DeduceBooleanNode.class.getClassLoader());
            return ImmutableList.of(
                    new Outcome(DeduceBooleanOutcome.TRUE.name(),
                                bundle.getString("trueOutcome")),
                    new Outcome(DeduceBooleanOutcome.FALSE.name(), bundle.getString("falseOutcome")),
                    new Outcome(DeduceBooleanOutcome.EXCEPTION.name(), bundle.getString("exceptionOutcome")));
        }
    }
}
