package org.forgerock.openam.auth.nodes;


import static org.forgerock.openam.auth.nodes.DeduceHelper.getFieldValue;

import java.util.List;
import java.util.ResourceBundle;

import javax.inject.Inject;

import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.InputState;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.OutcomeProvider;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.util.i18n.PreferredLocales;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;
import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = DeduceScoreNode.DeduceScoreOutcomeProvider.class,
        configClass = DeduceScoreNode.Config.class, tags = {"risk"})
public class DeduceScoreNode implements Node {

    private static final String BUNDLE = "org/forgerock/openam/auth/nodes/DeduceScoreNode";
    private final Config config;
    private final Logger logger = LoggerFactory.getLogger(DeduceBooleanNode.class);


    /**
     * Configuration for the node.
     */
    public interface Config {

        @Attribute(order = 100)
        String field();

        /**
         * Score Threshold
         */
        @Attribute(order = 200)
        default int scoreThreshold() {
            return 0;
        }

    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to obtain instances of other classes
     * from the plugin.
     *
     * @param config The service config.
     */
    @Inject
    public DeduceScoreNode(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) {

        JsonValue value;
        int score;
        try {
            value = getFieldValue(context.getStateFor(this), config.field());
            score = value.asInteger();
        } catch (Exception e) {
            logger.error(e.getMessage());
            return Action.goTo(DeduceScoreOutcome.EXCEPTION.name()).build();
        }

        if (score >= config.scoreThreshold()) {
            return Action.goTo(DeduceScoreOutcome.GREATER_THAN_OR_EQUAL.name()).build();
        }
        return Action.goTo(DeduceScoreOutcome.LESS_THAN.name()).build();
    }




    /**
     * The possible outcomes for the DeduceScoreNode.
     */
    private enum DeduceScoreOutcome {
        GREATER_THAN_OR_EQUAL,
        LESS_THAN,
        EXCEPTION
    }


    /**
     * Defines the possible outcomes from this DeduceScoreNode
     */
    public static class DeduceScoreOutcomeProvider implements OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            ResourceBundle bundle = locales.getBundleInPreferredLocale(BUNDLE, DeduceScoreNode.class.getClassLoader());
            return ImmutableList.of(
                    new Outcome(DeduceScoreOutcome.GREATER_THAN_OR_EQUAL.name(),
                                bundle.getString("greaterThanOrEqualOutcome")),
                    new Outcome(DeduceScoreOutcome.LESS_THAN.name(), bundle.getString("lessThanOutcome")),
                    new Outcome(DeduceScoreOutcome.EXCEPTION.name(), bundle.getString("exceptionOutcome")));
        }
    }

    @Override
    public InputState[] getInputs() {
        return new InputState[] {new InputState(DeduceHelper.INSIGHT_RESPONSE, true)};
    }
}
