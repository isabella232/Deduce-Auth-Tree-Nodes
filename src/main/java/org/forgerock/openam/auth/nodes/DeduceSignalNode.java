package org.forgerock.openam.auth.nodes;


import static java.util.Collections.emptyList;
import static org.forgerock.openam.auth.node.api.Action.goTo;
import static org.forgerock.openam.auth.nodes.DeduceHelper.NONE_TRIGGERED;
import static org.forgerock.openam.auth.nodes.DeduceHelper.getInsightResponse;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.forgerock.json.JsonValue;
import org.forgerock.json.JsonValueException;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.InputState;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.NodeState;
import org.forgerock.openam.auth.node.api.OutcomeProvider;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.util.i18n.PreferredLocales;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = DeduceSignalNode.DeduceSignalOutcomeProvider.class,
        configClass = DeduceSignalNode.Config.class, tags = {"risk"})
public class DeduceSignalNode implements Node {

    private final Config config;
    private final Logger logger = LoggerFactory.getLogger(DeduceSignalNode.class);


    /**
     * Configuration for the node.
     */
    public interface Config {

        /**
         * The list of possible outcomes.
         *
         * @return The possible outcomes.
         */
        @Attribute(order = 100)
        List<String> signalOutcomes();

    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to obtain instances of other classes
     * from the plugin.
     *
     * @param config The service config.
     */
    @Inject
    public DeduceSignalNode(@Assisted Config config) {
        this.config = config;
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        NodeState state = context.getStateFor(this);

        JsonValue insightResponse = getInsightResponse(state);
        JsonValue signal_section = insightResponse.get("data").get("signals");
        List<String> signals = new ArrayList<>() {{
            addAll(signal_section.get("info").asList(String.class));
            addAll(signal_section.get("risk").asList(String.class));
            addAll(signal_section.get("trust").asList(String.class));
        }};

        for (String signalOutcome : config.signalOutcomes()) {
            if (signals.contains(signalOutcome)) {
                logger.debug("Found outcome: " + signalOutcome);
                return goTo(signalOutcome).build();
            }
        }

        return goTo(NONE_TRIGGERED).build();
    }


    /**
     * Defines the possible outcomes from this Deduce Reason Code Node.
     */
    public static class DeduceSignalOutcomeProvider implements OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            try {
                List<Outcome> outcomes = nodeAttributes.get("signalOutcomes").required()
                                                       .asList(String.class)
                                                       .stream()
                                                       .map(outcome -> new Outcome(outcome, outcome))
                                                       .collect(Collectors.toList());
                outcomes.add(new Outcome(NONE_TRIGGERED, NONE_TRIGGERED));
                return outcomes;
            } catch (JsonValueException e) {
                return emptyList();
            }
        }
    }

    @Override
    public InputState[] getInputs() {
        return new InputState[] {new InputState(DeduceHelper.INSIGHT_RESPONSE, true)};
    }
}
