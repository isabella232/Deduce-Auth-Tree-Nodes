package org.forgerock.openam.auth.nodes;

import org.forgerock.json.JsonValue;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.NodeState;

class DeduceHelper {

    static final String INSIGHT_RESPONSE = "deduceResponse";
    static final String NONE_TRIGGERED = "None Triggered";

    static JsonValue getInsightResponse(NodeState state) throws NodeProcessException {
        if (!state.isDefined(INSIGHT_RESPONSE)) {
            throw new NodeProcessException("Unable to find Deduce" + INSIGHT_RESPONSE +
                                                   " in sharedState. Does the Deduce Insight node precede" +
                                                   " this node and return a successful response?");
        }
        return state.get(INSIGHT_RESPONSE);
    }

    static JsonValue getFieldValue(NodeState state, String field) throws NodeProcessException {

        String[] splitString = field.split("\\.");
        JsonValue section = getInsightResponse(state);
        for (String s : splitString) {
            if (!section.isDefined(s)) {
                throw new NodeProcessException(
                        "Configured field " + field + " does not exist in Deduce insights json response");
            }
            section = section.get(s);
        }
        return section;
    }
}
