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

package org.octopus.octopusNode;

import static org.octopus.octopusNode.PollingService.OCTOPUS_RESPONSE_ID;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.inject.Inject;

import org.forgerock.http.handler.HttpClientHandler;
import org.forgerock.http.protocol.Request;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.NodeState;
import org.forgerock.openam.auth.node.api.SharedStateConstants;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.openam.sm.annotations.adapters.Password;
import org.forgerock.services.context.RootContext;
import org.forgerock.util.i18n.PreferredLocales;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = OctopusNode.OutcomeProvider.class, configClass = OctopusNode.Config.class, tags = {"marketplace", "trustnetwork"})
public class OctopusNode extends AbstractDecisionNode {
    private final Logger logger = LoggerFactory.getLogger(OctopusNode.class);
    private final HttpClientHandler clientHandler;
    private final PollingService pollingService;
    private final String serviceUrl;
    private final String message;
    private final String applicationKey;

    private static final String loggerPrefix = "[Octopus][Marketplace] ";

    /**
     * Configuration for the Octopus node.
     */
    public interface Config {
        @Attribute(order = 100)
        @Password
        char[] apiToken();

        @Attribute(order = 200)
        default String serviceUrl() {
            return "";
        }

        @Attribute(order = 300)
        default String message() {
            return "";
        }
    }

    @Inject
    public OctopusNode(@Assisted final Config config, HttpClientService clientService, PollingService pollingService) {
        this.clientHandler = clientService.getClient();
        this.pollingService = pollingService;
        this.applicationKey = String.valueOf(config.apiToken());
        this.serviceUrl = config.serviceUrl();
        this.message = config.message();
    }

    @Override
    public Action process(final TreeContext context) throws NodeProcessException {
        Request request = null;
        try {
            logger.debug(loggerPrefix + "Started");
            NodeState ns = context.getStateFor(this);
            final String username = ns.get(SharedStateConstants.USERNAME).asString().toLowerCase();
            String requestId = UUID.randomUUID().toString();
            logger.debug(loggerPrefix + "Login request (ID: " + requestId + ") - username: " + username);
            request = authRequest(username);
            pollingService.put(requestId, clientHandler.handle(new RootContext(), request));
            ns.putShared(OCTOPUS_RESPONSE_ID, requestId);
            return Action.goTo("True").build();
        } catch (Exception ex) {
            logger.error(loggerPrefix + "Exception occurred: " + ex.getStackTrace());
            context.getStateFor(this).putShared(loggerPrefix + "Exception", new Date() + ": " + ex.getMessage());
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);
            context.getStateFor(this).putShared(loggerPrefix + "StackTracke", new Date() + ": " + sw.toString());
            return Action.goTo("Error").build();

        }
        finally {
            try {
                if (request!=null) {
                    request.close();
                    clientHandler.close();
                }
            }
            catch(Exception e) {
                //do nothing
            }
        }

    }

    private Request authRequest(final String username)
            throws URISyntaxException {
        String url = this.serviceUrl + "/1/auth";
        Map<String, String> reqMap = new HashMap<>();
        reqMap.put("appKey", this.applicationKey);
        reqMap.put("username", username);
        reqMap.put("message", message);
        JsonValue json = new JsonValue(reqMap);
        return new Request()
                .setUri(url)
                .setMethod("POST")
                .setEntity(json.asMap());
    }

    public static final class OutcomeProvider implements org.forgerock.openam.auth.node.api.OutcomeProvider {
        /**
         * Outcomes Ids for this node.
         */
        static final String SUCCESS_OUTCOME = "True";
        static final String ERROR_OUTCOME = "Error";
        // static final String NOT_MOBILE_OUTCOME = "Not Mobile";
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            List<Outcome> results = new ArrayList<>(
                    Arrays.asList(
                            new Outcome(SUCCESS_OUTCOME, SUCCESS_OUTCOME)
                    )
            );
            //results.add(new Outcome(NOT_MOBILE_OUTCOME, NOT_MOBILE_OUTCOME));
            results.add(new Outcome(ERROR_OUTCOME, ERROR_OUTCOME));

            return Collections.unmodifiableList(results);
        }
    }
}