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

import javax.inject.Inject;

import com.google.inject.assistedinject.Assisted;

import java.net.URISyntaxException;
import java.security.KeyStoreException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.forgerock.http.handler.HttpClientHandler;
import org.forgerock.http.protocol.Request;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.SharedStateConstants;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.services.context.RootContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Node.Metadata(outcomeProvider = AbstractDecisionNode.OutcomeProvider.class, configClass = octopusNode.Config.class)
public class octopusNode extends AbstractDecisionNode {
    private final Logger logger = LoggerFactory.getLogger("amAuth");
    private final HttpClientHandler clientHandler;
    private final PollingService pollingService;
    private final String serviceUrl;
    private final String message;
    private final String applicationKey;

    /**
     * Configuration for the Octopus node.
     */
    public interface Config {
        @Attribute(order = 100)
        default String apiToken() {
            return "";
        }

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
    public octopusNode(@Assisted final Config config, HttpClientService clientService, PollingService pollingService) throws NodeProcessException, KeyStoreException {
        this.clientHandler = clientService.getClient();
        this.pollingService = pollingService;
        this.applicationKey = config.apiToken();
        this.serviceUrl = config.serviceUrl();
        this.message = config.message();
    }

    @Override
    public Action process(final TreeContext context) throws NodeProcessException {
        final String username = context.sharedState.get(SharedStateConstants.USERNAME).asString().toLowerCase();
        String requestId = UUID.randomUUID().toString();
        logger.debug("Login request (ID: " + requestId + ") - username: " + username);
        Request request;
        try {
            request = authRequest(username);
        } catch (Exception e) {
            throw new NodeProcessException(e);
        }
        pollingService.put(requestId, clientHandler.handle(new RootContext(), request));
        return goTo(true).replaceSharedState(context.sharedState.add(OCTOPUS_RESPONSE_ID, requestId)).build();
    }

    private Request authRequest(final String username)
            throws URISyntaxException, InterruptedException {
        String url = this.serviceUrl + "/1/auth";
        Map<String, String> reqMap = new HashMap<String, String>();
        reqMap.put("appKey", this.applicationKey);
        reqMap.put("username", username);
        reqMap.put("message", message);
        JsonValue json = new JsonValue(reqMap);
        Request request = new Request()
            .setUri(url)
            .setMethod("POST")
            .setEntity(json.asMap());
        return request;
    }
}
