package org.octopus.octopusNode;

import static org.octopus.octopusNode.PollingService.OCTOPUS_RESPONSE_ID;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.util.Base64;
import java.util.List;
import java.util.ResourceBundle;

import javax.inject.Inject;

import org.forgerock.http.protocol.Response;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.util.i18n.PreferredLocales;
import org.forgerock.util.promise.NeverThrowsException;
import org.forgerock.util.promise.Promise;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = octopusReturnNode.OctopusReturnOutcomeProvider.class, configClass = octopusReturnNode.Config.class)
public class octopusReturnNode extends AbstractDecisionNode {

    private static final String BUNDLE = octopusReturnNode.class.getName();
    private final Logger logger = LoggerFactory.getLogger("amAuth");
    private final PollingService pollingService;
    private PublicKey publicKey;

    /**
     * Configuration for the node.
     */
    public interface Config {
        @Attribute(order = 100)
        default String serviceCert() {
            return "";
        }
    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to
     * obtain instances of other classes from the plugin.
     * 
     * @throws CertificateException
     */
    @Inject
    public octopusReturnNode(@Assisted final Config config, PollingService pollingService, OctopusCertService certService) throws CertificateException {
        this.pollingService = pollingService;
        this.publicKey = certService.getPublicKey(config.serviceCert());
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        String responseId = context.sharedState.get(OCTOPUS_RESPONSE_ID).asString();
        Promise<Response, NeverThrowsException> promise = null;
        try {
            promise = this.pollingService.get(responseId);
        } catch (Exception e) {
            // Error getting promise, go to False outcome
            logger.error("Error getting promise for response ID: " + responseId + ", error: " + e);
            return Action.goTo(AuthOutcome.FALSE.name()).build();
        }

        Response response;
        if (promise.isDone()) {
            try {
                // If request has completed, grab the response
                response = promise.getOrThrow();
                if (!response.getStatus().isSuccessful()) {
                    logger.debug("Auth response (ID: " + responseId + ") status code: " + response.getStatus().getCode());
                    return Action.goTo(AuthOutcome.FALSE.name()).build();
                }
                String status = getResponseStatus(response);
                logger.debug("Auth response (ID: " + responseId + "): " + status);
                // Validate the response and go to True outcome
                if (status.equals("accept")) {
                    return Action.goTo(AuthOutcome.TRUE.name()).build();
                }
                return Action.goTo(AuthOutcome.FALSE.name()).build();
            } catch (Exception e) {
                throw new NodeProcessException(e);
            } finally {
                this.pollingService.remove(responseId);
            }
        }
        return Action.goTo(AuthOutcome.UNANSWERED.name()).build();
    }

    String getResponseStatus(Response response)
            throws InvalidKeyException, NoSuchAlgorithmException, SignatureException, IOException {
        final String stringBody = response.getEntity().getString();
        final JsonParser jsonParser = new JsonParser();
        final JsonElement responseObj = jsonParser.parse(stringBody);
        final JsonObject jsonObject = responseObj.getAsJsonObject();
        final JsonElement b64payload = jsonObject.get("payload");
        final String payload = b64payload.getAsString();
        final JsonElement signatureElement = jsonObject.get("signature");
        final String signature = signatureElement.getAsString();
        final JsonElement algoElement = jsonObject.get("algorithm");
        final String algorithm = algoElement.getAsString();

        boolean sigResult = checkSignature(payload, signature, algorithm);
        if (!sigResult) {
            logger.error("Invalid signature");
            return "invalid";
        }

        final byte[] decoded = Base64.getDecoder().decode(payload);
        final String decodedString = new String(decoded, StandardCharsets.UTF_8);
        final JsonElement authTree = jsonParser.parse(decodedString);
        final JsonObject authObjString = authTree.getAsJsonObject();
        final JsonElement authStatusElement = authObjString.get("authStatus");
        return authStatusElement.getAsString();
    }

    private boolean checkSignature(String payload, String sig, String algorithm) {
        String algoString = "SHA1withRSA";
        if (algorithm.equals("sha256")) {
            algoString = "SHA256withRSA";
        }
        try {
            Signature signature = Signature.getInstance(algoString);
            signature.initVerify(this.publicKey);
            signature.update(payload.getBytes());
            final byte[] decoded = Base64.getDecoder().decode(sig);
            return signature.verify(decoded);
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * The possible outcomes for the octopusReturnNode.
     */
    public enum AuthOutcome {
        /**
         * Successful authentication.
         */
        TRUE,
        /**
         * Authentication failed.
         */
        FALSE,
        /**
         * Still waiting for result
         */
        UNANSWERED
    }

    /**
     * Defines the possible outcomes from this TestAsyncReturnNode.
     */
    public static class OctopusReturnOutcomeProvider implements org.forgerock.openam.auth.node.api.OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            ResourceBundle bundle = locales.getBundleInPreferredLocale(octopusReturnNode.BUNDLE,
                                                                       OctopusReturnOutcomeProvider.class
                                                                               .getClassLoader());
            return ImmutableList.of(
                new Outcome(AuthOutcome.TRUE.name(), bundle.getString("trueOutcome")),
                new Outcome(AuthOutcome.FALSE.name(), bundle.getString("falseOutcome")),
                new Outcome(AuthOutcome.UNANSWERED.name(), bundle.getString("unansweredOutcome")));
        }
    }
}