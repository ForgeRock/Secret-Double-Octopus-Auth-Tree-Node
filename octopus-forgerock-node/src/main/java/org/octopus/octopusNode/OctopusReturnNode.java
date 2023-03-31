package org.octopus.octopusNode;

import static org.octopus.octopusNode.PollingService.OCTOPUS_RESPONSE_ID;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.PublicKey;
import java.security.Signature;
import java.security.cert.CertificateException;
import java.util.Base64;
import java.util.List;
import java.util.ResourceBundle;
import java.io.PrintWriter;
import java.io.StringWriter;
import javax.inject.Inject;

import org.forgerock.http.protocol.Response;
import org.forgerock.json.JsonValue;
import org.forgerock.openam.annotations.sm.Attribute;
import org.forgerock.openam.auth.node.api.AbstractDecisionNode;
import org.forgerock.openam.auth.node.api.Action;
import org.forgerock.openam.auth.node.api.Node;
import org.forgerock.openam.auth.node.api.NodeProcessException;
import org.forgerock.openam.auth.node.api.TreeContext;
import org.forgerock.openam.sm.annotations.adapters.Password;
import org.forgerock.util.i18n.PreferredLocales;
import org.forgerock.util.promise.NeverThrowsException;
import org.forgerock.util.promise.Promise;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.*;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.inject.assistedinject.Assisted;

@Node.Metadata(outcomeProvider = OctopusReturnNode.OctopusReturnOutcomeProvider.class, configClass =
        OctopusReturnNode.Config.class, tags = {"marketplace", "trustnetwork"})
public class OctopusReturnNode extends AbstractDecisionNode {

    private static final String BUNDLE = OctopusReturnNode.class.getName();
    private final Logger logger = LoggerFactory.getLogger("amAuth");
    private final PollingService pollingService;
    private PublicKey publicKey;

    private static final String loggerPrefix = "[Octopus Return Node][Marketplace] ";

    /**
     * Configuration for the node.
     */
    public interface Config {
        @Attribute(order = 100)
        @Password
        char[] serviceCert();
    }

    /**
     * Create the node using Guice injection. Just-in-time bindings can be used to
     * obtain instances of other classes from the plugin.
     *
     * @throws CertificateException
     */
    @Inject
    public OctopusReturnNode(@Assisted final Config config, PollingService pollingService,
                             OctopusCertService certService) throws CertificateException {
        this.pollingService = pollingService;
        this.publicKey = certService.getPublicKey(String.valueOf(config.serviceCert()));
    }

    @Override
    public Action process(TreeContext context) throws NodeProcessException {
        String responseId = context.sharedState.get(OCTOPUS_RESPONSE_ID).asString();
        try {
            logger.debug(loggerPrefix + "Started");

            Promise<Response, NeverThrowsException> promise;

            promise = this.pollingService.get(responseId);



            Response response;
            if (promise.isDone()) {
                // If request has completed, grab the response
                response = promise.getOrThrow();
                if (!response.getStatus().isSuccessful()) {
                    logger.debug(
                            loggerPrefix +  "Auth response (ID: " + responseId + ") status code: " + response.getStatus().getCode());
                    return Action.goTo(AuthOutcome.FALSE.name()).build();
                }
                String status = getResponseStatus(response);
                logger.debug(loggerPrefix + "Auth response (ID: " + responseId + "): " + status);
                // Validate the response and go to True outcome
                if (status.equals("accept")) {
                    return Action.goTo(AuthOutcome.TRUE.name()).build();
                }
                return Action.goTo(AuthOutcome.FALSE.name()).build();
            }
            return Action.goTo(AuthOutcome.UNANSWERED.name()).build();
        } catch (Exception ex) {
            logger.error(loggerPrefix + "Exception occurred: " + ex.getStackTrace());
            context.getStateFor(this).putShared(loggerPrefix + "Exception", new Date() + ": " + ex.getMessage());
            return Action.goTo(AuthOutcome.ERROR.name()).build();
        } finally {
            if(this.pollingService != null && responseId != null) {
                this.pollingService.remove(responseId);
            }
        }

    }

    private String getResponseStatus(Response response)
            throws IOException {
        final JsonParser jsonParser = new JsonParser();
        final JsonObject jsonObject = jsonParser.parse(response.getEntity().getString()).getAsJsonObject();

        final String payload = jsonObject.get("payload").getAsString();
        final String signature = jsonObject.get("signature").getAsString();
        final String algorithm = jsonObject.get("algorithm").getAsString();

        if (!checkSignature(payload, signature, algorithm)) {
            logger.error(loggerPrefix + "Invalid signature");
            return "invalid";
        }
        return jsonParser.parse(new String(Base64.getDecoder().decode(payload), StandardCharsets.UTF_8))
                         .getAsJsonObject().get("authStatus").getAsString();
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
        } catch (Exception ex) {
            logger.error(loggerPrefix + "Exception occurred: " + ex.getStackTrace());
            return false;
        }
    }

    /**
     * The possible outcomes for the OctopusReturnNode.
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
        UNANSWERED,
        ERROR
    }

    /**
     * Defines the possible outcomes from this OctopusReturnNode.
     */
    public static class OctopusReturnOutcomeProvider implements org.forgerock.openam.auth.node.api.OutcomeProvider {
        @Override
        public List<Outcome> getOutcomes(PreferredLocales locales, JsonValue nodeAttributes) {
            ResourceBundle bundle = locales.getBundleInPreferredLocale(OctopusReturnNode.BUNDLE,
                                                                       OctopusReturnOutcomeProvider.class
                                                                               .getClassLoader());
            return ImmutableList.of(
                    new Outcome(AuthOutcome.TRUE.name(), bundle.getString("trueOutcome")),
                    new Outcome(AuthOutcome.FALSE.name(), bundle.getString("falseOutcome")),
                    new Outcome(AuthOutcome.UNANSWERED.name(), bundle.getString("unansweredOutcome")),
                    new Outcome(AuthOutcome.ERROR.name(), bundle.getString("errorOutcome")));
        }
    }
}