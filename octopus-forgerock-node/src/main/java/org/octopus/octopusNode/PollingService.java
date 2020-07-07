package org.octopus.octopusNode;

import java.util.concurrent.ExecutionException;

import org.forgerock.http.protocol.Response;
import org.forgerock.util.promise.NeverThrowsException;
import org.forgerock.util.promise.Promise;

public interface PollingService {

	String OCTOPUS_RESPONSE_ID = "octopus_response_id";

	void put(String guid, Promise<Response, NeverThrowsException> promise);

	Promise<Response, NeverThrowsException> get(String guid) throws ExecutionException;

	void remove(String guid);

}
