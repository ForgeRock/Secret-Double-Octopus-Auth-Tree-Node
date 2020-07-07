package org.octopus.octopusNode;

import org.forgerock.http.handler.HttpClientHandler;

public interface HttpClientService {
    HttpClientHandler getClient();
}
