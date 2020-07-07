package org.octopus.octopusNode;

import static org.forgerock.http.handler.HttpClientHandler.OPTION_SO_TIMEOUT;

import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import org.forgerock.http.HttpApplicationException;
import org.forgerock.http.handler.HttpClientHandler;
import org.forgerock.util.Options;
import org.forgerock.util.time.Duration;

import com.google.inject.Singleton;

@Singleton
public class HttpClientServiceImpl implements HttpClientService {

    private HttpClientHandler client;

    @Inject
    public HttpClientServiceImpl() throws HttpApplicationException {
        Options options = Options.defaultOptions();
        Duration duration = Duration.duration(2, TimeUnit.MINUTES);
        options.set(OPTION_SO_TIMEOUT, duration);
        this.client = new HttpClientHandler(options);
    }

    @Override
    public HttpClientHandler getClient() {
        return this.client;
    }
}
