package org.octopus.octopusNode;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;

import org.forgerock.http.protocol.Response;
import org.forgerock.util.promise.NeverThrowsException;
import org.forgerock.util.promise.Promise;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.inject.Singleton;

@Singleton
public class PollingServiceImpl implements PollingService {

    private LoadingCache<String, Promise<Response, NeverThrowsException>> cache;

    @Inject
    public PollingServiceImpl() {
    	// Create a thread safe cache that will expire our Promise after 2 minutes (time should be configurable via node)
        cache = CacheBuilder.newBuilder().maximumSize(10000).expireAfterWrite(2, TimeUnit.MINUTES).build(
                new CacheLoader<String, Promise<Response, NeverThrowsException>>() {
                    public Promise<Response, NeverThrowsException> load(String key) throws ExecutionException {
                    	return cache.get(key);
                    }});
    }

    @Override
    public void put(String uid, Promise<Response, NeverThrowsException> promise) {
        cache.put(uid, promise);
    }

    @Override
    public Promise<Response, NeverThrowsException> get(String uid) throws ExecutionException {
        return cache.get(uid);
    }

    @Override
    public void remove(String uid) {
        cache.invalidate(uid);
    }
}
