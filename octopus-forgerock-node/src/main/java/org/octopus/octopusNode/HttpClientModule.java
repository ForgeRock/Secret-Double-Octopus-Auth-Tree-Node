package org.octopus.octopusNode;

import com.google.inject.AbstractModule;

public class HttpClientModule extends AbstractModule {
	@Override
	protected void configure() {

		/*
		 * This tells Guice that whenever it sees a dependency on a PollingService, it
		 * should satisfy the dependency using a PollingServiceImpl.
		 */
		bind(HttpClientService.class).to(HttpClientServiceImpl.class);
	}
}