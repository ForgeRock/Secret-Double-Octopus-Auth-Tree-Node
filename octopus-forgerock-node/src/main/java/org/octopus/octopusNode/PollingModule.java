package org.octopus.octopusNode;

import com.google.inject.AbstractModule;

public class PollingModule extends AbstractModule {
	@Override
	protected void configure() {

		/*
		 * This tells Guice that whenever it sees a dependency on a PollingService, it
		 * should satisfy the dependency using a PollingServiceImpl.
		 */
		bind(PollingService.class).to(PollingServiceImpl.class);
	}
}