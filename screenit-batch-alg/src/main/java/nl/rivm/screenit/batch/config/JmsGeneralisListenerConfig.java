package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.base.config.JmsBatchBaseConfig;
import nl.rivm.screenit.config.JmsConfig;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
@Profile("!test")
@AllArgsConstructor
public class JmsGeneralisListenerConfig
{
	private final JmsConfig jmsConfig;

	private final JmsBatchBaseConfig jmsBatchBaseConfig;

	@Bean
	public DefaultMessageListenerContainer generalisJobJmsContainer()
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.generalisJobsDestination());
		listenerContainer.setMessageListener(jmsBatchBaseConfig.startJobMessageListener());
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}
}
