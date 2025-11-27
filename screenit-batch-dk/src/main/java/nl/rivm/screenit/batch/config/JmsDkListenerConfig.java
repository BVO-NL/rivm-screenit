package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import nl.rivm.screenit.batch.jms.listener.JMSDossierLegenListener;
import nl.rivm.screenit.batch.jms.listener.JMSVerwerkCdaBerichtListener;
import nl.rivm.screenit.batch.jms.listener.JMSVerwerkFitBerichtListener;
import nl.rivm.screenit.config.JmsConfig;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
@Profile("!test")
@AllArgsConstructor
public class JmsDkListenerConfig
{
	private final JmsConfig jmsConfig;

	private final JmsBatchBaseConfig jmsBatchBaseConfig;

	private final ColonDossierBaseService dossierService;

	@Bean
	public JMSVerwerkCdaBerichtListener verwerkColonCdaBerichtListener()
	{
		var listener = new JMSVerwerkCdaBerichtListener();
		listener.setBvo(Bevolkingsonderzoek.COLON);
		return listener;
	}

	@Bean
	public DefaultMessageListenerContainer verwerkIFobtBerichtListenerContainer(JMSVerwerkFitBerichtListener verwerkIFobtBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verwerkIFobtBerichtDestination());
		listenerContainer.setMessageListener(verwerkIFobtBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer verwerkCdaBerichtListenerContainer(JMSVerwerkCdaBerichtListener verwerkColonCdaBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verwerkColonCdaBerichtDestination());
		listenerContainer.setMessageListener(verwerkColonCdaBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer colonJobJmsContainer()
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.colonJobsDestination());
		listenerContainer.setMessageListener(jmsBatchBaseConfig.startJobMessageListener());
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer clientgegevensVerwijderenListenerContainer(JMSDossierLegenListener dossierLegenListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.colonDossierLegenDestination());
		listenerContainer.setMessageListener(dossierLegenListener);
		listenerContainer.setSessionTransacted(true);
		dossierLegenListener.setDossierService(dossierService);
		return listenerContainer;
	}

}
