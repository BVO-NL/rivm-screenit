package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import jakarta.jms.Destination;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.command.ActiveMQQueue;
import org.apache.activemq.command.ActiveMQTopic;
import org.apache.activemq.pool.PooledConnectionFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;

@Configuration
@Profile("filler & !test")
public class JmsFillerConfig
{
	@Bean
	public Destination huisartsportaalDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.huisartsportaal.filler");
	}

	@Bean
	public Destination verwerkColonCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.cdaberichten.filler");
	}

	@Bean
	public Destination verwerkCervixCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.cdaberichten.filler");
	}

	@Bean
	public Destination verwerkMammaCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.cdaberichten.filler");
	}

	@Bean
	public Destination verwerkMammaIMSBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.hl7berichten.filler");
	}

	@Bean
	public Destination verzamelOnderzoekDataBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.verzamelondezoeksdata.filler");
	}

	@Bean
	public Destination uploadBeeldenVerzoekBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.uploadbeeldenverzoek.filler");
	}

	@Bean
	public Destination verwerkHpvBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.hpvberichten.filler");
	}

	@Bean
	public Destination verwerkIFobtBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.ifobtberichten.filler");
	}

	@Bean
	public Destination verwerkBulkHuisartsenDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.bulkhuisarsen.filler");
	}

	@Bean
	public Destination verwerkMammaSeRestDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.mamma.se.jms.listener.filler");
	}

	@Bean
	public Destination changeDateTimeOffsetDestination()
	{
		return new ActiveMQTopic("nl.rivm.screenit.algemeen.changedatetimeoffset.filler");
	}

	@Bean
	public Destination colonJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.jobs.filler");
	}

	@Bean
	public Destination generalisJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.generalis.jobs.filler");
	}

	@Bean
	public Destination cervixJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.jobs.filler");
	}

	@Bean
	public Destination mammaJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.jobs.filler");
	}

	@Bean
	public Destination huisartsportaalListenerDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.huisartsportaal.listener.filler");
	}

	@Bean
	public Destination batchServerStatusDestination()
	{
		return new ActiveMQTopic("nl.rivm.screenit.batch.batchServerStatus.filler");
	}

	@Bean
	public Destination quartzDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.quartz.filler");
	}

	@Bean
	public Destination colonDossierLegenDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.dossierlegen.filler");
	}

	@Bean
	public Destination cervixDossierLegenDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.dossierlegen.filler");
	}

	@Bean
	public Destination mammaDossierLegenDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.dossierlegen.filler");
	}

	@Bean(initMethod = "start", destroyMethod = "stop")
	public PooledConnectionFactory jmsFactory()
	{
		return new PooledConnectionFactory();
	}

	@Bean
	public ActiveMQConnectionFactory activeMQConnectionFactory()
	{
		return new ActiveMQConnectionFactory();
	}

	@Bean
	public JmsTemplate jmsTemplate()
	{
		var jmsTemplate = new JmsTemplate()
		{
			@Override
			public void send(String destinationName, MessageCreator messageCreator)
			{

			}
		};
		jmsTemplate.setConnectionFactory(jmsFactory());
		return jmsTemplate;
	}
}
