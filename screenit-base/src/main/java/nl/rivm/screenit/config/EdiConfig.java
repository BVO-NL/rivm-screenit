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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.edi.service.ValidatedMessageFactory;
import nl.rivm.screenit.edi.service.impl.EdiMessageServiceImpl;
import nl.rivm.screenit.edi.service.impl.ValidatedMessageFactoryImpl;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "edi")
@Setter
public class EdiConfig
{

	@Autowired
	private LogService logInformatieService;

	private String afleverAdres;

	private String fromAddress;

	private String applicationId;

	@Getter
	private Mail mail;

	@Getter
	private SMTP smtp;

	@Setter
	private static class Mail
	{
		@Getter
		private Relay relay;

		@Setter
		private static class Relay
		{
			private String ip;

			private Integer port;
		}
	}

	@Setter
	private static class SMTP
	{
		private boolean overSsl;

		@Getter
		private Auth auth;

		@Setter
		private static class Auth
		{
			private String password;

			private String username;
		}
	}

	@Bean
	EdiMessageServiceImpl ediMessageService()
	{
		var ediMessageService = new EdiMessageServiceImpl();
		ediMessageService.setLogInformatieService(logInformatieService);
		ediMessageService.setMailRelayIP(mailRelayIp());
		ediMessageService.setMailRelayPort(mailRelayPort());
		ediMessageService.setSmtpPort(smtpPort());
		ediMessageService.setSmtpOverSsl(smtpOverSsl());
		ediMessageService.setSmtpAuthUsername(smtpAuthUsername());
		ediMessageService.setSmtpAuthPassword(smtpAuthPassword());
		ediMessageService.setFromAddress(fromAddress());
		return ediMessageService;
	}

	@Bean
	ValidatedMessageFactory validatedMessageFactory()
	{
		var validatedMessageFactory = new ValidatedMessageFactoryImpl();
		validatedMessageFactory.setEdifactHandlingProperties("nl/rivm/screenit/edi/xml/edimessages.properties");
		validatedMessageFactory.setApplicationId(applicationId());
		validatedMessageFactory.init();
		return validatedMessageFactory;
	}

	@Bean
	String jndiSmtpBindIP()
	{
		return "";
	}

	@Bean
	String smtpIP()
	{
		return "";
	}

	@Bean
	Integer smtpPort()
	{
		return 0;
	}

	@Bean
	String mailRelayIp()
	{
		return mail != null ? mail.relay.ip : "";
	}

	@Bean
	Integer mailRelayPort()
	{
		return mail != null ? mail.relay.port : 0;
	}

	@Bean
	Boolean smtpOverSsl()
	{
		return smtp != null && smtp.overSsl;
	}

	@Bean
	String smtpAuthUsername()
	{
		return smtp != null && smtp.auth != null ? smtp.auth.username : "";
	}

	@Bean
	String smtpAuthPassword()
	{
		return smtp != null && smtp.auth != null ? smtp.auth.password : "";
	}

	@Bean
	String fromAddress()
	{
		return StringUtils.defaultIfBlank(fromAddress, "");
	}

	@Bean
	String ediAfleverAdres()
	{
		return StringUtils.defaultIfBlank(afleverAdres, "devnull@topicus.nl");
	}

	@Bean
	String applicationId()
	{
		return StringUtils.defaultIfBlank(applicationId, "");
	}
}
