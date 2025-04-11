package nl.rivm.screenit.huisartsenportaal.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.exception.UserNotFoundException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Medewerker;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.filter.CommonsRequestLoggingFilter;

@Configuration
@AllArgsConstructor
public class ApplicationConfiguration
{
	private final HuisartsRepository huisartsRepository;

	@Bean
	BCryptPasswordEncoder passwordEncoder()
	{
		return new BCryptPasswordEncoder();
	}

	@Bean
	public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception
	{
		return config.getAuthenticationManager();
	}

	@Bean
	public UserDetailsService userDetailsService()
	{
		return (String username) ->
		{
			Huisarts huisarts;
			try
			{
				var gebruikersnaam = Long.parseLong(username);
				huisarts = huisartsRepository.findByHuisartsportaalId(gebruikersnaam);
			}
			catch (NumberFormatException e)
			{
				huisarts = huisartsRepository.findByGebruikersnaam(username);
			}

			if (huisarts == null || !huisarts.isAccountNonExpired())
			{
				throw new UserNotFoundException("Inloggen mislukt. Gebruiker niet gevonden.");
			}
			if (AanmeldStatus.GEREGISTREERD != huisarts.getAanmeldStatus() && huisarts.getInlogCode() != null)
			{
				throw new CredentialsExpiredException("U moet zich eerst opnieuw registreren voor u weer kunt inloggen. Volg de instructies die u per brief heeft ontvangen.");
			}
			if (AanmeldStatus.GEREGISTREERD == huisarts.getAanmeldStatus() && huisarts.getInlogCode() != null)
			{
				throw new CredentialsExpiredException("U heeft een nieuw wachtwoord aangevraagd. Volg de instructies die u per e-mail heeft ontvangen.");
			}
			if (!huisarts.isAccountNonLocked())
			{
				var remainingMinutes = getRemainingMinutes(huisarts);
				throw new LockedException("Inloggen mislukt. Uw account is voor " + remainingMinutes + " minuten geblokkeerd.");
			}

			return huisarts;
		};
	}

	private static long getRemainingMinutes(Huisarts huisarts)
	{
		var lastAttempt = DateUtil.toLocalDateTime(huisarts.getLastAttemptDate());
		var lockdownTime = LocalDateTime.now().minusMinutes(Medewerker.MAX_LOCKED);
		return ChronoUnit.MINUTES.between(lastAttempt, lockdownTime);
	}

	@Bean
	AuthenticationProvider authenticationProvider()
	{
		var authProvider = new DaoAuthenticationProvider();

		authProvider.setUserDetailsService(userDetailsService());
		authProvider.setPasswordEncoder(passwordEncoder());

		return authProvider;
	}

	@Bean
	public CommonsRequestLoggingFilter logFilter()
	{
		return new HttpRequestResponseLoggingFilter();
	}
}
