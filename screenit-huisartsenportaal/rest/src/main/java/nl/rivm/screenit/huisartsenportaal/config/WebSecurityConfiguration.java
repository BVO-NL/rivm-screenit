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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.filter.JwtAuthenticationFilter;
import nl.rivm.screenit.webcommons.config.CsrfCustomAccessDeniedHandler;
import nl.rivm.screenit.webcommons.config.SpaCsrfTokenRequestHandler;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;

import static org.springframework.security.config.Customizer.withDefaults;

@Configuration
@EnableWebSecurity
@AllArgsConstructor
public class WebSecurityConfiguration
{
	private final JwtAuthenticationFilter jwtAuthenticationFilter;

	private final AuthenticationProvider authenticationProvider;

	@Bean
	public SecurityFilterChain securityFilterChain(HttpSecurity httpSecurity) throws Exception
	{
		httpSecurity
			.cors(withDefaults())
			.csrf(csrf ->
				{
					var repository = CookieCsrfTokenRepository.withHttpOnlyFalse();
					repository.setCookiePath("/");
					csrf.csrfTokenRepository(repository)
						.csrfTokenRequestHandler(new SpaCsrfTokenRequestHandler());
				}
			)
			.exceptionHandling(exceptionHandling -> exceptionHandling.accessDeniedHandler(new CsrfCustomAccessDeniedHandler()))
			.authorizeHttpRequests(authorize -> authorize
				.requestMatchers("/auth/**", "/build", "/status")
				.permitAll()
				.anyRequest()
				.authenticated()
			)
			.authenticationProvider(authenticationProvider)
			.addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class)
			.sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
			.headers(headers ->
				headers
					.contentSecurityPolicy(policy -> policy.policyDirectives(
						"form-action 'self'; frame-ancestors 'self'; default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'")
					)
					.referrerPolicy(policy -> policy.policy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN))
					.contentTypeOptions(withDefaults())
			);
		return httpSecurity.build();
	}
}
