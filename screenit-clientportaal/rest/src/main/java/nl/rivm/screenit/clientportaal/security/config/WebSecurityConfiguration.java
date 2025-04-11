package nl.rivm.screenit.clientportaal.security.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import nl.rivm.screenit.clientportaal.filter.MDCLogFilter;
import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetailsService;
import nl.rivm.screenit.webcommons.config.CsrfCustomAccessDeniedHandler;
import nl.rivm.screenit.webcommons.config.SpaCsrfTokenRequestHandler;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;

import static org.springframework.security.config.Customizer.withDefaults;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity(securedEnabled = true)
@AllArgsConstructor
public class WebSecurityConfiguration
{
	private final ScreenitUserDetailsService userDetailsService;

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
				.anyRequest()
				.authenticated()
			)
			.sessionManagement(management -> management.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
			.headers(headers -> headers
				.contentSecurityPolicy(
					policy -> policy.policyDirectives("default-src 'self'; frame-src 'none'; frame-ancestors 'none'; upgrade-insecure-requests; object-src 'none';"))
				.referrerPolicy(policy -> policy.policy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN))
				.contentTypeOptions(withDefaults())
			)
			.oauth2ResourceServer(server -> server.jwt(jwt -> jwt.jwtAuthenticationConverter(new ScreenItJwtAuthenticationConverter(userDetailsService))))
			.addFilterAfter(new MDCLogFilter(), BearerTokenAuthenticationFilter.class);

		return httpSecurity.build();
	}

	@Autowired
	protected void configure(AuthenticationManagerBuilder authenticationManagerBuilder) throws Exception
	{
		authenticationManagerBuilder
			.userDetailsService(userDetailsService)
			.passwordEncoder(new BCryptPasswordEncoder());
	}
}
