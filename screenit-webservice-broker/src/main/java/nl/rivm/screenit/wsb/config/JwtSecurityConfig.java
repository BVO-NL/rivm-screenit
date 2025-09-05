package nl.rivm.screenit.wsb.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang.StringUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.mapping.GrantedAuthoritiesMapper;
import org.springframework.security.core.authority.mapping.SimpleAuthorityMapper;
import org.springframework.security.oauth2.core.DelegatingOAuth2TokenValidator;
import org.springframework.security.oauth2.core.OAuth2TokenValidator;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtClaimValidator;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.JwtDecoders;
import org.springframework.security.oauth2.jwt.JwtValidators;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter;
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;
import org.springframework.security.web.SecurityFilterChain;

import static org.springframework.security.oauth2.jwt.JwtClaimNames.AUD;

@Slf4j
@Configuration
@EnableWebSecurity
@EnableMethodSecurity
@Profile("!test")
public class JwtSecurityConfig
{

	private static final String SERVER_VALIDATOR_AUDIENCE = "screenit";

	public static final String REALM_ACCESS = "realm_access";

	public static final String ROLES = "roles";

	public static final String ROLE_PREFIX = "ROLE_";

	@Bean
	public SecurityFilterChain securityFilterChain(HttpSecurity http, String inpakcentrumIdpIssuer)
		throws Exception
	{
		var heeftInpakcentrumIdpIssuer = StringUtils.isNotBlank(inpakcentrumIdpIssuer);
		var httpSecurity = http.csrf(AbstractHttpConfigurer::disable)
			.authorizeHttpRequests(authz ->
			{
				if (heeftInpakcentrumIdpIssuer)
				{
					authz.requestMatchers("/services/**").permitAll()
						.requestMatchers("/api/inpakcentrum/v2/**").hasRole("screenit_app").anyRequest().authenticated();
				}
				else
				{
					authz.anyRequest().permitAll();
				}
			})
			.addFilterBefore(new SecurityLogFilter(), BearerTokenAuthenticationFilter.class);

		if (heeftInpakcentrumIdpIssuer)
		{
			httpSecurity.oauth2ResourceServer(oauth2ResourceServer ->
				oauth2ResourceServer.jwt(jwt ->
					jwt.jwtAuthenticationConverter(jwtAuthenticationConverter())
				)
			);
		}
		return http.build();
	}

	@Bean
	JwtDecoder jwtDecoder(String inpakcentrumIdpIssuer)
	{
		if (StringUtils.isBlank(inpakcentrumIdpIssuer))
		{
			return null;
		}
		NimbusJwtDecoder jwtDecoder = JwtDecoders.fromIssuerLocation(inpakcentrumIdpIssuer);
		jwtDecoder.setJwtValidator(jwtValidator(inpakcentrumIdpIssuer, SERVER_VALIDATOR_AUDIENCE));
		return jwtDecoder;
	}

	@Bean
	JwtAuthenticationConverter jwtAuthenticationConverter()
	{
		var jwtAuthenticationConverter = new JwtAuthenticationConverter();
		jwtAuthenticationConverter.setJwtGrantedAuthoritiesConverter(this::getAuthorities);
		return jwtAuthenticationConverter;
	}

	@Bean
	GrantedAuthoritiesMapper authoritiesMapper()
	{
		var mapper = new SimpleAuthorityMapper();
		mapper.setPrefix("");
		mapper.setConvertToUpperCase(true);

		return mapper;
	}

	private Set<GrantedAuthority> getAuthorities(Jwt jwt)
	{
		var authorities = new LinkedHashSet<GrantedAuthority>();
		var realm = jwt.getClaimAsMap(REALM_ACCESS);
		if (realm == null)
		{
			return authorities;
		}
		var roles = (List<?>) realm.get(ROLES);
		if (roles != null)
		{
			roles.forEach(r -> authorities.add(() -> ROLE_PREFIX + r.toString()));
		}
		return authorities;
	}

	private OAuth2TokenValidator<Jwt> jwtValidator(String issuer, String audience)
	{
		var issuerValidator = JwtValidators.createDefaultWithIssuer(issuer);
		var audienceValidator = new JwtClaimValidator<List<String>>(AUD,
			aud ->
			{
				var isValid = aud != null && aud.contains(audience);
				if (!isValid)
				{
					LOG.error("JWT audience validatie gefaald. Verwacht: {}, Actueel: {}", audience, aud);
				}
				return isValid;
			});
		return new DelegatingOAuth2TokenValidator<>(issuerValidator, audienceValidator);
	}
}
