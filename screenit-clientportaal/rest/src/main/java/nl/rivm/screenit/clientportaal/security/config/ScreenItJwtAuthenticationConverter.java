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

import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetails;
import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetailsService;

import org.springframework.core.convert.converter.Converter;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.oauth2.core.oidc.StandardClaimNames;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter;

@AllArgsConstructor
public class ScreenItJwtAuthenticationConverter implements Converter<Jwt, AbstractAuthenticationToken> {

	private final ScreenitUserDetailsService userDetailsService;

	@Override
	public AbstractAuthenticationToken convert(Jwt jwt)
	{
		var userDetails = extractAndRetrieveUserDetails(jwt);
		var authorities = new JwtGrantedAuthoritiesConverter().convert(jwt);

		return new ScreenItAuthenticationToken(userDetails, authorities);
	}

	private ScreenitUserDetails extractAndRetrieveUserDetails(Jwt jwt)
	{
		var username = jwt.getClaimAsString(StandardClaimNames.PREFERRED_USERNAME);
		return userDetailsService.loadUserByUsername(username.replace("screenit-test-user-", ""));
	}
}
