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

import java.io.IOException;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.extern.slf4j.Slf4j;

import org.springframework.web.filter.GenericFilterBean;

@Slf4j
public class SecurityLogFilter extends GenericFilterBean
{

	private static final String AUTH_HEADER = "WWW-Authenticate";

	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException
	{
		filterChain.doFilter(servletRequest, servletResponse);
		var request = (HttpServletRequest) servletRequest;
		var response = (HttpServletResponse) servletResponse;
		if (response.containsHeader(AUTH_HEADER))
		{
			var value = response.getHeader(AUTH_HEADER);
			if (value != null && value.length() > "Bearer ".length())
			{
				LOG.warn("Auth gefaald voor user[{}], request[{}]: {}", request.getUserPrincipal(), request.getRequestURI(),
					value.substring(7));
			}
		}
	}
}
