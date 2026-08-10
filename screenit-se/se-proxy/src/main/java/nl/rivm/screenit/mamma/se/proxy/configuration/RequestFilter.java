package nl.rivm.screenit.mamma.se.proxy.configuration;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Component
@Order(1)
public class RequestFilter implements Filter
{
	private static final Logger LOG = LoggerFactory.getLogger(RequestFilter.class);

	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException
	{
		var startTime = System.currentTimeMillis();

		filterChain.doFilter(servletRequest, servletResponse);

		if (servletRequest instanceof HttpServletRequest && servletResponse instanceof HttpServletResponse)
		{
			var request = (HttpServletRequest) servletRequest;
			var response = (HttpServletResponse) servletResponse;
			if (!request.getRequestURI().equals("/api/authenticatie/identificeren"))
			{
				var duration = System.currentTimeMillis() - startTime;
				LOG.info(String.format("duration=%d, %s <= %s[%s]%s", duration, response.getStatus(), request.getRemoteAddr(), request.getMethod(), request.getRequestURI()));
			}
		}
	}
}
