package nl.rivm.screenit.main.web.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.extern.slf4j.Slf4j;

import com.google.common.base.Stopwatch;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@Slf4j
public class MedewerkerportaalControllerLoggingFilter implements Filter
{

	@Override
	public void init(FilterConfig filterConfig)
	{
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
		throws IOException, ServletException
	{
		if (LOG.isDebugEnabled())
		{
			var httpServletRequest = (HttpServletRequest) request;
			var httpServletResponse = (HttpServletResponse) response;

			var stopWatch = Stopwatch.createStarted();
			try
			{
				chain.doFilter(request, response);
				LOG.debug("{}:{}|Response:{}|Duration:{}", httpServletRequest.getMethod(), httpServletRequest.getServletPath(), httpServletResponse.getStatus(), stopWatch.stop());
			}
			catch (Exception e)
			{
				LOG.error("Error in request: {}:{}|Duration:{}", httpServletRequest.getMethod(), httpServletRequest.getServletPath(), stopWatch.stop(), e);
				throw e;
			}
		}
		else
		{
			chain.doFilter(request, response);
		}
	}

	@Override
	public void destroy()
	{
	}
}
