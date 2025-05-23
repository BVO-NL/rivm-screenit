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

import java.io.IOException;

import org.springframework.web.filter.CommonsRequestLoggingFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class HttpRequestResponseLoggingFilter extends CommonsRequestLoggingFilter
{

	public HttpRequestResponseLoggingFilter()
	{
		setIncludeQueryString(true);

	}

	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException
	{

		boolean isFirstRequest = !isAsyncDispatch(request);

		boolean shouldLog = shouldLog(request);
		if (shouldLog && isFirstRequest)
		{
			logger.debug(createMessage(request, "API Request [", "]"));
		}

		try
		{
			filterChain.doFilter(request, response);
		}
		finally
		{
			if (shouldLog && !isAsyncStarted(request))
			{
				logger.debug(createMessage(request, "API Response on [", "]") + " with statusCode=" + response.getStatus());
			}
		}
	}
}
