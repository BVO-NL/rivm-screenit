package nl.rivm.screenit.mamma.se.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.SERequestHeader;

import org.apache.commons.lang.math.NumberUtils;
import org.slf4j.MDC;

import jakarta.servlet.Filter;

public class SELogFilter implements Filter
{
	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		if (request instanceof HttpServletRequest)
		{
			HttpServletRequest httpRequest = (HttpServletRequest) request;

			MDC.put("SE", httpRequest.getHeader(SERequestHeader.SE_CODE));
			MDC.put("PROXY-DT", httpRequest.getHeader(SERequestHeader.SE_PROXY_DATUMTIJD));

			String accountId = httpRequest.getHeader(SERequestHeader.ACCOUNT_ID);
			if (NumberUtils.isNumber(accountId))
			{
				MDC.put("A", "M" + accountId);
			}
		}
		chain.doFilter(request, response);

		MDC.remove("A");
		MDC.remove("SE");
		MDC.remove("PROXY-DT");
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{

	}

	@Override
	public void destroy()
	{

	}

}
