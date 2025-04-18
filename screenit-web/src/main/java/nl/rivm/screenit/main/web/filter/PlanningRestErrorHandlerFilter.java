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

import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningNietOperationeelPage;
import nl.rivm.screenit.util.rest.ScreenitRestErrorHandler;

import org.apache.wicket.RestartResponseAtInterceptPageException;
import org.apache.wicket.Session;
import org.springframework.http.HttpStatus;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

public class PlanningRestErrorHandlerFilter implements Filter
{
	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		if (request instanceof HttpServletRequest httpRequest)
		{
			HttpSession httpSession = httpRequest.getSession(true);

			String attributeName = ScreenitApplication.getSessionAttributePrefix() + Session.SESSION_ATTRIBUTE_NAME;
			final ScreenitSession session = (ScreenitSession) httpSession.getAttribute(attributeName);

			if (session != null && session.isInPlanningmodule())
			{
				ScreenitRestErrorHandler.set((httpStatus) ->
				{
					if (HttpStatus.LOCKED == httpStatus || HttpStatus.SERVICE_UNAVAILABLE == httpStatus)
					{
						throw new RestartResponseAtInterceptPageException(MammaPlanningNietOperationeelPage.class);
					}
				});
			}
		}

		try
		{
			chain.doFilter(request, response);
		}
		finally
		{
			ScreenitRestErrorHandler.set(null);
		}
	}

	@Override
	public void destroy()
	{

	}
}
