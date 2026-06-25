package nl.rivm.screenit.main.web.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Locale;

import jakarta.persistence.EntityManagerFactory;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.LocaleResolver;
import nl.rivm.screenit.main.service.LocaleResolverService;
import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;

import org.apache.wicket.Session;
import org.slf4j.MDC;
import org.springframework.orm.jpa.EntityManagerHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

@Slf4j
public class LogFilter implements Filter
{
	private WebApplicationContext webApplicationContext;

	private LocaleResolverService localeResolverService;

	private static final LocaleResolver DEFAULT_LOCALE_RESOLVER = () -> Locale.ENGLISH;

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
		webApplicationContext = WebApplicationContextUtils.getRequiredWebApplicationContext(filterConfig.getServletContext());

		localeResolverService = webApplicationContext.getBean(LocaleResolverService.class);
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{

		var hasOpenEntityManagerSession = hasOpenSession();

		var localeResolver = DEFAULT_LOCALE_RESOLVER;

		if (hasOpenEntityManagerSession && request instanceof HttpServletRequest httpRequest)
		{

			var httpSession = httpRequest.getSession(true);

			var attributeName = ScreenitApplication.getSessionAttributePrefix() + Session.SESSION_ATTRIBUTE_NAME;
			var session = (ScreenitSession) httpSession.getAttribute(attributeName);

			if (session != null)
			{
				Class<?> ingelogdAccountClass = session.getLoggedInAccountClass();
				if (ingelogdAccountClass != null)
				{
					String prefix = "";
					if (Medewerker.class.isAssignableFrom(ingelogdAccountClass))
					{
						prefix = "G";
					}
					if (OrganisatieMedewerker.class.isAssignableFrom(ingelogdAccountClass))
					{
						prefix = "M";
					}
					else if (Client.class.isAssignableFrom(ingelogdAccountClass))
					{
						prefix = "C";
					}
					MDC.put("A", prefix + session.getLoggedInAccountId());
				}

				localeResolver = session::getLocale;
			}
		}

		try
		{
			localeResolverService.setLocaleResolver(localeResolver);
			chain.doFilter(request, response);
		}
		finally
		{
			localeResolverService.setLocaleResolver(null);
			if (hasOpenEntityManagerSession)
			{
				MDC.remove("A");
			}
		}
	}

	private boolean hasOpenSession()
	{
		var entityManagerFactory = webApplicationContext.getBean("entityManagerFactory", EntityManagerFactory.class);
		var entityManagerHolder = (EntityManagerHolder) TransactionSynchronizationManager.getResource(entityManagerFactory);
		return entityManagerHolder != null && entityManagerHolder.getEntityManager().isOpen();
	}

	@Override
	public void destroy()
	{

	}
}
