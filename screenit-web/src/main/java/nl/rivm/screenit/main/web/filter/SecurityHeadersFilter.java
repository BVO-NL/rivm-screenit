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

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.ThreadContext;
import org.apache.wicket.csp.CSPDirective;
import org.apache.wicket.csp.CSPHeaderMode;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.request.http.WebResponse;

@Slf4j
public class SecurityHeadersFilter implements Filter
{
	private static final String DEFAULT_CONTENT_SECURITY_POLICY_WITH_UNSAFE_SCRIPT_INLINE = "form-action 'self'; frame-ancestors 'self'; default-src 'self'; " +
		"script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; object-src 'self'";

	private static final String CONTENT_SECURITY_POLICY_HEADER = "Content-Security-Policy";

	private static final String REQUEST_ATTR_CSP_BUFFER = "SecurityHeadersFilter.cspBuffer";

	public static void allowUnsafeInlineSecurityPolicy(WebResponse response)
	{
		response.setHeader(CONTENT_SECURITY_POLICY_HEADER, SecurityHeadersFilter.DEFAULT_CONTENT_SECURITY_POLICY_WITH_UNSAFE_SCRIPT_INLINE);
		zetCspInRequestBuffer(SecurityHeadersFilter.DEFAULT_CONTENT_SECURITY_POLICY_WITH_UNSAFE_SCRIPT_INLINE);
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
	}

	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException
	{
		HttpServletRequest request = (HttpServletRequest) servletRequest;
		HttpServletResponse response = (HttpServletResponse) servletResponse;

		LOG.debug("Invoking SecurityHeadersFilter for " + request.getRequestURI());

		response.setHeader("X-Frame-Options", "sameorigin");
		response.setHeader("X-XSS-Protection", "1; mode=block");
		response.setHeader("X-Content-Type-Options", "nosniff");
		response.setHeader("Referrer-Policy", "same-origin");

		filterChain.doFilter(servletRequest, servletResponse);
	}

	public static void voegSrcToeAanContentSecurityPolicy(WebResponse response, String src, CSPDirective srcType)
	{
		var httpServletResponse = (HttpServletResponse) response.getContainerResponse();
		var bestaandeCsp = leesBestaandeCsp(httpServletResponse);

		var directive = srcType.getValue();
		var startIndex = bestaandeCsp.indexOf(directive);
		if (startIndex == -1)
		{
			var nieuweCsp = bestaandeCsp.trim();
			if (!nieuweCsp.endsWith(";"))
			{
				nieuweCsp += ";";
			}
			nieuweCsp += " " + directive + " " + src + ";";
			response.setHeader(CONTENT_SECURITY_POLICY_HEADER, nieuweCsp);
			httpServletResponse.setHeader(CONTENT_SECURITY_POLICY_HEADER, nieuweCsp);
			zetCspInRequestBuffer(nieuweCsp);
			return;
		}
		var endIndex = bestaandeCsp.indexOf(";", startIndex);
		if (endIndex == -1)
		{
			endIndex = bestaandeCsp.length();
		}
		var directiveString = bestaandeCsp.substring(startIndex, endIndex);
		if (directiveString.contains(src))
		{
			response.setHeader(CONTENT_SECURITY_POLICY_HEADER, bestaandeCsp);
			httpServletResponse.setHeader(CONTENT_SECURITY_POLICY_HEADER, bestaandeCsp);
			zetCspInRequestBuffer(bestaandeCsp);
			return;
		}
		var nieuweDirectiveString = directiveString + " " + src;
		var nieuweCsp = bestaandeCsp.substring(0, startIndex) + nieuweDirectiveString + bestaandeCsp.substring(endIndex);
		response.setHeader(CONTENT_SECURITY_POLICY_HEADER, nieuweCsp);
		httpServletResponse.setHeader(CONTENT_SECURITY_POLICY_HEADER, nieuweCsp);
		zetCspInRequestBuffer(nieuweCsp);
	}

	private static String leesBestaandeCsp(HttpServletResponse httpServletResponse)
	{
		var requestCycle = ThreadContext.getRequestCycle();
		if (requestCycle != null)
		{
			var containerRequest = requestCycle.getRequest().getContainerRequest();
			if (containerRequest instanceof HttpServletRequest httpServletRequest)
			{
				var bufferedCsp = (String) httpServletRequest.getAttribute(REQUEST_ATTR_CSP_BUFFER);
				if (bufferedCsp != null)
				{
					return bufferedCsp;
				}
			}
		}

		var bestaandeCsp = httpServletResponse.getHeader(CONTENT_SECURITY_POLICY_HEADER);
		if (bestaandeCsp != null)
		{
			return bestaandeCsp;
		}

		var cspSettings = WebApplication.get().getCspSettings();
		var cspHeaderConfiguration = cspSettings.getConfiguration().get(CSPHeaderMode.BLOCKING);
		return cspHeaderConfiguration.renderHeaderValue(cspSettings, ThreadContext.getRequestCycle());
	}

	private static void zetCspInRequestBuffer(String csp)
	{
		var requestCycle = ThreadContext.getRequestCycle();
		if (requestCycle == null)
		{
			return;
		}
		var containerRequest = requestCycle.getRequest().getContainerRequest();
		if (containerRequest instanceof HttpServletRequest httpServletRequest)
		{
			httpServletRequest.setAttribute(REQUEST_ATTR_CSP_BUFFER, csp);
		}
	}

	@Override
	public void destroy()
	{
	}
}
