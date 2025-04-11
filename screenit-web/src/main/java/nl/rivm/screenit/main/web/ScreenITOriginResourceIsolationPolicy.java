package nl.rivm.screenit.main.web;

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

import java.util.Locale;

import lombok.extern.slf4j.Slf4j;

import org.apache.logging.log4j.util.Strings;
import org.apache.wicket.protocol.http.IResourceIsolationPolicy;
import org.apache.wicket.protocol.http.OriginResourceIsolationPolicy;
import org.apache.wicket.request.component.IRequestablePage;
import org.apache.wicket.request.http.WebRequest;

import jakarta.servlet.http.HttpServletRequest;

@Slf4j
public class ScreenITOriginResourceIsolationPolicy extends OriginResourceIsolationPolicy
{

	@Override
	public IResourceIsolationPolicy.ResourceIsolationOutcome isRequestAllowed(HttpServletRequest request,
		IRequestablePage targetPage)
	{
		String sourceUri = getSourceUri(request);

		if (sourceUri == null || sourceUri.isEmpty())
		{
			LOG.debug("Source URI not present in request to {}", request.getPathInfo());
			return IResourceIsolationPolicy.ResourceIsolationOutcome.UNKNOWN;
		}
		sourceUri = sourceUri.toLowerCase(Locale.ROOT);

		if (isWhitelistedHost(sourceUri))
		{
			return IResourceIsolationPolicy.ResourceIsolationOutcome.ALLOWED;
		}

		if (!isLocalOrigin(request, sourceUri))
		{
			LOG.debug("Source URI conflicts with request origin");
			return IResourceIsolationPolicy.ResourceIsolationOutcome.DISALLOWED;
		}

		return IResourceIsolationPolicy.ResourceIsolationOutcome.ALLOWED;
	}

	private String getSourceUri(HttpServletRequest containerRequest)
	{
		String sourceUri = containerRequest.getHeader(WebRequest.HEADER_ORIGIN);
		if (Strings.isEmpty(sourceUri))
		{
			sourceUri = containerRequest.getHeader(WebRequest.HEADER_REFERER);
		}

		if (sourceUri != null && sourceUri.startsWith("https") && "http".equals(containerRequest.getScheme()))
		{
			sourceUri = sourceUri.replace("https", "http");
			LOG.debug("Https vervangen voor http:" + sourceUri);
		}

		return normalizeUri(sourceUri);
	}
}
