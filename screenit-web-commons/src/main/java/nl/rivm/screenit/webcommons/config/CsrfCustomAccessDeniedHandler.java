package nl.rivm.screenit.webcommons.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-web-commons
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

import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.csrf.InvalidCsrfTokenException;
import org.springframework.security.web.csrf.MissingCsrfTokenException;

import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class CsrfCustomAccessDeniedHandler implements AccessDeniedHandler
{
	@Override
	public void handle(HttpServletRequest request, HttpServletResponse response, AccessDeniedException accessDeniedException) throws IOException
	{
		response.setStatus(HttpStatus.FORBIDDEN.value());
		response.setContentType("application/json");

		String message = "Onbekende fout";
		if (accessDeniedException instanceof InvalidCsrfTokenException)
		{
			message = "Het CSRF token mist in het request.";
		}
		else if (accessDeniedException instanceof MissingCsrfTokenException)
		{
			message = "De gebruikte CSRF token kon niet worden vergeleken met de verwachte waarde.";
		}
		var objectMapper = new ObjectMapper();
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		node.put("exception", accessDeniedException.getClass().getSimpleName());
		response.getWriter().write(node.toString());
	}
}
