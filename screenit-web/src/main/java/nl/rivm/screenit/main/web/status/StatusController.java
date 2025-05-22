package nl.rivm.screenit.main.web.status;

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
import java.io.InputStream;
import java.util.Properties;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.repository.algemeen.PreferenceItemRepository;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;

import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

@Slf4j
@RestController
@RequiredArgsConstructor
public class StatusController
{
	@Autowired
	private String applicationInstance;

	private final PreferenceItemRepository preferenceItemRepository;

	private static final String ONBEKEND = "Onbekend";

	private static final String OK = "OK";

	@GetMapping(value = "/status", produces = APPLICATION_JSON_VALUE)
	protected String getStatus()
	{
		LOG.debug("Er is een GET verzoek binnengekomen op de Status pagina");
		return maakResponse();
	}

	private String maakResponse()
	{

		var prefItemCount = preferenceItemRepository.count();
		var databaseStatus = prefItemCount > 0 ? OK : ONBEKEND;

		var applicatieVersie = readVersie();
		applicatieVersie = StringUtils.isBlank(applicatieVersie) ? ONBEKEND : applicatieVersie;

		var node = new ObjectMapper().createObjectNode();
		node.put("versie", applicatieVersie);
		node.put("instantie", applicationInstance);
		node.put("databaseStatus", databaseStatus);

		return node.toString();
	}

	private String readVersie()
	{
		var versieBuilder = new StringBuilder();
		var applicationProperties = new Properties();
		try (InputStream resourceAsStream = this.getClass().getResourceAsStream("/build-info.properties"))
		{
			applicationProperties.load(resourceAsStream);
			var version = applicationProperties.getProperty("build.version");
			versieBuilder.append(version);
			if (!version.endsWith("SNAPSHOT"))
			{
				String buildnumber = applicationProperties.getProperty("build.number");

				if (!"${BUILD_NUMBER}".equals(buildnumber))
				{
					versieBuilder.append("-").append(buildnumber);
				}
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij laden van build-info.properties (voor versienummer)");
		}
		return versieBuilder.toString();
	}
}
