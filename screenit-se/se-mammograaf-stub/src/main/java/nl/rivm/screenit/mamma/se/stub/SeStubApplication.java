package nl.rivm.screenit.mamma.se.stub;

/*-
 * ========================LICENSE_START=================================
 * se-mammograaf-stub
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
import java.util.Properties;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.stub.model.BuildinfoDto;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;

@SpringBootApplication
@Slf4j
public class SeStubApplication
{

	private static BuildinfoDto buildinfo = null; 

	public static void main(String[] args)
	{
		new SpringApplicationBuilder()
			.sources(SeStubApplication.class)
			.run(args);
		LOG.info("Screenit versie {}", getBuildinfo().getVersion());
	}

	public static BuildinfoDto getBuildinfo()
	{
		if (buildinfo == null)
		{
			buildinfo = new BuildinfoDto();
			var applicationProperties = new Properties();
			try (var resourceAsStream = SeStubApplication.class.getResourceAsStream("/build-info.properties"))
			{
				applicationProperties.load(resourceAsStream);
				buildinfo.setVersion(applicationProperties.getProperty("build.version"));
			}
			catch (IOException e)
			{
				LOG.error("Fout bij laden van build-info.properties (voor versienummer)");
			}
		}
		return buildinfo;
	}
}
