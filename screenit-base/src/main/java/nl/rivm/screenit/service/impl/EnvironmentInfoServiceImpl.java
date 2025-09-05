package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.EnvironmentInfoService;

import org.springframework.boot.info.BuildProperties;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.util.DateUtil.LOCAL_DATE_TIME_FORMAT;
import static nl.rivm.screenit.util.DateUtil.SCREENIT_DEFAULT_ZONE;

@Service
@Slf4j
public class EnvironmentInfoServiceImpl implements EnvironmentInfoService
{
	@Getter
	private final String version;

	@Getter
	private final String buildTime;

	public EnvironmentInfoServiceImpl(BuildProperties buildProperties)
	{
		version = buildProperties.getVersion();
		buildTime = buildProperties.getTime().atZone(SCREENIT_DEFAULT_ZONE).format(LOCAL_DATE_TIME_FORMAT);
		LOG.info("ScreenIT versie: {}, buildTime: {}", version, buildTime);
	}
}
