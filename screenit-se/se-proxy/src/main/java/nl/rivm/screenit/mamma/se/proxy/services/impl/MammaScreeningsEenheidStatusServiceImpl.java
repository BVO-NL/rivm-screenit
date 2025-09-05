package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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

import java.time.LocalDateTime;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.se.proxy.model.SeStatusDto;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.EnvironmentInfoService;
import nl.rivm.screenit.mamma.se.proxy.services.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class MammaScreeningsEenheidStatusServiceImpl implements MammaScreeningsEenheidStatusService
{
	private final AchtergrondRequestService achtergrondRequestService;

	private final ProxyService proxyService;

	private final SeDaglijstService daglijstService;

	private final EnvironmentInfoService environmentInfoService;

	@Override
	public void maakStatusEnQueueRequestNaarCentraal()
	{
		proxyService.cacheVullingInfo();
		SeStatusDto statusDto = new SeStatusDto();
		statusDto.setVersie(environmentInfoService.getVersion());
		statusDto.setHuisartsenAanwezig(proxyService.huisartsenInCache());
		statusDto.setZorginstellingenAanwezig(proxyService.zorginstellingeninCache());
		statusDto.setMammografenAanwezig(proxyService.mammografenInCache());
		statusDto.setOfflineDaglijsten(daglijstService.dagenInCache());
		statusDto.setStatusMoment(LocalDateTime.now());
		achtergrondRequestService.queueStatusPostenRequest(statusDto);
		achtergrondRequestService.ensureRunning();
	}
}
