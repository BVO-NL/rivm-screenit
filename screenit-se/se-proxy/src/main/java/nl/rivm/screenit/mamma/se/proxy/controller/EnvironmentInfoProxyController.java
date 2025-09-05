package nl.rivm.screenit.mamma.se.proxy.controller;

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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.se.proxy.model.EnvironmentInfoDto;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.EnvironmentInfoService;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.PersistableTransactionService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/environmentInfo")
@RequiredArgsConstructor
public class EnvironmentInfoProxyController
{
	private final EnvironmentInfoService environmentInfoService;

	private final LogischeSessieService logischeSessieService;

	private final PersistableTransactionService persistableTransactionService;

	private final SeDaglijstService seDaglijstService;

	private final ProxyService proxyService;

	private final ConfiguratieService configuratieService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<EnvironmentInfoDto> readBuildInfo(HttpSession httpSession, HttpServletRequest request)
	{
		var environmentInfo = new EnvironmentInfoDto();
		environmentInfo.setVersion(environmentInfoService.getVersion());
		environmentInfo.setTimestamp(environmentInfoService.getBuildTime());
		environmentInfo.setEnvironment(environmentInfoService.getEnvironmentName());
		environmentInfo.setNfcEnabled(String.valueOf(environmentInfoService.isNfcEnabled()));
		environmentInfo.setHuidigWerkstationIpAdres(request.getRemoteAddr());
		environmentInfo.setMagUpdaten(!logischeSessieService.zijnErNietVerlopenSessies() && !persistableTransactionService.zijnErWachtendeTransacties());
		environmentInfo.setDagenInDaglijstCache(seDaglijstService.dagenInCache());
		environmentInfo.setCacheVulling(proxyService.cacheVullingInfo());
		environmentInfo.setDagenDaglijstOphalenLimiet(configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN));
		environmentInfo.setTomosyntheseMogelijk(configuratieService.getConfiguratieBooleanValue(SeConfiguratieKey.TOMOSYNTHESE_MOGELIJK, false));
		return ResponseEntity.ok(environmentInfo);
	}
}
