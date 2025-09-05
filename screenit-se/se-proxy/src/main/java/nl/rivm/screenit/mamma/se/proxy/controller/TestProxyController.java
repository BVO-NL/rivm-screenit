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

import jakarta.servlet.http.HttpSession;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.services.EnvironmentInfoService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/test")
@Slf4j
@RequiredArgsConstructor
public class TestProxyController
{
	private final SeRestSocketService socketService;

	private final SeStatusService statusService;

	private final EnvironmentInfoService environmentInfoService;

	@RequestMapping(value = "/offline", method = RequestMethod.POST)
	public ResponseEntity<String> gaOffline(HttpSession httpSession)
	{
		if (!environmentInfoService.isTestEnvironment())
		{
			LOG.warn("Omgeving is niet Test en er is een request uitgevoerd om de SE offline te halen.");
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}

		statusService.setTestOnline(false);
		socketService.closeSocket();
		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/online", method = RequestMethod.POST)
	public ResponseEntity<String> gaOnline(HttpSession httpSession)
	{
		if (!environmentInfoService.isTestEnvironment())
		{
			LOG.warn("Omgeving is niet Test en er is een request uitgevoerd om de SE online te zetten.");
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}

		statusService.setTestOnline(true);
		return ResponseEntity.ok().build();
	}
}
