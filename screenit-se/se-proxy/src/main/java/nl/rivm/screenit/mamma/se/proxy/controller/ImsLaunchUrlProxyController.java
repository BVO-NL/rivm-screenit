package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.imsapi.service.MammaImsLaunchUrlGenerator;
import nl.rivm.screenit.mamma.se.proxy.model.ImsLaunchUrlContext;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/ims/launchUrl")
@RequiredArgsConstructor
public class ImsLaunchUrlProxyController
{
	private final LogischeSessieService logischeSessieService;

	private final ConfiguratieService configuratieService;

	private final MammaImsLaunchUrlGenerator launchUrlGenerator;

	@PostMapping("/login")
	public ResponseEntity<String> getLoginLaunchUrl(@RequestBody ImsLaunchUrlContext context, @RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie)
	{
		if (geldigeCredentials(yubikeyIdentificatie, context.gebruikersnaam()))
		{
			var launchUrl = launchUrlGenerator.generateLoginLaunchUrl(getEncryptieKey(), context.gebruikersnaam(), "Mbb", getSha1Mode());
			return ResponseEntity.ok(launchUrl);
		}

		return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
	}

	@PostMapping("/desktopSync")
	public ResponseEntity<String> getDesktopSyncLaunchUrl(@RequestBody ImsLaunchUrlContext context, @RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie)
	{
		if (geldigeCredentials(yubikeyIdentificatie, context.gebruikersnaam()))
		{
			var launchUrl = launchUrlGenerator.generateDesktopSyncLaunchUrl(getEncryptieKey(), context.gebruikersnaam(), "Mbb",
				context.bsn(), context.uitnodigingsNr(), getSha1Mode());
			return ResponseEntity.ok(launchUrl);
		}

		return ResponseEntity.status(HttpStatus.FORBIDDEN).build();

	}

	private boolean geldigeCredentials(String yubikeyIdentificatie, String gebruikersnaam)
	{
		return logischeSessieService.geldigeYubikey(yubikeyIdentificatie)
			&& StringUtils.isNotBlank(gebruikersnaam)
			&& gebruikersnaamMatchMetSessie(yubikeyIdentificatie, gebruikersnaam);
	}

	private boolean gebruikersnaamMatchMetSessie(String yubikeyIdentificatie, String gebruikersnaam)
	{

		var logischeSessie = logischeSessieService.getLogischeSessieMetIdentificatie(yubikeyIdentificatie);
		return logischeSessie == null || gebruikersnaam.equals(logischeSessie.getGebruikersnaam());
	}

	private String getEncryptieKey()
	{
		return configuratieService.getConfiguratieValue(SeConfiguratieKey.IMS_LAUNCH_URL_PASSWORD);
	}

	private Boolean getSha1Mode()
	{
		return configuratieService.getConfiguratieBooleanValue(SeConfiguratieKey.MAMMA_IMS_LAUNCH_URL_SHA1_MODE, true);
	}
}
