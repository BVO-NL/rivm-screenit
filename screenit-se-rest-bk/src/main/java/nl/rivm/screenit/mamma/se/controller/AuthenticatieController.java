package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import java.time.LocalDateTime;

import jakarta.servlet.http.HttpServletRequest;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.se.SELogin;
import nl.rivm.screenit.mamma.se.SERequestHeader;
import nl.rivm.screenit.mamma.se.dto.LoginDto;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.security.SERealm;
import nl.rivm.screenit.mamma.se.service.ConfiguratieService;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.lang.codec.Base64;
import org.springframework.data.util.Version;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;

@RestController
@RequestMapping("/api/authenticatie")
@RequiredArgsConstructor
public class AuthenticatieController extends AuthorizedController
{
	private static final String LOGGING = "logging";

	private static final SimpleBeanPropertyFilter FILTER_JSON_VELDEN_OUDE_VERSIE = SimpleBeanPropertyFilter.serializeAllExcept("organisatieMedewerkerId");

	private static final SimpleBeanPropertyFilter FILTER_JSON_VELDEN_NIEUWE_VERSIE = SimpleBeanPropertyFilter.serializeAllExcept("instellingGebruikerId");

	private static final String NIEUWE_VERSIE = "25.5.26";

	private final MammaScreeningsEenheidService screeningsEenheidService;

	private final LogService logService;

	private final SERealm seRealm;

	private final ConfiguratieService configuratieService;

	private final ObjectMapper objectMapper;

	@RequestMapping(value = "/inloggen/{genereerLogging}", method = RequestMethod.POST)
	public ResponseEntity login(@RequestHeader(value = "Authorization") String credentials,
		@PathVariable String genereerLogging,
		@RequestHeader(value = SERequestHeader.SE_PROXY_DATUMTIJD) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime proxyDatumTijd,
		@RequestHeader(value = "versie", required = false) String seVersie,
		@RequestHeader(value = "nfcServerVersie", required = false) String nfcServerVersie,
		@RequestHeader(value = "Yubikey") String yubikey, @RequestHeader(value = "navigatie") String navigatie, HttpServletRequest request) throws IOException
	{
		String base64Credentials = Base64.decodeToString(credentials.substring(6));
		String[] credentialArray = StringUtils.split(base64Credentials, ":", 2);
		if (proxyDatumTijd == null)
		{
			throw new IllegalArgumentException("Datum komt niet mee uit de request");
		}

		LoginDto loginDto;

		SELogin login = new SELogin();

		String seCode = getSeCode(request);
		if (seCode.isEmpty())
		{
			final String proxy_ip = request.getHeader("PROXY_IP");
			seCode = screeningsEenheidService.getSeCodeMetIpAdres(proxy_ip);
			if (seCode == null)
			{
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_SE_CODE_OPHALEN_MET_IPADRES, "Kon SE-Code niet vinden op basis van IP-adres: " + proxy_ip,
					Bevolkingsonderzoek.MAMMA);
				return ResponseEntity.status(HttpStatus.PRECONDITION_FAILED).build();
			}
		}
		loginDto = login.doLogin(seCode, proxyDatumTijd, credentialArray[0], credentialArray[1], yubikey, seVersie, nfcServerVersie, genereerLogging.equals(LOGGING));

		if (loginDto.isSuccess())
		{
			OrganisatieMedewerker ingelogdeOrganisatieMedewerker = login.getIngelogdeOrganisatieMedewerker();
			SeAutorisatieDto result = getSeRechtenOrganisatieMedewerker(ingelogdeOrganisatieMedewerker.getId());
			MammaScreeningsEenheid ingelogdeScreeningsEenheid = login.getIngelogdeScreeningsEenheid();
			Medewerker medewerker = ingelogdeOrganisatieMedewerker.getMedewerker();

			result.setDisplayName(NaamUtil.getNaamMedewerker(medewerker));
			result.setOrganisatieMedewerkerId(ingelogdeOrganisatieMedewerker.getId());
			result.setSeCode(ingelogdeScreeningsEenheid.getCode());
			result.setSeNaam(ingelogdeScreeningsEenheid.getNaam());
			result.setUsername(medewerker.getGebruikersnaam());
			result.setMedewerkercode(medewerker.getMedewerkercode().toString());
			result.setNavigatie(navigatie);

			configuratieService.voegParametersToe(result, seVersie, ingelogdeScreeningsEenheid);

			var filterProvider = new SimpleFilterProvider();
			filterProvider.addFilter("autorisatieFilter", isNieuweSEVersie(seVersie) ? FILTER_JSON_VELDEN_NIEUWE_VERSIE : FILTER_JSON_VELDEN_OUDE_VERSIE);

			return ResponseEntity.ok(objectMapper.readTree(objectMapper.writer(filterProvider).writeValueAsString(result)));
		}

		return ResponseEntity.status(HttpStatus.FORBIDDEN).body(loginDto);
	}

	private static boolean isNieuweSEVersie(String seVersie)
	{
		var startDash = seVersie.indexOf('-');
		if (startDash > 0)
		{
			seVersie = seVersie.substring(0, startDash);
		}
		return seVersie.startsWith("0") || Version.parse(seVersie).isGreaterThanOrEqualTo(Version.parse(NIEUWE_VERSIE));
	}

	@RequestMapping(value = "/uitloggen", method = RequestMethod.POST)
	public ResponseEntity logout(HttpServletRequest request)
	{
		OrganisatieMedewerker organisatieMedewerker = getOrganisatieMedewerker(request);
		if (organisatieMedewerker != null)
		{
			seRealm.clearCachedAuthorizationInfo(organisatieMedewerker);
		}
		SecurityUtils.getSubject().logout();
		return ResponseEntity.ok(true);
	}
}
