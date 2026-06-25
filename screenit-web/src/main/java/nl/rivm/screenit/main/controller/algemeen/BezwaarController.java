package nl.rivm.screenit.main.controller.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.algemeen.BezwaarClientDto;
import nl.rivm.screenit.main.dto.algemeen.BezwaarHerstellenDto;
import nl.rivm.screenit.main.mappers.algemeen.ClientMapper;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/bezwaar")
@Tag(name = "Bezwaren", description = "Beheer van BRP-bezwaren van clienten")
public class BezwaarController
{
	private final BezwaarService bezwaarService;

	private final ClientService clientService;

	private final ClientMapper clientMapper;

	@PostMapping(value = "/herstellen", consumes = "multipart/form-data")
	@Operation(summary = "Herstel een BRP-bezwaar", description = "Trekt een bestaand BRP-bezwaar van een client in op basis van BSN, geboortedatum en een PDF-bestand.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "BRP-bezwaar succesvol hersteld"),
		@ApiResponse(responseCode = "400", description = "Ongeldige invoer opgegeven"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_BEZWAAR_BRP, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> herstellen(@ModelAttribute BezwaarHerstellenDto herstellenDto)
		throws IOException
	{
		var client = clientService.getClientByBsn(herstellenDto.getBsn());
		if (client == null)
		{
			throw new IllegalStateException("error.client.niet.gevonden");
		}

		var geboortedatumClient = DateUtil.formatShortDate(client.getPersoon().getGeboortedatum());
		if (!geboortedatumClient.equals(herstellenDto.getGeboortedatum()))
		{
			throw new IllegalStateException("error.client.niet.gevonden");
		}

		if (GbaStatus.BEZWAAR != client.getGbaStatus())
		{
			throw new IllegalStateException("error.client.geen.brp");
		}

		var briefBestand = herstellenDto.getBestand();
		if (briefBestand == null)
		{
			throw new IllegalStateException("error.bestand.verplicht");
		}
		if (!FileType.PDF.getAllowedContentTypes().contains(briefBestand.getContentType()))
		{
			throw new IllegalStateException("error.bestandtype.niet.toegestaan");
		}

		bezwaarService.bezwaarBRPIntrekken(ScreenitSession.get().getIngelogdAccount(), client, briefBestand);

		LOG.info("Bezwaar BRP ingetrokken");
		return ResponseEntity.ok().build();
	}

	@GetMapping("/clienten")
	@Operation(summary = "Zoek clienten met BRP-bezwaar", description = "Zoekt clienten met een actief BRP-bezwaar op basis van BSN en geboortedatum.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Clienten met BRP-bezwaar succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_BEZWAAR_BRP, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<BezwaarClientDto>> getClienten(@RequestParam String bsn,
		@DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam LocalDate geboortedatum)
	{
		var account = ScreenitSession.get().getIngelogdeOrganisatieMedewerker();
		var clienten = bezwaarService.getClientenMetBezwaarBrp(bsn, geboortedatum, account);
		var clientDtos = clienten.stream().map(clientMapper::clientToBezwaarDto).toList();
		return ResponseEntity.ok(clientDtos);
	}
}
