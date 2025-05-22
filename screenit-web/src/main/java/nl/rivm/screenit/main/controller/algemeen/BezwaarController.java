package nl.rivm.screenit.main.controller.algemeen;

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
import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.algemeen.BezwaarHerstellenDto;
import nl.rivm.screenit.main.mappers.ClientMapper;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.UploadDocumentService;
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

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/algemeen/bezwaar")
public class BezwaarController
{
	private final BezwaarService bezwaarService;

	private final ClientService clientService;

	private final UploadDocumentService uploadDocumentService;

	private final ClientMapper clientMapper;

	@PostMapping(value = "/herstellen", consumes = "multipart/form-data")
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_BEZWAAR_BRP, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity herstellen(@ModelAttribute BezwaarHerstellenDto herstellenDto) throws IOException
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

		bezwaarService.bezwaarBRPIntrekken(ScreenitSession.get().getLoggedInAccount(), client, briefBestand);

		LOG.info("Bezwaar BRP ingetrokken");
		return ResponseEntity.ok().build();
	}

	@GetMapping("/clienten")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_BEZWAAR_BRP, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity getClienten(@RequestParam String bsn, @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate geboortedatum)
	{
		var account = ScreenitSession.get().getLoggedInInstellingGebruiker();
		var clienten = bezwaarService.getClientenMetBezwaarBrp(bsn, geboortedatum, account);
		var clientDtos = clienten.stream().map(clientMapper::clientToBezwaarDto).toList();
		return ResponseEntity.ok(clientDtos);
	}
}
