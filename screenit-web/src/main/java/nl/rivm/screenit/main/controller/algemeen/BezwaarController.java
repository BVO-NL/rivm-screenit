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

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.BezwaarClientDto;
import nl.rivm.screenit.main.dto.algemeen.BezwaarHerstellenDto;
import nl.rivm.screenit.main.dto.algemeen.BriefDto;
import nl.rivm.screenit.main.dto.algemeen.OnderzoeksresultatenActieDto;
import nl.rivm.screenit.main.dto.algemeen.VervangDocumentDto;
import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.mappers.algemeen.BriefMapper;
import nl.rivm.screenit.main.mappers.algemeen.ClientMapper;
import nl.rivm.screenit.main.mappers.algemeen.OnderzoeksresultatenActieMapper;
import nl.rivm.screenit.main.model.BriefActie;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.algemeen.BezwaarService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.repository.algemeen.BezwaarMomentRepository;
import nl.rivm.screenit.repository.algemeen.OnderzoeksresultatenActieRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/bezwaar")
@Tag(name = "Bezwaren", description = "Beheer van bezwaren van clienten")
public class BezwaarController extends BaseController
{
	private final BezwaarService bezwaarService;

	private final ClientService clientService;

	private final ClientMapper clientMapper;

	private OnderzoeksresultatenActieRepository onderzoeksresultatenActieRepository;

	private BezwaarMomentRepository bezwaarMomentRepository;

	private UploadDocumentService uploadDocumentService;

	private OnderzoeksresultatenActieMapper onderzoeksresultatenActieMapper;

	private BriefService briefService;

	private BriefMapper briefMapper;

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

		var briefBestand = valideerPdfBestand(herstellenDto.getBestand());

		bezwaarService.bezwaarBRPIntrekken(getIngelogdeGebruiker(), client, briefBestand);

		LOG.info("Bezwaar BRP ingetrokken");
		return ResponseEntity.ok().build();
	}

	private MultipartFile valideerPdfBestand(MultipartFile briefBestand)
	{
		if (briefBestand == null)
		{
			throw new IllegalStateException("error.bestand.verplicht");
		}
		if (!FileType.PDF.getAllowedContentTypes().contains(briefBestand.getContentType()))
		{
			throw new IllegalStateException("error.bestandtype.niet.toegestaan");
		}
		return briefBestand;
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
		var clienten = bezwaarService.getClientenMetBezwaarBrp(bsn, geboortedatum, getIngelogdeGebruiker());
		var clientDtos = clienten.stream().map(clientMapper::clientToBezwaarDto).toList();
		return ResponseEntity.ok(clientDtos);
	}

	@PutMapping(value = "/onderzoeksresultaten-actie/vervang-document", consumes = "multipart/form-data")
	@Operation(summary = "Vervang document van onderzoeksresultaten actie", description = "Vervangt de ondergetekende brief die hoort bij het onderzoeksresultaten actie")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Brief id van nieuwe getekende brief"),
		@ApiResponse(responseCode = "400", description = "Ongeldige invoer opgegeven"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.VERVANGEN_DOCUMENTEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<OnderzoeksresultatenActieDto> vervangOndergetekendeBrief(@ModelAttribute VervangDocumentDto vervangDocumentDto)
		throws IOException
	{
		var actie = onderzoeksresultatenActieRepository.findById(vervangDocumentDto.getId())
			.orElseThrow(() -> new EntityNietGevondenException("Document", vervangDocumentDto.getId()));
		var briefBestand = valideerPdfBestand(vervangDocumentDto.getBestand());

		var uploadDocument = uploadDocumentService.multipartToUploadDocument(briefBestand);
		bezwaarService.ondertekendeOnderzoeksresultatenBriefVervangen(uploadDocument, actie);

		return ResponseEntity.ok(onderzoeksresultatenActieMapper.onderzoeksresultatenActieNaarDto(actie));
	}

	@PutMapping(value = "/bezwaar-moment/vervang-document", consumes = "multipart/form-data")
	@Operation(summary = "Vervang de ondertekende bezwaarbrief", description = "Vervangt de ondertekende bezwaarbrief die hoort bij het bezwaarmoment")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bezwaarbrief succesvol vervangen"),
		@ApiResponse(responseCode = "400", description = "Ongeldige invoer opgegeven"),
		@ApiResponse(responseCode = "404", description = "Bezwaarmoment niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.VERVANGEN_DOCUMENTEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> vervangBezwaarBrief(@ModelAttribute VervangDocumentDto vervangDocumentDto)
		throws IOException
	{
		var bezwaarMoment = bezwaarMomentRepository.findByBezwaarBriefId(vervangDocumentDto.getId())
			.orElseThrow(() -> new EntityNietGevondenException("BezwaarMoment", vervangDocumentDto.getId()));
		var briefBestand = valideerPdfBestand(vervangDocumentDto.getBestand());

		var uploadDocument = uploadDocumentService.multipartToUploadDocument(briefBestand);
		var huidigDocument = bezwaarMoment.getBezwaarBrief();
		if (!bezwaarService.ondertekendeBezwaarBriefVervangen(uploadDocument, bezwaarMoment, huidigDocument))
		{
			throw new IllegalStateException("error.onbekend");
		}

		return ResponseEntity.ok().build();
	}

	@PostMapping("/onderzoeksresultaten-actie/{id}/bevestigingsbrieven-nogmaals-versturen")
	@Operation(summary = "Verstuur de bevestigingsbrieven nogmaals", description = "Maakt de oorspronkelijke bevestigingsbrieven van de onderzoeksresultaten actie opnieuw aan")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bevestigingsbrieven succesvol opnieuw aangemaakt"),
		@ApiResponse(responseCode = "404", description = "Onderzoeksresultaten actie niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_BEZWAAR, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> verstuurBevestigingsbrievenOnderzoeksresultatenActieNogmaals(@PathVariable Long id)
	{
		var actie = onderzoeksresultatenActieRepository.findById(id)
			.orElseThrow(() -> new EntityNietGevondenException("OnderzoeksresultatenActie", id));

		bezwaarService.verstuurBevestigingsbrievenOnderzoeksresultatenActieNogmaals(actie, ScreenitSession.get().getIngelogdAccount());

		return ResponseEntity.ok().build();
	}

	@PostMapping("/bezwaar-moment/{id}/bevestigingsbrieven-nogmaals-versturen")
	@Operation(summary = "Verstuur de bevestigingsbrieven van een bezwaar nogmaals",
		description = "Maakt de oorspronkelijke bevestigingsbrieven van het bezwaarmoment opnieuw aan")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Bevestigingsbrieven succesvol opnieuw aangemaakt"),
		@ApiResponse(responseCode = "401", description = "Niet gemachtigd om de bevestigingsbrieven opnieuw te versturen"),
		@ApiResponse(responseCode = "404", description = "Bezwaarmoment niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_BEZWAAR, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<BriefDto>> verstuurBevestigingsbrievenBezwaarMomentNogmaals(@PathVariable Long id)
	{
		var bezwaarMoment = bezwaarMomentRepository.findById(id)
			.orElseThrow(() -> new EntityNietGevondenException("BezwaarMoment", id));
		if (!bezwaarService.getBriefActies(bezwaarMoment).contains(BriefActie.NOGMAALS_VERSTUREN))
		{
			return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
		}

		var brieven = bezwaarService.verstuurBevestigingsbrievenBezwaarMomentNogmaals(bezwaarMoment, ScreenitSession.get().getIngelogdAccount());

		return ResponseEntity.ok(brieven.stream().map(briefMapper::briefNaarDto).toList());
	}

	@PostMapping("/brief/{briefType}/{id}/activeren")
	@Operation(summary = "Activeer een tegengehouden bezwaarbrief", description = "Zorgt dat een tegengehouden bezwaarbrief niet meer tegengehouden wordt zodat deze opnieuw verstuurd wordt")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Brief wordt niet meer tegengehouden"),
		@ApiResponse(responseCode = "404", description = "Brief niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<Void> briefNietMeerTegenhouden(@PathVariable Long id, @PathVariable String briefType)
	{
		bezwaarService.briefNietMeerTegenhouden(id, briefType, getIngelogdeGebruiker());

		LOG.info("Bezwaarbrief wordt niet meer tegengehouden");
		return ResponseEntity.ok().build();
	}
}
