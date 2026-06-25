package nl.rivm.screenit.main.controller.colon;

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

import java.time.DayOfWeek;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonIntakekamerService;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterInstellingenDto;
import nl.rivm.screenit.model.colon.dto.KamerDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonBaseUitnodigingService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
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
@RequestMapping("/colon/rooster")
@Tag(name = "Colon rooster", description = "Opvragen en beheren van roosterinstellingen voor colon")
public class ColonRoosterController
{
	private final ColonIntakekamerService locatieService;

	private final ColonBaseUitnodigingService uitnodigingService;

	private final ColonRoosterBeperkingService beperkingService;

	private final OrganisatieParameterService organisatieParameterService;

	@GetMapping("/kamers")
	@Operation(summary = "Haal kamers op", description = "Geeft de kamers van de ingelogde intakelocatie terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Kamers succesvol opgehaald"),
		@ApiResponse(responseCode = "400", description = "Geen intakelocatie beschikbaar in de sessie"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public List<KamerDto> getKamers()
	{
		var intakeLocatie = ScreenitSession.get().getIntakelocatie();

		if (intakeLocatie == null)
		{
			throw new IllegalStateException("error.geen.intakelocatie");
		}

		return locatieService.getKamers(intakeLocatie).stream().map(KamerDto::fromKamer).toList();
	}

	@GetMapping("/beperkingen")
	@Operation(summary = "Haal roosterbeperkingen op", description = "Geeft de huidige roosterbeperkingen terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Roosterbeperkingen succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER,
		Recht.MEDEWERKER_LOCATIE_ROOSTER }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<ColonRoosterBeperkingenDto> getRoosterBeperkingen()
	{
		var roosterBeperkingen = beperkingService.getRoosterBeperkingen();
		return ResponseEntity.ok(roosterBeperkingen);
	}

	@PutMapping("/beperkingen")
	@Operation(summary = "Werk roosterbeperkingen bij", description = "Past de roosterbeperkingen aan.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Roosterbeperkingen succesvol bijgewerkt"),
		@ApiResponse(responseCode = "422", description = "Validatiefout bij het bijwerken van de roosterbeperkingen"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.AANPASSEN, constraint = ShiroConstraint.HasPermission, recht = Recht.COLON_WEEKEND_WERK_DAG_BEPERKINGEN_BEHEER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ResponseEntity<Void> updateRoosterBeperkingen(@Parameter(description = "Nieuwe roosterbeperkingen") @RequestBody ColonRoosterBeperkingenDto roosterBeperkingenDto)
		throws ValidatieException
	{
		beperkingService.updateRoosterBeperkingen(roosterBeperkingenDto);
		return ResponseEntity.ok().build();
	}

	@GetMapping("/instellingen")
	@Operation(summary = "Haal roosterinstellingen op", description = "Geeft de roosterinstellingen voor de huidige intakelocatie terug.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Roosterinstellingen succesvol opgehaald"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_LOCATIE_ROOSTER, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
	public ColonRoosterInstellingenDto getInstellingen()
	{
		var instellingen = new ColonRoosterInstellingenDto();
		instellingen.setGeprognosticeerdeVanafDatum(uitnodigingService.getGeprognotiseerdeIntakeDatum(true).with(DayOfWeek.MONDAY));
		instellingen.setDuurAfspraakInMinuten(organisatieParameterService.getOrganisatieParameter(ScreenitSession.get().getIntakelocatie(),
			OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN));
		return instellingen;
	}
}
