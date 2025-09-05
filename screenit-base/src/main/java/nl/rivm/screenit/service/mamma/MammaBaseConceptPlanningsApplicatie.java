package nl.rivm.screenit.service.mamma;

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

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Date;
import java.util.NavigableSet;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStatusDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
public interface MammaBaseConceptPlanningsApplicatie
{

	void sendPostcodeReeks(MammaPostcodeReeks postcodeReeks, boolean isNieuw);

	NavigableSet<String> getUncoveredPostcodes(ScreeningOrganisatie screeningOrganisatie);

	void deletePostcodeReeks(MammaPostcodeReeks postcodeReeks);

	void sendStandplaats(MammaStandplaats standplaats, boolean isNieuw);

	void sendScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid, boolean isNieuw);

	PlanningWeekDto getWeek(MammaScreeningsEenheid screeningEenheid, Date start);

	String getAfspraakDrempelOverzichtStandplaats(long standplaatsId);

	String getAfspraakDrempelOverzichtScreeningsOrganisatie(long screeningsOrganisatieId);

	PlanningScreeningsEenheidMetaDataDto getScreeningsEenheidMetaData(MammaScreeningsEenheid screeningEenheid);

	void sendCapaciteitBlok(PlanningCapaciteitBlokDto blok, boolean isNieuw, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	String deleteCapaciteitBlok(PlanningCapaciteitBlokDto blok, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	PlanningStandplaatsPeriodeDto[] getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid);

	void changeRoute(PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void sendAfspraakDrempelStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	Long[] getStandplaatsenZonderRoute(ScreeningOrganisatie screeningOrganisatie);

	Long[] getStandplaatsenMetRoute(ScreeningOrganisatie screeningOrganisatie);

	PlanningConceptMeldingenDto saveConcept(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, boolean runDry);

	void conceptAnnuleren(OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void sendBlokkade(MammaBlokkade blokkade, boolean isNieuw);

	int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete);

	void herhaalWeek(MammaScreeningsEenheid screeningsEenheidVan, MammaScreeningsEenheid screeningsEenheidNaar, LocalDate teHerhalenWeek, LocalDate herhalenVanafWeek,
		LocalDate herhalenTotEnMetWeek, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void updateScreeningsOrganisatie(PlanningScreeningsOrganisatieDto screeningsOrganisatieDto);

	Long[] getConceptGewijzigdDoor(ScreeningOrganisatie screeningOrganisatie);

	Date getPlannenTotEnMetDatum();

	void verzetClienten(PlanningVerzetClientenDto verzetClientenDto);

	PlanningStatusDto getStatus();

	void kopieerDag(MammaScreeningsEenheid bronScreeningsEenheid, MammaScreeningsEenheid doelScreeningsEenheid, LocalDate bronDag, LocalTime bronVanTijd, LocalTime bronTotTijd,
		LocalDate doelDag, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

}
