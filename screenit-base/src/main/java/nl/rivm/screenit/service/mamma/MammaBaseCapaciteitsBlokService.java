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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;

import com.google.common.collect.Range;

public interface MammaBaseCapaciteitsBlokService
{
	String saveOrUpdate(PlanningCapaciteitBlokDto blok, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	String delete(PlanningCapaciteitBlokDto blok, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete);

	MammaCapaciteit getCapaciteit(Collection<MammaCapaciteitBlokDto> nietGeblokkeerdeCapaciteitsBlokDtos);

	MammaCapaciteitBlok getCapaciteitsBlokOpTijdstipVoorSe(Client client, MammaScreeningsEenheid screeningsEenheid, Date nu);

	List<MammaCapaciteitBlok> getAlleCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik);

	List<MammaCapaciteitBlok> getScreeningCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik, boolean bepaalCapaciteit);

	List<MammaCapaciteitBlok> getGeenScreeningCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik);

	Collection<MammaCapaciteitBlokDto> getNietGeblokkeerdeScreeningCapaciteitBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet, Client client);

}
