package nl.rivm.screenit.service;

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

import java.util.List;

import jakarta.annotation.Nonnull;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.enums.ToegangLevel;

public interface OrganisatieZoekService
{
	List<Organisatie> zoekOrganisaties(Organisatie searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		OrganisatieMedewerker organisatieMedewerker, long first, long count, String sortProperty, boolean asc);

	long countOrganisaties(Organisatie searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		OrganisatieMedewerker organisatieMedewerker);

	List<Organisatie> getOrganisatiesForNiveau(OrganisatieMedewerker organisatieMedewerker, OrganisatieType organisatieTypeGekozen, ToegangLevel toegangLevel);

	List<Organisatie> getAllActieveOrganisatiesWithType(Class<? extends Organisatie> organisatie);

	List<ColoscopieCentrumWrapper> zoekIntakeLocaties(ColoscopieCentrumZoekCriteria zoekObject, Client client, boolean alleenActiefKamers);

	List<Organisatie> getMogelijkeParents(@Nonnull Organisatie organisatie, @Nonnull OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	List<Long> getZichtbareOrganisatiesOpToegangLevel(Organisatie organisatie, ToegangLevel level, List<OrganisatieType> types);

	List<Organisatie> screeningsorganisatiesWaarOrganisatieOndervalt(Organisatie organisatie);

	ColoscopieCentrumWrapper getNearestIntakeLocatie(Client client);

	List<Organisatie> zoekOrganisatieMetFqdn(String fqdn);
}
