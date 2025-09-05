package nl.rivm.screenit.main.service;

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

import java.util.List;

import nl.rivm.screenit.dto.OrganisatieMedewerkerRolDto;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.data.domain.Sort;

public interface MedewerkerService
{
	List<OrganisatieMedewerker> zoekOrganisatieMedewerkers(OrganisatieMedewerker zoekOrganisatieMedewerker, long first, long count, Sort sort);

	List<OrganisatieMedewerker> getActieveRadiologen(OrganisatieMedewerker zoekOrganisatieMedewerker, List<Long> exclIds, Sort sort);

	long countOrganisatieMedewerkers(OrganisatieMedewerker organisatieMedewerker);

	void addOrganisatieMedewerker(Organisatie organisatie, Medewerker medewerker);

	void saveOrUpdateRollen(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, List<OrganisatieMedewerkerRolDto> initieleRollen, OrganisatieMedewerker organisatieMedewerker);

	boolean saveOrUpdateMedewerker(Medewerker medewerker, boolean isBestaande, boolean wordGeblokkeerd);

	boolean resetWachtwoord(Medewerker medewerker);

	OrganisatieMedewerker getOrganisatieMedewerker(Organisatie organisatie, Medewerker medewerker);

	List<OrganisatieMedewerkerRol> getOrganisatieMedewerkersMetRol(Rol rol);

	boolean zijnErOrganisatieMedewerkersMetRol(Rol rol);

	List<Medewerker> getActieveMedewerkersMetRecht(Recht recht);

	List<OrganisatieMedewerkerRol> getOrganisatieMedewerkersMetRolEnBvos(Rol rol, List<Bevolkingsonderzoek> onderzoeken);

}
