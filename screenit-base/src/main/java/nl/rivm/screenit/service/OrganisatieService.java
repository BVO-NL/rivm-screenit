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

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;

public interface OrganisatieService
{

	List<CentraleEenheid> getMogelijkeCentraleEenheden(Organisatie organisatie);

	List<OrganisatieMedewerker> getActieveOrganisatieMedewerkers(@Nonnull Medewerker medewerker);

	List<OrganisatieMedewerker> getActieveOrganisatieMedewerkersMetRollen(Medewerker medewerker);

	List<ColonIntakelocatie> getActieveIntakelocaties();

	List<ColonIntakelocatie> getActieveIntakelocatiesBinnenRegio(ScreeningOrganisatie screeningOrganisatie);

	List<BeoordelingsEenheid> getActieveBeoordelingseenhedenBinnenRegio(ScreeningOrganisatie screeningOrganisatie);

	List<CentraleEenheid> getActieveCentraleEenhedenBinnenRegio(ScreeningOrganisatie screeningOrganisatie);

	List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties();

	void saveOrUpdateScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie, List<Gemeente> choices, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void saveOrUpdateSoPlanningBk(ScreeningOrganisatie screeningOrganisatie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void saveOrUpdate(Organisatie organisatie);

	void saveOrUpdateColoscopieCentrum(ColonIntakelocatie intakelocatie);

	<T extends Organisatie> List<T> getActieveOrganisaties(Class<T> organisatieClass);

	Organisatie getOrganisatieByUzinummer(String uzinummer);

	Organisatie getOrganisatieByRootOid(String rootOid);

	List<Organisatie> getPathologieLabs(@Nonnull Organisatie organisatie);

	<T extends Organisatie> List<T> getChildrenOrganisaties(@Nonnull Organisatie organisatie, @Nonnull Class<T> organisatieClass);

	List<Organisatie> getOrganisatieByOrganisatieTypes(List<OrganisatieType> organisatieTypes);

	List<Medewerker> getActieveMedewerkers(@Nonnull Organisatie organisatie);

	void saveDocumentForOrganisatie(UploadDocument uploadDocument, Organisatie organisatie);

	void deleteDocumentForOrganisatie(UploadDocument document, Organisatie organisatie);

	ScreeningOrganisatie getScreeningOrganisatie(long screeningOrganisatieId);

	List<Long> getOrganisatieIdsMetType(OrganisatieType type);
}
