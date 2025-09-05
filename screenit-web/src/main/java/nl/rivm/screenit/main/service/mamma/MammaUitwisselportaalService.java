package nl.rivm.screenit.main.service.mamma;

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
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;

public interface MammaUitwisselportaalService
{

	void maakDownloadVerzoek(List<MammaOnderzoek> onderzoeken, OrganisatieMedewerker ingelogdeOrganisatieMedewerker) throws IOException;

	void startDownloading();

	void resetDownloadVerzoek(MammaDownloadOnderzoekenVerzoek object) throws IOException;

	Optional<MammaDownloadOnderzoekenVerzoek> geldigDownloadVerzoekVoorIngelogdeOrganisatieMedewerker(long downloadVerzoekid, OrganisatieMedewerker organisatieMedewerker);

	MammaDownloadOnderzoekenVerzoek maakDownloadVerzoekFilter(OrganisatieMedewerker organisatieMedewerker);

	boolean zipKanGedownloadWorden(MammaDownloadOnderzoekenVerzoek downloadOnderzoekenVerzoek);

	void updateDownloadVerzoekInformatie(MammaDownloadOnderzoekenVerzoek verzoek, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	MammaFollowUpRadiologieVerslag getFollowUpRadiologieVerslag(MammaScreeningRonde screeningRonde, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	Organisatie getLaatstGedownloadDoorOrganisatie(MammaDossier dossier);

	List<MammaScreeningRonde> beschikbareRondesVoorDownload(Client client);
}
