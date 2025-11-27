package nl.rivm.screenit.main.service.colon;

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

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.logging.LogRegel;

public interface ColonDossierService
{

	void monsterNietBeoordeelbaar(ColonFitRegistratie fitRegistratie);

	void conclusieOpslaan(ColonIntakeAfspraak afspraak, ColonVervolgonderzoekKeuzesDto keuzes, OrganisatieMedewerker ingelogdeOrganisatieMedewerker,
		ColonConclusieType voorgaandeConclusie);

	void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	boolean magConclusieAanpassenVerwijderen(ColonIntakeAfspraak afspraak, ColonConclusieType origConclusie);

	void conclusieVerwijderen(ColonIntakeAfspraak afspraak, OrganisatieMedewerker ingelogdeOrganisatieMedewerker, ColonConclusieType origConclusie);

	void verwijderFitAnalyseResultaat(ColonFitRegistratie fitRegistratie, UploadDocument uploadDocument, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void vervangUitslagVerwijderenDocument(ColonFitRegistratie fitRegistratie, UploadDocument uploadDocument);

	boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, OrganisatieMedewerker medewerker, DashboardStatus dashboardStatus);
}
