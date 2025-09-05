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

import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;

import org.springframework.data.domain.Sort;

public interface MammaUploadBeeldenService
{
	List<MammaUploadBeeldenVerzoek> zoekOpenstaandeUploadBeeldenVerzoeken(Organisatie organisatie, ScreeningOrganisatie regio, long first, long count, Sort sort);

	long countOpenstaandeUploadBeeldenVerzoeken(Organisatie organisatie, ScreeningOrganisatie regio);

	List<MammaUploadBeeldenVerzoekDto> zoekOrganisatiesMetOpenstaandeUploadVerzoeken(ScreeningOrganisatie regio);

	void maakUploadVerzoek(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, Client client, OrganisatieMedewerker organisatieMedewerker);

	String uploadBeelden(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, List<UploadDocument> uploadDocumenten, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void setGeenBeeldenBeschikbaar(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, OrganisatieMedewerker organisatieMedewerker);

	void annuleerVerzoek(MammaUploadBeeldenVerzoek uploadBeeldenVerzoek, OrganisatieMedewerker organisatieMedewerker);
}
