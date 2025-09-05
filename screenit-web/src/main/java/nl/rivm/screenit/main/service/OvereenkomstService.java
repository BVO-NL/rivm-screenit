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

import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekFilter;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenOrganisatieOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;

import org.springframework.data.domain.Sort;

public interface OvereenkomstService
{
	void saveOrUpdateOvereenkomst(Overeenkomst overeenkomst, UploadDocument uploadDocument, Account account);

	void saveOrUpdateOvereenkomst(AbstractAfgeslotenOvereenkomst overeenkomst, UploadDocument uploadDocument, Account account);

	List<Overeenkomst> getOvereenkomsten(Boolean actief, long first, long size, Sort sort);

	long countOvereenkomsten(Boolean actief);

	void updateOvereenkomst(Overeenkomst overeenkomst, Account account);

	List<AfgeslotenOrganisatieOvereenkomst> getAfgeslotenOrganisatieOvereenkomsten(Organisatie zoekObject, Boolean actief, Long first, Long size, Sort sort);

	long countAfgeslotenOrganisatieOvereenkomsten(Organisatie zoekObject, Boolean actief);

	List<AfgeslotenMedewerkerOvereenkomst> getAfgeslotenMedewerkerOvereenkomsten(Medewerker zoekObject, Boolean actief, Long first, Long size, Sort sort);

	long countAfgeslotenMedewerkerOvereenkomsten(Medewerker zoekObject, Boolean actief);

	List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes);

	List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(OrganisatieMedewerker inTeLoggenOrganisatieMedewerker);

	long countTeAccoderenOvereenkomsten(OrganisatieMedewerker inTeLoggenOrganisatieMedewerker);

	void accodeerOvereenkomsten(OrganisatieMedewerker organisatieMedewerker, Account account);

	List<Organisatie> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, Sort sort, int first, int count);

	long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter);

	List<AfgeslotenOrganisatieOvereenkomst> getAfgeslotenOvereenkomstenVanOrganisatie(OvereenkomstZoekFilter filter, Organisatie organisatie);

	List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst();

	boolean isErAlEenZakelijkOvereenkomst(Overeenkomst overeenkomst);
}
