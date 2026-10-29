package nl.rivm.screenit.main.service.algemeen;

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

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.main.model.BriefActie;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BezwaarType;

import org.springframework.web.multipart.MultipartFile;

public interface BezwaarService
{
	void bezwaarBRPIntrekken(OrganisatieMedewerker organisatieMedewerker, Client client, MultipartFile document) throws IOException;

	List<Bezwaar> getBewerkbareBezwaren(Client client);

	void bezwaarDirectVerwerken(Client client, List<Bezwaar> gekozenBezwaren, MultipartFile briefBestand, String notitie, OrganisatieMedewerker organisatieMedewerker)
		throws IOException;

	void gegevensVerwijderen(Client client, List<Bezwaar> gekozenBezwaren, List<BezwaarType> toegestaneBezwaarTypes, MultipartFile briefBestand, String notitie,
		OrganisatieMedewerker organisatieMedewerker) throws IOException;

	List<Client> getClientenMetBezwaarBrp(String bsn, LocalDate geboortedatum, OrganisatieMedewerker organisatieMedewerker);

	boolean ondertekendeOnderzoeksresultatenBriefVervangen(UploadDocument nieuwDocument, OnderzoeksresultatenActie actie);

	boolean ondertekendeBezwaarBriefVervangen(UploadDocument nieuwDocument, BezwaarMoment bezwaarMoment, UploadDocument huidigDocument);

	List<BriefActie> getBriefActies(BezwaarMoment bezwaarMoment);

	List<BezwaarBrief> verstuurBevestigingsbrievenBezwaarMomentNogmaals(BezwaarMoment bezwaarMoment, Account ingelogdAccount);

	List<BezwaarBrief> verstuurBevestigingsbrievenOnderzoeksresultatenActieNogmaals(OnderzoeksresultatenActie actie, Account ingelogdAccount);

	void briefNietMeerTegenhouden(Long briefId, String briefType, Account account);
}
