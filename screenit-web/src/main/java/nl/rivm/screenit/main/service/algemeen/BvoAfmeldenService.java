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
import java.util.List;

import nl.rivm.screenit.main.dto.algemeen.BaseAfmeldenDto;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.springframework.web.multipart.MultipartFile;

public interface BvoAfmeldenService
{
	List<AfmeldingType> getAfmeldOpties(Client client, Bevolkingsonderzoek bevolkingsonderzoek);

	void afmelden(Client client, Bevolkingsonderzoek bevolkingsonderzoek, BaseAfmeldenDto afmelden, MultipartFile handtekeningBestand,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker) throws IOException;
}
