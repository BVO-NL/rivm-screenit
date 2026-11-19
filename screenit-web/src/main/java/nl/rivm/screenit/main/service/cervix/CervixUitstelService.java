package nl.rivm.screenit.main.service.cervix;

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

import java.time.LocalDate;

import nl.rivm.screenit.main.dto.algemeen.CervixUitstelAanvragenDto;
import nl.rivm.screenit.main.dto.algemeen.CervixUitstelDto;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;

public interface CervixUitstelService
{
	CervixUitstelDto getHuidigUitstel(Client client);

	CervixUitstel maakUitstel(Client client, CervixUitstelAanvragenDto dto);

	void valideerUitstel(Client client, CervixUitstelType uitstelType, LocalDate datum) throws ValidatieException;
}
