package nl.rivm.screenit.huisartsenportaal.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;

public interface HuisartsRepository extends BaseRepository<Huisarts>
{

	Huisarts findByAgbcode(String agbcode);

	Huisarts findByGebruikersnaam(String gebruikersnaam);

	List<Huisarts> findByEmail(String email);

	Huisarts findByEmailAndGebruikersnaam(String email, String gebruikersnaam);

	Long countByHuisartsportaalId(Long id);

	Huisarts findByAgbcodeAndInlogCodeAndInlogMethode(String agbcode, String inlogCode, InlogMethode inlogMethode);

	Huisarts findByGebruikersnaamAndInlogCodeAndInlogMethode(String gebruikersnaam, String inlogCode, InlogMethode inlogMethode);

	Huisarts findByEmailAndInlogCodeAndInlogMethode(String email, String inlogCode, InlogMethode inlogMethode);
}
