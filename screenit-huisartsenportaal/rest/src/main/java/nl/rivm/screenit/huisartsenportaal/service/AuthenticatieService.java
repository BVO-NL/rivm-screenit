package nl.rivm.screenit.huisartsenportaal.service;

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

import nl.rivm.screenit.huisartsenportaal.dto.LoginDto;
import nl.rivm.screenit.huisartsenportaal.dto.RegistrerenDto;
import nl.rivm.screenit.huisartsenportaal.dto.TokenDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordAanvragenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;

public interface AuthenticatieService
{
	String getScope();

	TokenDto registreren(RegistrerenDto registrerenDto);

	TokenDto wachtwoordAanvragen(WachtwoordAanvragenDto wachtwoordAanvragenDto);

	TokenDto inloggen(LoginDto loginDto);

	Huisarts wachtwoordVergeten(Huisarts huisarts) throws IllegalStateException;

	Huisarts updateWachtwoord(Huisarts huisarts, String wachtwoord);

	Integer incrementAttempts(Huisarts huisarts);

	void resetAttempts(Huisarts huisarts);

	boolean controleerWachtwoord(String plainWachtwoord, String encodedWachtwoord);
}
