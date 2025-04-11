package nl.rivm.screenit.huisartsenportaal.exception;

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

import org.springframework.security.authentication.AccountStatusException;
import org.springframework.validation.ObjectError;

public class ValidatieException extends AccountStatusException
{

	private final List<ObjectError> errors;

	public ValidatieException(List<ObjectError> errors)
	{
		super("Er zijn validatie fouten gevonden.");
		this.errors = errors;
	}

	public String getValidatieMessage()
	{
		var message = new StringBuilder();
		message.append("Er zijn validatie fouten: ");
		for (var i = 0; i < errors.size(); i++)
		{
			if (i < errors.size() - 1)
			{
				message.append(", ");
			}
			message.append(errors.get(i).getDefaultMessage());
		}
		return message.toString();
	}
}
