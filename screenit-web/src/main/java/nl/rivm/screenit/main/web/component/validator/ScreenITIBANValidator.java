package nl.rivm.screenit.main.web.component.validator;

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

import org.apache.commons.validator.routines.IBANValidator;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

public class ScreenITIBANValidator implements IValidator<String>
{

	@Override
	public void validate(IValidatable<String> iValidatable)
	{
		String iban = iValidatable.getValue();
		if (!controleerIban(iban))
		{
			ValidationError error = new ValidationError();
			error.addKey("IBANValidator.not.valid");
			iValidatable.error(error);
		}
	}

	public boolean controleerIban(String iban)
	{
		return IBANValidator.getInstance().isValid(iban);
	}
}
