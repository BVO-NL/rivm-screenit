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

import lombok.NoArgsConstructor;

import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;

import com.google.common.collect.Range;

@NoArgsConstructor(access = lombok.AccessLevel.PRIVATE)
public abstract class TijdstipValidator
{
	public static IValidator<String> uurValidator()
	{
		return new TijdstipValidator.UurValidator();
	}

	public static IValidator<String> minuutAfgerondOpVijfValidator()
	{
		return new MinuutAfgerondOpVijfValidator();
	}

	private static final class UurValidator implements IValidator<String>
	{
		@Override
		public void validate(IValidatable<String> validatable)
		{
			var value = validatable.getValue();
			try
			{
				var uur = Integer.parseInt(value);
				if (!Range.closed(0, 23).contains(uur))
				{
					validatable.error(maakValidationErrorMetKey("TijdstipValidator.uur.buiten.range"));
				}
			}
			catch (NumberFormatException e)
			{
				validatable.error(maakValidationErrorMetKey("TijdstipValidator.uur.geen.geldig.getal"));
			}
		}

	}

	private static final class MinuutAfgerondOpVijfValidator implements IValidator<String>
	{
		@Override
		public void validate(IValidatable<String> validatable)
		{
			var value = validatable.getValue();
			try
			{
				var minuut = Integer.parseInt(value);
				if (!Range.closed(0, 55).contains(minuut))
				{
					validatable.error(maakValidationErrorMetKey("TijdstipValidator.minuut.buiten.range"));
				}
				if (minuut % 5 != 0)
				{
					validatable.error(maakValidationErrorMetKey("TijdstipValidator.minuut.geen.blok.van.5.minuten"));
				}
			}
			catch (NumberFormatException e)
			{
				validatable.error(maakValidationErrorMetKey("TijdstipValidator.minuut.geen.geldig.getal"));
			}
		}
	}

	private static ValidationError maakValidationErrorMetKey(String key)
	{
		var error = new ValidationError();
		error.addKey(key);
		return error;
	}
}
