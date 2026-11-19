package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;

import org.hibernate.AssertionFailure;
import org.hibernate.exception.GenericJDBCException;
import org.springframework.dao.DataAccessException;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ExceptionUtil
{
	public static String getMedewerkerMeldingUitTriggerMessage(RuntimeException e)
	{
		String uiMessage = null;
		Throwable cause;
		if (e instanceof DataAccessException exception)
		{
			cause = exception.getCause();
		}
		else if (e instanceof AssertionFailure failure)
		{
			cause = failure.getCause();
			cause = cause != null ? cause.getCause() : null;
		}
		else
		{
			cause = e;
		}
		if (cause instanceof GenericJDBCException exception)
		{
			if (exception.getSQLException() != null)
			{
				var message = exception.getSQLException().getMessage();
				var startTriggerMessage = message.indexOf("[" + Constants.EXCEPTION_GEBRUIKERMELDING_MARKER);
				var endTriggerMessage = message.indexOf(Constants.EXCEPTION_GEBRUIKERMELDING_MARKER + "]");
				if (startTriggerMessage > 0 && endTriggerMessage > startTriggerMessage)
				{
					uiMessage = " "
						+ message.substring(startTriggerMessage + Constants.EXCEPTION_GEBRUIKERMELDING_MARKER.length() + 1, endTriggerMessage).trim();
				}
			}
		}
		else if (cause instanceof IllegalStateException)
		{
			uiMessage = " " + e.getMessage();
		}
		if (uiMessage == null)
		{
			uiMessage = " Onbekende fout.";
			LOG.error("Onverwachte jdbc fout.", e);
		}
		return uiMessage;
	}
}
