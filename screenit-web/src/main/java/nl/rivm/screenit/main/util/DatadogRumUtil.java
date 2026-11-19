package nl.rivm.screenit.main.util;

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

import nl.rivm.screenit.util.ScreenitObjectMapper;

import org.apache.wicket.ajax.AjaxRequestTarget;

public class DatadogRumUtil
{
	private static final ScreenitObjectMapper OBJECT_MAPPER = new ScreenitObjectMapper();

	public static void pushEvent(AjaxRequestTarget target, String eventName, Object eventData)
	{
		var jsObject = convertToJsObject(eventData);
		var jsCall = String.format("pushDatadogEvent('%s', %s);", eventName, jsObject);
		target.appendJavaScript(jsCall);
	}

	private static String convertToJsObject(Object data)
	{
		if (data == null)
		{
			return "null";
		}
		try
		{
			return OBJECT_MAPPER.writeValueAsString(data);
		}
		catch (Exception e)
		{
			throw new RuntimeException("Kan eventData niet omzetten naar een Javascript object", e);
		}
	}
}
