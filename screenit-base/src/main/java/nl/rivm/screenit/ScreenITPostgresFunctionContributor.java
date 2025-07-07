package nl.rivm.screenit;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.sql.Types;

import org.hibernate.boot.model.FunctionContributions;
import org.hibernate.boot.model.FunctionContributor;

public class ScreenITPostgresFunctionContributor implements FunctionContributor
{
	@Override
	public void contributeFunctions(FunctionContributions functionContributions)
	{
		var ddlTypeRegistry = functionContributions.getTypeConfiguration().getDdlTypeRegistry();
		var timestampZType = ddlTypeRegistry.getDescriptor(Types.TIMESTAMP_WITH_TIMEZONE);
		ddlTypeRegistry.addDescriptor(Types.TIMESTAMP, timestampZType);

		functionContributions.getFunctionRegistry().registerPattern("intervalInDagen", "?2 + INTERVAL '?1 day'");
		functionContributions.getFunctionRegistry().registerPattern("YEAR", "extract(year from ?1)");
	}
}
