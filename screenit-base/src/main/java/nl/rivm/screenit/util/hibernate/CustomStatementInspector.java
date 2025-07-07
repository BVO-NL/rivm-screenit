package nl.rivm.screenit.util.hibernate;

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

import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.resource.jdbc.spi.StatementInspector;

public class CustomStatementInspector implements StatementInspector
{

	private static final Pattern DISCRIMINATOR_PATTERN =
		Pattern.compile("join \\(select \\* from (\\S+) t where t\\.dtype(?:='[^']+'| in \\([^)]+\\))\\)\\s+(\\S+)");

	@Override
	public String inspect(String sql)
	{
		if (sql == null || !StringUtils.startsWithIgnoreCase(sql, "select"))
		{
			return sql;
		}

		var modifiedSql = sql;

		var matcher = DISCRIMINATOR_PATTERN.matcher(modifiedSql);
		modifiedSql = matcher.replaceAll("join $1 $2");

		if (!sql.equals(modifiedSql))
		{
			modifiedSql += " /* Discriminator subselects transformed */";
		}

		return modifiedSql;
	}
}
