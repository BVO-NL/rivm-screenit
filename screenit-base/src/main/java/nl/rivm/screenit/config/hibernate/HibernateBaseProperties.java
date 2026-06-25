package nl.rivm.screenit.config.hibernate;

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

import java.util.Properties;

import nl.rivm.screenit.util.hibernate.CustomStatementInspector;
import nl.topicuszorg.hibernate.spring.util.naming.ImplicitHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.naming.PhysicalHibernate4LegacyNamingStrategy;

import org.hibernate.cfg.AvailableSettings;
import org.hibernate.cfg.MappingSettings;
import org.hibernate.envers.configuration.EnversSettings;
import org.hibernate.query.sqm.mutation.internal.temptable.GlobalTemporaryTableStrategy;

public final class HibernateBaseProperties
{
	public static Properties getProperties()
	{
		Properties properties = new Properties();

		properties.put(AvailableSettings.SHOW_SQL, "false");
		properties.put(AvailableSettings.FORMAT_SQL, "false");
		properties.put(AvailableSettings.USE_SQL_COMMENTS, "false");
		properties.put(AvailableSettings.GENERATE_STATISTICS, "false");
		properties.put(AvailableSettings.MAX_FETCH_DEPTH, "1"); 
		properties.put(AvailableSettings.ID_DB_STRUCTURE_NAMING_STRATEGY, "single");
		properties.put(AvailableSettings.SESSION_FACTORY_NAME, "ScreenITSessionFactory");
		properties.put(AvailableSettings.SESSION_FACTORY_NAME_IS_JNDI, "false");
		properties.put(AvailableSettings.NON_CONTEXTUAL_LOB_CREATION, "true");
		properties.put(AvailableSettings.IMPLICIT_NAMING_STRATEGY, ImplicitHibernate4LegacyNamingStrategy.class.getName());
		properties.put(AvailableSettings.PHYSICAL_NAMING_STRATEGY, PhysicalHibernate4LegacyNamingStrategy.class.getName());
		properties.put(AvailableSettings.USE_SECOND_LEVEL_CACHE, "false");
		properties.put(EnversSettings.AUDIT_TABLE_PREFIX, "");
		properties.put(EnversSettings.AUDIT_TABLE_SUFFIX, "_aud");
		properties.put(EnversSettings.STORE_DATA_AT_DELETE, "true");
		properties.put(EnversSettings.REVISION_ON_COLLECTION_CHANGE, "false");
		properties.put(MappingSettings.DEFAULT_SCHEMA, "gedeeld");
		properties.put(GlobalTemporaryTableStrategy.CREATE_ID_TABLES, "false");
		properties.put(GlobalTemporaryTableStrategy.DROP_ID_TABLES, "false");
		properties.put("hibernate.jdbc.use_streams_for_binary", "true");
		properties.put("hibernate.listeners.envers.autoRegister", "true");
		properties.put(AvailableSettings.STATEMENT_BATCH_SIZE, "20");
		properties.put(AvailableSettings.STATEMENT_INSPECTOR, CustomStatementInspector.class.getName());
		properties.put(AvailableSettings.ISOLATION, "2");
		properties.put("connection.isolation", "2"); 

		return properties;
	}
}
