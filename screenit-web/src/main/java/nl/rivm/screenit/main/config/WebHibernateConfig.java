package nl.rivm.screenit.main.config;

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

import java.util.List;

import nl.rivm.screenit.config.hibernate.HibernateOrmMappingResourceProvider;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateServiceImpl;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class WebHibernateConfig
{
	public static final String WEB_ORM_MAPPING_RESOURCE = "META-INF/screenit-web-orm.xml";

	@Bean
	HibernateOrmMappingResourceProvider hibernateOrmMappingResourceProvider()
	{
		return () -> List.of(WEB_ORM_MAPPING_RESOURCE);
	}

	@Bean
	@Deprecated
	HibernateService hibernateServiceOud()
	{
		return new HibernateServiceImpl();
	}
}
