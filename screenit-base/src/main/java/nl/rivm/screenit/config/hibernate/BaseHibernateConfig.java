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

import java.util.ArrayList;
import java.util.Properties;

import javax.sql.DataSource;

import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.impl.HibernateServiceImpl;

import org.hibernate.cfg.AvailableSettings;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.sql.init.dependency.DependsOnDatabaseInitialization;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@EnableTransactionManagement
public class BaseHibernateConfig
{
	public static final String BASE_ORM_MAPPING_RESOURCE = "META-INF/screenit-base-orm.xml";

	@ConfigurationProperties(prefix = "spring.datasource.hikari")
	@Bean
	@Profile("!cucumber & !test")
	HikariDataSource dataSource()
	{
		return new HikariDataSource();
	}

	@Bean(name = { "entityManagerFactory" })
	@Profile("!test & !cucumber")
	public LocalContainerEntityManagerFactoryBean entityManagerFactory(DataSource dataSource,
		ObjectProvider<HibernateOrmMappingResourceProvider> ormMappingResourceProvider)
	{
		var entityManagerFactoryBean = new LocalContainerEntityManagerFactoryBean();
		entityManagerFactoryBean.setDataSource(dataSource);
		entityManagerFactoryBean.setJpaVendorAdapter(new HibernateJpaVendorAdapter());
		entityManagerFactoryBean.setPackagesToScan("nl.rivm.screenit.model", "nl.topicuszorg.wicket.password.model", "nl.topicuszorg.yubikey.model",
			"nl.topicuszorg.organisatie.model");
		entityManagerFactoryBean.setMappingResources(maakOrmMappingResources(ormMappingResourceProvider.getIfAvailable()));
		var properties = new Properties();

		properties.put(AvailableSettings.DIALECT, ScreenITPostgreSQLDialect.class.getName());

		properties.putAll(HibernateBaseProperties.getProperties());

		entityManagerFactoryBean.setJpaProperties(properties);

		return entityManagerFactoryBean;
	}

	public static String[] maakOrmMappingResources(HibernateOrmMappingResourceProvider ormMappingResourceProvider)
	{
		var ormMappingResources = new ArrayList<String>();
		ormMappingResources.add(BASE_ORM_MAPPING_RESOURCE);
		if (ormMappingResourceProvider != null)
		{
			ormMappingResources.addAll(ormMappingResourceProvider.getOrmMappingResources());
		}
		return ormMappingResources.toArray(String[]::new);
	}

	@Bean
	@Profile("!test")
	HibernateService hibernateService(ObjectProvider<HibernateOrmMappingResourceProvider> ormMappingResourceProvider)
	{
		var hibernateService = new HibernateServiceImpl();
		hibernateService.setOrmMappingResourceProvider(ormMappingResourceProvider.getIfAvailable());
		return hibernateService;
	}

	@Bean
	@DependsOnDatabaseInitialization
	JpaTransactionManager transactionManager(LocalContainerEntityManagerFactoryBean entityManagerFactory)
	{
		var transactionManager = new JpaTransactionManager();
		transactionManager.setEntityManagerFactory(entityManagerFactory.getObject());
		transactionManager.setTransactionSynchronization(AbstractPlatformTransactionManager.SYNCHRONIZATION_ON_ACTUAL_TRANSACTION);
		return transactionManager;
	}
}
