package nl.rivm.screenit.main.config;

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

import java.util.HashMap;

import jakarta.servlet.Filter;

import nl.rivm.screenit.security.ScreenitRealm;

import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;

@Configuration
public class ShiroConfig
{
	@Bean
	@Profile("!test")
	public DefaultWebSecurityManager shiroSecurityManager(ScreenitRealm realm)
	{
		var securityManager = new DefaultWebSecurityManager();
		securityManager.setRealm(realm);
		return securityManager;
	}

	@Bean(name = "shiroSecurityManager")
	@Profile("test")
	public DefaultWebSecurityManager shiroSecurityManagerTest(ScreenitRealm realm)
	{
		var securityManager = new DefaultWebSecurityManager();
		securityManager.setRealm(realm);
		return securityManager;
	}

	@Bean
	public ShiroFilterFactoryBean shiroFilter(DefaultWebSecurityManager shiroSecurityManager)
	{
		var filter = new ShiroFilterFactoryBean();
		filter.setSecurityManager(shiroSecurityManager);
		filter.setFilterChainDefinitionMap(getFilterChainMapping());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<Filter> shiroFilterProxy(ShiroFilterFactoryBean shiroFilter) throws Exception
	{
		var filter = new FilterRegistrationBean<>();
		filter.setFilter((Filter) shiroFilter.getObject());
		filter.setName("shiroFilter");
		filter.addInitParameter("targetFilterLifecycle", "true");
		filter.addUrlPatterns("/*");
		filter.setOrder(Ordered.HIGHEST_PRECEDENCE);
		return filter;
	}

	private HashMap<String, String> getFilterChainMapping()
	{
		var filterMap = new HashMap<String, String>();
		filterMap.put("/api/**", "noSessionCreation");
		return filterMap;
	}
}
