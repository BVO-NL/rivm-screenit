package nl.rivm.screenit.clientportaal.rest;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.rest.mapping.CustomObjectMapper;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.orm.jpa.support.OpenEntityManagerInViewFilter;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@EnableWebMvc
@AllArgsConstructor
@Configuration
public class WebConfig implements WebMvcConfigurer
{

	private final CustomObjectMapper customObjectMapper;

	@Bean
	public FilterRegistrationBean<OpenEntityManagerInViewFilter> openEntityManagerInViewFilter()
	{
		var filter = new FilterRegistrationBean<OpenEntityManagerInViewFilter>();
		filter.setFilter(new OpenEntityManagerInViewFilter());
		filter.addUrlPatterns("/*");
		return filter;
	}

	@Override
	public void extendMessageConverters(List<HttpMessageConverter<?>> converters)
	{
		converters.stream()
			.filter(MappingJackson2HttpMessageConverter.class::isInstance)
			.map(MappingJackson2HttpMessageConverter.class::cast)
			.forEach(converter -> converter.setObjectMapper(customObjectMapper));
	}

	@Override
	public void configureContentNegotiation(ContentNegotiationConfigurer configurer)
	{

		configurer.favorPathExtension(false);
	}

	@Override
	public void configurePathMatch(PathMatchConfigurer matcher)
	{

		matcher.setUseSuffixPatternMatch(false);
	}
}
