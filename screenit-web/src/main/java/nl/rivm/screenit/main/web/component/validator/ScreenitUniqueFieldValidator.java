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

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.MapModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ScreenitUniqueFieldValidator<T extends HibernateObject> extends UniqueFieldValidator<T, String>
{
	@SpringBean
	private HibernateService hibernateService;

	public ScreenitUniqueFieldValidator(Class type, Serializable objectId, String field, boolean ignoreCase)
	{
		super(type, objectId, field, ignoreCase);
		Injector.get().inject(this);
	}

	public ScreenitUniqueFieldValidator(final Class<T> type, final Serializable objectId, final String field,
		final Map<String, Object> restrictions)
	{
		this(type, objectId, field, restrictions, false);
	}

	public ScreenitUniqueFieldValidator(final Class<T> type, final Serializable objectId, final String field,
		final Map<String, Object> restrictions, final boolean ignoreCase)
	{
		this(type, objectId, field, new MapModel<>(new HashMap<>(
			restrictions)), ignoreCase);
	}

	public ScreenitUniqueFieldValidator(final Class<T> type, final Serializable objectId, final String field,
		final IModel<Map<String, Object>> restrictions, final boolean ignoreCase)
	{
		super(type, objectId, field, restrictions, ignoreCase);
		Injector.get().inject(this);
	}

	@Override
	protected boolean existsOther(Map<String, Object> innerRestrictions)
	{
		return hibernateService.existsOther(getType(), getObjectId(), innerRestrictions, isIgnoreCase());
	}
}
