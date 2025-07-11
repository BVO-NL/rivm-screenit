package nl.rivm.screenit.main.web.component;

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

import java.util.Date;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.model.IModel;

public abstract class AjaxDateTimeField extends DateTimeField
{
	public AjaxDateTimeField(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		getDatePicker().setOutputMarkupId(true);
		getDatePicker().add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				AjaxDateTimeField.this.convertInput();
				AjaxDateTimeField.this.updateModel();
				AjaxDateTimeField.this.onUpdate(target);
			}
		});
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		getDatePicker().setRequired(isRequired());
		getTimeField().setRequired(isRequired());
	}

	@Override
	protected TimeField newTimeField(String wicketId, IModel<Date> model)
	{
		return new AjaxTimeField(wicketId, model)
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				AjaxDateTimeField.this.convertInput();
				AjaxDateTimeField.this.updateModel();
				AjaxDateTimeField.this.onUpdate(target);
			}
		};
	}

	protected abstract void onUpdate(AjaxRequestTarget target);
}
