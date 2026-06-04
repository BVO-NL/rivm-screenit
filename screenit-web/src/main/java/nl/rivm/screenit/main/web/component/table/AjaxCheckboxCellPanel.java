package nl.rivm.screenit.main.web.component.table;

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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class AjaxCheckboxCellPanel<T> extends GenericPanel<T>
{
	public AjaxCheckboxCellPanel(String id, IModel<T> rowModel, IModel<Boolean> checkboxModel)
	{
		super(id, rowModel);
		setOutputMarkupId(true);
		add(new AjaxCheckBox("checkbox", checkboxModel)
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				AjaxCheckboxCellPanel.this.onUpdate(target);
			}
		});
	}

	protected abstract void onUpdate(AjaxRequestTarget target);
}
