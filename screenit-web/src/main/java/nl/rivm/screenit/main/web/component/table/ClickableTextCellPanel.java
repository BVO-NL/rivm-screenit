package nl.rivm.screenit.main.web.component.table;

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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class ClickableTextCellPanel<T> extends GenericPanel<T>
{
	protected ClickableTextCellPanel(String id, IModel<T> rowModel, final String tekst, String cssClass)
	{
		super(id, rowModel);
		setOutputMarkupId(true);
		final AjaxLink<T> ajaxLink = new AjaxLink<>("link")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ClickableTextCellPanel.this.onClick(target);
			}

			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				super.onComponentTag(tag);
				tag.append("class", cssClass, " ");
			}
		};
		ajaxLink.add(new Label("tekst", tekst));
		add(ajaxLink);
	}

	protected abstract void onClick(AjaxRequestTarget target);
}
