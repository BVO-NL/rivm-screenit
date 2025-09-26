package nl.rivm.screenit.main.web.component.form;

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

import nl.rivm.screenit.model.BriefDefinitiesFilter;

import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;

public abstract class FilterBriefBeheerFormPanel extends FilterBvoFormPanel<BriefDefinitiesFilter>
{
	public FilterBriefBeheerFormPanel(String id, IModel<BriefDefinitiesFilter> model, boolean altijdZichtbaar, boolean showExactMatch)
	{
		super(id, model, altijdZichtbaar, showExactMatch, false);

		form.add(new TextField<>("naam"));
		form.add(new CheckBox("brievenNietMeerInGebruikOokTonen"));
	}
}
