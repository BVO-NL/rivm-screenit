package nl.rivm.screenit.main.web.component.bezwaar.edit;

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

import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

public class BezwaarEditPanel extends Panel
{
	public BezwaarEditPanel(String id, List<BezwaarGroupViewWrapper> wrappers)
	{
		super(id);

		var sortedWrappers = wrappers.stream()
			.sorted(Comparator.comparing(
				BezwaarGroupViewWrapper::getBevolkingsonderzoek,
				Comparator.nullsFirst(Comparator.comparing(Bevolkingsonderzoek::getNaam))
			))
			.toList();

		var listView = new ListView<>("listView", sortedWrappers)
		{
			@Override
			protected void populateItem(ListItem<BezwaarGroupViewWrapper> item)
			{
				BezwaarGroupViewWrapper wrapper = item.getModelObject();

				WebMarkupContainer container = new WebMarkupContainer("container");
				container.add(new Label("bvo", getString("Bevolkingsonderzoek." + wrapper.getKey())));

				var bezwaren = new ListView<>("bezwaren", wrapper.getBezwaren())
				{
					@Override
					protected void populateItem(ListItem<BezwaarViewWrapper> item)
					{
						item.add(new BezwaarCheckBox("bezwaarCheckBox", item.getModel()));
					}
				};
				container.add(bezwaren);
				item.add(container);
			}
		};
		add(listView);
	}
}
