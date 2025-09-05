
package nl.rivm.screenit.main.web.gebruiker.base;

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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;

import org.apache.wicket.Session;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;

public class SubMenuPanel extends Panel
{
	public SubMenuPanel(String id, List<IMenuItem> model)
	{
		super(id);

		Iterator<IMenuItem> gebruikerMenuItemIterator = model.iterator();
		while (gebruikerMenuItemIterator.hasNext())
		{
			IMenuItem menuItem = gebruikerMenuItemIterator.next();
			if (menuItem instanceof MedewerkerMenuItem medewerkerMenuItem)
			{
				Class<? extends MedewerkerBasePage> targetPageClass = MedewerkerMenuItem.getTargetPageClass(medewerkerMenuItem);
				if (targetPageClass == null || !Session.get().getAuthorizationStrategy().isInstantiationAuthorized(targetPageClass))
				{
					gebruikerMenuItemIterator.remove();
				}
			}
		}

		add(new ListView<>("subMenuItems", model)
		{
			@Override
			protected void populateItem(ListItem<IMenuItem> item)
			{
				if (item.getModelObject() instanceof MedewerkerMenuItem medewerkerMenuItem)
				{
					Link<Object> link = new Link<Object>("subMenuLink")
					{
						@Override
						public void onClick()
						{
							ScreenitSession.get().resetZoekStatus();
							setResponsePage(MedewerkerMenuItem.getTargetPageClass(medewerkerMenuItem));
						}

					};
					item.add(link);
					link.add(new Label("subMenuNaam", new SimpleStringResourceModel(medewerkerMenuItem.getResourceTag())));
				}
				else if (item.getModelObject() instanceof MenuDivider)
				{
					item.add(new AttributeAppender("class", "divider"));
					item.add(new WebMarkupContainer("subMenuLink").setVisible(false));
				}
			}
		});
	}
}
