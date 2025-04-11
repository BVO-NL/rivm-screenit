package nl.rivm.screenit.main.web.gebruiker.algemeen.extrabeveiligdeomgeving;

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

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.angular.AngularBasePage;

import java.util.ArrayList;
import java.util.List;

public abstract class ExtraBeveiligdeOmgevingBasePage extends AngularBasePage
{
	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.ALGEMEEN;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		var contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.extra.beveiligde.omgeving.keuze.herstellen", ExtraBeveiligdeOmgevingKeuzeHerstellenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.extra.beveiligde.omgeving.client.zoeken", ExtraBeveiligdeOmgevingClientZoekenPage.class));
		return contextMenuItems;
	}
}
