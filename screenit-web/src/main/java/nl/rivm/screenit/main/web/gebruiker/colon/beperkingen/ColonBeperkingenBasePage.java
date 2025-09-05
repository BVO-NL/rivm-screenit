package nl.rivm.screenit.main.web.gebruiker.colon.beperkingen;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.angular.AngularBasePage;

public abstract class ColonBeperkingenBasePage extends AngularBasePage
{
	@Override
	protected MedewerkerHoofdMenuItem getActieveMenuItem()
	{
		return MedewerkerHoofdMenuItem.COLON;
	}

	@Override
	protected List<MedewerkerMenuItem> getContextMenuItems()
	{
		var contextMenuItems = new ArrayList<MedewerkerMenuItem>();
		contextMenuItems.add(new MedewerkerMenuItem("menu.colonscreening.feestdagen", ColonFeestdagenPage.class));
		contextMenuItems.add(new MedewerkerMenuItem("menu.colonscreening.tijd.weekend", ColonWeekendWerkDagBeperkingenPage.class));
		return contextMenuItems;
	}
}
