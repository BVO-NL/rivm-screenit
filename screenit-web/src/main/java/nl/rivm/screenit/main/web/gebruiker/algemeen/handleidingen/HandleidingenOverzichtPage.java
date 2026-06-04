package nl.rivm.screenit.main.web.gebruiker.algemeen.handleidingen;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.angular.AngularBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.HANDLEIDINGEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class HandleidingenOverzichtPage extends AngularBasePage
{
	@Override
	protected String getComponent()
	{
		return "handleidingen-overzicht";
	}

	@Override
	protected MedewerkerHoofdMenuItem getActieveMenuItem()
	{
		return MedewerkerHoofdMenuItem.ALGEMEEN;
	}

	@Override
	protected List<MedewerkerMenuItem> getContextMenuItems()
	{
		List<MedewerkerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new MedewerkerMenuItem("label.handleidingen", HandleidingenOverzichtPage.class));
		return contextMenuItems;
	}
}
