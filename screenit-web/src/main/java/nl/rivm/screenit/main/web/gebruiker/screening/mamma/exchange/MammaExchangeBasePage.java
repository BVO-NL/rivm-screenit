package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

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

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaClientZoekenBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.download.MammaExchangeDownloadPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup.MammaFollowUpRadiologieVerslagPage;
import nl.rivm.screenit.model.Client;

public abstract class MammaExchangeBasePage extends MammaClientZoekenBasePage
{

	protected MammaExchangeBasePage()
	{
		super();
	}

	protected MammaExchangeBasePage(Client client)
	{
		super(client);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return getContextMenuItemsList();
	}

	public static List<GebruikerMenuItem> getContextMenuItemsList()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.exchange.download",
			MammaExchangeDownloadPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.exchange.upload",
			MammaExchangeUploadPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.mammascreening.radiologie", MammaFollowUpRadiologieVerslagPage.class));

		return contextMenuItems;
	}
}
