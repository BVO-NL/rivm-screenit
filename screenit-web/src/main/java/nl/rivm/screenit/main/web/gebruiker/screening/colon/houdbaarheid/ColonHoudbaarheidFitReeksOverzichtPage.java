package nl.rivm.screenit.main.web.gebruiker.screening.colon.houdbaarheid;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidEditPage;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidOverzichtPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.COLON_BEHEER_HOUDBAARHEID_FIT_REEKSEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonHoudbaarheidFitReeksOverzichtPage extends HoudbaarheidOverzichtPage<ColonHoudbaarheidFitReeks>
{

	@Override
	protected List<IColumn<ColonHoudbaarheidFitReeks, String>> createColumns()
	{
		List<IColumn<ColonHoudbaarheidFitReeks, String>> columns = super.createColumns();
		columns.add(new PropertyColumn<ColonHoudbaarheidFitReeks, String>(new Model<String>("Type"), "type", "type"));
		return columns;
	}

	@Override
	protected HoudbaarheidEditPage<ColonHoudbaarheidFitReeks> createEditPage(IModel<ColonHoudbaarheidFitReeks> model)
	{
		if (model == null)
		{
			return new ColonHoudbaarheidFitReeksEditPage();
		}
		else
		{
			return new ColonHoudbaarheidFitReeksEditPage(model);
		}
	}

	@Override
	protected MedewerkerHoofdMenuItem getActieveMenuItem()
	{
		return MedewerkerHoofdMenuItem.COLON;
	}

}
