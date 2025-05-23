package nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.colon.MdlVerslag;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ColonMdlVerslagInzienPanel extends GenericPanel<MdlVerslag>
{
	public ColonMdlVerslagInzienPanel(String id, IModel<MdlVerslag> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new Label("verslagContent.coloscopieMedischeObservatie.definitiefVervolgbeleidVoorBevolkingsonderzoekg.definitiefVervolgbeleidVoorBevolkingsonderzoek.displayNameNl"));
		add(new Label("verslagContent.coloscopieMedischeObservatie.definitiefVervolgbeleidVoorBevolkingsonderzoekg.periodeVervolgSurveillancescopie.displayNameNl"));
		add(DateLabel.forDatePattern("verslagContent.verrichting.aanvangVerrichting", Constants.DEFAULT_DATE_FORMAT));
	}
}
