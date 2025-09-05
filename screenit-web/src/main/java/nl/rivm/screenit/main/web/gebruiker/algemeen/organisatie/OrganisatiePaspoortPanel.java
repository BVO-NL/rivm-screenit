package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.util.AdresUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class OrganisatiePaspoortPanel extends GenericPanel<Organisatie>
{
	public OrganisatiePaspoortPanel(String id, final IModel<Organisatie> model)
	{
		super(id, new CompoundPropertyModel<>(model));
		add(new Label("naam"));
		add(new Label("organisatieType"));
		add(new Label("bvos", (IModel<String>) () -> Bevolkingsonderzoek.getAfkortingen(model.getObject().getOrganisatieType().getBevolkingsonderzoeken())));
		add(new Label("adres", (IModel<String>) () -> AdresUtil.getAdres(model.getObject().getAdres())));
		add(new Label("postcodePlaats", (IModel<String>) () ->
			StringUtils.defaultIfBlank(model.getObject().getAdres().getPostcode(), "") + "  "
				+ StringUtils.defaultIfBlank(model.getObject().getAdres().getPlaats(), "")));
	}
}
