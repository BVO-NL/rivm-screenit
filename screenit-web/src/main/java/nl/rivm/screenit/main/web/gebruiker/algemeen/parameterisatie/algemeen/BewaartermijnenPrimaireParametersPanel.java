package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.algemeen;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.BasePrimaireParametersPanel;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.validation.validator.RangeValidator;

public class BewaartermijnenPrimaireParametersPanel extends BasePrimaireParametersPanel
{
	public BewaartermijnenPrimaireParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");
		form.add(new TextField<>("ilmBewaartermijn", Integer.class).add(RangeValidator.minimum(5475)).setRequired(true));
		form.add(new TextField<>("ilmBewaartermijnNietMedisch", Integer.class).add(RangeValidator.minimum(1825)).setRequired(true));
		form.add(new TextField<>("ilmSignaleertermijnBeeldenStatus", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("ilmBewaartermijnPalga", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		form.add(new TextField<>("leeftijdPersoonsgegevensUitExtraBeveiligdeOmgevingVerwijderen", Integer.class).add(RangeValidator.minimum(1)).setRequired(true));
		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("opslaan")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan(target, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA);
				info("Bewaartermijnen zijn opgeslagen");
			}
		};
	}
}
