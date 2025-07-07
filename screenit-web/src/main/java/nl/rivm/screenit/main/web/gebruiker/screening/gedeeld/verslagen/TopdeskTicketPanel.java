package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen;

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

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public abstract class TopdeskTicketPanel extends GenericPanel<MeldingOngeldigCdaBericht>
{
	protected TopdeskTicketPanel(String id, IModel<MeldingOngeldigCdaBericht> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		var form = new ScreenitForm<>("form", getModel());
		ComponentHelper.addTextField(form, "topdeskTicket", false, 20, false);

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				TopdeskTicketPanel.this.opslaan(target, TopdeskTicketPanel.this.getModel());

			}

		});
		add(form);
	}

	protected abstract void opslaan(AjaxRequestTarget target, IModel<MeldingOngeldigCdaBericht> model);
}
