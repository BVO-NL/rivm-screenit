package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.service.ClientContactService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public abstract class MammaEmailAfspraakBevestigingPanel extends GenericPanel<MammaAfspraak>
{

	private final IModel<String> emailModel;

	@SpringBean
	private ClientContactService clientContactService;

	public MammaEmailAfspraakBevestigingPanel(String id, IModel<MammaAfspraak> afspraakModel)
	{
		super(id, afspraakModel);

		var form = new Form<>("emailForm");
		add(form);

		emailModel = Model.of(afspraakModel.getObject().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon().getEmailadres());
		var textField = new TextField<>("email", emailModel);
		textField.setRequired(true);
		textField.add(EmailAddressValidator.getInstance());
		textField.add(StringValidator.maximumLength(Persoon.MAX_EMAIL_LENGTH));
		textField.setOutputMarkupId(true);
		form.add(textField);

		form.add(new IndicatingAjaxButton("versturen")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				clientContactService.verstuurBevestigingsmail(afspraakModel.getObject(), ScreenitSession.get().getIngelogdAccount(), emailModel.getObject());
				MammaEmailAfspraakBevestigingPanel.this.close(target);
				success(getString("message.verstuurd"));
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);
}
