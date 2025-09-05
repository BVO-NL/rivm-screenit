package nl.rivm.screenit.main.web.gebruiker.login;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class PasswordRequestPage extends LoginBasePage
{
	@SpringBean
	private AuthenticatieService authenticatieService;

	@SpringBean
	private LogService logService;

	public PasswordRequestPage(PageParameters pageParameters)
	{
		Medewerker medewerker = new Medewerker();
		if (pageParameters != null && pageParameters.get("naam") != null && !pageParameters.get("naam").equals("null"))
		{
			medewerker.setGebruikersnaam(pageParameters.get("naam").toString());
		}
		final Form<Medewerker> form = new Form<>("requestForm");
		form.setDefaultModel(ModelUtil.csModel(medewerker));

		ComponentHelper.addTextField(form, "gebruikersnaam", false, 50, false).add(new FocusBehavior());
		ComponentHelper.addTextField(form, "emailextra", false, 50, false);

		AjaxButton zoeken = new AjaxButton("zoek", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var zoekMedewerker = form.getModelObject();
				if (StringUtils.isBlank(zoekMedewerker.getGebruikersnaam()) && StringUtils.isBlank(zoekMedewerker.getEmailextra()))
				{
					error(getString("error.password.request"));
					return;
				}

				requestPassword(zoekMedewerker);
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
				super.onError(target);
				error(getString("error.password.request"));
			}

		};
		form.add(zoeken);
		form.setDefaultButton(zoeken);
		add(form);

	}

	private void requestPassword(Medewerker zoekMedewerker)
	{
		var medewerker = authenticatieService.requestNewPassword(zoekMedewerker.getGebruikersnaam(), zoekMedewerker.getEmailextra());

		if (medewerker != null)
		{
			if (medewerker.getEmailextra() == null)
			{
				logService.logGebeurtenis(LogGebeurtenis.AANVRAGEN_ACCOUNT_MISLUKT, medewerker, "Er is geen mail adres aanwezig voor deze medewerker");
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_AANGEVRAAGD, medewerker);
			}
		}
		info(getString("info.gegevens.verstuurd"));
	}
}
