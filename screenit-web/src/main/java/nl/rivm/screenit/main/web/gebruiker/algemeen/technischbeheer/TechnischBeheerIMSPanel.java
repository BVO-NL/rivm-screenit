package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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

import java.util.Base64;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.model.mamma.IMSConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.NumberTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.RawValidationError;

public class TechnischBeheerIMSPanel extends GenericPanel<IMSConfiguratie>
{
	@SpringBean
	private ParameterisatieService parameterisatieService;

	public TechnischBeheerIMSPanel(String imsConfiguratie, IModel<IMSConfiguratie> imsConfiguratieModel)
	{
		super(imsConfiguratie, CompoundPropertyModel.of(imsConfiguratieModel));
		createIMSConfigForm();
	}

	private void createIMSConfigForm()
	{
		var imsConfigForm = new Form<IMSConfiguratie>("imsConfigForm");
		imsConfigForm.add(new TextField<>("hostName").setRequired(true));
		imsConfigForm.add(new NumberTextField<>("ormPort").setRequired(true));
		imsConfigForm.add(new NumberTextField<>("adtPort").setRequired(true));
		imsConfigForm.add(new NumberTextField<>("ilmPort").setRequired(true));

		imsConfigForm.add(new NumberTextField<>("bezwaarTermijnVerwijderdeBeelden").setRequired(true));
		imsConfigForm.add(new NumberTextField<>("imsQueueSizeWarningThreshold").setRequired(true));
		imsConfigForm.add(new TextField<String>("launchUrlPassword").setRequired(true).add(new Base64EncodedValidator()));

		var opslaanButton = createAndGetOpslaanButton();
		opslaanButton.setVisible(magAanpassen());
		imsConfigForm.add(opslaanButton);

		add(imsConfigForm.setEnabled(magAanpassen()));
	}

	private Component createAndGetOpslaanButton()
	{
		return new AjaxSubmitLink("submitIMSConfiguratie")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaanIMSConfiguratieSettings();
				info("IMS configuratie is opgeslagen");
			}
		};
	}

	private void opslaanIMSConfiguratieSettings()
	{
		parameterisatieService.saveIMSConfiguratie(ScreenitSession.get().getIngelogdAccount(), getModelObject());
	}

	private boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.TECHNISCH_BEHEER, Actie.AANPASSEN);
	}

	@RequiredArgsConstructor
	private static class Base64EncodedValidator implements IValidator<String>
	{

		@Override
		public void validate(IValidatable<String> validatable)
		{
			try
			{
				Base64.getDecoder().decode(validatable.getValue());
			}
			catch (IllegalArgumentException e)
			{
				validatable.error(new RawValidationError("Launch URL wachtwoord moet een Base64 gecodeerde waarde zijn."));
			}
		}
	}
}
