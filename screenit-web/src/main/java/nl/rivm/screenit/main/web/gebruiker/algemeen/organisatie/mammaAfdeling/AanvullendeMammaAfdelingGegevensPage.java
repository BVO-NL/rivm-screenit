package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.mammaAfdeling;

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
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.OrganisatieService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class AanvullendeMammaAfdelingGegevensPage extends OrganisatieBeheer
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	protected boolean inzien = false;

	protected void createSubmit(Form form, IModel model)
	{
		form.add(new AjaxSubmitLink("submit")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Organisatie organisatie = (Organisatie) model.getObject();
				organisatieService.saveOrUpdate(organisatie);
				BasePage.markeerFormulierenOpgeslagen(target);
				this.info("Gegevens zijn succesvol opgeslagen");
			}
		});
	}

	protected void createAnnuleren(Form form)
	{
		AjaxLink<Medewerker> annuleren = new AjaxLink<Medewerker>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}

			@Override
			public boolean isVisible()
			{
				return !inzien;
			}
		};
		form.add(annuleren);
	}

	protected void setAlleenInzien(Organisatie organisatie, Recht recht)
	{
		Actie actie = autorisatieService.getActieVoorOrganisatie(getIngelogdeOrganisatieMedewerker(), organisatie,
			recht);
		inzien = !isMinimumActie(actie, Actie.AANPASSEN);
	}

}
