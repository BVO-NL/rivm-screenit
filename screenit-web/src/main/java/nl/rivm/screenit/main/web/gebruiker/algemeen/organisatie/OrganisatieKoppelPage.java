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

import java.util.List;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerBasisgegevens;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker.OrganisatieMedewerkerKoppelPage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OrganisatieKoppelPage extends OrganisatieMedewerkerKoppelPage
{
	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private LogService logService;

	public OrganisatieKoppelPage()
	{
		super(true);
	}

	@Override
	public Actie getActie(Recht recht)
	{
		Actie actie = autorisatieService.getActieVoorMedewerker(
			getIngelogdeOrganisatieMedewerker(), 
			getCurrentSelectedMedewerker(), 
			recht);
		return actie;
	}

	@Override
	protected Panel getPaspoortPanel(String id)
	{
		return new OrganisatiePaspoortPanel(id, ModelUtil.cRModel(getCurrentSelectedOrganisatie()));
	}

	@Override
	protected void onNavigeerNaar(IModel<OrganisatieMedewerker> rowModel, AjaxRequestTarget target)
	{
		OrganisatieMedewerker organisatieMedewerker = rowModel.getObject();
		Medewerker medewerker = organisatieMedewerker.getMedewerker();
		setCurrentSelectedMedewerker(medewerker);
		setCurrentSelectedOrganisatie(null);
		setResponsePage(new MedewerkerBasisgegevens(ModelUtil.cModel(medewerker)));
	}

	@Override
	protected boolean magNavigerenNaar(IModel<OrganisatieMedewerker> rowModel)
	{
		OrganisatieMedewerker organisatieMedewerker = rowModel.getObject();
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker = getIngelogdeOrganisatieMedewerker();
		return autorisatieService.getActieVoorMedewerker(ingelogdeOrganisatieMedewerker, organisatieMedewerker.getMedewerker(), Recht.MEDEWERKER_BEHEER) != null;
	}

	@Override
	protected OrganisatieMedewerker createSearchObject()
	{
		OrganisatieMedewerker searchObject = super.createSearchObject();
		searchObject.setOrganisatie(getCurrentSelectedOrganisatie());
		Medewerker medewerker = new Medewerker();
		medewerker.setActief(Boolean.TRUE);
		searchObject.setMedewerker(medewerker);
		return searchObject;
	}

	@Override
	protected void onToevoegen(final IDialog dialog, AjaxRequestTarget target, final WebMarkupContainer refreshContainer)
	{
		dialog.setContent(new MedewerkerSmallZoekPanel(IDialog.CONTENT_ID, null)
		{

			@Override
			protected void setVorigZoekObject(Medewerker organisatieSearchObject)
			{
			}

			@Override
			protected Medewerker getVorigZoekObject()
			{
				return null;
			}

			@Override
			protected void onCloseWithSelected(AjaxRequestTarget target, IModel<Medewerker> model)
			{
				Organisatie organisatie = getCurrentSelectedOrganisatie();
				Organisatie loggedInOrganisatie = getIngelogdeOrganisatieMedewerker().getOrganisatie();
				if (organisatie.equals(loggedInOrganisatie))
				{
					organisatie = loggedInOrganisatie;
				}
				Medewerker medewerker = ModelUtil.nullSafeGet(model);
				if (medewerker != null)
				{
					medewerkerService.addOrganisatieMedewerker(organisatie, medewerker);
					logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_MEDEWERKER_KOPPEL, getIngelogdeOrganisatieMedewerker(),
						String.format("Medewerker %1$s gekoppeld aan organisatie %2$s", medewerker.getNaamVolledigMetVoornaam(), organisatie.getNaam()));
					target.add(refreshContainer);
				}
				dialog.close(target);
			}
		});
		dialog.open(target);
	}

	@Override
	protected List<MedewerkerMenuItem> getContextMenuItems()
	{
		return OrganisatieBeheer.createContextMenu();
	}
}
