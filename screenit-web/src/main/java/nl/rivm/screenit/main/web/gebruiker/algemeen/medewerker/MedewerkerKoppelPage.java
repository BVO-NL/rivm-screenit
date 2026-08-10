package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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

import java.util.List;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBasisgegevens;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker.OrganisatieMedewerkerKoppelPage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
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

public class MedewerkerKoppelPage extends OrganisatieMedewerkerKoppelPage
{

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private MedewerkerService medewerkerService;

	public MedewerkerKoppelPage()
	{
		super(false);
	}

	@Override
	public Actie getActie(Recht recht)
	{
		var actie = autorisatieService.getActieVoorOrganisatie(
			getIngelogdeOrganisatieMedewerker(), 
			getCurrentSelectedOrganisatie(), 
			recht);
		return actie;
	}

	@Override
	protected Panel getPaspoortPanel(String id)
	{
		return new MedewerkerPaspoortPanel(id, ModelUtil.cRModel(getCurrentSelectedMedewerker()));
	}

	@Override
	protected void onNavigeerNaar(IModel<OrganisatieMedewerker> rowModel, AjaxRequestTarget target)
	{
		var organisatie = rowModel.getObject().getOrganisatie();
		setCurrentSelectedOrganisatie(organisatie);
		setResponsePage(new OrganisatieBasisgegevens(ModelUtil.cModel(organisatie)));
	}

	@Override
	protected boolean magNavigerenNaar(IModel<OrganisatieMedewerker> rowModel)
	{
		var ingelogdeOrganisatieMedewerker = getIngelogdeOrganisatieMedewerker();
		var organisatieMedewerker = rowModel.getObject();
		var organisatie = organisatieMedewerker.getOrganisatie();
		var recht = organisatie.getOrganisatieType().getRecht();
		if (recht != null && autorisatieService.getActieVoorOrganisatie(ingelogdeOrganisatieMedewerker, organisatie, recht) != null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected OrganisatieMedewerker createSearchObject()
	{
		var searchObject = super.createSearchObject();
		searchObject.setMedewerker(getCurrentSelectedMedewerker());
		return searchObject;
	}

	@Override
	protected void onToevoegen(final IDialog dialog, AjaxRequestTarget target, final WebMarkupContainer medewerkerContainer)
	{
		dialog.setContent(new OrganisatieSmallZoekPanel(IDialog.CONTENT_ID)
		{
			@Override
			protected void setVorigZoekObject(Organisatie organisatieSearchObject)
			{
			}

			@Override
			protected Organisatie getVorigZoekObject()
			{
				return null;
			}

			@Override
			protected void onCloseWithSelected(AjaxRequestTarget target, IModel<Organisatie> model)
			{
				var medewerker = getCurrentSelectedMedewerker();
				var loggedInMedewerker = getIngelogdeOrganisatieMedewerker().getMedewerker();
				if (medewerker.equals(loggedInMedewerker))
				{
					medewerker = loggedInMedewerker;
				}
				var organisatie = ModelUtil.nullSafeGet(model);
				if (organisatie != null)
				{

					medewerkerService.addOrganisatieMedewerker(organisatie, medewerker);
					logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_MEDEWERKER_KOPPEL, getIngelogdeOrganisatieMedewerker(),
						String.format("Medewerker %1$s gekoppeld aan organisatie %2$s", medewerker.getNaamVolledig(), organisatie.getNaam()));
					target.add(medewerkerContainer);
				}
				dialog.close(target);
			}
		});
		dialog.open(target);
	}

	@Override
	protected List<MedewerkerMenuItem> getContextMenuItems()
	{
		return MedewerkerBeheer.createContextMenu();
	}
}
