package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels;

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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CeRadioloogZoekPanel extends GenericPanel<MammaBeoordeling>
{

	private IModel<OrganisatieMedewerker> selectedOrganisatieMedewerkerModel;

	private WebMarkupContainer medewerkerLijstWrapper;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	public CeRadioloogZoekPanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model);
		createMedewerkerLijst();
		createHistoryList();
	}

	private static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor)
	{
		Set<Object> seen = ConcurrentHashMap.newKeySet();
		return t -> seen.add(keyExtractor.apply(t));
	}

	private void createHistoryList()
	{
		RepeatingView repeatingView = new RepeatingView("historyRepeater");
		if (getModelObject().getVerslagLezing() != null)
		{
			MammaLezing verslagLezing = baseBeoordelingService.getOrineleVerslagLezing(getModelObject());
			if (verslagLezing != null)
			{
				repeatingView.add(
					new Label(repeatingView.newChildId(), "Orginele radioloog van verslaglezing: " +
						NaamUtil.getNaamMedewerker(verslagLezing.getBeoordelaar().getMedewerker())));
			}
		}
		List<Object[]> beoordelingen = beoordelingService.beoordelingGeschiedenis(getModelObject());
		repeatingView.setVisible(false);
		beoordelingen.stream()
			.map(beoordelingRev -> (MammaBeoordeling) EntityAuditUtil.getRevisionEntity(beoordelingRev))
			.filter(beoordeling -> beoordeling.getToegewezenOp() != null && beoordeling.getToegewezenOrganisatieMedewerker() != null)
			.filter(distinctByKey(MammaBeoordeling::getToegewezenOp))
			.map(beoordelingRev -> new Label(repeatingView.newChildId(), String.format("Lezing toegewezen aan %s, op %s",
				NaamUtil.getNaamMedewerker((Medewerker) HibernateHelper.deproxy(beoordelingRev.getToegewezenOrganisatieMedewerker().getMedewerker())),
				Constants.getDateTimeFormat().format(beoordelingRev.getToegewezenOp()))))
			.peek(x -> repeatingView.setVisible(true))
			.forEachOrdered(repeatingView::add);
		add(repeatingView);
	}

	protected void createMedewerkerLijst()
	{
		medewerkerLijstWrapper = new WebMarkupContainer("medewerkerLijstWrapper");
		RepeatingView medewerkerLijst = new RepeatingView("medewerkerlijst");
		for (OrganisatieMedewerker organisatieMedewerker : getRadiologen())
		{
			WebMarkupContainer row = new WebMarkupContainer(medewerkerLijst.newChildId());
			if (selectedOrganisatieMedewerkerModel == null && getModelObject().getToegewezenOrganisatieMedewerker() != null && getModelObject().getToegewezenOrganisatieMedewerker()
				.getId()
				.equals(organisatieMedewerker.getId())
				|| selectedOrganisatieMedewerkerModel != null && selectedOrganisatieMedewerkerModel.getObject().getId().equals(organisatieMedewerker.getId()))
			{
				row.add(new AttributeAppender("class", "selected"));
			}
			row.add(new Label("naam", NaamUtil.getNaamMedewerker(organisatieMedewerker.getMedewerker())));
			row.add(new IndicatingAjaxLink<OrganisatieMedewerker>("opslaan", ModelUtil.sModel(organisatieMedewerker))
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					CeRadioloogZoekPanel.this.selectedOrganisatieMedewerkerModel = getModel();
					callback(target, getModel());
				}
			});
			row.setOutputMarkupId(true);
			medewerkerLijst.add(row);
		}
		medewerkerLijstWrapper.add(medewerkerLijst);
		medewerkerLijstWrapper.setOutputMarkupId(true);
		medewerkerLijstWrapper.setOutputMarkupPlaceholderTag(true);
		addOrReplace(medewerkerLijstWrapper);
	}

	public abstract void callback(AjaxRequestTarget target, IModel<OrganisatieMedewerker> radioloog);

	private List<OrganisatieMedewerker> getRadiologen()
	{
		return getModelObject()
			.getBeoordelingsEenheid()
			.getOrganisatieMedewerkers().stream()
			.filter(organisatieMedewerker -> getModelObject().getArbitrageLezing() != null
				? autorisatieService.getToegangLevel(organisatieMedewerker, Actie.INZIEN, true, Recht.MEDEWERKER_SCREENING_MAMMA_ARBITRAGE_WERKLIJST) != null
				: autorisatieService.getToegangLevel(organisatieMedewerker, Actie.INZIEN, true, Recht.MEDEWERKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST) != null)
			.filter(organisatieMedewerker -> getModelObject().getVerslagLezing() == null
				|| !organisatieMedewerker.getId().equals(getModelObject().getVerslagLezing().getBeoordelaar().getId())
				&& organisatieMedewerker.getMedewerker().getActief())
			.collect(Collectors.toList());
	}

	public WebMarkupContainer getMedewerkerLijstWrapper()
	{
		return medewerkerLijstWrapper;
	}

	public IModel<OrganisatieMedewerker> getSelectedOrganisatieMedewerkerModel()
	{
		return selectedOrganisatieMedewerkerModel;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedOrganisatieMedewerkerModel);
	}
}
