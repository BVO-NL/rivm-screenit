package nl.rivm.screenit.main.web.gebruiker.screening.colon.houdbaarheid;

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

import java.util.Arrays;

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidEditPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.COLON_BEHEER_HOUDBAARHEID_FIT_REEKSEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonHoudbaarheidFitReeksEditPage extends HoudbaarheidEditPage<ColonHoudbaarheidFitReeks>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private HoudbaarheidService houdbaarheidService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ColonHoudbaarheidFitReeksEditPage()
	{
		this(ModelUtil.ccModel(new ColonHoudbaarheidFitReeks()));
	}

	public ColonHoudbaarheidFitReeksEditPage(IModel<ColonHoudbaarheidFitReeks> model)
	{
		super(model);

		if (model.getObject().getType() == null)
		{
			model.getObject().setType(ColonFitType.GOLD);
		}
	}

	@Override
	protected IModel<String> getTitleModel(IModel<ColonHoudbaarheidFitReeks> model)
	{
		final boolean isNieuw = model.getObject().getId() == null;
		return () ->
		{
			if (isNieuw)
			{
				return "Nieuwe FIT batch";
			}
			return "Wijzigen FIT batch";
		};
	}

	@Override
	protected FormComponent<ColonFitType> createTypeField(String id)
	{
		ScreenitDropdown<ColonFitType> type = new ScreenitDropdown<ColonFitType>(id, Arrays.asList(ColonFitType.values()), new EnumChoiceRenderer<ColonFitType>());
		type.setRequired(true);
		return type;
	}

	@Override
	protected Class<? extends MedewerkerBasePage> getActiveSubMenuClass()
	{
		return ColonHoudbaarheidFitReeksOverzichtPage.class;
	}

	@Override
	protected MedewerkerHoofdMenuItem getActieveMenuItem()
	{
		return MedewerkerHoofdMenuItem.COLON;
	}
}
