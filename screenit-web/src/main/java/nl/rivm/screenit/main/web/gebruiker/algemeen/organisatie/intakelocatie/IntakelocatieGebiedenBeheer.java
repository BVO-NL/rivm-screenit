package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.intakelocatie;

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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.service.colon.impl.ColoscopieCentrumColonCapaciteitVerdelingDataProviderServiceImpl;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.util.PercentageUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = {
	Recht.GEBRUIKER_BEHEER_CC_GEBIEDEN }, level = ToegangLevel.INSTELLING, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class IntakelocatieGebiedenBeheer extends OrganisatieBeheer
{

	private final WebMarkupContainer refreshContainer;

	@SpringBean
	private ColoscopieCentrumColonCapaciteitVerdelingDataProviderServiceImpl dataProviderService;

	public IntakelocatieGebiedenBeheer()
	{
		var organisatie = (ColonIntakelocatie) getCurrentSelectedOrganisatie();

		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.cRModel(organisatie)));
		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		List<IColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Naam gebied"), "uitnodigingsGebied.naam", "uitnodigingsGebied.naam"));
		columns.add(new PropertyColumn<ColoscopieCentrumColonCapaciteitVerdeling, String>(Model.of("Capaciteitspercentage"), "percentageCapaciteit")
		{

			@Override
			public IModel<Object> getDataModel(IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				return new Model(PercentageUtil.percentageToString(rowModel.getObject().getPercentageCapaciteit()));
			}

		});
		ScreenitDataTable<ColoscopieCentrumColonCapaciteitVerdeling, String> gebieden = new ScreenitDataTable<ColoscopieCentrumColonCapaciteitVerdeling, String>("gebieden",
			columns, new ColoscopieCentrumColonCapaciteitVerdelingDataProvider(), 10, new Model<>("gebieden"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ColoscopieCentrumColonCapaciteitVerdeling> model)
			{

			}

			@Override
			protected boolean isRowClickable(IModel<ColoscopieCentrumColonCapaciteitVerdeling> rowModel)
			{
				return false;
			}

		};
		refreshContainer.add(gebieden);
	}

	class ColoscopieCentrumColonCapaciteitVerdelingDataProvider extends SortableDataProvider<ColoscopieCentrumColonCapaciteitVerdeling, String>
	{
		public ColoscopieCentrumColonCapaciteitVerdelingDataProvider()
		{
			super();

			setSort("uitnodigingsGebied.naam", SortOrder.ASCENDING);
		}

		@Override
		public Iterator<ColoscopieCentrumColonCapaciteitVerdeling> iterator(long first, long count)
		{
			return dataProviderService.findPage(first, count, (ColonIntakelocatie) getCurrentSelectedOrganisatie(), getSort()).iterator();
		}

		@Override
		public long size()
		{
			return dataProviderService.size((ColonIntakelocatie) getCurrentSelectedOrganisatie());
		}

		@Override
		public IModel<ColoscopieCentrumColonCapaciteitVerdeling> model(ColoscopieCentrumColonCapaciteitVerdeling object)
		{
			return ModelUtil.sModel(object);
		}
	}
}
