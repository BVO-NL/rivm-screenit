package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.listview;

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

import java.util.Iterator;

import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class RoosterListViewDataProvider extends SortableDataProvider<ColonAfspraakslotListViewWrapper, String>
{

	private static final long serialVersionUID = 1L;

	private final IModel<RoosterListViewFilter> zoekModel;

	private final IModel<ColonIntakelocatie> intakelocatie;

	@SpringBean
	private RoosterService roosterService;

	public RoosterListViewDataProvider(IModel<RoosterListViewFilter> zoekModel, ColonIntakelocatie intakelocatie)
	{
		this.zoekModel = zoekModel;
		this.intakelocatie = ModelUtil.sModel(intakelocatie);
		setSort("vanaf", SortOrder.ASCENDING);
		Injector.get().inject(this);

	}

	@Override
	public Iterator<ColonAfspraakslotListViewWrapper> iterator(long first, long count)
	{
		return roosterService.getAfspraakslots(getSort().getProperty(), getSort().isAscending(), first, count, zoekModel.getObject(), intakelocatie.getObject()).iterator();
	}

	@Override
	public long size()
	{
		return roosterService.getAfspraakslotsCount(zoekModel.getObject(), intakelocatie.getObject());
	}

	@Override
	public IModel<ColonAfspraakslotListViewWrapper> model(ColonAfspraakslotListViewWrapper object)
	{
		return Model.of(object);
	}

}
