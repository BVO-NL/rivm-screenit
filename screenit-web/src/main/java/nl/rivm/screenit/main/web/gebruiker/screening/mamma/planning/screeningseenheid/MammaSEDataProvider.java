package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

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

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaScreeningsEenheidFilter;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;

public class MammaSEDataProvider extends SortableDataProvider<MammaScreeningsEenheid, String>
{
	private final IModel<MammaScreeningsEenheidFilter> filter;

	private final boolean gesteldheidOphalen;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	public MammaSEDataProvider(IModel<MammaScreeningsEenheidFilter> filter)
	{
		this(filter, false);
	}

	public MammaSEDataProvider(IModel<MammaScreeningsEenheidFilter> filter, boolean gesteldheidOphalen)
	{
		Injector.get().inject(this);
		this.filter = filter;
		setSort("code", SortOrder.ASCENDING);
		this.gesteldheidOphalen = gesteldheidOphalen;
	}

	@Override
	public Iterator<? extends MammaScreeningsEenheid> iterator(long first, long count)
	{
		var filterObject = filter.getObject();
		var screeningsEenheden = screeningsEenheidService.zoekScreeningsEenheden(filterObject.getScreeningsEenheid(), filterObject.getRegio(),
			first, count, toSpringSort(getSort()));

		if (gesteldheidOphalen)
		{
			for (var screeningsEenheid : screeningsEenheden)
			{
				screeningsEenheid.setMetaDataDto(baseConceptPlanningsApplicatie.getScreeningsEenheidMetaData(screeningsEenheid));
			}
		}
		return screeningsEenheden.iterator();
	}

	@Override
	public long size()
	{
		return screeningsEenheidService.countScreeningsEenheden(filter.getObject().getScreeningsEenheid(), filter.getObject().getRegio());
	}

	@Override
	public IModel<MammaScreeningsEenheid> model(MammaScreeningsEenheid screeningsEenheid)
	{
		return ModelUtil.sModel(screeningsEenheid);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(filter);
	}
}
