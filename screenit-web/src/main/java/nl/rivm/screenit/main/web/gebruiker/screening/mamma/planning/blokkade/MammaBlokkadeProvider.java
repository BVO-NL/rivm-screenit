
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.blokkade;

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

import nl.rivm.screenit.main.service.mamma.impl.MammaBlokkadeDataProviderServiceImpl;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaBlokkadeProvider extends SortableDataProvider<MammaBlokkade, String>
{
	private IModel<MammaBlokkade> zoekObjectModel;

	@SpringBean
	private MammaBlokkadeDataProviderServiceImpl blokkadeDataProviderService;

	public MammaBlokkadeProvider(IModel<MammaBlokkade> zoekObjectModel)
	{
		Injector.get().inject(this);
		setSort("vanaf", SortOrder.ASCENDING);
		this.zoekObjectModel = zoekObjectModel;
	}

	@Override
	public Iterator<? extends MammaBlokkade> iterator(long first, long count)
	{
		var sort = WicketSpringDataUtil.toSpringSort(getSort());

		return blokkadeDataProviderService.findPage(first, count, zoekObjectModel.getObject(), sort).iterator();
	}

	@Override
	public long size()
	{
		return blokkadeDataProviderService.size(zoekObjectModel.getObject());
	}

	@Override
	public IModel<MammaBlokkade> model(MammaBlokkade blokkade)
	{
		return ModelUtil.cModel(blokkade);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
