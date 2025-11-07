package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.afspraken.MammaBaseAfspraakOptieDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.MammaDagEnDagdeelFilter;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaAfspraakOptiesProvider extends SortableDataProvider<MammaBaseAfspraakOptieDto, String>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	private final IModel<Client> clientModel;

	private final IModel<MammaAfspraakWijzigenFilter> filterModel;

	private final MammaDagEnDagdeelFilter dagEnDagdeelFilter;

	private List<MammaBaseAfspraakOptieDto> afspraakOpties;

	private List<MammaBaseAfspraakOptieDto> afspraakOptiesCache = new ArrayList<>();

	private boolean lijstBehouden = false;

	public MammaAfspraakOptiesProvider(IModel<Client> clientModel, IModel<MammaAfspraakWijzigenFilter> filterModel, MammaDagEnDagdeelFilter dagEnDagdeelFilter)
	{
		Injector.get().inject(this);
		this.filterModel = filterModel;
		this.clientModel = clientModel;
		this.dagEnDagdeelFilter = dagEnDagdeelFilter;
	}

	@Override
	public Iterator<? extends MammaBaseAfspraakOptieDto> iterator(long first, long count)
	{
		return afspraakOpties.subList((int) first, (int) (first + count)).iterator();
	}

	@Override
	public long size()
	{
		if (!lijstBehouden)
		{
			afspraakOptiesCache = baseAfspraakService.getAfspraakOpties(clientModel.getObject(), filterModel.getObject()).stream().distinct().sorted()
				.collect(Collectors.toList());
		}
		lijstBehouden = false;
		afspraakOpties = baseAfspraakService.filterAfspraakOptiesOpDagEnDagdeel(afspraakOptiesCache, dagEnDagdeelFilter);
		return afspraakOpties.size();
	}

	@Override
	public IModel<MammaBaseAfspraakOptieDto> model(MammaBaseAfspraakOptieDto afspraakOptieDto)
	{
		return Model.of(afspraakOptieDto);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(filterModel);
		ModelUtil.nullSafeDetach(clientModel);
	}

	public void setLijstBehouden(boolean lijstBehouden)
	{
		this.lijstBehouden = lijstBehouden;
	}
}
