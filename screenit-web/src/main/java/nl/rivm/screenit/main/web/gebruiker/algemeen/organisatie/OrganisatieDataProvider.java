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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OrganisatieDataProvider extends SortableDataProvider<Instelling, String>
{

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private AutorisatieService autorisatieService;

	private final IModel<List<OrganisatieType>> selectedOrganisatieTypes;

	private final List<OrganisatieType> excludeOrganisatieTypes;

	private final IModel<Instelling> criteralModel;

	public OrganisatieDataProvider(IModel<Instelling> criteriaModel, IModel<List<OrganisatieType>> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		String defaultSortProperty)
	{
		Injector.get().inject(this);
		this.criteralModel = criteriaModel;
		setSort(defaultSortProperty, SortOrder.ASCENDING);
		this.selectedOrganisatieTypes = selectedOrganisatieTypes;
		this.excludeOrganisatieTypes = excludeOrganisatieTypes;
	}

	@Override
	public Iterator<Instelling> iterator(long first, long count)
	{
		String sortProperty = null;
		boolean asc = true;
		if (getSort() != null)
		{
			sortProperty = getSort().getProperty();
			asc = getSort().isAscending();
		}
		return organisatieZoekService.zoekOrganisaties(ModelUtil.nullSafeGet(criteralModel), getSelectedOrganisatieTypes(), excludeOrganisatieTypes,
			ScreenitSession.get().getLoggedInInstellingGebruiker(), first, count, sortProperty, asc).iterator();
	}

	@Override
	public long size()
	{
		return organisatieZoekService.countOrganisaties(ModelUtil.nullSafeGet(criteralModel), getSelectedOrganisatieTypes(), excludeOrganisatieTypes,
			ScreenitSession.get().getLoggedInInstellingGebruiker());
	}

	@Override
	public IModel<Instelling> model(Instelling object)
	{
		return ModelUtil.sModel(object);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(criteralModel);
	}

	private List<OrganisatieType> getSelectedOrganisatieTypes()
	{
		List<OrganisatieType> organisatieTypes = ModelUtil.nullSafeGet(selectedOrganisatieTypes);
		if (selectedOrganisatieTypes != null && CollectionUtils.isEmpty(organisatieTypes))
		{

			organisatieTypes = autorisatieService.getOrganisatieTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), true);
		}
		return organisatieTypes;
	}
}
