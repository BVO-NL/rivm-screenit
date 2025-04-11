package nl.rivm.screenit.main.web.gebruiker.algemeen.rollenrechten;

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

import nl.rivm.screenit.main.service.algemeen.impl.RolDataProviderServiceImpl;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.BvoColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_ROLLEN_BEHEREN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class RollenOverzichtPanel extends GenericPanel<Rol>
{
	@SpringBean
	private RolDataProviderServiceImpl rolDataProviderService;

	private WebMarkupContainer refreshContainer;

	private IModel<Rol> rolModel;

	private final boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_ROLLEN_BEHEREN, Actie.VERWIJDEREN);

	public RollenOverzichtPanel(String id)
	{
		super(id);
		this.rolModel = getZoekRolModel();

		add(new FilterBvoFormPanel<Rol>("bvoFilter", this.rolModel)
		{
			@Override
			protected void doFilter(IModel<Rol> filterModel, AjaxRequestTarget target)
			{
				ScreenitSession.get().setZoekObject(RollenOverzichtPanel.class, filterModel);
				target.add(refreshContainer);
			}

		});

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		getRollenListView(this.rolModel);

		AjaxLink<Void> rolToevoegenKnop = new AjaxLink<>("rolToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				Rol rol = new Rol();
				Permissie rolRecht = new Permissie(rol);
				rol.getPermissies().add(rolRecht);
				((RollenBeheer) getPage()).addOrReplaceContentWith(new RolEditPanel(RollenBeheer.CONTENT_ID, ModelUtil.cModel(rol)));
			}
		};
		rolToevoegenKnop.setVisible(magVerwijderen);
		add(rolToevoegenKnop);
	}

	private IModel<Rol> getZoekRolModel()
	{
		ScreenitSession session = ScreenitSession.get();
		if ((rolModel = (IModel<Rol>) session.getZoekObject(RollenOverzichtPanel.class)) != null)
		{
			return rolModel;
		}
		return ModelUtil.cModel(new Rol());
	}

	private ScreenitDataTable<Rol, String> getRollenListView(IModel<Rol> zoekRol)
	{
		List<IColumn<Rol, String>> columns = new ArrayList<>();
		columns.add(new BvoColumn<>(Model.of("Bvo")));
		columns.add(new PropertyColumn<>(Model.of("Naam"), "naam", "naam"));
		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", refreshContainer, zoekRol));

		ScreenitDataTable<Rol, String> linkDataTable = new ScreenitDataTable<Rol, String>("rollen", columns,
			new RolDataProvider(), Model.of("rollen"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<Rol> model)
			{
				((RollenBeheer) getPage()).addOrReplaceContentWith(new RolEditPanel(RollenBeheer.CONTENT_ID, ModelUtil.cModel(model.getObject())));
			}
		};
		refreshContainer.add(linkDataTable);
		return linkDataTable;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(this.rolModel);
	}

	class RolDataProvider extends SortableDataProvider<Rol, String>
	{
		public RolDataProvider()
		{
			super();
			setSort("naam", SortOrder.ASCENDING);
		}

		@Override
		public Iterator<Rol> iterator(long first, long count)
		{
			return rolDataProviderService.findPage(first, count, ModelUtil.nullSafeGet(rolModel), getSort()).iterator();
		}

		@Override
		public long size()
		{
			return rolDataProviderService.size(ModelUtil.nullSafeGet(rolModel));
		}

		@Override
		public IModel<Rol> model(Rol rol)
		{
			return ModelUtil.sModel(rol);
		}
	}
}
