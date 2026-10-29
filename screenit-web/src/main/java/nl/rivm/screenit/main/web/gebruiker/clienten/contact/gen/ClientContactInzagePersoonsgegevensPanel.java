package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.algemeen.OverdrachtType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientContactInzagePersoonsgegevensPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private final IModel<Client> clientModel;

	private final IModel<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevensModel = maakOverdrachtPersoonsgegevens();

	@SpringBean
	private OverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	private WebMarkupContainer keuzesContainer;

	private ScreenitDropdown<OverdrachtType> typeDropDown;

	public ClientContactInzagePersoonsgegevensPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;

		var container = new WebMarkupContainer("container", overdrachtPersoonsgegevensModel);
		add(container);

		addTypeSelect(container);
		addBvoKeuzeCheckboxes(container);
	}

	private IModel<OverdrachtPersoonsgegevens> maakOverdrachtPersoonsgegevens()
	{
		var overdrachtPersoonsgegevens = new OverdrachtPersoonsgegevens();
		overdrachtPersoonsgegevens.setBkGegevens(false);
		overdrachtPersoonsgegevens.setBkBeelden(false);
		overdrachtPersoonsgegevens.setBmhkGegevens(false);
		overdrachtPersoonsgegevens.setDkGegevens(false);
		return ModelUtil.ccModel(overdrachtPersoonsgegevens);
	}

	private void addTypeSelect(WebMarkupContainer container)
	{
		var aanvraagTypes = List.of(OverdrachtType.values());

		typeDropDown = ComponentHelper.addDropDownChoice(container, "type", true, aanvraagTypes, false);
		typeDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(keuzesContainer);
			}
		});
	}

	private void addBvoKeuzeCheckboxes(WebMarkupContainer container)
	{
		keuzesContainer = new WebMarkupContainer("keuzesContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(typeDropDown.getModelObject() == OverdrachtType.CLIENTPORTAAL);
			}
		};
		container.add(keuzesContainer);
		keuzesContainer.setOutputMarkupPlaceholderTag(true);
		var client = clientModel.getObject();
		keuzesContainer.add(ComponentHelper.newCheckBox("bkGegevens").setEnabled(client.getMammaDossier() != null));
		keuzesContainer.add(ComponentHelper.newCheckBox("bkBeelden").setEnabled(client.getMammaDossier() != null));
		keuzesContainer.add(ComponentHelper.newCheckBox("bmhkGegevens").setEnabled(client.getCervixDossier() != null));
		keuzesContainer.add(ComponentHelper.newCheckBox("dkGegevens").setEnabled(client.getColonDossier() != null));
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		var opslaanObjecten = new HashMap<ExtraOpslaanKey, Object>();
		opslaanObjecten.put(ExtraOpslaanKey.OVERDRACHT_PERSOONSGEGEVENS, ModelUtil.nullSafeGet(overdrachtPersoonsgegevensModel));
		return opslaanObjecten;
	}

	@Override
	public void validate()
	{
		if (overdrachtPersoonsgegevensService.heeftOpenstaandVerzoek(clientModel.getObject()))
		{
			error(getString("message.aanvraagbriefOnderweg"));
		}
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(clientModel);
		super.onDetach();
	}
}
