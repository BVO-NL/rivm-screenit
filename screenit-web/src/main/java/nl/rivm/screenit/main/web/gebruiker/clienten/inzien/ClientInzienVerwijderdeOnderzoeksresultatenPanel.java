package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import nl.rivm.screenit.main.model.OnderzoekresultatenActieDossierGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.verwijderdeonderzoeksresultaten.VerwijderdeOnderzoeksresultatenInzienPopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienVerwijderdeOnderzoeksresultatenPanel extends GenericPanel<Client>
{
	@SpringBean
	private DossierService dossierService;

	private final BootstrapDialog dialog;

	private WebMarkupContainer meldingenContainer;

	public ClientInzienVerwijderdeOnderzoeksresultatenPanel(String id, IModel<Client> model, BootstrapDialog dialog)
	{
		super(id, model);
		this.dialog = dialog;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addOrReplaceMeldingContainer();
	}

	private void addOrReplaceMeldingContainer()
	{
		var onderzoeksresultatenContainer = new WebMarkupContainer("onderzoeksresultatenContainer");
		var container = new WebMarkupContainer("meldingenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		var client = getModelObject();
		var listMeldingen = client.getOnderzoeksresultatenActies().stream().map(actie ->
		{
			var dossierGebeurtenis = new OnderzoekresultatenActieDossierGebeurtenis(getString("onderzoeksresultaten.actie.dossier.verwijderd"),
				DateUtil.toUtilDate(actie.getMoment()));
			dossierGebeurtenis.setActieModel(ModelUtil.sModel(actie));
			dossierGebeurtenis.setBron(dossierService.bepaalGebeurtenisBron(actie));

			return dossierGebeurtenis;
		}).toList();

		container.add(new ListView<>("meldingen", listMeldingen)
		{
			@Override
			protected void populateItem(ListItem<OnderzoekresultatenActieDossierGebeurtenis> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				var meldingContainer = new WebMarkupContainer("melding");
				meldingContainer.add(new Label("omschrijving"));
				meldingContainer.add(DateLabel.forDatePattern("tijd", "dd-MM-yyyy HH:mm:ss"));
				meldingContainer.add(new EnumLabel<>("bron"));

				meldingContainer.add(new AjaxEventBehavior("click")
				{
					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						dialog.openWith(target, new VerwijderdeOnderzoeksresultatenInzienPopupPanel(IDialog.CONTENT_ID, item.getModelObject().getActieModel())
						{
							@Override
							protected void close(AjaxRequestTarget target)
							{
								dialog.close(target);
							}
						});
					}

				});
				item.add(meldingContainer);
			}
		});

		onderzoeksresultatenContainer.add(container);
		onderzoeksresultatenContainer.setVisible(!listMeldingen.isEmpty());
		add(onderzoeksresultatenContainer);
	}
}
