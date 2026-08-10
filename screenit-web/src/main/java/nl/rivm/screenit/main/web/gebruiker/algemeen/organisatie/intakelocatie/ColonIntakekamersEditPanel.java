package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.intakelocatie;

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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefCellPanel;
import nl.rivm.screenit.main.web.component.table.ActiefHeaderInFormPanel;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.HibernateService;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class ColonIntakekamersEditPanel extends GenericPanel<ColonIntakelocatie>
{
	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private HibernateService hibernateService;

	private Component header;

	public ColonIntakekamersEditPanel(String id, IModel<ColonIntakelocatie> intakelocatie)
	{
		super(id, intakelocatie);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final var dialog = new BootstrapDialog("dialog");
		add(dialog);

		var intakelocatie = getModelObject();
		final var actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getIngelogdeOrganisatieMedewerker(), intakelocatie,
			Recht.MEDEWERKER_BEHEER_CC_LOCATIES);

		var form = new ScreenitForm<ColonIntakelocatie>("form");
		add(form);

		final var refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(true);
		form.add(refreshContainer);
		var searchObject = new ColonIntakekamer();
		searchObject.setActief(true);
		final IModel<ColonIntakekamer> searchObjectModel = Model.of(searchObject);
		header = new ActiefHeaderInFormPanel<>("actiefHeader", refreshContainer, searchObjectModel);
		form.add(header);

		var kamerToevoegen = new ScreenitAjaxLink("kamerToevoegen")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var intakelocatie = ColonIntakekamersEditPanel.this.getModelObject();
				var kamer = new ColonIntakekamer();
				kamer.setNaam("");
				kamer.setActief(true);
				kamer.setIntakelocatie(intakelocatie);
				intakelocatie.getKamers().add(kamer);
				if (Boolean.FALSE.equals(searchObjectModel.getObject().getActief()))
				{
					searchObjectModel.getObject().setActief(Boolean.TRUE);
					var newHeader = new ActiefHeaderInFormPanel<ColonIntakekamer>("actiefHeader", refreshContainer, searchObjectModel);
					header.replaceWith(newHeader);
					header = newHeader;
					target.add(header);
				}
				target.add(refreshContainer);
			}
		};
		kamerToevoegen.setVisible(actie != null && actie.getNiveau() >= Actie.TOEVOEGEN.getNiveau());
		form.add(kamerToevoegen);

		var kamers = new ListView<ColonIntakekamer>("kamers", new PropertyModel<>(getModel(), "kamers"))
		{
			@Override
			protected void populateItem(ListItem<ColonIntakekamer> item)
			{
				var kamerActief = item.getModelObject().getActief();
				var searchActief = searchObjectModel.getObject().getActief();
				var visible = searchActief == null
					|| Boolean.TRUE.equals(searchActief) && !Boolean.FALSE.equals(kamerActief)
					|| Boolean.FALSE.equals(searchActief) && Boolean.FALSE.equals(kamerActief);

				item.setVisible(visible);

				var magVerwijderen = actie != null && actie.getNiveau() >= Actie.VERWIJDEREN.getNiveau();
				var magAanpassen = actie != null && actie.getNiveau() >= Actie.AANPASSEN.getNiveau();

				item.add(new TextField<>("naamEdit", new CompoundPropertyModel<>(new PropertyModel<>(item.getModel(), "naam"))).setRequired(true).setLabel(Model.of("Naam kamer"))
					.setEnabled(magAanpassen));

				item.add(new ActiefCellPanel<>("actiefToggle", item.getModel(), magVerwijderen, dialog, "question.remove.kamer")
				{

					@Override
					protected void onAfterToggleActief(AjaxRequestTarget target, ColonIntakekamer kamer)
					{
						if (kamer.getId() != null)
						{
							hibernateService.saveOrUpdate(kamer);
						}
						target.add(refreshContainer);
					}

					@Override
					protected boolean isActief(IModel<ColonIntakekamer> rowModel)
					{
						return !Boolean.FALSE.equals(rowModel.getObject().getActief());
					}
				});
			}
		};
		refreshContainer.add(kamers);
		var opslaan = new ScreenitAjaxLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var intakelocatie = ColonIntakekamersEditPanel.this.getModelObject();
				var activeKamers = new ArrayList<ColonIntakekamer>();
				for (var kamer : intakelocatie.getKamers())
				{
					if (!Boolean.FALSE.equals(kamer.getActief()))
					{
						activeKamers.add(kamer);
					}
				}
				Set<String> kamers = new HashSet<String>();
				for (var kamer : activeKamers)
				{
					var kamerStringToUpper = kamer.getNaam().toUpperCase();
					kamers.add(kamerStringToUpper);
				}

				if (kamers.size() < activeKamers.size())
				{
					error("De intakelocatie mag geen kamers met dezelfde naam bevatten.");
				}
				else
				{
					onSaveOrUpdateKamers(target, intakelocatie);
				}
			}

		};
		var magAanpassen = actie != null && actie.getNiveau() >= Actie.AANPASSEN.getNiveau();
		opslaan.setVisible(magAanpassen);
		form.add(opslaan);
		form.setDefaultButton(opslaan);

	}

	protected abstract void onSaveOrUpdateKamers(AjaxRequestTarget target, ColonIntakelocatie intakelocatie);

}
