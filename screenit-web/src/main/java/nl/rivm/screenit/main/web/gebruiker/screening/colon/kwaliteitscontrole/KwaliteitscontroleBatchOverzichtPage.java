package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole;

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

import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.colon.ColonHoudbaarheidFitReeksFilter;
import nl.rivm.screenit.main.service.QbaseService;
import nl.rivm.screenit.main.service.colon.ColonFitAnalyseResultaatSetService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxButtonGroup;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Organisatie_;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaat;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet_;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium_;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.radiochoice.BooleanRadioChoice;
import nl.topicuszorg.wicket.input.simplechoice.SimpleChoiceRenderer;
import nl.topicuszorg.wicket.search.column.ClickablePropertyColumn;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;
import nl.topicuszorg.wicket.search.column.HibernateObjectCheckBoxUpdatingColumn;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.io.IOUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.markup.html.TransparentWebMarkupContainer;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.COLON_VERWIJDEREN_FIT_ANALYSE_RESULTATEN_AANLEVERING,
		Recht.COLON_AUTORISATIE_FIT_ANALYSE_RESULTATEN_AANLEVERING, Recht.COLON_VERWIJDEREN_FIT_ANALYSE_RESULTATEN_AANLEVERING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON },
	organisatieTypeScopes = { OrganisatieType.LABORATORIUM, OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.RIVM })
public class KwaliteitscontroleBatchOverzichtPage extends KwaliteitscontroleLabBasePage
{
	private final ScreenitDataTable<ColonFitAnalyseResultaatSet, String> table;

	@SpringBean
	private OrganisatieService organisatieService;

	@SpringBean
	private ColonFitAnalyseResultaatSetService fitBestandService;

	@SpringBean
	private QbaseService qbaseService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final IModel<HibernateCheckBoxListContainer<ColonFitAnalyseResultaatSet>> checkBoxListContainer = Model.of(new HibernateCheckBoxListContainer<>());

	private final TransparentWebMarkupContainer statusIconFragmentContainer;

	private final ColonFitAnalyseResultaatSetDataProvider fitAnalyseResultaatSetDataProvider;

	public KwaliteitscontroleBatchOverzichtPage()
	{
		final IModel<ColonHoudbaarheidFitReeksFilter> zoekModel = new CompoundPropertyModel<>(new ColonHoudbaarheidFitReeksFilter());
		ColonHoudbaarheidFitReeksFilter filter = zoekModel.getObject();
		filter.setStatus(ColonFitAnalyseResultaatSetStatus.INGELEZEN);
		filter.setDatumTot(currentDateSupplier.getDate());
		filter.setDatumVan(DateUtil.minDagen(currentDateSupplier.getDate(), 7));
		filter.setAnalyseDatum(true);

		fitAnalyseResultaatSetDataProvider = new ColonFitAnalyseResultaatSetDataProvider(zoekModel);

		table = new ScreenitDataTable<>("tabel", createColumns(), fitAnalyseResultaatSetDataProvider, 30, Model.of(""))
		{

			@Override
			protected void onBeforeRender()
			{

				checkBoxListContainer.getObject().getValueMap().clear();
				super.onBeforeRender();
			}

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ColonFitAnalyseResultaatSet> model)
			{

			}

			@Override
			protected boolean isRowClickable(IModel<ColonFitAnalyseResultaatSet> model)
			{
				return false;
			}
		};

		Form<ColonHoudbaarheidFitReeksFilter> form = new Form<ColonHoudbaarheidFitReeksFilter>("form", new CompoundPropertyModel<>(zoekModel));
		add(form);
		BooleanRadioChoice analyseDatum = new BooleanRadioChoice("analyseDatum");
		analyseDatum.setPrefix("<label class=\"radio inline\">");
		analyseDatum.setSuffix("</label>");

		analyseDatum.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(table);
			}
		});
		form.add(analyseDatum);
		form.add(new AjaxButtonGroup<>("status",
			new ListModel<>(Arrays.asList(null, ColonFitAnalyseResultaatSetStatus.INGELEZEN)), new SimpleChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(ColonFitAnalyseResultaatSetStatus object)
			{
				if (ColonFitAnalyseResultaatSetStatus.INGELEZEN.equals(object))
				{
					return "Niet geautoriseerd";
				}
				return "Alles";
			}

		})
		{
			@Override
			protected void onSelectionChanged(ColonFitAnalyseResultaatSetStatus selection, AjaxRequestTarget target, String markupId)
			{
				target.add(table);
			}
		});

		FormComponent<Date> datumVan = ComponentHelper.addTextField(form, "datumVan", false, 10, Date.class, false);
		datumVan.setType(Date.class);
		datumVan.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(table);
			}

		});

		FormComponent<Date> datumTot = ComponentHelper.addTextField(form, "datumTot", false, 10, Date.class, false);
		datumTot.setType(Date.class);
		datumTot.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(table);
			}

		});

		Organisatie organisatie = ScreenitSession.get().getOrganisatie();
		final boolean isLabMedewerker = organisatie.getOrganisatieType().equals(OrganisatieType.LABORATORIUM);
		if (isLabMedewerker)
		{
			filter.setLab((ColonFitLaboratorium) organisatie);
		}

		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);
		final Form<?> buttonForm = new Form<>("buttonForm");
		add(buttonForm);
		buttonForm.add(table);
		maakVerwijderButton(dialog, buttonForm);

		final WebMarkupContainer beoordelenContainer = maakBeoordelenButton(zoekModel, buttonForm);

		maakLabDropDown(form, isLabMedewerker, beoordelenContainer);

		maakAutoriseerButton(dialog, buttonForm);

		statusIconFragmentContainer = new TransparentWebMarkupContainer("statusIconFragmentContainer");
		add(statusIconFragmentContainer);
	}

	protected void maakLabDropDown(Form<ColonHoudbaarheidFitReeksFilter> form, final boolean isLabMedewerker, final WebMarkupContainer beoordelenContainer)
	{
		Component labDropDown = new ScreenitDropdown<>("lab", ModelUtil.listRModel(organisatieService.getActieveOrganisaties(ColonFitLaboratorium.class)),
			new ChoiceRenderer<>("naam"))
			.setNullValid(true).setVisible(!isLabMedewerker);
		labDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(beoordelenContainer, table);
			}

		});
		form.add(labDropDown);
	}

	protected WebMarkupContainer maakBeoordelenButton(final IModel<ColonHoudbaarheidFitReeksFilter> zoekModel, Form<?> buttonForm)
	{
		final Component beoordelen = new ResourceLink<Void>("beoordelen", new AbstractResource()
		{
			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse response = new ResourceResponse();
				response.setFileName("qbase.txt");
				response.setContentType("text/plain");
				response.getHeaders().addHeader("Cache-Control", "no-cache");
				response.setContentDisposition(ContentDisposition.ATTACHMENT);

				response.setWriteCallback(new WriteCallback()
				{
					@Override
					public void writeData(Attributes attributes)
					{
						try
						{
							Iterator<? extends ColonFitAnalyseResultaatSet> iterator = fitAnalyseResultaatSetDataProvider.iterator(-1, -1);
							List<ColonFitAnalyseResultaatSet> lijst = new ArrayList<>();
							while (iterator.hasNext())
							{
								lijst.add(iterator.next());
							}
							try (InputStream writer = IOUtils.toInputStream(qbaseService.maakQbaseBestand(lijst, ScreenitSession.get().getIngelogdAccount()));
								OutputStream outputStream = attributes.getResponse().getOutputStream())
							{
								IOUtils.copy(writer, outputStream);
							}
						}
						catch (Exception e)
						{
							LOG.error("Fout bij maken/laden qbase bestand: {}", e.getMessage(), e);
						}
					}
				});
				return response;
			}
		})
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(
					zoekModel.getObject().getLab() != null && ScreenitSession.get().checkPermission(Recht.COLON_VERWIJDEREN_FIT_ANALYSE_RESULTATEN_AANLEVERING, Actie.INZIEN));
			}

		};
		final WebMarkupContainer beoordelenContainer = new WebMarkupContainer("beoordelenContainer");
		beoordelenContainer.add(beoordelen);
		buttonForm.add(beoordelenContainer);
		beoordelenContainer.setOutputMarkupId(true);
		beoordelen.setOutputMarkupId(true);
		return beoordelenContainer;
	}

	protected void maakVerwijderButton(BootstrapDialog dialog, Form<?> buttonForm)
	{
		Component verwijderen = new ConfirmingIndicatingAjaxSubmitLink<Void>("verwijderen", buttonForm, dialog, "confirm.ifobt.bestanden.verwijderen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<ColonFitAnalyseResultaatSet> list = checkBoxListContainer.getObject().getList();
				fitBestandService.verwijderResultaatSets(list, ScreenitSession.get().getIngelogdAccount());
				success("Bestanden zijn verwijderd.");
				target.add(table);
			}

		};
		buttonForm.add(verwijderen);
		verwijderen.setVisible(ScreenitSession.get().checkPermission(Recht.COLON_VERWIJDEREN_FIT_ANALYSE_RESULTATEN_AANLEVERING, Actie.INZIEN));
		verwijderen.setOutputMarkupId(true);
	}

	protected void maakAutoriseerButton(BootstrapDialog dialog, Form<?> buttonForm)
	{
		Component autoriseer = new ConfirmingIndicatingAjaxLink<Void>("autoriseer", dialog, "confirm.ifobt.bestanden.autoriseren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				List<ColonFitAnalyseResultaatSet> list = IteratorUtils.toList(fitAnalyseResultaatSetDataProvider.iterator(-1L, -1L));
				fitBestandService.autoriseerResultaatSets(list, ScreenitSession.get().getIngelogdAccount());
				success("Bestanden zijn geautoriseerd.");
				target.add(table);
			}

		};
		buttonForm.add(autoriseer);
		autoriseer.setVisible(ScreenitSession.get().checkPermission(Recht.COLON_AUTORISATIE_FIT_ANALYSE_RESULTATEN_AANLEVERING, Actie.INZIEN));
		autoriseer.setOutputMarkupId(true);
	}

	private List<IColumn<ColonFitAnalyseResultaatSet, String>> createColumns()
	{
		List<IColumn<ColonFitAnalyseResultaatSet, String>> columns = new ArrayList<>();
		ScreenitSession screenitSession = ScreenitSession.get();
		if (screenitSession.checkPermission(Recht.COLON_VERWIJDEREN_FIT_ANALYSE_RESULTATEN_AANLEVERING, Actie.INZIEN))
		{
			columns.add(new HibernateObjectCheckBoxUpdatingColumn<>(Model.of(""), "", checkBoxListContainer.getObject(), true, false)
			{
				@Override

				public boolean checkBoxVisible(IModel<ColonFitAnalyseResultaatSet> rowModel)
				{
					if (rowModel != null)
					{
						return magVerwijderen(rowModel);
					}
					return super.checkBoxVisible(rowModel);
				}

				@Override
				protected boolean isRowInitialChecked(IModel<ColonFitAnalyseResultaatSet> rowModel)
				{
					if (rowModel != null)
					{
						return magVerwijderen(rowModel);
					}
					return true;
				}

				private boolean magVerwijderen(IModel<ColonFitAnalyseResultaatSet> rowModel)
				{
					ColonFitAnalyseResultaatSet bestand = rowModel.getObject();
					String naamBestand = bestand.getNaamBestand();
					boolean isSentinelBestand = naamBestand.startsWith("QC");
					boolean magNogAutoriseren = ColonFitAnalyseResultaatSetStatus.INGELEZEN.equals(bestand.getStatus());
					return !isSentinelBestand && magNogAutoriseren;
				}
			});
		}
		columns.add(new DateTimePropertyColumn<ColonFitAnalyseResultaatSet, String>(Model.of("Datum ingelezen"), ColonFitAnalyseResultaatSet_.STATUS_DATUM,
			ColonFitAnalyseResultaatSet_.STATUS_DATUM,
			new SimpleDateFormat("dd-MM-yyyy"))
			.setCssClass("table-col-datum"));
		columns.add(new ClickablePropertyColumn<>(Model.of("Datum/tijd van"), ColonFitAnalyseResultaatSet_.STATUS_DATUM)
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonFitAnalyseResultaatSet> rowModel)
			{
				Date datumFrom = null;
				for (ColonFitAnalyseResultaat uitslag : rowModel.getObject().getUitslagen())
				{
					if (datumFrom == null || uitslag.getAnalyseDatum().before(datumFrom))
					{
						datumFrom = uitslag.getAnalyseDatum();
					}
				}

				if (datumFrom == null)
				{
					return new Model("");
				}
				return new Model(Constants.getDateTimeSecondsFormat().format(datumFrom));
			}

			@Override
			public String getCssClass()
			{
				return "table-col-datum";
			}

		});
		columns.add(new ClickablePropertyColumn<>(Model.of("Datum/tijd tot"), ColonFitAnalyseResultaatSet_.STATUS_DATUM)
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonFitAnalyseResultaatSet> rowModel)
			{
				Date datumTot = null;
				for (ColonFitAnalyseResultaat uitslag : rowModel.getObject().getUitslagen())
				{
					if (datumTot == null || uitslag.getAnalyseDatum().after(datumTot))
					{
						datumTot = uitslag.getAnalyseDatum();
					}
				}

				if (datumTot == null)
				{
					return new Model("");
				}
				return new Model(Constants.getDateTimeSecondsFormat().format(datumTot));
			}

			@Override
			public String getCssClass()
			{
				return "table-col-datum";
			}

		});
		columns.add(new ClickablePropertyColumn<>(Model.of("Aantal client uitslagen"), propertyChain(ColonFitAnalyseResultaatSet_.UITSLAGEN, "size"))
		{
			@Override
			public IModel<Object> getDataModel(IModel<ColonFitAnalyseResultaatSet> rowModel)
			{
				ColonFitAnalyseResultaatSet bestand = rowModel.getObject();
				Integer aantalControleUitslagen = bestand.getAantalControleUitslagen();
				int aantal = bestand.getUitslagen().size() - (aantalControleUitslagen == null ? 0 : aantalControleUitslagen);
				return new Model(aantal);
			}

			@Override
			public String getCssClass()
			{
				return "table-col-aantal";
			}

		});
		columns.add(
			new ClickablePropertyColumn<ColonFitAnalyseResultaatSet, String>(Model.of("Aantal controle uitslagen"),
				ColonFitAnalyseResultaatSet_.AANTAL_CONTROLE_UITSLAGEN).setCssClass(
				"table-col-aantal"));
		columns.add(new ClickablePropertyColumn<>(Model.of("Lab"), propertyChain(ColonFitAnalyseResultaatSet_.LABORATORIUM, Organisatie_.NAAM),
			propertyChain(ColonFitAnalyseResultaatSet_.LABORATORIUM, ColonFitLaboratorium_.NAAM)));
		columns.add(new ClickablePropertyColumn<>(Model.of("Bestand"), ColonFitAnalyseResultaatSet_.NAAM_BESTAND, ColonFitAnalyseResultaatSet_.NAAM_BESTAND));
		columns.add(new ClickablePropertyColumn<>(Model.of("Geautoriseerd"), ColonFitAnalyseResultaatSet_.STATUS, ColonFitAnalyseResultaatSet_.STATUS)
		{
			@Override
			public void populateItem(Item<ICellPopulator<ColonFitAnalyseResultaatSet>> cellItem, String componentId, IModel<ColonFitAnalyseResultaatSet> rowModel)
			{
				cellItem.add(new StatusIconFragment(componentId, rowModel.getObject().getStatus()));
			}

			@Override
			public String getCssClass()
			{
				return "table-col-icon";
			}
		});
		return columns;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(fitAnalyseResultaatSetDataProvider);
	}

	private class StatusIconFragment extends Fragment
	{
		public StatusIconFragment(String id, ColonFitAnalyseResultaatSetStatus status)
		{
			super(id, "statusIconFragment", statusIconFragmentContainer);

			WebMarkupContainer icon = new WebMarkupContainer("icon");
			if (ColonFitAnalyseResultaatSetStatus.INGELEZEN.equals(status))
			{
				icon.add(new AttributeAppender("class", Model.of("icon-ban-circle")));
			}
			else
			{
				icon.add(new AttributeAppender("class", Model.of("icon-ok")));
			}
			add(icon);
		}
	}

}
