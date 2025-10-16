package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

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

import java.util.Arrays;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.DateTimeField;
import nl.rivm.screenit.main.web.component.form.ScreenITDateTimeField;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.input.timefield.TimeField;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;

public abstract class MammaCapaciteitBlokEditPopup extends GenericPanel<PlanningCapaciteitBlokDto>
{
	private BootstrapDialog confirmPopup;

	private final boolean magAanpassen;

	private WebMarkupContainer aantalOnderzoekenContainer;

	private WebMarkupContainer opmerkingenContainer;

	private MammaMinderValideReserveringEditPanel minderValideReserveringenPanel;

	private WebMarkupContainer minderValideAfspraakMogelijkContainer;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private final boolean staatNieuweMinderValideReserveringAan;

	private final TimeField endTime = new TimeField("tot", true);

	public MammaCapaciteitBlokEditPopup(String id, IModel<PlanningCapaciteitBlokDto> model)
	{
		super(id, model);
		this.magAanpassen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);
		this.staatNieuweMinderValideReserveringAan = preferenceService.getBoolean(PreferenceKey.MAMMA_MINDERVALIDE_RESERVERING_ACTIEF.name(), false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		confirmPopup = new BootstrapDialog("confirmPopup");
		confirmPopup.setOutputMarkupPlaceholderTag(true);
		add(confirmPopup);

		final var editForm = new Form<>("editForm", getModel());
		add(editForm);

		initForm(editForm);

		addOpslaanLink(editForm);
		addDeleteLink();
	}

	private void initForm(Form<PlanningCapaciteitBlokDto> form)
	{
		var blok = form.getModelObject();
		var isNieuw = blok.conceptId == null;

		var wijzigOfNieuw = "Wijzig";

		if (isNieuw)
		{
			wijzigOfNieuw = "Nieuw";
		}

		var tijdString = Constants.getDateTimeFormat().format(blok.vanaf) + "-" + Constants.getTimeFormat().format(blok.tot);

		aantalOnderzoekenContainer = new WebMarkupContainer("aantalOnderzoekenContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(!MammaCapaciteitBlokType.GEEN_SCREENING.equals(getModelObject().blokType));
			}
		};
		aantalOnderzoekenContainer.setOutputMarkupId(true);
		aantalOnderzoekenContainer.setOutputMarkupPlaceholderTag(true);

		form.add(aantalOnderzoekenContainer);

		opmerkingenContainer = new WebMarkupContainer("opmerkingenContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(MammaCapaciteitBlokType.GEEN_SCREENING.equals(getModelObject().blokType));
			}
		};
		opmerkingenContainer.setOutputMarkupId(true);
		opmerkingenContainer.setOutputMarkupPlaceholderTag(true);

		ComponentHelper.addTextField(aantalOnderzoekenContainer, "aantalOnderzoeken", true, 10, Integer.class, false).add(RangeValidator.minimum(0));

		var opmerkingen = ComponentHelper.addTextArea(opmerkingenContainer, "opmerkingen", true, 255,
			false);
		opmerkingenContainer.add(opmerkingen);

		form.add(opmerkingenContainer);

		maakMinderValideMogelijkContainer();
		form.add(minderValideAfspraakMogelijkContainer);

		minderValideReserveringenPanel = new MammaMinderValideReserveringEditPanel("minderValideReserveringenPanel", getModel());
		minderValideReserveringenPanel.setOutputMarkupPlaceholderTag(true);
		updateMinderValideAfspraakReserveringContainerVisibility();
		form.add(minderValideReserveringenPanel);

		var bloktypen = ComponentHelper.newDropDownChoice("blokType",
			new ListModel<>(Arrays.asList(MammaCapaciteitBlokType.values())), new EnumChoiceRenderer<>(), true);

		bloktypen.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				updateMinderValideAfspraakMogelijkContainerVisibility();
				updateMinderValideAfspraakReserveringContainerVisibility();
				target.add(aantalOnderzoekenContainer, opmerkingenContainer, minderValideAfspraakMogelijkContainer, minderValideReserveringenPanel);
			}
		});

		form.add(bloktypen);

		form.add(new Label("actie", wijzigOfNieuw));
		form.add(new Label("screeningsEenheid.naam", hibernateService.get(MammaScreeningsEenheid.class, blok.screeningsEenheidId).getNaam()));
		form.add(new Label("tijd", tijdString));

		endTime.setOutputMarkupId(true);
		endTime.setEnabled(magAanpassen);
		endTime.setRequired(true);
		form.add(endTime);
		final DateTimeField startTime = new ScreenITDateTimeField("vanaf")
		{

			@Override
			public String getDatePickerLabel()
			{
				return "'Starttijd datum'";
			}

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				setDateForEndTimeField();
			}
		};

		startTime.setOutputMarkupId(true);
		startTime.setRequired(true);
		startTime.setEnabled(magAanpassen);
		form.add(startTime);
	}

	private void maakMinderValideMogelijkContainer()
	{
		var minderValideAfspraakMogelijk = ComponentHelper.newCheckBox("minderValideAfspraakMogelijk");
		minderValideAfspraakMogelijkContainer = new WebMarkupContainer("minderValideAfspraakMogelijkContainer");
		minderValideAfspraakMogelijkContainer.add(minderValideAfspraakMogelijk);
		minderValideAfspraakMogelijkContainer.setOutputMarkupId(true);
		minderValideAfspraakMogelijkContainer.setOutputMarkupPlaceholderTag(true);
		updateMinderValideAfspraakMogelijkContainerVisibility();
	}

	private void updateMinderValideAfspraakMogelijkContainerVisibility()
	{
		minderValideAfspraakMogelijkContainer.setVisible(getModelObject().blokType.equals(MammaCapaciteitBlokType.SCREENING) && !staatNieuweMinderValideReserveringAan);
	}

	private void updateMinderValideAfspraakReserveringContainerVisibility()
	{
		minderValideReserveringenPanel.setVisible(getModelObject().blokType.equals(MammaCapaciteitBlokType.SCREENING) && staatNieuweMinderValideReserveringAan);
	}

	private void setDateForEndTimeField()
	{

		var endDateTime = DateUtil.toLocalDateTime(endTime.getDate());
		var currentStartDate = DateUtil.toLocalDateTime(getModelObject().vanaf);

		if (endDateTime.getHour() == 0 && endDateTime.getMinute() == 0)
		{
			endDateTime = endDateTime.withDayOfYear(currentStartDate.plusDays(1).getDayOfYear());
		}
		else
		{
			endDateTime = endDateTime.withDayOfYear(currentStartDate.getDayOfYear());
		}
		endTime.setDate(DateUtil.toUtilDate(endDateTime));
	}

	private void addOpslaanLink(final Form<PlanningCapaciteitBlokDto> editForm)
	{
		IndicatingAjaxButton opslaanLink = new ConfirmingIndicatingAjaxSubmitLink<Void>("opslaan", editForm, confirmPopup, "opslaan.popup")
		{
			private int aantalAfspraken = 0;

			@Override
			protected boolean skipConfirmation()
			{
				var blokModel = MammaCapaciteitBlokEditPopup.this.getModel();
				aantalAfspraken = baseCapaciteitsBlokService.getAantalAfsprakenOpBlok(blokModel.getObject(), false);
				return aantalAfspraken == 0;
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				if (aantalAfspraken > 0)
				{
					return Model.of(String.format(getString("opslaan.popup.content"), aantalAfspraken));
				}
				else
				{
					return Model.of();
				}

			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				setDateForEndTimeField();
				onOpslaan(target, MammaCapaciteitBlokEditPopup.this.getModel());
			}

		};

		add(opslaanLink);
		opslaanLink.setVisible(magAanpassen);
		opslaanLink.add(new Label("opslaanTekst", getOpslaanTekst()));
	}

	protected abstract void onOpslaan(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model);

	private void addDeleteLink()
	{
		final IndicatingAjaxLink<Void> deleteSubmit = new ConfirmingIndicatingAjaxLink<>("verwijderen", confirmPopup, "verwijder.popup")
		{
			@Override
			protected IModel<String> getContentStringModel()
			{
				var blokModel = MammaCapaciteitBlokEditPopup.this.getModel();
				var aantalAfspraken = baseCapaciteitsBlokService.getAantalAfsprakenOpBlok(blokModel.getObject(), true);
				if (aantalAfspraken > 0)
				{
					return Model.of(String.format(getString("verwijder.popup.content"), aantalAfspraken));
				}
				else
				{
					return Model.of();
				}
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onVerwijderen(target, MammaCapaciteitBlokEditPopup.this.getModel());
			}

		};
		add(deleteSubmit);

		var blok = getModelObject();
		deleteSubmit.setVisible(blok.conceptId != null && ScreenitSession.get().checkPermission(Recht.MEDEWERKER_SCREENING_MAMMA_PLANNING, Actie.VERWIJDEREN));
		deleteSubmit.add(new Label("verwijderenTekst", getDeleteTekst()));
	}

	protected abstract void onVerwijderen(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model);

	protected IModel<String> getDeleteTekst()
	{
		return Model.of("Verwijderen");
	}

	protected IModel<String> getOpslaanTekst()
	{
		return Model.of("Onthouden");
	}
}
