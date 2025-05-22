package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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
import java.util.Date;
import java.util.List;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag_;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting_;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent_;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValue_;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
public abstract class ColonHandmatigVervolgbeleidVastleggenPanel extends GenericPanel<MdlVerslag>
{

	private final ScreenitDropdown<DSValue> definitiefVervolgbeleid;

	private final Form<MdlVerslag> form;

	@SpringBean
	private BaseVerslagService baseVerslagService;

	@SpringBean
	private ColonVerwerkVerslagService verwerkVerslagService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ColonHandmatigVervolgbeleidVastleggenPanel(String id, IModel<MdlVerslag> model)
	{
		super(id, model);
		form = new Form<>("form");
		form.setOutputMarkupId(true);
		add(form);

		var aanvangVerrichting = ComponentHelper.addTextField(form, propertyChain(MdlVerslag_.VERSLAG_CONTENT, MdlVerslagContent_.VERRICHTING, MdlVerrichting_.AANVANG_VERRICHTING),
			true, 10, Date.class,
			false);
		aanvangVerrichting.add(DateValidator.maximum(currentDateSupplier.getDate()));

		var eindconclusieOpties = getDsValueSet(MdlColoscopieMedischeObservatie_.EINDCONCLUSIE, MdlColoscopieMedischeObservatie.class,
			getModelObject().getVerslagContent().getColoscopieMedischeObservatie().getEindconclusie());
		var eindConclusie = ComponentHelper.newDropDownChoice(
			propertyChain(MdlVerslag_.VERSLAG_CONTENT, MdlVerslagContent_.COLOSCOPIE_MEDISCHE_OBSERVATIE, MdlColoscopieMedischeObservatie_.EINDCONCLUSIE),
			ModelUtil.listRModel(eindconclusieOpties, false),
			new ChoiceRenderer<>(DSValue_.DISPLAY_NAME_NL), true);
		eindConclusie.setOutputMarkupId(true);
		form.add(eindConclusie);

		var definitiefVervolgbeleidOpties = Arrays.stream(MdlVervolgbeleid.values()).map(value -> getDsValue(value.getCode(), value.getCodeSystem(), "vs_vervolgbeleid")).toList();
		definitiefVervolgbeleid = ComponentHelper.newDropDownChoice(
			propertyChain(MdlVerslag_.VERSLAG_CONTENT, MdlVerslagContent_.COLOSCOPIE_MEDISCHE_OBSERVATIE,
				MdlColoscopieMedischeObservatie_.DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG,
				MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEK),
			ModelUtil.listRModel(definitiefVervolgbeleidOpties, false), new ChoiceRenderer<>(DSValue_.DISPLAY_NAME_NL), true);
		definitiefVervolgbeleid.setOutputMarkupId(true);

		form.add(definitiefVervolgbeleid);

		addOrReplacePeriodeVervolgSurveillance(null);

		definitiefVervolgbeleid.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				addOrReplacePeriodeVervolgSurveillance(target);
			}
		});

		form.add(new ScreenitIndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
				var verslag = ModelProxyHelper.deproxy(getModelObject());

				verwerkVerslagService.handmatigMdlVerslagOpslaan(verslag, loggedInInstellingGebruiker);
				close(target);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

	private void addOrReplacePeriodeVervolgSurveillance(AjaxRequestTarget target)
	{
		var container = new WebMarkupContainer("periodeVervolgSurveillancescopie");
		container.setOutputMarkupId(true);
		container.setOutputMarkupPlaceholderTag(true);
		form.addOrReplace(container);

		var selectedValue = (DSValue) definitiefVervolgbeleid.getDefaultModelObject();
		MdlVervolgbeleid selectedVervolgbeleid = null;
		if (selectedValue != null)
		{
			selectedVervolgbeleid = MdlVervolgbeleid.fromCode(selectedValue.getCode());
		}
		final var selectedVervolgBeleidFinal = selectedVervolgbeleid;
		getModelObject().setVervolgbeleid(selectedVervolgBeleidFinal);

		var periodeVervolgSurveillanceOpties = getDsValueSet(MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.PERIODE_VERVOLG_SURVEILLANCESCOPIE,
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg.class,
			getModelObject().getVerslagContent().getColoscopieMedischeObservatie().getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg().getPeriodeVervolgSurveillancescopie())
			.stream()
			.filter(dsValue ->
			{
				var isDrieJaarOptie = dsValue.getCode().equals("14");
				if (selectedVervolgBeleidFinal == MdlVervolgbeleid.SURVEILLANCE)
				{
					return isDrieJaarOptie;
				}
				return !isDrieJaarOptie;
			})
			.toList();

		var periodeVervolgSurveillance = ComponentHelper.newDropDownChoice(
			propertyChain(MdlVerslag_.VERSLAG_CONTENT, MdlVerslagContent_.COLOSCOPIE_MEDISCHE_OBSERVATIE,
				MdlColoscopieMedischeObservatie_.DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG,
				MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.PERIODE_VERVOLG_SURVEILLANCESCOPIE),
			ModelUtil.listRModel(periodeVervolgSurveillanceOpties, false),
			new ChoiceRenderer<>(DSValue_.DISPLAY_NAME_NL), true);
		periodeVervolgSurveillance.setOutputMarkupId(true);

		var vervolgbeleidOptiesMetSurveillance = List.of(MdlVervolgbeleid.POLIEPECTOMIE, MdlVervolgbeleid.SURVEILLANCE, MdlVervolgbeleid.COLONOSCOPY,
			MdlVervolgbeleid.COLONOSCOPY_NEW);

		container.add(periodeVervolgSurveillance);
		container.setVisible(selectedVervolgbeleid != null && vervolgbeleidOptiesMetSurveillance.contains(selectedVervolgbeleid));

		if (target != null)
		{
			target.add(container);
		}
	}

	private List<DSValue> getDsValueSet(String varName, Class<?> clazz, DSValue geselecteerdeWaarde)
	{
		try
		{
			var dsValueSet = clazz.getDeclaredField(varName).getAnnotation(DSValueSet.class);
			if (dsValueSet == null)
			{
				return List.of();
			}
			return Stream.of(dsValueSet.values())
				.filter(dsValue -> !dsValue.deprecated() || geselecteerdeWaarde != null && dsValue.code().equals(geselecteerdeWaarde.getCode()) && dsValue.codeSystem()
					.equals(geselecteerdeWaarde.getCodeSystem()))
				.map(dsValue -> getDsValue(dsValue.code(), dsValue.codeSystem(), dsValueSet.name()))
				.toList();
		}
		catch (NoSuchFieldException e)
		{
			LOG.error("Er is een fout opgetreden bij het ophalen van DSValueSet voor field: {}", varName, e);
		}
		return List.of();
	}

	private DSValue getDsValue(String code, String codeSystem, String valueSetName)
	{
		return baseVerslagService.getDsValue(code, codeSystem, valueSetName, false);
	}
}
