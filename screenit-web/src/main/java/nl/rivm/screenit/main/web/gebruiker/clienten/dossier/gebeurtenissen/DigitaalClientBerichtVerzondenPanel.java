package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.DigitaalClientBerichtInfoPanel;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MailOpnieuwVerzendenPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.DigitaalClientBerichtService;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.MEDEWERKER_CLIENT_SR_MAILS_OPNIEUW_VERZENDEN, Recht.MEDEWERKER_CLIENT_SCREENINGSRONDE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class DigitaalClientBerichtVerzondenPanel extends AbstractGebeurtenisDetailPanel
{
	@SpringBean
	private DigitaalClientBerichtService digitaalClientBerichtService;

	public DigitaalClientBerichtVerzondenPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var digitaalClientBericht = getModelObject().getDigitaalClientBericht();
		var digitaalClientBerichtModel = new CompoundPropertyModel(new PropertyModel(getModel(), "digitaalClientBericht"));
		add(new DigitaalClientBerichtInfoPanel("digitaalClientBerichtInfo", digitaalClientBerichtModel));
		add(new MailOpnieuwVerzendenPanel("mailOpnieuwVerzenden", digitaalClientBerichtModel));

		var isSms = DigitaalBerichtType.SMS == digitaalClientBericht.getDigitaalBerichtTemplateType().getBerichtType();
		var smsBerichtContainer = new WebMarkupContainer("smsBerichtContainer");
		smsBerichtContainer.setVisible(isSms);
		smsBerichtContainer.add(new Label("smsBericht", isSms ? Model.of(haalInhoudOp(digitaalClientBericht)) : Model.of("")));
		add(smsBerichtContainer);
	}

	private String haalInhoudOp(DigitaalClientBericht<?> digitaalClientBericht)
	{
		var smsInhoudOfKey = digitaalClientBerichtService.haalSmsBerichtOp(digitaalClientBericht);
		if (DigitaalClientBerichtService.GEEN_BERICHT_BESCHIKBAAR_KEY.equals(smsInhoudOfKey)
			|| DigitaalClientBerichtService.BERICHT_KON_NIET_WORDEN_OPGEHAALD_KEY.equals(smsInhoudOfKey))
		{
			return getString(smsInhoudOfKey);
		}
		return smsInhoudOfKey;
	}
}
