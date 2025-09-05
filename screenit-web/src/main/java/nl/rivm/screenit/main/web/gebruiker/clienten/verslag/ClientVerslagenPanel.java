package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBasePaVerslagService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientVerslagenPanel extends GenericPanel<Client>
{

	private Boolean bezwaarOpIntake;

	@SpringBean
	private ColonBaseAfspraakService baseAfspraakService;

	@SpringBean
	private VerwerkVerslagService verwerkVerslagService;

	@SpringBean
	private MammaBasePaVerslagService basePaVerslagService;

	public ClientVerslagenPanel(String id, IModel<Client> model)
	{
		super(id, model);

		add(new ClientPaspoortPanel("passpoort", model));

		Client client = model.getObject();
		boolean magMdlVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.TOEVOEGEN, client);
		boolean magPaVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.TOEVOEGEN, client);
		boolean magFollowUpPaVerslagToevoegen = ScreenitSession.get().checkPermission(Recht.MEDEWERKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.TOEVOEGEN, client)
			&& basePaVerslagService.verwachtGegevensVoor(model.getObject().getPersoon().getBsn());

		if (getLaatsteScreeningronde(VerslagType.MDL) == null)
		{
			magMdlVerslagToevoegen = magPaVerslagToevoegen = false;
		}

		boolean magToevoegen = magMdlVerslagToevoegen || magPaVerslagToevoegen || magFollowUpPaVerslagToevoegen;
		bezwaarOpIntake = baseAfspraakService.heeftClientIntakeAfspraakMetConclusieBezwaar(client.getPersoon().getBsn());

		var aanmakenAlert = new WebMarkupContainer("aanmakenAlert");
		aanmakenAlert.setOutputMarkupId(true);
		aanmakenAlert.setVisible(magToevoegen);
		add(aanmakenAlert);

		ColonClientVerslagenOverzichtPanel colonVerslagen = new ColonClientVerslagenOverzichtPanel("colonVerslagen", model);
		add(colonVerslagen);
		CervixClientVerslagenOverzichtPanel cervixVerslagen = new CervixClientVerslagenOverzichtPanel("cervixVerslagen", model);
		add(cervixVerslagen);
		MammaClientVerslagenOverzichtPanel mammaVerslagen = new MammaClientVerslagenOverzichtPanel("mammaVerslagen", model);
		add(mammaVerslagen);

	}

	private <T extends ScreeningRonde> T getLaatsteScreeningronde(VerslagType verslagType)
	{
		return (T) HibernateHelper.deproxy(verwerkVerslagService.getValideScreeningsRonde(verslagType, getModelObject(), null, null));
	}

}
