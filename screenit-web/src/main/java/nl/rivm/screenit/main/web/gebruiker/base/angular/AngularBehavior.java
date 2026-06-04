package nl.rivm.screenit.main.web.gebruiker.base.angular;

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

import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaFotobesprekingOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaFotobesprekingService;
import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking.MammaFotobesprekingOnderzoekenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenOnderdeelInsteltechniekWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenOnderdeelIntervalcarcinomenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenWerklijstPage;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;

@RequiredArgsConstructor
@Slf4j
public class AngularBehavior extends AbstractDefaultAjaxBehavior
{
	private final String namespace;

	private final List<String> payload;

	private final String serverUrl;

	@SpringBean
	private MammaVisitatieService mammaVisitatieService;

	@SpringBean
	private MammaFotobesprekingService mammaFotobesprekingService;

	@SpringBean
	private ClientService clientService;

	@Override
	protected void respond(AjaxRequestTarget ajaxRequestTarget)
	{
		final var action = RequestCycle.get().getRequest().getRequestParameters().getParameterValue("action");
		try
		{
			switch (action.toString())
			{
			case "toVisitatiePage":
				var visitatieId = getAngularURLParameter("visitatie");
				gaNaarVisitatie(visitatieId);
				break;
			case "toFotobesprekingPage":
				var fotobesprekingId = getAngularURLParameter("fotobespreking");
				gaNaarFotobespreking(fotobesprekingId);
				break;
			case "toClientgegevens":
				var clientgegevensId = getAngularURLParameter("client");
				gaNaarClientgegevens(clientgegevensId);
				break;
			}
		}
		catch (Exception e)
		{
			throw e;
		}
	}

	private void gaNaarVisitatie(String visitatieId)
	{
		var visitatie = mammaVisitatieService.getById(Long.parseLong(visitatieId));
		var zoekObjectModel = new CompoundPropertyModel<>(new MammaVisitatieOnderzoekenWerklijstZoekObject());
		zoekObjectModel.getObject().setVisitatie(visitatie);
		ScreenitSession.get().setZoekObject(MammaVisitatieOnderzoekenWerklijstPage.SESSION_KEY, zoekObjectModel);
		if (ScreenitSession.get().checkPermission(Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK, Actie.INZIEN))
		{
			this.getComponent().setResponsePage(new MammaVisitatieOnderzoekenOnderdeelInsteltechniekWerklijstPage());
		}
		else
		{
			this.getComponent().setResponsePage(new MammaVisitatieOnderzoekenOnderdeelIntervalcarcinomenWerklijstPage());
		}
	}

	private void gaNaarFotobespreking(String fotobesprekingId)
	{
		var fotobespreking = mammaFotobesprekingService.getFotobespreking(Long.parseLong(fotobesprekingId));
		var zoekObjectModel = new CompoundPropertyModel<>(new MammaFotobesprekingOnderzoekenWerklijstZoekObject());
		zoekObjectModel.getObject().setFotobespreking(fotobespreking);
		ScreenitSession.get().setZoekObject(MammaFotobesprekingOnderzoekenWerklijstPage.SESSION_KEY, zoekObjectModel);
		this.getComponent().setResponsePage(new MammaFotobesprekingOnderzoekenWerklijstPage());
	}

	private void gaNaarClientgegevens(String clientId)
	{
		var client = clientService.getClientById(Long.parseLong(clientId)).orElseThrow();
		var clientModel = new SimpleHibernateModel<>(client);
		this.getComponent().setResponsePage(new ClientInzienPage(clientModel));
	}

	private String getAngularURLParameter(final String keyVanParameter)
	{
		return RequestCycle.get().getRequest().getRequestParameters().getParameterValue(keyVanParameter).toString();
	}

	@Override
	public void renderHead(final Component component, final IHeaderResponse response)
	{
		super.renderHead(component, response);
		initialiseerAngularComponent(response);
	}

	private void initialiseerAngularComponent(final IHeaderResponse response)
	{
		final var callbackUrl = getCallbackUrl().toString();
		final var builder = new ScriptBuilder(namespace, serverUrl, callbackUrl);
		response.render(JavaScriptHeaderItem.forScript(builder.bouwWicketPropertiesScript(), null));
	}
}
