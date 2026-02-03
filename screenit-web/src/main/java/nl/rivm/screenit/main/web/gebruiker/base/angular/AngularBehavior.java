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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaVisitatieOnderzoekenWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenOnderdeelInsteltechniekWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenOnderdeelIntervalcarcinomenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie.MammaVisitatieOnderzoekenWerklijstPage;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

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
