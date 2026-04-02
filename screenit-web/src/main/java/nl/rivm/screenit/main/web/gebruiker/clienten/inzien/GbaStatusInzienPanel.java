package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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
import java.util.Comparator;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class GbaStatusInzienPanel extends GenericPanel<Client>
{
	@SpringBean
	private BaseGbaVraagService baseGbaVraagService;

	public GbaStatusInzienPanel(String id, IModel<Client> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addGbaStatusInfo();
		addGbaVraagInfo();
	}

	private void addGbaStatusInfo()
	{
		var gbaStatusInformatie = new WebMarkupContainer("gbaStatusInfo");

		var gbaStatus = getModelObject().getGbaStatus();
		var redenIntrekken = getModelObject().getRedenIntrekkenGbaIndicatieDoorBvo();
		if (gbaStatus == GbaStatus.INDICATIE_AANWEZIG && redenIntrekken != RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN)
		{
			gbaStatusInformatie.add(new EnumLabel<>("indicatieStatus", redenIntrekken));
		}
		else
		{
			gbaStatusInformatie.add(new EnumLabel<>("indicatieStatus", gbaStatus));
		}

		gbaStatusInformatie.add(DateLabel.forDatePattern("laatsteGbaMutatie.mutatieDatum",
			Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT
		));

		gbaStatusInformatie.add(DateLabel.forDatePattern("laatstAangevraagdOp",
			(IModel<Date>) this::getMomentLaatsteVerwijderIndicatie,
			Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT
		));

		add(gbaStatusInformatie);
	}

	private void addGbaVraagInfo()
	{
		var client = getModelObject();
		var gbaStatus = client.getGbaStatus();
		var meestRecenteGbaVraag = getMeestRecenteGbaVraag();

		var gbaVraagInformatie = new WebMarkupContainer("gbaVraag",
			ModelUtil.csModel(meestRecenteGbaVraag));

		var isZichtbaar = meestRecenteGbaVraag != null && gbaStatus == GbaStatus.INDICATIE_AANGEVRAAGD;
		gbaVraagInformatie.setVisible(isZichtbaar);

		gbaVraagInformatie.add(DateLabel.forDatePattern("creatieDatum",
			(IModel<Date>) () -> DateUtil.toUtilDate(meestRecenteGbaVraag.getDatum()),
			Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT
		));

		gbaVraagInformatie.add(new EnumLabel<>("vraagType"));

		gbaVraagInformatie.add(new EnumLabel<>("reden"));

		gbaVraagInformatie.add(new BooleanLabel("reactieOntvangen"));

		add(gbaVraagInformatie);
	}

	private Date getMomentLaatsteVerwijderIndicatie()
	{
		var gbaVragen = new ArrayList<>(getModelObject().getGbaVragen());
		var momentLaatsteVerwijderIndicatie = gbaVragen.stream()
			.filter(v -> v.getVraagType() == GbaVraagType.VERWIJDER_INDICATIE)
			.map(GbaVraag::getDatum)
			.max(Comparator.naturalOrder())
			.orElse(null);
		return DateUtil.toUtilDate(momentLaatsteVerwijderIndicatie);
	}

	private GbaVraag getMeestRecenteGbaVraag()
	{
		var client = getModelObject();
		return baseGbaVraagService.findLaatsteGbaVraag(client.getPersoon().getBsn(), client).orElse(null);
	}
}
