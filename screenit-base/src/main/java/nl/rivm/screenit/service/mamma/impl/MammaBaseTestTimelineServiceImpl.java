package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseHL7v24MessageService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineService;
import nl.rivm.screenit.service.mamma.MammaBaseTestTimelineTimeService;
import nl.rivm.screenit.service.mamma.enums.MammaTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseTestTimelineServiceImpl implements MammaBaseTestTimelineService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseFactory factory;

	@Autowired
	private MammaBaseTestTimelineTimeService testTimelineTimeService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaBaseHL7v24MessageService baseHL7v24MessageService;

	@Override
	public MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid)
	{
		return nieuweRondeAfspraakUitnodiging(client, screeningsEenheid, null, true);
	}

	@Override
	public MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, MammaStandplaatsRonde standplaatsRonde,
		boolean zetDossierTerug)
	{
		return nieuweRondeAfspraakUitnodiging(client, screeningsEenheid, standplaatsRonde, true, zetDossierTerug);
	}

	@Override
	public MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht)
	{
		return nieuweRondeAfspraakUitnodiging(client, screeningsEenheid, null, stuurHl7Bericht);
	}

	@Override
	public MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, MammaStandplaatsRonde standplaatsRonde,
		boolean stuurHl7Bericht, boolean rekenDossierTerug)
	{
		var ronde = nieuweRonde(client, standplaatsRonde, rekenDossierTerug);
		var uitnodiging = factory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(), BriefType.MAMMA_AFSPRAAK_UITNODIGING);
		verzendLaatsteBrief(ronde);

		maakAfspraak(uitnodiging.getScreeningRonde(), screeningsEenheid, stuurHl7Bericht);

		return uitnodiging;
	}

	@Override
	public MammaUitnodiging nieuweRondeAfspraakUitnodiging(Client client, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht, Long uitnodigingsNr)
	{
		var ronde = nieuweRonde(client, null, true);
		ronde.setUitnodigingsNr(uitnodigingsNr);
		var uitnodiging = factory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(), BriefType.MAMMA_AFSPRAAK_UITNODIGING);
		verzendLaatsteBrief(ronde);

		maakAfspraak(uitnodiging.getScreeningRonde(), screeningsEenheid, stuurHl7Bericht);

		return uitnodiging;
	}

	@Override
	public MammaUitnodiging nieuweRondeMetOpenUitnodiging(Client client)
	{
		return nieuweRondeMetOpenUitnodiging(client, true);
	}

	@Override
	public MammaUitnodiging nieuweRondeMetOpenUitnodiging(Client client, boolean stuurHl7Bericht)
	{
		var ronde = nieuweRonde(client, null, true);
		var uitnodiging = factory.maakUitnodiging(ronde, ronde.getStandplaatsRonde(), BriefType.MAMMA_OPEN_UITNODIGING);
		verzendLaatsteBrief(ronde);

		return uitnodiging;
	}

	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde ronde, MammaScreeningsEenheid screeningsEenheid)
	{
		return maakAfspraak(ronde, screeningsEenheid, true);
	}

	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde ronde, MammaScreeningsEenheid screeningsEenheid, boolean stuurHl7Bericht)
	{
		testTimelineTimeService.rekenDossierTerug(ronde.getDossier(), MammaTestTimeLineDossierTijdstip.DATUM_TIJD_AFSPRAAK);

		var nu = currentDateSupplier.getLocalDateTime();
		var afspraakDatum = nu.truncatedTo(ChronoUnit.HOURS).plusMinutes(30L * (nu.getMinute() / 30)).plusWeeks(5);
		var standplaatsPeriode = screeningsEenheid.getStandplaatsPerioden().get(0);
		if (standplaatsPeriode == null)
		{
			throw new IllegalArgumentException("Screeningeenheid " + screeningsEenheid.getNaam() + " heeft geen standplaatsperiode");
		}
		baseKansberekeningService.resetPreferences();
		return baseAfspraakService.maakAfspraak(ronde, null, DateUtil.toUtilDate(afspraakDatum), standplaatsPeriode, null, true, true, false, stuurHl7Bericht, false, null, false);
	}

	private MammaScreeningRonde nieuweRonde(Client client, MammaStandplaatsRonde standplaatsRonde, boolean rekenDossierTerug)
	{
		baseHL7v24MessageService.verwijderBerichtVoorClient(client, true);
		var dossier = client.getMammaDossier();
		if (rekenDossierTerug)
		{
			testTimelineTimeService.rekenDossierTerug(dossier, MammaTestTimeLineDossierTijdstip.NIEUWE_RONDE);
		}

		if (standplaatsRonde == null)
		{
			var lijstMetStandplaatsronden = hibernateService.loadAll(MammaStandplaatsRonde.class);
			var eersteStandplaatsRondeMetAchtervang = lijstMetStandplaatsronden
				.stream()
				.filter(standplaatsRondeToDetermine -> standplaatsRondeToDetermine.getAchtervangToegepast().equals(Boolean.TRUE))
				.findFirst();
			standplaatsRonde = eersteStandplaatsRondeMetAchtervang.orElse(lijstMetStandplaatsronden.get(0));
		}
		return factory.maakRonde(dossier, standplaatsRonde, false);
	}

	private void verzendLaatsteBrief(MammaScreeningRonde ronde)
	{
		var brief = ronde.getLaatsteBrief();

		var mergedBrieven = new MammaMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(brief.getBriefType());
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		mergedBrieven.setScreeningOrganisatie(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		brief.setGegenereerd(true);
		brief.setMergedBrieven(mergedBrieven);
		var fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");
		hibernateService.saveOrUpdate(fakeMergeDocument);
		mergedBrieven.setMergedBrieven(fakeMergeDocument);
		hibernateService.saveOrUpdate(mergedBrieven);
		hibernateService.saveOrUpdate(brief);
	}
}
