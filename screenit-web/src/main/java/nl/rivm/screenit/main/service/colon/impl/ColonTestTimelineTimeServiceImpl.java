package nl.rivm.screenit.main.service.colon.impl;

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

import java.time.temporal.ChronoUnit;
import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineTimeService;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.service.BaseTestTimelineService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class ColonTestTimelineTimeServiceImpl implements ColonTestTimelineTimeService
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseTestTimelineService baseTestTimelineService;

	@Override
	public boolean calculateBackwards(ColonDossier dossier, TestTimeLineDossierTijdstip tijdstip)
	{
		var dagen = aantalDagenCalculator(dossier, tijdstip);
		calculateBackwards(dossier, dagen);
		return true;
	}

	@Override
	public boolean calculateBackwards(ColonDossier dossier, int dagen)
	{
		LOG.debug("Dossier aantal dagen terug gezet: " + dagen);
		baseTestTimelineService.rekenObjectTerug(dossier, dagen);
		baseTestTimelineService.rekenObjectTerug(dossier.getColonVooraankondiging(), dagen);
		baseTestTimelineService.rekenObjectTerug(dossier.getVolgendeUitnodiging(), dagen);
		baseTestTimelineService.rekenObjectTerug(dossier.getAfmeldingen(), dagen);
		rekenAlleScreeningRondesTerug(dossier, dagen);
		baseTestTimelineService.rekenAllePersoonsDatumTerug(dossier.getClient().getPersoon(), dagen);
		return true;
	}

	@Override
	public Date getVooraankondigingsPeriodeDatum()
	{
		int vooraankondigingsPeriode = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
		return DateUtil.plusDagen(currentDateSupplier.getDate(), vooraankondigingsPeriode);
	}

	private int aantalDagenCalculator(ColonDossier dossier, TestTimeLineDossierTijdstip tijdstip)
	{
		var aantalDagen = 0;
		switch (tijdstip)
		{
		case DAG_UITNODIGING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			var vooraankondiging = dossier.getColonVooraankondiging();
			return overgeblevenDagen(vooraankondiging.getCreatieDatum(), aantalDagen);
		case DAG_NA_UITNODIGING_KOPPELEN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			aantalDagen = aantalDagen + 3;
			vooraankondiging = dossier.getColonVooraankondiging();
			return overgeblevenDagen(vooraankondiging.getCreatieDatum(), aantalDagen);
		case ANTWOORDFORMULIER_ONTVANGEN:
		case IFOBT_TERUG_ONTVANGEN:
			aantalDagen = 3;
			return overgeblevenDagen(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getVerstuurdDatum(), aantalDagen);
		case EINDE_RONDE:
			aantalDagen = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
			return overgeblevenDagen(dossier.getLaatsteScreeningRonde().getCreatieDatum(), aantalDagen);
		case DAG_HERINNERING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			return overgeblevenDagen(getTestMetEersteStatusDatum(dossier).getStatusDatum(), aantalDagen);
		case DAG_NA_HERINNERING_VERSTUREN:
			aantalDagen = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
			aantalDagen = aantalDagen + 1;
			return overgeblevenDagen(getTestMetEersteStatusDatum(dossier).getStatusDatum(), aantalDagen);
		case INTAKE_AFSPRAAK_CONCLUSIE:
			var ronde = dossier.getLaatsteScreeningRonde();
			var afspraak = ronde.getLaatsteAfspraak();
			return 3 + DateUtil.getPeriodeTussenTweeDatums(currentDateSupplier.getLocalDate(), afspraak.getVanaf(), ChronoUnit.DAYS);
		case MDL_VERSLAG:
			return 30;
		default:
			return aantalDagen;
		}
	}

	private IFOBTTest getTestMetEersteStatusDatum(ColonDossier dossier)
	{
		IFOBTTest eersteTest = null;
		var ronde = dossier.getLaatsteScreeningRonde();
		for (var test : ronde.getIfobtTesten())
		{
			if (!test.isHerinnering() && (eersteTest == null || test.getStatusDatum().before(eersteTest.getStatusDatum())))
			{
				eersteTest = test;
			}
		}
		return eersteTest;
	}

	private int overgeblevenDagen(Date date, int aantalDagen)
	{
		var aantalDagenReverse = currentDateSupplier.getLocalDate().minusDays(aantalDagen);
		var dagen = DateUtil.getPeriodeTussenTweeDatums(aantalDagenReverse, DateUtil.toLocalDate(date), ChronoUnit.DAYS);
		return Math.max(dagen, 0);
	}

	private void rekenAlleScreeningRondesTerug(ColonDossier dossier, int dagen)
	{
		var rondes = dossier.getScreeningRondes();
		for (var ronde : rondes)
		{
			baseTestTimelineService.rekenObjectTerug(ronde, dagen);

			rekenAlleUitnodigingenTerug(ronde, dagen);
			baseTestTimelineService.rekenObjectTerug(ronde.getBrieven(), dagen); 
			rekenAlleIntakeAfsprakenTerug(ronde, dagen);
			rekenAlleVerslagenTerug(ronde, dagen);
			baseTestTimelineService.rekenObjectTerug(ronde.getHuisartsBerichten(), dagen);
			baseTestTimelineService.rekenObjectTerug(ronde.getAfmeldingen(), dagen);
		}
	}

	private void rekenAlleVerslagenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (ColonVerslag<?> verslag : ronde.getVerslagen())
		{
			if (VerslagType.MDL == verslag.getType() || VerslagType.PA_LAB == verslag.getType())
			{
				baseTestTimelineService.rekenObjectTerug(verslag, dagen);
			}
		}
	}

	private void rekenAlleIntakeAfsprakenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (var afspraak : ronde.getAfspraken())
		{
			baseTestTimelineService.rekenObjectTerug(afspraak.getConclusie(), dagen);
			baseTestTimelineService.rekenObjectTerug(afspraak, dagen);
		}
	}

	private void rekenAlleUitnodigingenTerug(ColonScreeningRonde ronde, int dagen)
	{
		for (var uitnodiging : ronde.getUitnodigingen())
		{
			baseTestTimelineService.rekenObjectTerug(uitnodiging, dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getGekoppeldeTest(), dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getGekoppeldeExtraTest(), dagen);
			baseTestTimelineService.rekenObjectTerug(uitnodiging.getAntwoordFormulier(), dagen);
		}
	}
}
