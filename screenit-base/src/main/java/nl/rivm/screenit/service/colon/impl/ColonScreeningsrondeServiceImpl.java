package nl.rivm.screenit.service.colon.impl;

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

import java.util.ArrayList;
import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.rivm.screenit.util.colon.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class ColonScreeningsrondeServiceImpl implements ColonScreeningsrondeService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	@Lazy
	private ColonBaseFitService fitService;

	@Autowired
	private ColonBaseAfspraakService afspraakService;

	@Override
	public boolean heeftUitslag(ColonUitnodiging uitnodiging, boolean checkAlleenUitslagGecommuniceerd)
	{
		var screeningRonde = uitnodiging.getScreeningRonde();
		for (var brief : screeningRonde.getBrieven())
		{
			if (BriefUtil.isOngunstigeUitslagBrief(brief))
			{
				return true;
			}
			else if (brief.getBriefType() == BriefType.COLON_GUNSTIGE_UITSLAG)
			{
				var laatsteFitRegistratie = screeningRonde.getLaatsteFitRegistratie();
				var laatsteFitRegistratieExtra = screeningRonde.getLaatsteExtraFitRegistratie();
				if (laatsteFitRegistratie != null && laatsteFitRegistratie.getStatus() != ColonFitRegistratieStatus.VERWIJDERD)
				{
					return true;
				}
				if (laatsteFitRegistratieExtra != null && laatsteFitRegistratieExtra.getStatus() != ColonFitRegistratieStatus.VERWIJDERD)
				{
					return true;
				}
			}
		}
		if (!checkAlleenUitslagGecommuniceerd)
		{
			var gekoppeldeFitRegistratie = uitnodiging.getGekoppeldeFitRegistratie();
			var gekoppeldeFitRegistratieExtra = uitnodiging.getGekoppeldeExtraFitRegistratie();

			return gekoppeldeFitRegistratieExtra != null && gekoppeldeFitRegistratieExtra.getUitslag() != null
				|| gekoppeldeFitRegistratie != null && gekoppeldeFitRegistratie.getUitslag() != null
				|| uitnodiging.getAntwoordFormulier() != null;
		}
		return false;
	}

	@Override
	@Transactional
	public ColonUitnodiging createNieuweUitnodiging(ColonScreeningRonde ronde, ColonUitnodigingscategorie nieuweUitnodigingscategorie)
	{
		ColonUitnodiging uitnodiging = null;
		var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && heeftUitslag(laatsteUitnodiging, true))
		{
			LOG.warn("Geen nieuwe uitnodiging aangemaakt, omdat er al een uitslag in de ronde aanwezig is. UID: " + laatsteUitnodiging.getUitnodigingsId());
		}
		else
		{
			var nu = currentDateSupplier.getLocalDateTime();
			uitnodiging = new ColonUitnodiging();
			uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
			uitnodiging.setOnderzoeksVariant(ColonOnderzoeksVariant.STANDAARD);
			if (laatsteUitnodiging == null)
			{
				if (ronde.getDossier().getScreeningRondes().size() == 1)
				{

					nieuweUitnodigingscategorie = ColonUitnodigingscategorie.U1;
				}
				else
				{
					nieuweUitnodigingscategorie = ColonUitnodigingscategorie.U2;
				}
			}
			uitnodiging.setUitnodigingscategorie(nieuweUitnodigingscategorie);
			uitnodiging.setScreeningRonde(ronde);
			uitnodiging.setCreatieDatum(DateUtil.toUtilDate(nu.plusSeconds(1)));
			var vooraankondigingsperiode = preferenceService.getInteger(PreferenceKey.VOORAANKONDIGINSPERIODE.name());
			var nieuweUitnodigingsDatum = DateUtil.toUtilDate(DateUtil.toLocalDate(uitnodiging.getCreatieDatum()).plusDays(1));
			var minUitnodigingsDatum = DateUtil.toUtilDate(DateUtil.toLocalDateTime(ronde.getCreatieDatum()).plusDays(vooraankondigingsperiode));
			if (nieuweUitnodigingsDatum.before(minUitnodigingsDatum))
			{
				nieuweUitnodigingsDatum = minUitnodigingsDatum;
			}

			if (laatsteUitnodiging != null)
			{
				if (!laatsteUitnodiging.isVerstuurd())
				{

					ronde.getUitnodigingen().remove(laatsteUitnodiging);
					nieuweUitnodigingsDatum = laatsteUitnodiging.getUitnodigingsDatum();
					ronde.setLaatsteUitnodiging(null);
					hibernateService.delete(laatsteUitnodiging);
				}
				else
				{
					var fitRegistratie = ColonFitRegistratieUtil.getFitRegistratie(laatsteUitnodiging);
					fitService.setFitRegistratiesVerlorenIndienActief(fitRegistratie);
				}
			}
			nieuweUitnodigingsDatum = fixUitnodigingsDatumBij2OpEenAdres(nieuweUitnodigingsDatum, uitnodiging);
			uitnodiging.setUitnodigingsDatum(nieuweUitnodigingsDatum);
			ronde.setLaatsteUitnodiging(uitnodiging);
			ronde.setLaatsteFitRegistratie(null);
			ronde.setLaatsteExtraFitRegistratie(null);
			ronde.getUitnodigingen().add(uitnodiging);

			hibernateService.saveOrUpdate(uitnodiging);
			hibernateService.saveOrUpdate(ronde);
		}
		return uitnodiging;
	}

	private Date fixUitnodigingsDatumBij2OpEenAdres(Date nieuweUitnodigingsDatum, ColonUitnodiging uitnodiging)
	{
		var wachttijdVerzendenPakket = preferenceService.getInteger(PreferenceKey.WACHTTIJD_VERZENDEN_PAKKET_TWEE_OP_EEN_ADRES.name());
		if (wachttijdVerzendenPakket == null)
		{
			throw new IllegalStateException("Wachttijd verzenden pakket bij 2 op 1 adres op de parameterisatie pagina is niet gezet");
		}
		var client = uitnodiging.getScreeningRonde().getDossier().getClient();

		var andereClientMetActieveFit = fitService.getAndereClientOpZelfdeAdresEnActieveFit(client, new ArrayList<>());

		if (andereClientMetActieveFit != null)
		{
			var creatieDatumUitnodigingAndereClient = DateUtil
				.toLocalDate(andereClientMetActieveFit.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum());
			creatieDatumUitnodigingAndereClient = creatieDatumUitnodigingAndereClient.plusDays(wachttijdVerzendenPakket);
			if (nieuweUitnodigingsDatum.before(DateUtil.toUtilDate(creatieDatumUitnodigingAndereClient)))
			{
				nieuweUitnodigingsDatum = DateUtil.toUtilDate(creatieDatumUitnodigingAndereClient);
			}
		}
		return nieuweUitnodigingsDatum;
	}

	@Override
	@Transactional
	public boolean maakGunstigeUitslagBriefVoorLaatsteRonde(Client client)
	{
		var laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
		var eersteGunstigeUitslag = ColonScreeningRondeUtil.getEersteGunstigeFitRegistratie(laatsteScreeningRonde);
		if (eersteGunstigeUitslag != null && !ColonScreeningRondeUtil.zijnErOngunstigeFitRegistraties(laatsteScreeningRonde))
		{
			var brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_GUNSTIGE_UITSLAG);
			brief.setFitRegistratie(eersteGunstigeUitslag);
			hibernateService.saveOrUpdate(brief);

			if (!ColonScreeningRondeUtil.zijnErActieveFitRegistraties(laatsteScreeningRonde))
			{
				sluitRonde(laatsteScreeningRonde);
			}
			logService.logGebeurtenis(LogGebeurtenis.GUNSTIGE_UITSLAG_VERSTUURD, client, Bevolkingsonderzoek.COLON);
			return true;
		}
		return false;
	}

	@Override
	public boolean isRondeStatusBuitenDoelgroep(ColonScreeningRonde ronde)
	{
		var uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		var maxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		var creatieDatumRonde = DateUtil.toLocalDate(ronde.getCreatieDatum());
		var vandaag = currentDateSupplier.getLocalDate();
		var geboortedatum = DateUtil.toLocalDate(ronde.getDossier().getClient().getPersoon().getGeboortedatum());

		return creatieDatumRonde.plusDays(uitnodigingsinterval).isBefore(vandaag) && geboortedatum.plusYears(maxLeeftijd).plusYears(1).isBefore(vandaag);
	}

	@Override
	public boolean heeftMaxAantalFitAanvragenBereikt(ColonScreeningRonde laatsteScreeningRonde)
	{
		var maxAantalFitAanvragenPerRonde = preferenceService.getInteger(PreferenceKey.COLON_MAX_AANTAL_FITS.name());

		return laatsteScreeningRonde.getUitnodigingen().size() >= maxAantalFitAanvragenPerRonde;
	}

	@Override
	@Transactional
	public void sluitRonde(ColonScreeningRonde ronde)
	{
		ronde.setStatus(ScreeningRondeStatus.AFGEROND);
		ronde.setStatusDatum(currentDateSupplier.getDate());
		var laatsteAfspraak = ronde.getLaatsteAfspraak();
		if (laatsteAfspraak != null && laatsteAfspraak.getStatus() == ColonAfspraakStatus.GEPLAND)
		{
			afspraakService.annuleerAfspraak(laatsteAfspraak, null, ColonAfspraakStatus.GEANNULEERD_ONBEKEND, true);
		}
	}
}
