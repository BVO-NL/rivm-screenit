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

import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OpenUitnodigingService;
import nl.rivm.screenit.service.colon.ColonAfmeldService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional
public class ColonAfmeldServiceImpl implements ColonAfmeldService
{
	private static final Logger LOG = LoggerFactory.getLogger(ColonAfmeldServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OpenUitnodigingService openUitnodigingService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private ColonBaseAfspraakService afspraakService;

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Override
	public ColonAfmelding maakAfmelding()
	{
		return new ColonAfmelding();
	}

	@Override
	public void definitieveAfmeldingAanvragen(ColonAfmelding afmelding, boolean rappelBrief)
	{
		var briefType = BriefType.COLON_AFMELDING_AANVRAAG;
		if (rappelBrief)
		{
			briefType = BriefType.COLON_AFMELDING_HANDTEKENING;
		}
		afmelding.setAfmeldingAanvraag(briefService.maakBvoBrief(afmelding, briefType, currentDateSupplier.getDate()));
		hibernateService.saveOrUpdate(afmelding);
	}

	@Override
	public void eenmaligAfmelden(ColonAfmelding afmelding, Account account)
	{
		var ronde = afmelding.getScreeningRonde();

		afspraakAfzeggen(ronde);
		nietVerzondenUitnodigingenVerwijderen(ronde);
		openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(afmelding, ronde, account);
	}

	private void afspraakAfzeggen(ColonScreeningRonde ronde)
	{
		var laatsteAfspraak = ronde.getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			ColonConclusieType conclusieType = null;
			var conclusie = laatsteAfspraak.getConclusie();
			if (conclusie != null)
			{
				conclusieType = conclusie.getType();
			}
			var status = laatsteAfspraak.getStatus();
			if (ColonAfspraakStatus.GEPLAND.equals(status)
				|| ColonAfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(conclusieType))
			{
				afspraakService.afspraakAfzeggen(laatsteAfspraak, ColonAfspraakStatus.GEANNULEERD_AFMELDEN, currentDateSupplier.getLocalDateTime(), false);

				LOG.info("Afmelding: laatste intake afspraak is afgezegd");
			}
		}
	}

	private void nietVerzondenUitnodigingenVerwijderen(ColonScreeningRonde ronde)
	{
		var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging != null && !laatsteUitnodiging.isVerstuurd())
		{
			var uitnodigingen = ronde.getUitnodigingen();
			uitnodigingen.remove(laatsteUitnodiging);

			ColonUitnodiging nieuweLaatsteUitnodiging = null;
			for (var uitnodiging : uitnodigingen)
			{
				if (nieuweLaatsteUitnodiging == null || nieuweLaatsteUitnodiging.getUitnodigingsId() < uitnodiging.getUitnodigingsId())
				{
					nieuweLaatsteUitnodiging = uitnodiging;
				}
			}
			ronde.setLaatsteUitnodiging(nieuweLaatsteUitnodiging);
			hibernateService.delete(laatsteUitnodiging);
		}
	}

	@Override
	public void vervolgAfmelden(ColonAfmelding afmelding)
	{
		if (AfmeldingType.DEFINITIEF.equals(afmelding.getType()) || AfmeldingType.TIJDELIJK.equals(afmelding.getType()))
		{
			if (!ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden()))
			{
				afmelding.setAfmeldingBevestiging(
					briefService.maakBvoBrief(afmelding, BriefType.COLON_AFMELDING_BEVESTIGING, currentDateSupplier.getDate()));
				hibernateService.saveOrUpdate(afmelding);
			}
			var dossier = afmelding.getDossier();
			if (dossier == null)
			{
				dossier = afmelding.getScreeningRonde().getDossier();
			}
			var ronde = dossier.getLaatsteScreeningRonde();
			if (ronde != null && ronde.getOpenUitnodiging() != null && ronde.getOpenUitnodiging().getUitslag() == null)
			{
				var uitnodiging = ronde.getOpenUitnodiging();
				uitnodiging.setUitslag(OpenUitnodigingUitslag.AFMELDING);
				uitnodiging.setAfmelding(afmelding);
				hibernateService.saveOrUpdate(uitnodiging);
			}
		}
	}

	@Override
	public void vervolgHeraanmelden(ColonAfmelding herAanTeMeldenAfmelding, Account account)
	{
		var ronde = getGeldigeRondeVoorHeraanmelding(herAanTeMeldenAfmelding);

		var nu = currentDateSupplier.getLocalDateTime();
		if (herAanTeMeldenAfmelding.getType() != AfmeldingType.EENMALIG
			&& herAanTeMeldenAfmelding.getReden() != ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK
			&& BooleanUtils.isNotTrue(herAanTeMeldenAfmelding.getHeraanmeldingBevestigingsBriefTegenhouden()) &&
			(herAanTeMeldenAfmelding.getType() == AfmeldingType.DEFINITIEF || herAanTeMeldenAfmelding.getAfmeldingStatus() == AanvraagBriefStatus.VERWERKT))
		{
			herAanTeMeldenAfmelding.setHeraanmeldBevestiging(
				briefService.maakBvoBrief(herAanTeMeldenAfmelding, BriefType.COLON_HERAANMELDING_BEVESTIGING, DateUtil.toUtilDate(nu)));
			hibernateService.saveOrUpdate(herAanTeMeldenAfmelding);
		}

		var afspraak = herAanTeMeldenAfmelding.getHeraanmeldingAfspraak();
		if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging()) && afspraak == null)
		{
			screeningsrondeService.createNieuweUitnodiging(ronde, ColonUitnodigingscategorie.U4_2);
		}

		if (afspraak != null)
		{
			var client = ronde.getDossier().getClient();
			afspraak.setClient(client);
			afspraak.setScreeningRonde(ronde);
			afspraak.setGewijzigdOp(nu.plus(100, ChronoUnit.MILLIS));

			ColonAfspraakslot afspraakslot = null;
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getHeraanmeldingAfspraakUitRooster()))
			{
				afspraakslot = afspraakService.getAfspraakslotVoorAfspraak(afspraak);
				afspraak.setAfspraakslot(afspraakslot);
			}
			hibernateService.saveOrUpdate(afspraak);
			if (afspraakslot != null)
			{
				afspraakslot.setAfspraak(afspraak);
				hibernateService.saveOrUpdate(afspraakslot);
			}

			ronde.getAfspraken().add(afspraak);
			ronde.setLaatsteAfspraak(afspraak);
			client.getAfspraken().add(afspraak);
			hibernateService.saveOrUpdate(client);
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

			var afspraakBriefType = herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefType();
			if (afspraakBriefType == null)
			{
				afspraakBriefType = BriefType.COLON_UITNODIGING_INTAKE;
			}
			var brief = briefService.maakBvoBrief(ronde, afspraakBriefType, DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS)));

			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getHeraanmeldingAfspraakBriefTegenhouden()))
			{
				hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
			}
			brief.setIntakeAfspraak(afspraak);
			hibernateService.saveOrUpdate(brief);
			LOG.info("Heraanmelden: nieuw afspraak aangemaakt");
		}

		if (ronde != null)
		{
			openUitnodigingService.afmeldingHeraanmeldingReactieOpOpenUitnodiging(herAanTeMeldenAfmelding, ronde, account);
			if (Boolean.FALSE.equals(herAanTeMeldenAfmelding.getClientWilNieuweUitnodiging()))
			{
				var uitnodiging = ronde.getLaatsteUitnodiging();
				if (uitnodiging != null)
				{
					var gekoppeldeFitRegistratie = uitnodiging.getGekoppeldeFitRegistratie();
					var fitStatus = ColonFitRegistratieUtil.getActieveFitRegistratieStatusNaHeraanmelding(uitnodiging);
					if (fitStatus != null && gekoppeldeFitRegistratie != null
						&& gekoppeldeFitRegistratie.getStatus().magWijzigenNaarStatus(fitStatus, gekoppeldeFitRegistratie))
					{
						gekoppeldeFitRegistratie.setStatus(fitStatus);
						gekoppeldeFitRegistratie.setStatusDatum(DateUtil.toUtilDate(nu.plus(100, ChronoUnit.MILLIS)));
						hibernateService.saveOrUpdate(gekoppeldeFitRegistratie);
					}
				}
			}
		}
	}

	@Override
	public ColonScreeningRonde getGeldigeRondeVoorHeraanmelding(ColonAfmelding herAanTeMeldenAfmelding)
	{
		ColonScreeningRonde ronde = null;
		switch (herAanTeMeldenAfmelding.getType())
		{
		case EENMALIG:
		case TIJDELIJK:
			if (Boolean.TRUE.equals(herAanTeMeldenAfmelding.getRondeGesloten()))
			{
				ronde = herAanTeMeldenAfmelding.getScreeningRonde();
			}
			break;
		case DEFINITIEF:
			ronde = herAanTeMeldenAfmelding.getDossier().getLaatsteScreeningRonde();
			break;
		default:
			throw new IllegalStateException();
		}

		return ronde;
	}

	@Override
	public String getAanvullendeHeraanmeldLogMelding(ColonAfmelding afmelding)
	{
		return "";
	}
}
