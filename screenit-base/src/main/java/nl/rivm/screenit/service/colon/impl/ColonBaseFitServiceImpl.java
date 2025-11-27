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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitRegistratie_;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatSetRepository;
import nl.rivm.screenit.repository.colon.ColonFitRegistratieRepository;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonStudieRegistratieService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.rivm.screenit.util.colon.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.SessionFactory;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSetSpecification.heeftBestandsnaam;
import static nl.rivm.screenit.specification.colon.ColonFitAnalyseResultaatSetSpecification.heeftStatussen;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftDossier;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftFitType;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftStatusDatumVoorOfOp;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.valideerFitUitslagStatus;
import static nl.rivm.screenit.util.colon.ColonFitRegistratieUtil.UITSLAG_FLAG_PRO;

@Slf4j
@Service
public class ColonBaseFitServiceImpl implements ColonBaseFitService
{
	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Setter
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseAfmeldService baseAfmeldService;

	@Autowired
	private ColonScreeningsrondeService screeningsrondeService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Autowired
	private ColonStudieRegistratieService studieRegistratieService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ColonFitRegistratieRepository fitRegistratieRepository;

	@Autowired
	private ColonFitAnalyseResultaatSetRepository fitAnalyseResultaatSetRepository;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

	private void rondeSluitenIndienMogelijk(LocalDateTime nu, ColonScreeningRonde ronde)
	{
		var allesAfgerondEnGunstig = true;
		var isUitslagBriefVerstuurd = false;
		for (var fitRegistratie : ronde.getFitRegistraties())
		{
			if (ColonFitRegistratieStatus.ACTIEF.equals(fitRegistratie.getStatus()) || ColonFitRegistratieUtil.isOngunstig(fitRegistratie))
			{
				allesAfgerondEnGunstig = false;
				break;
			}
		}

		isUitslagBriefVerstuurd = ColonScreeningRondeUtil.heeftUitslagBrief(ronde);

		if (allesAfgerondEnGunstig && isUitslagBriefVerstuurd)
		{
			ronde.setStatus(ScreeningRondeStatus.AFGEROND);
			ronde.setStatusDatum(DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS)));
		}
	}

	private void setNormWaarde(ColonFitRegistratie fitRegistratie, BigDecimal normWaarde)
	{
		if (fitRegistratie.getStatus() != ColonFitRegistratieStatus.NIETTEBEOORDELEN)
		{
			fitRegistratie.setNormWaarde(normWaarde);
		}
		if (fitRegistratie.getStatus() != ColonFitRegistratieStatus.VERWIJDERD)
		{
			checkVervaldatumVerlopen(fitRegistratie);
		}
	}

	@Override
	@Transactional
	public void verwerkAnalyseResultaat(ColonFitRegistratie teVerwerkenFit)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("verwerkAnalyseResultaat {}", teVerwerkenFit.getBarcode());
		}

		var nu = currentDateSupplier.getLocalDateTime();
		var screeningRonde = teVerwerkenFit.getScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var projectClient = ProjectUtil.getHuidigeProjectClient(dossier.getClient(), currentDateSupplier.getDate());
		var normWaardeGold = getFitNormWaarde(projectClient);
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(teVerwerkenFit);

		if (!teVerwerkenFit.getScreeningRonde().equals(dossier.getLaatsteScreeningRonde()))
		{
			studieRegistratieService.projectClientInactiverenBijVergelijkendOnderzoek(teVerwerkenFit.getScreeningRonde());
		}
		uitnodigingService.berekenEnSetUitgesteldeUitslagDatum(uitnodiging);

		setNormWaarde(teVerwerkenFit, normWaardeGold);
		var isNietVerlopen = teVerwerkenFit.getStatus() != ColonFitRegistratieStatus.VERVALDATUMVERLOPEN;
		var isOngunstigOfNietVerwijderd = ColonFitRegistratieUtil.isOngunstig(teVerwerkenFit) || teVerwerkenFit.getStatus() != ColonFitRegistratieStatus.VERWIJDERD;
		if (isNietVerlopen && isOngunstigOfNietVerwijderd)
		{
			setStatus(uitnodiging, ColonFitRegistratieStatus.UITGEVOERD);
		}

		vervolgVerwerkingUitslag(teVerwerkenFit);

		if (isNietVerlopen)
		{
			rondeSluitenIndienMogelijk(nu, dossier.getLaatsteScreeningRonde());
			saveOrUpdateFitRegistratie(uitnodiging);
		}
	}

	private void vervolgVerwerkingUitslag(ColonFitRegistratie teVerwerkenFit)
	{
		var screeningRonde = fitNaarLaatsteRondeIndienNodig(teVerwerkenFit);
		if (ColonFitRegistratieStatus.isMislukteAnalyse(teVerwerkenFit.getStatus()))
		{
			studieRegistratieService.projectClientInactiverenBijVergelijkendOnderzoek(screeningRonde);
		}
		bepaalEnSetHeraanmeldenTekstKey(teVerwerkenFit);
		heraanmelden(screeningRonde);
		maakBuitenDoelgroepIndienNodig(teVerwerkenFit);
		verwijderNogNietVerstuurdeUitnodigingIndienNodig(teVerwerkenFit);
		maakExtraMonsterBriefIndienNodig(teVerwerkenFit);
		maakNieuweUitnodigingNavMislukteAnalyseIndienNodig(teVerwerkenFit);
	}

	private void verwijderNogNietVerstuurdeUitnodigingIndienNodig(ColonFitRegistratie teVerwerkenFit)
	{
		var screeningRonde = teVerwerkenFit.getScreeningRonde();
		boolean teVerwerkenFitHeeftSuccesvolleAnalyse = ColonFitRegistratieUtil.heeftSuccesvolleAnalyse(teVerwerkenFit);
		var rondeHeeftFitMetMislukteAnalyse = screeningRonde.getFitRegistraties()
			.stream()
			.filter(fit -> !fit.equals(teVerwerkenFit))
			.anyMatch(fit -> ColonFitRegistratieStatus.isMislukteAnalyse(fit.getStatus()));
		if (laatsteUitnodigingNietVerstuurd(screeningRonde) && (rondeHeeftFitMetMislukteAnalyse || teVerwerkenFitHeeftSuccesvolleAnalyse))
		{
			verwijderLaatsteUitnodiging(screeningRonde);
		}
	}

	private void maakExtraMonsterBriefIndienNodig(ColonFitRegistratie teVerwerkenFit)
	{

		var ronde = teVerwerkenFit.getScreeningRonde();
		var heeftRondeSuccesvolleFitMetBrief = ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_SUCCESVOLLE_ANALYSE_BRIEVEN.contains(brief.getBriefType()));
		var heeftRondeGunstigeUitslagMetBrief = ronde.getBrieven().stream().anyMatch(brief -> brief.getBriefType() == BriefType.COLON_GUNSTIGE_UITSLAG);
		var mislukteAnalyseEnUitnodigingNietVerstuurd =
			ColonFitRegistratieStatus.isMislukteAnalyse(teVerwerkenFit.getStatus()) && laatsteUitnodigingNietVerstuurd(ronde);
		var heeftExtraMonsterBrief = ronde.getBrieven().stream().anyMatch(brief -> !brief.isGegenereerd() && brief.getBriefType() == BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
		var heeftGeenFitMetZelfdeStatusdatum = ronde.getFitRegistraties().stream()
			.filter(fit -> !fit.equals(teVerwerkenFit))
			.map(fit -> DateUtil.toLocalDate(fit.getStatusDatum()))
			.noneMatch(verwerkingsdatum -> DateUtil.toLocalDate(teVerwerkenFit.getStatusDatum()).equals(verwerkingsdatum));
		var isBuitenDoelgroep = screeningsrondeService.isRondeStatusBuitenDoelgroep(ronde);
		var rondeHeeftAlGunstigeUitslag = !(ColonFitRegistratieUtil.isOngunstig(teVerwerkenFit) && heeftRondeGunstigeUitslagMetBrief);
		var rondeHeeftAlUitslag = rondeHeeftAlGunstigeUitslag || mislukteAnalyseEnUitnodigingNietVerstuurd;
		if (heeftRondeSuccesvolleFitMetBrief
			&& rondeHeeftAlUitslag
			&& !heeftExtraMonsterBrief
			&& heeftGeenFitMetZelfdeStatusdatum
			&& !isBuitenDoelgroep)
		{
			maakBriefEnKoppelAanTest(ronde, teVerwerkenFit, BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
		}
	}

	private void maakNieuweUitnodigingNavMislukteAnalyseIndienNodig(ColonFitRegistratie teVerwerkenFit)
	{

		var ronde = teVerwerkenFit.getScreeningRonde();
		if (!ColonFitRegistratieUtil.heeftSuccesvolleAnalyse(ronde))
		{
			var nietTeBeoordelen = teVerwerkenFit.getStatus() == ColonFitRegistratieStatus.NIETTEBEOORDELEN;
			var categorie = nietTeBeoordelen ? ColonUitnodigingscategorie.U3 : ColonUitnodigingscategorie.U6;
			screeningsrondeService.createNieuweUitnodiging(ronde, categorie);
		}
	}

	private boolean laatsteUitnodigingNietVerstuurd(ColonScreeningRonde screeningRonde)
	{
		var uitnodiging = screeningRonde.getLaatsteUitnodiging();
		return uitnodiging == null || uitnodiging.getVerstuurdDatum() == null;
	}

	private void verwijderLaatsteUitnodiging(ColonScreeningRonde ronde)
	{
		var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		if (laatsteUitnodiging == null || laatsteUitnodiging.getGekoppeldeFitRegistratie() != null || laatsteUitnodiging.getGekoppeldeExtraFitRegistratie() != null)
		{
			return;
		}

		ronde.getUitnodigingen().remove(laatsteUitnodiging);
		uitnodigingRepository.delete(laatsteUitnodiging);

		ronde.setLaatsteUitnodiging(ronde.getUitnodigingen().stream().max(Comparator.comparing(ColonUitnodiging::getUitnodigingsId)).orElse(null));
	}

	private void maakBuitenDoelgroepIndienNodig(ColonFitRegistratie fitVoorBrief)
	{
		var ronde = fitVoorBrief.getScreeningRonde();
		if (screeningsrondeService.isRondeStatusBuitenDoelgroep(ronde))
		{
			if (ColonFitRegistratieUtil.isOngunstig(fitVoorBrief))
			{
				maakBriefEnKoppelAanTest(ronde, fitVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP);
			}
			else if (ColonFitRegistratieStatus.isMislukteAnalyse(fitVoorBrief.getStatus()))
			{
				maakBriefEnKoppelAanTest(ronde, fitVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP);
			}
		}
	}

	private void maakBriefEnKoppelAanTest(ColonScreeningRonde ronde, ColonFitRegistratie fitVoorBrief, BriefType briefType)
	{
		var brief = briefService.maakBvoBrief(ronde, briefType);
		brief.setFitRegistratie(fitVoorBrief);
		hibernateService.saveOrUpdate(brief);
	}

	private ColonScreeningRonde fitNaarLaatsteRondeIndienNodig(ColonFitRegistratie fitRegistratieMetUitslag)
	{
		var uitnodiging = fitRegistratieMetUitslag.getUitnodiging();
		var screeningRonde = uitnodiging.getScreeningRonde();
		var laatsteScreeningRonde = screeningRonde.getDossier().getLaatsteScreeningRonde();
		if (!screeningRonde.equals(laatsteScreeningRonde))
		{

			screeningRonde.getFitRegistraties().remove(fitRegistratieMetUitslag);
			laatsteScreeningRonde.getFitRegistraties().add(fitRegistratieMetUitslag);
			fitRegistratieMetUitslag.setScreeningRonde(laatsteScreeningRonde);
			var gekoppeldeExtraFitRegistratie = uitnodiging.getGekoppeldeExtraFitRegistratie();
			if (gekoppeldeExtraFitRegistratie != null)
			{
				screeningRonde.getFitRegistraties().remove(gekoppeldeExtraFitRegistratie);
				laatsteScreeningRonde.getFitRegistraties().add(gekoppeldeExtraFitRegistratie);
				gekoppeldeExtraFitRegistratie.setScreeningRonde(laatsteScreeningRonde);
				hibernateService.saveOrUpdate(gekoppeldeExtraFitRegistratie);
			}
			laatsteScreeningRonde.setLaatsteFitRegistratie(fitRegistratieMetUitslag);
			laatsteScreeningRonde.setLaatsteExtraFitRegistratie(gekoppeldeExtraFitRegistratie);
			ColonFitRegistratie nieuweLaatstefitRegistratieGeslotenRonde = null;
			for (var fitRegistratie : screeningRonde.getFitRegistraties().stream().filter(i -> i.getType().equals(ColonFitType.GOLD)).collect(Collectors.toList()))
			{
				if (nieuweLaatstefitRegistratieGeslotenRonde == null
					|| DateUtil.compareBefore(nieuweLaatstefitRegistratieGeslotenRonde.getUitnodiging().getCreatieDatum(), fitRegistratie.getUitnodiging().getCreatieDatum()))
				{
					nieuweLaatstefitRegistratieGeslotenRonde = fitRegistratie;
				}
			}
			screeningRonde.setLaatsteFitRegistratie(nieuweLaatstefitRegistratieGeslotenRonde);
			screeningRonde.setLaatsteExtraFitRegistratie(null);
			hibernateService.saveOrUpdate(screeningRonde);
			screeningRonde = laatsteScreeningRonde;

		}
		return screeningRonde;
	}

	private void saveOrUpdateFitRegistratie(ColonUitnodiging uitnodiging)
	{
		var fitRegistratie = uitnodiging.getGekoppeldeFitRegistratie();
		if (fitRegistratie != null)
		{
			hibernateService.saveOrUpdate(fitRegistratie);
		}
		var studieRegistratie = uitnodiging.getGekoppeldeExtraFitRegistratie();
		if (studieRegistratie != null)
		{
			hibernateService.saveOrUpdate(studieRegistratie);
		}
	}

	private void uitslagVerwijderen(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie != null)
		{
			fitRegistratie.setGeinterpreteerdeUitslag(null);
			fitRegistratie.setNormWaarde(null);
			fitRegistratie.setUitslag(null);
		}
	}

	@Override
	public void setStatus(ColonFitRegistratie fitRegistratie, ColonFitRegistratieStatus nieuweStatus)
	{
		var newStatusDatumTijd = currentDateSupplier.getDate();
		setStatusEnDatum(fitRegistratie, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, ColonFitRegistratieStatus nieuweStatus)
	{
		var newStatusDatumTijd = currentDateSupplier.getDate();
		setStatus(uitnodiging, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, ColonFitRegistratieStatus nieuweStatus, Date newStatusDatumTijd)
	{
		setStatusEnDatum(uitnodiging.getGekoppeldeFitRegistratie(), nieuweStatus, newStatusDatumTijd);
		if (nieuweStatus.equals(ColonFitRegistratieStatus.VERLOREN) || nieuweStatus.equals(ColonFitRegistratieStatus.VERWIJDERD))
		{
			var extra = uitnodiging.getGekoppeldeExtraFitRegistratie();
			if (extra != null)
			{
				setStatusEnDatum(extra, nieuweStatus, newStatusDatumTijd);
			}
		}
	}

	private void setStatusEnDatum(ColonFitRegistratie fitRegistratie, ColonFitRegistratieStatus nieuweStatus, Date newStatusDatumTijd)
	{
		if (fitRegistratie != null && (ColonFitRegistratieStatus.ACTIEF.equals(nieuweStatus) && fitRegistratie.getStatus() == null ||
			ColonFitRegistratieStatus.VERLOREN.equals(nieuweStatus) && fitRegistratie.getStatus() == null
			|| fitRegistratie.getStatus().magWijzigenNaarStatus(nieuweStatus, fitRegistratie)))
		{
			logStatusChange(fitRegistratie, nieuweStatus);

			fitRegistratie.setStatus(nieuweStatus);
			fitRegistratie.setStatusDatum(newStatusDatumTijd);
		}
	}

	private static void logStatusChange(ColonFitRegistratie fitRegistratie, ColonFitRegistratieStatus nieuweStatus)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("Status fitRegistratie (id: '{}'): {} -> {}", fitRegistratie.getId(), fitRegistratie.getStatus(), nieuweStatus);
		}
	}

	@Override
	public void checkVervaldatumVerlopen(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie != null && ColonFitRegistratieUtil.isGunstig(fitRegistratie) && fitRegistratie.getStatus() != ColonFitRegistratieStatus.VERVALDATUMVERLOPEN
			&& !houdbaarheidService.isFitHoudbaar(
			fitRegistratie.getBarcode()))
		{
			var nu = currentDateSupplier.getDate();
			setStatusEnDatum(fitRegistratie, ColonFitRegistratieStatus.VERVALDATUMVERLOPEN, nu);
		}
	}

	@Override
	public void heraanmelden(ColonScreeningRonde screeningRonde)
	{

		var dossier = screeningRonde.getDossier();
		var afmelding = AfmeldingUtil.getLaatsteAfmelding(screeningRonde, dossier);
		if (afmelding != null)
		{
			afmelding.setHeraanmeldingAfspraakBriefTegenhouden(false);
			afmelding.setHeraanmeldingBevestigingsBriefTegenhouden(true);
			afmelding.setHeraanmeldingAfspraakUitRooster(true);
			afmelding.setClientWilNieuweUitnodiging(false);
			baseAfmeldService.heraanmelden(afmelding, null);
		}
		if (ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
		{
			screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			screeningRonde.setStatusDatum(currentDateSupplier.getDate());
			screeningRonde.setAfgerondReden(null);
			hibernateService.saveOrUpdate(screeningRonde);
		}
	}

	@Override
	@Transactional
	public void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging)
	{
		if (LOG.isTraceEnabled())
		{
			var logMessage = uitnodiging.getUitnodigingsId() + "(";
			if (uitnodiging.getGekoppeldeFitRegistratie() != null)
			{
				logMessage += uitnodiging.getGekoppeldeFitRegistratie().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraFitRegistratie() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraFitRegistratie().getBarcode();
			}
			LOG.trace("verwijderScannedAntwoordFormulier {})", logMessage);
		}
		var antwoordFormulier = uitnodiging.getAntwoordFormulier();
		var fitRegistratie = ColonFitRegistratieUtil.getFitRegistratie(uitnodiging);

		hibernateService.saveOrUpdate(fitRegistratie);

		antwoordFormulier.setStatus(ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER);
		hibernateService.saveOrUpdate(antwoordFormulier);
	}

	@Override
	@Transactional
	public void verwijderFitAnalyseResultaat(ColonFitRegistratie fitRegistratie, UploadDocument uploadDocument)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("verwijder uitslag FIT registratie (id: '{}')", fitRegistratie.getId());
		}
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(fitRegistratie);
		var ronde = fitRegistratie.getScreeningRonde();
		var fobGold = uitnodiging.getGekoppeldeFitRegistratie();
		var extra = uitnodiging.getGekoppeldeExtraFitRegistratie();
		var datum = currentDateSupplier.getDate();
		if (fobGold != null && fobGold.getUitslag() != null)
		{
			uitslagVerwijderen(fobGold);
			setStatusEnDatum(fobGold, ColonFitRegistratieStatus.VERWIJDERD, datum);
			fobGold.setVerwijderbrief(uploadDocument);
		}
		else
		{
			setStatusEnDatum(fobGold, ColonFitRegistratieStatus.ACTIEF, datum);
		}

		if (extra != null)
		{
			if (extra.getUitslag() != null)
			{
				setStatusEnDatum(extra, ColonFitRegistratieStatus.VERWIJDERD, datum);
			}
			else if (extra.getType().equals(ColonFitType.STUDIE))
			{
				uitslagVerwijderen(extra);
				setStatusEnDatum(extra, ColonFitRegistratieStatus.VERWIJDERD, datum);
			}
			else
			{
				setStatusEnDatum(extra, ColonFitRegistratieStatus.ACTIEF, datum);
			}
			extra.setVerwijderbrief(uploadDocument);
		}
		saveOrUpdateFitRegistratie(uitnodiging);
		setVolgendeUitnodigingdatumAlsGunstig(ronde);
	}

	private void setVolgendeUitnodigingdatumAlsGunstig(ColonScreeningRonde ronde)
	{
		if (ColonScreeningRondeUtil.zijnErOngunstigeFitRegistraties(ronde))
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
		}
		else if (ColonScreeningRondeUtil.getEersteGunstigeFitRegistratie(ronde) != null)
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GUNSTIGE_UITSLAG);
		}
		else
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.UITNODIGING_ONTVANGEN);
		}
	}

	@Override
	@Transactional
	public void markeerRegistratieAlsVerloren(ColonUitnodiging uitnodiging)
	{
		if (LOG.isTraceEnabled())
		{
			var logMessage = uitnodiging.getUitnodigingsId() + "(";
			if (uitnodiging.getGekoppeldeFitRegistratie() != null)
			{
				logMessage += uitnodiging.getGekoppeldeFitRegistratie().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraFitRegistratie() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraFitRegistratie().getBarcode();
			}
			LOG.trace("nieuwefitRegistratieAanvragen {})", logMessage);
		}
		setStatus(uitnodiging, ColonFitRegistratieStatus.VERLOREN);
		studieRegistratieService.projectClientInactiverenBijVergelijkendOnderzoek(uitnodiging.getScreeningRonde());
		saveOrUpdateFitRegistratie(uitnodiging);
	}

	@Override
	@Transactional
	public void monsterNietBeoordeelbaar(ColonFitRegistratie fitRegistratie)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("monsterNietBeoordeelbaar id: '{}'", fitRegistratie.getId());
		}

		setStatusEnDatum(fitRegistratie, ColonFitRegistratieStatus.NIETTEBEOORDELEN, currentDateSupplier.getDate());

		studieRegistratieService.projectClientInactiverenBijVergelijkendOnderzoek(fitRegistratie.getScreeningRonde());

		vervolgVerwerkingUitslag(fitRegistratie);
	}

	@Override
	public void bepaalEnSetHeraanmeldenTekstKey(ColonFitRegistratie fitRegistratie)
	{
		var screeningRonde = fitRegistratie.getScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var colonAfmelding = AfmeldingUtil.getLaatsteAfmelding(screeningRonde, dossier);
		PreferenceKey heraanmeldenTekstKey = null;

		if (AfmeldingUtil.isAfgerondeDefinitieveAfmelding(colonAfmelding))
		{
			heraanmeldenTekstKey = PreferenceKey.COLON_DEFINITIEF_HERAANMELDEN_TEKST;
		}
		else if (AfmeldingUtil.isAfgerondeEenmaligeAfmelding(colonAfmelding))
		{
			heraanmeldenTekstKey = PreferenceKey.COLON_EENMALIG_HERAANMELDEN_TEKST;
		}
		else if (AfmeldingUtil.isAfgerondeTijdelijkeAfmelding(colonAfmelding))
		{
			heraanmeldenTekstKey = PreferenceKey.COLON_TIJDELIJK_HERAANMELDEN_TEKST;
		}
		fitRegistratie.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
	}

	@Override
	public void setFitRegistratiesVerlorenIndienActief(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie != null && !ColonFitRegistratieStatus.isUnmutableEindStatus(fitRegistratie.getStatus()) && !ColonFitRegistratieStatus.isMutableEindStatus(
			fitRegistratie.getStatus()))
		{
			var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(fitRegistratie);
			setStatus(uitnodiging, ColonFitRegistratieStatus.VERLOREN);
			saveOrUpdateFitRegistratie(uitnodiging);
		}
	}

	private BigDecimal getFitNormWaarde(ProjectClient projectClient)
	{
		if (projectClient != null && !projectClient.getActief()
			|| ProjectUtil.hasParameterSet(projectClient, ProjectParameterKey.COLON_FIT_NORM_WAARDE))
		{
			return new BigDecimal(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_FIT_NORM_WAARDE));
		}
		else
		{
			return BigDecimal.valueOf(simplePreferenceService.getInteger(PreferenceKey.COLON_FIT_NORM_WAARDE.name())).divide(BigDecimal.valueOf(100));
		}
	}

	@Override
	public Client getAndereClientOpZelfdeAdresEnActieveFit(Client client, List<Long> uitgenodigdeClientIds)
	{
		var uitnodigingsInterval = simplePreferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}
		var minimaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		if (minimaleLeeftijd == null)
		{
			throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
		}

		var maximaleLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}
		var wachttijdVerzendenPakket = simplePreferenceService.getInteger(PreferenceKey.WACHTTIJD_VERZENDEN_PAKKET_TWEE_OP_EEN_ADRES.name());
		if (wachttijdVerzendenPakket == null)
		{
			throw new IllegalStateException("Wachttijd verzenden pakket bij 2 op 1 adres op de parameterisatie pagina is niet gezet");
		}

		var clientenOpAdres = clientService.getClientenOpAdresMetLimiet(client.getPersoon().getGbaAdres(), minimaleLeeftijd,
			maximaleLeeftijd, uitnodigingsInterval);
		var andereClient = getAndereClient(clientenOpAdres, client);

		if (clientenOpAdres.size() == 2
			&& isFitRegistratieActief(andereClient, uitgenodigdeClientIds)
			&& !isWachttijdOpPakketVerstreken(andereClient, wachttijdVerzendenPakket, uitgenodigdeClientIds,
			currentDateSupplier.getLocalDate()))
		{
			return andereClient;
		}
		return null;
	}

	private Client getAndereClient(List<Client> clientenOpAdres, Client item)
	{
		return clientenOpAdres.stream().filter(client -> !client.getId().equals(item.getId())).findFirst().orElse(null);
	}

	private boolean isFitRegistratieActief(Client andereClient, List<Long> uitgenodigdeClientIds)
	{
		if (andereClient == null)
		{
			return false;
		}

		if (uitgenodigdeClientIds.contains(andereClient.getId()))
		{
			return true;
		}

		if (andereClient.getColonDossier().getLaatsteScreeningRonde() == null
			|| andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() == null)
		{
			return false;
		}

		ColonFitRegistratie fitRegistratie = andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteFitRegistratie();
		if (fitRegistratie == null)
		{
			return true;
		}

		return fitRegistratie.getStatus() == ColonFitRegistratieStatus.ACTIEF;
	}

	private boolean isWachttijdOpPakketVerstreken(Client andereClient, Integer wachttijdVerzendenPakket, List<Long> uitgenodigdeClientIds, LocalDate vandaag)
	{
		if (andereClient.getColonDossier().getLaatsteScreeningRonde() != null
			&& andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			&& andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum() != null)
		{

			if (uitgenodigdeClientIds.contains(andereClient.getId()))
			{
				return false;
			}

			LocalDate createDatumUitnodiging = DateUtil.toLocalDate(andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum());
			createDatumUitnodiging = createDatumUitnodiging.plusDays(wachttijdVerzendenPakket);
			return !createDatumUitnodiging.isAfter(vandaag);
		}
		return false;
	}

	@Override
	public Optional<ColonFitRegistratie> getFit(String barcode)
	{
		return fitRegistratieRepository.findByBarcode(barcode);
	}

	@Override
	public Optional<ColonFitRegistratie> getLaatsteFitMetMissendeUitslagVanDossier(ColonDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		var specification = valideerFitUitslagStatus(signalerenVanaf)
			.and(heeftDossier(dossier))
			.and(heeftStatusDatumVoorOfOp(minimaleSignaleringsDatum.atStartOfDay()))
			.and(heeftFitType(ColonFitType.GOLD))
			.and(heeftActieveClient());
		return fitRegistratieRepository.findFirst(specification, Sort.by(Sort.Direction.DESC, ColonFitRegistratie_.ANALYSE_DATUM));
	}

	@Override
	public Optional<ColonFitAnalyseResultaatSet> getFitAnalyseResultaatSet(String bestandsnaam)
	{
		return fitAnalyseResultaatSetRepository.findOne(
			heeftBestandsnaam(bestandsnaam).and(
				heeftStatussen(Arrays.asList(ColonFitAnalyseResultaatSetStatus.NIEUW, ColonFitAnalyseResultaatSetStatus.KAN_ORIG_BESTAND_VERWIJDEREN))));
	}

	@Override
	public boolean isVerwijderdeBarcode(String barcode)
	{
		var reader = AuditReaderFactory.get(sessionFactory.getCurrentSession());
		var auditQuery = reader.createQuery().forRevisionsOfEntity(ColonFitRegistratie.class, false, true)
			.add(AuditEntity.property("barcode").eq(barcode))
			.addProjection(AuditEntity.id().count());

		return ((Long) auditQuery.getSingleResult()) > 0;
	}

	@Override
	public String getToonbareWaarde(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie == null)
		{
			return null;
		}

		var betrouwbareLimiet = simplePreferenceService.getLong(PreferenceKey.COLON_BETROUWBARE_LIMIET_FIT.name(), 80L);
		var waardeHogerDanLimiet = fitRegistratie.getUitslag() != null && fitRegistratie.getUitslag().compareTo(BigDecimal.valueOf(betrouwbareLimiet)) >= 0;
		var heeftProFlag = UITSLAG_FLAG_PRO.equals(fitRegistratie.getFlag());
		if (isDk2026Actief() && (waardeHogerDanLimiet || heeftProFlag))
		{
			return "> " + betrouwbareLimiet;
		}

		return fitRegistratie.getUitslag() != null ? fitRegistratie.getUitslag().toString() : "";
	}

	@Override
	public boolean isDk2026Actief()
	{
		var peilDatum = currentDateSupplier.getLocalDate();
		var startDk2026 = simplePreferenceService.getString(PreferenceKey.COLON_START_DK2026.name(), "20260101");
		return !peilDatum.isBefore(DateUtil.parseLocalDateForPattern(startDk2026, Constants.DATE_FORMAT_YYYYMMDD));
	}

	@Override
	public void koppelTestIndienMogelijk(String fitBarcode, ColonFitType fitType, ColonUitnodiging uitnodiging, Date datumVerstuurd, ColonScreeningRonde screeningRonde)
	{
		if (StringUtils.isBlank(fitBarcode))
		{
			return;
		}

		ColonFitRegistratie fitRegistratie;
		if (fitType.equals(ColonFitType.GOLD))
		{
			fitRegistratie = uitnodiging.getGekoppeldeFitRegistratie();
		}
		else
		{
			fitRegistratie = uitnodiging.getGekoppeldeExtraFitRegistratie();
		}

		if (fitRegistratie != null && !fitBarcode.equals(fitRegistratie.getBarcode()))
		{
			fitRegistratie = null;
		}
		if (fitRegistratie == null)
		{
			fitRegistratie = new ColonFitRegistratie();
			fitRegistratie.setType(fitType);
			fitRegistratie.setBarcode(fitBarcode);
		}
		fitRegistratie.setScreeningRonde(screeningRonde);
		fitRegistratie.setUitnodiging(uitnodiging);
		fitRegistratie.setDatumVerstuurd(datumVerstuurd);

		if (fitRegistratie.getStatus() == null)
		{
			if (uitnodiging.equals(screeningRonde.getLaatsteUitnodiging()))
			{
				setStatus(fitRegistratie, ColonFitRegistratieStatus.ACTIEF);
				if (fitType.equals(ColonFitType.GOLD))
				{
					screeningRonde.setLaatsteFitRegistratie(fitRegistratie);
				}
				else
				{
					screeningRonde.setLaatsteExtraFitRegistratie(fitRegistratie);
				}
			}
			else
			{
				setStatus(fitRegistratie, ColonFitRegistratieStatus.VERLOREN);
			}
			if (fitType.equals(ColonFitType.GOLD))
			{
				screeningRonde.setLaatsteExtraFitRegistratie(null);
			}
		}

		if (fitType.equals(ColonFitType.GOLD))
		{
			uitnodiging.setGekoppeldeFitRegistratie(fitRegistratie);
		}
		else
		{
			uitnodiging.setGekoppeldeExtraFitRegistratie(fitRegistratie);
		}
		fitRegistratieRepository.save(fitRegistratie);
	}
}
