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
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.colon.ColonFITBestandRepository;
import nl.rivm.screenit.repository.colon.ColonFITRepository;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.ProjectUtil;
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

import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.heeftBestandsnaam;
import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.heeftStatussen;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftDossier;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftFitType;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.heeftStatusDatumVoorOfOp;
import static nl.rivm.screenit.specification.colon.ColonFITSpecification.valideerFitUitslagStatus;
import static nl.rivm.screenit.util.FITTestUtil.UITSLAG_FLAG_PRO;

@Slf4j
@Service
public class ColonBaseFITServiceImpl implements ColonBaseFITService
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
	private ColonStudietestService studietestService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ColonFITRepository fitRepository;

	@Autowired
	private ColonFITBestandRepository fitBestandRepository;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

	private void rondeSluitenIndienMogelijk(LocalDateTime nu, ColonScreeningRonde ronde)
	{
		var allesAfgerondEnGunstig = true;
		var isUitslagBriefVerstuurd = false;
		for (var fit : ronde.getIfobtTesten())
		{
			if (IFOBTTestStatus.ACTIEF.equals(fit.getStatus()) || FITTestUtil.isOngunstig(fit))
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

	private void setNormWaarde(IFOBTTest buis, BigDecimal normWaarde)
	{
		if (buis.getStatus() != IFOBTTestStatus.NIETTEBEOORDELEN)
		{
			buis.setNormWaarde(normWaarde);
		}
		if (buis.getStatus() != IFOBTTestStatus.VERWIJDERD)
		{
			checkVervaldatumVerlopen(buis);
		}
	}

	@Override
	@Transactional
	public void verwerkAnalyseResultaat(IFOBTTest teVerwerkenFit)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("verwerkAnalyseResultaat {}", teVerwerkenFit.getBarcode());
		}

		var nu = currentDateSupplier.getLocalDateTime();
		var screeningRonde = teVerwerkenFit.getColonScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var projectClient = ProjectUtil.getHuidigeProjectClient(dossier.getClient(), currentDateSupplier.getDate());
		var normWaardeGold = getFitNormWaarde(projectClient);
		var uitnodiging = FITTestUtil.getUitnodiging(teVerwerkenFit);

		if (!teVerwerkenFit.getColonScreeningRonde().equals(dossier.getLaatsteScreeningRonde()))
		{
			studietestService.projectClientInactiverenBijVergelijkendOnderzoek(teVerwerkenFit.getColonScreeningRonde());
		}
		uitnodigingService.berekenEnSetUitgesteldeUitslagDatum(uitnodiging);

		setNormWaarde(teVerwerkenFit, normWaardeGold);
		var isNietVerlopen = teVerwerkenFit.getStatus() != IFOBTTestStatus.VERVALDATUMVERLOPEN;
		var isOngunstigOfNietVerwijderd = FITTestUtil.isOngunstig(teVerwerkenFit) || teVerwerkenFit.getStatus() != IFOBTTestStatus.VERWIJDERD;
		if (isNietVerlopen && isOngunstigOfNietVerwijderd)
		{
			setStatus(uitnodiging, IFOBTTestStatus.UITGEVOERD);
		}

		vervolgVerwerkingUitslag(teVerwerkenFit);

		if (isNietVerlopen)
		{
			rondeSluitenIndienMogelijk(nu, dossier.getLaatsteScreeningRonde());
			saveOrUpdateBuis(uitnodiging);
		}
	}

	private void vervolgVerwerkingUitslag(IFOBTTest teVerwerkenFit)
	{
		var screeningRonde = fitNaarLaatsteRondeIndienNodig(teVerwerkenFit);
		if (IFOBTTestStatus.isMislukteAnalyse(teVerwerkenFit.getStatus()))
		{
			studietestService.projectClientInactiverenBijVergelijkendOnderzoek(screeningRonde);
		}
		bepaalEnSetHeraanmeldenTekstKey(teVerwerkenFit);
		heraanmelden(screeningRonde);
		maakBuitenDoelgroepIndienNodig(teVerwerkenFit);
		verwijderNogNietVerstuurdeUitnodigingIndienNodig(teVerwerkenFit);
		maakExtraMonsterBriefIndienNodig(teVerwerkenFit);
		maakNieuweUitnodigingNavMislukteAnalyseIndienNodig(teVerwerkenFit);
	}

	private void verwijderNogNietVerstuurdeUitnodigingIndienNodig(IFOBTTest teVerwerkenFit)
	{
		var screeningRonde = teVerwerkenFit.getColonScreeningRonde();
		boolean teVerwerkenFitHeeftSuccesvolleAnalyse = FITTestUtil.heeftSuccesvolleAnalyse(teVerwerkenFit);
		var rondeHeeftFitMetMislukteAnalyse = screeningRonde.getIfobtTesten()
			.stream()
			.filter(fit -> !fit.equals(teVerwerkenFit))
			.anyMatch(fit -> IFOBTTestStatus.isMislukteAnalyse(fit.getStatus()));
		if (laatsteUitnodigingNietVerstuurd(screeningRonde) && (rondeHeeftFitMetMislukteAnalyse || teVerwerkenFitHeeftSuccesvolleAnalyse))
		{
			verwijderLaatsteUitnodiging(screeningRonde);
		}
	}

	private void maakExtraMonsterBriefIndienNodig(IFOBTTest teVerwerkenFit)
	{

		var ronde = teVerwerkenFit.getColonScreeningRonde();
		var heeftRondeSuccesvolleFitMetBrief = ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_SUCCESVOLLE_ANALYSE_BRIEVEN.contains(brief.getBriefType()));
		var heeftRondeGunstigeUitslagMetBrief = ronde.getBrieven().stream().anyMatch(brief -> brief.getBriefType() == BriefType.COLON_GUNSTIGE_UITSLAG);
		var mislukteAnalyseEnUitnodigingNietVerstuurd =
			IFOBTTestStatus.isMislukteAnalyse(teVerwerkenFit.getStatus()) && laatsteUitnodigingNietVerstuurd(ronde);
		var heeftExtraMonsterBrief = ronde.getBrieven().stream().anyMatch(brief -> !brief.isGegenereerd() && brief.getBriefType() == BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
		var heeftGeenFitMetZelfdeStatusdatum = ronde.getIfobtTesten().stream()
			.filter(fit -> !fit.equals(teVerwerkenFit))
			.map(fit -> DateUtil.toLocalDate(fit.getStatusDatum()))
			.noneMatch(verwerkingsdatum -> DateUtil.toLocalDate(teVerwerkenFit.getStatusDatum()).equals(verwerkingsdatum));
		var isBuitenDoelgroep = screeningsrondeService.isRondeStatusBuitenDoelgroep(ronde);
		var rondeHeeftAlGunstigeUitslag = !(FITTestUtil.isOngunstig(teVerwerkenFit) && heeftRondeGunstigeUitslagMetBrief);
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

	private void maakNieuweUitnodigingNavMislukteAnalyseIndienNodig(IFOBTTest teVerwerkenFit)
	{

		var ronde = teVerwerkenFit.getColonScreeningRonde();
		if (!FITTestUtil.heeftSuccesvolleAnalyse(ronde))
		{
			var nietTeBeoordelen = teVerwerkenFit.getStatus() == IFOBTTestStatus.NIETTEBEOORDELEN;
			var categorie = nietTeBeoordelen ? ColonUitnodigingCategorie.U3 : ColonUitnodigingCategorie.U6;
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
		if (laatsteUitnodiging == null || laatsteUitnodiging.getGekoppeldeTest() != null || laatsteUitnodiging.getGekoppeldeExtraTest() != null)
		{
			return;
		}

		ronde.getUitnodigingen().remove(laatsteUitnodiging);
		uitnodigingRepository.delete(laatsteUitnodiging);

		ronde.setLaatsteUitnodiging(ronde.getUitnodigingen().stream().max(Comparator.comparing(ColonUitnodiging::getUitnodigingsId)).orElse(null));
	}

	private void maakBuitenDoelgroepIndienNodig(IFOBTTest fitVoorBrief)
	{
		var ronde = fitVoorBrief.getColonScreeningRonde();
		if (screeningsrondeService.isRondeStatusBuitenDoelgroep(ronde))
		{
			if (FITTestUtil.isOngunstig(fitVoorBrief))
			{
				maakBriefEnKoppelAanTest(ronde, fitVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP);
			}
			else if (IFOBTTestStatus.isMislukteAnalyse(fitVoorBrief.getStatus()))
			{
				maakBriefEnKoppelAanTest(ronde, fitVoorBrief, BriefType.COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP);
			}
		}
	}

	private void maakBriefEnKoppelAanTest(ColonScreeningRonde ronde, IFOBTTest fitVoorBrief, BriefType briefType)
	{
		var brief = briefService.maakBvoBrief(ronde, briefType);
		brief.setIfobtTest(fitVoorBrief);
		hibernateService.saveOrUpdate(brief);
	}

	private ColonScreeningRonde fitNaarLaatsteRondeIndienNodig(IFOBTTest buisMetUitslag)
	{
		var uitnodiging = buisMetUitslag.getColonUitnodiging();
		var screeningRonde = uitnodiging.getScreeningRonde();
		var laatsteScreeningRonde = screeningRonde.getDossier().getLaatsteScreeningRonde();
		if (!screeningRonde.equals(laatsteScreeningRonde))
		{

			screeningRonde.getIfobtTesten().remove(buisMetUitslag);
			laatsteScreeningRonde.getIfobtTesten().add(buisMetUitslag);
			buisMetUitslag.setColonScreeningRonde(laatsteScreeningRonde);
			var gekoppeldeExtraTest = uitnodiging.getGekoppeldeExtraTest();
			if (gekoppeldeExtraTest != null)
			{
				screeningRonde.getIfobtTesten().remove(gekoppeldeExtraTest);
				laatsteScreeningRonde.getIfobtTesten().add(gekoppeldeExtraTest);
				gekoppeldeExtraTest.setColonScreeningRonde(laatsteScreeningRonde);
				hibernateService.saveOrUpdate(gekoppeldeExtraTest);
			}
			laatsteScreeningRonde.setLaatsteIFOBTTest(buisMetUitslag);
			laatsteScreeningRonde.setLaatsteIFOBTTestExtra(gekoppeldeExtraTest);
			IFOBTTest nieuweLaatsteBuisGeslotenRonde = null;
			for (var test : screeningRonde.getIfobtTesten().stream().filter(i -> i.getType().equals(IFOBTType.GOLD)).collect(Collectors.toList()))
			{
				if (nieuweLaatsteBuisGeslotenRonde == null
					|| DateUtil.compareBefore(nieuweLaatsteBuisGeslotenRonde.getColonUitnodiging().getCreatieDatum(), test.getColonUitnodiging().getCreatieDatum()))
				{
					nieuweLaatsteBuisGeslotenRonde = test;
				}
			}
			screeningRonde.setLaatsteIFOBTTest(nieuweLaatsteBuisGeslotenRonde);
			screeningRonde.setLaatsteIFOBTTestExtra(null);
			hibernateService.saveOrUpdate(screeningRonde);
			screeningRonde = laatsteScreeningRonde;

		}
		return screeningRonde;
	}

	private void saveOrUpdateBuis(ColonUitnodiging uitnodiging)
	{
		var fobGold = uitnodiging.getGekoppeldeTest();
		if (fobGold != null)
		{
			hibernateService.saveOrUpdate(fobGold);
		}
		var studietest = uitnodiging.getGekoppeldeExtraTest();
		if (studietest != null)
		{
			hibernateService.saveOrUpdate(studietest);
		}
	}

	private void uitslagVerwijderen(IFOBTTest buis)
	{
		if (buis != null)
		{
			buis.setGeinterpreteerdeUitslag(null);
			buis.setNormWaarde(null);
			buis.setUitslag(null);
		}
	}

	@Override
	public void setStatus(IFOBTTest buis, IFOBTTestStatus nieuweStatus)
	{
		var newStatusDatumTijd = currentDateSupplier.getDate();
		setStatusEnDatum(buis, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, IFOBTTestStatus nieuweStatus)
	{
		var newStatusDatumTijd = currentDateSupplier.getDate();
		setStatus(uitnodiging, nieuweStatus, newStatusDatumTijd);
	}

	private void setStatus(ColonUitnodiging uitnodiging, IFOBTTestStatus nieuweStatus, Date newStatusDatumTijd)
	{
		setStatusEnDatum(uitnodiging.getGekoppeldeTest(), nieuweStatus, newStatusDatumTijd);
		if (nieuweStatus.equals(IFOBTTestStatus.VERLOREN) || nieuweStatus.equals(IFOBTTestStatus.VERWIJDERD))
		{
			var extra = uitnodiging.getGekoppeldeExtraTest();
			if (extra != null)
			{
				setStatusEnDatum(extra, nieuweStatus, newStatusDatumTijd);
			}
		}
	}

	private void setStatusEnDatum(IFOBTTest buis, IFOBTTestStatus nieuweStatus, Date newStatusDatumTijd)
	{
		if (buis != null && (IFOBTTestStatus.ACTIEF.equals(nieuweStatus) && buis.getStatus() == null ||
			IFOBTTestStatus.VERLOREN.equals(nieuweStatus) && buis.getStatus() == null
			|| buis.getStatus().magWijzigenNaarStatus(nieuweStatus, buis)))
		{
			logStatusChange(buis, nieuweStatus);

			buis.setStatus(nieuweStatus);
			buis.setStatusDatum(newStatusDatumTijd);
		}
	}

	private static void logStatusChange(IFOBTTest buis, IFOBTTestStatus nieuweStatus)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("Status FIT (id: '{}'): {} -> {}", buis.getId(), buis.getStatus(), nieuweStatus);
		}
	}

	@Override
	public void checkVervaldatumVerlopen(IFOBTTest buis)
	{
		if (buis != null && FITTestUtil.isGunstig(buis) && buis.getStatus() != IFOBTTestStatus.VERVALDATUMVERLOPEN && !houdbaarheidService.isFitHoudbaar(buis.getBarcode()))
		{
			var nu = currentDateSupplier.getDate();
			setStatusEnDatum(buis, IFOBTTestStatus.VERVALDATUMVERLOPEN, nu);
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
			if (uitnodiging.getGekoppeldeTest() != null)
			{
				logMessage += uitnodiging.getGekoppeldeTest().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraTest() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraTest().getBarcode();
			}
			LOG.trace("verwijderScannedAntwoordFormulier {})", logMessage);
		}
		var antwoordFormulier = uitnodiging.getAntwoordFormulier();
		var buis = FITTestUtil.getFITTest(uitnodiging);

		hibernateService.saveOrUpdate(buis);

		antwoordFormulier.setStatus(ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER);
		hibernateService.saveOrUpdate(antwoordFormulier);
	}

	@Override
	@Transactional
	public void verwijderUitslag(IFOBTTest buis, UploadDocument uploadDocument)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("verwijder uitslag FIT (id: '{}')", buis.getId());
		}
		var uitnodiging = FITTestUtil.getUitnodiging(buis);
		var ronde = buis.getColonScreeningRonde();
		var fobGold = uitnodiging.getGekoppeldeTest();
		var extra = uitnodiging.getGekoppeldeExtraTest();
		var datum = currentDateSupplier.getDate();
		if (fobGold != null && fobGold.getUitslag() != null)
		{
			uitslagVerwijderen(fobGold);
			setStatusEnDatum(fobGold, IFOBTTestStatus.VERWIJDERD, datum);
			fobGold.setVerwijderbrief(uploadDocument);
		}
		else
		{
			setStatusEnDatum(fobGold, IFOBTTestStatus.ACTIEF, datum);
		}

		if (extra != null)
		{
			if (extra.getUitslag() != null)
			{
				setStatusEnDatum(extra, IFOBTTestStatus.VERWIJDERD, datum);
			}
			else if (extra.getType().equals(IFOBTType.STUDIE))
			{
				uitslagVerwijderen(extra);
				setStatusEnDatum(extra, IFOBTTestStatus.VERWIJDERD, datum);
			}
			else
			{
				setStatusEnDatum(extra, IFOBTTestStatus.ACTIEF, datum);
			}
			extra.setVerwijderbrief(uploadDocument);
		}
		saveOrUpdateBuis(uitnodiging);
		setVolgendeUitnodigingdatumAlsGunstig(ronde);
	}

	private void setVolgendeUitnodigingdatumAlsGunstig(ColonScreeningRonde ronde)
	{
		if (ColonScreeningRondeUtil.zijnErOngunstigeIfobts(ronde))
		{
			dossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
		}
		else if (ColonScreeningRondeUtil.getEersteGunstigeTest(ronde) != null)
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
	public void markeerBuisAlsVerloren(ColonUitnodiging uitnodiging)
	{
		if (LOG.isTraceEnabled())
		{
			var logMessage = uitnodiging.getUitnodigingsId() + "(";
			if (uitnodiging.getGekoppeldeTest() != null)
			{
				logMessage += uitnodiging.getGekoppeldeTest().getBarcode();
			}
			if (uitnodiging.getGekoppeldeExtraTest() != null)
			{
				logMessage += "/" + uitnodiging.getGekoppeldeExtraTest().getBarcode();
			}
			LOG.trace("nieuweBuisAanvragen {})", logMessage);
		}
		setStatus(uitnodiging, IFOBTTestStatus.VERLOREN);
		studietestService.projectClientInactiverenBijVergelijkendOnderzoek(uitnodiging.getScreeningRonde());
		saveOrUpdateBuis(uitnodiging);
	}

	@Override
	@Transactional
	public void monsterNietBeoordeelbaar(IFOBTTest fit)
	{
		if (LOG.isTraceEnabled())
		{
			LOG.trace("monsterNietBeoordeelbaar id: '{}'", fit.getId());
		}

		setStatusEnDatum(fit, IFOBTTestStatus.NIETTEBEOORDELEN, currentDateSupplier.getDate());

		studietestService.projectClientInactiverenBijVergelijkendOnderzoek(fit.getColonScreeningRonde());

		vervolgVerwerkingUitslag(fit);
	}

	@Override
	public void bepaalEnSetHeraanmeldenTekstKey(IFOBTTest fit)
	{
		var screeningRonde = fit.getColonScreeningRonde();
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
		fit.setHeraanmeldenTekstKey(heraanmeldenTekstKey);
	}

	@Override
	public void setTestenVerlorenIndienActief(IFOBTTest test)
	{
		if (test != null && !IFOBTTestStatus.isUnmutableEindStatus(test.getStatus()) && !IFOBTTestStatus.isMutableEindStatus(test.getStatus()))
		{
			var uitnodiging = FITTestUtil.getUitnodiging(test);
			setStatus(uitnodiging, IFOBTTestStatus.VERLOREN);
			saveOrUpdateBuis(uitnodiging);
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
			return BigDecimal.valueOf(simplePreferenceService.getInteger(PreferenceKey.IFOBT_NORM_WAARDE.name())).divide(BigDecimal.valueOf(100));
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
			&& isIfobtActief(andereClient, uitgenodigdeClientIds)
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

	private boolean isIfobtActief(Client andereClient, List<Long> uitgenodigdeClientIds)
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

		IFOBTTest ifobtTest = andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteIFOBTTest();
		if (ifobtTest == null)
		{
			return true;
		}

		return ifobtTest.getStatus() == IFOBTTestStatus.ACTIEF;
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
	public Optional<IFOBTTest> getFit(String barcode)
	{
		return fitRepository.findByBarcode(barcode);
	}

	@Override
	public Optional<IFOBTTest> getLaatsteFitMetMissendeUitslagVanDossier(ColonDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		var specification = valideerFitUitslagStatus(signalerenVanaf)
			.and(heeftDossier(dossier))
			.and(heeftStatusDatumVoorOfOp(minimaleSignaleringsDatum.atStartOfDay()))
			.and(heeftFitType(IFOBTType.GOLD))
			.and(heeftActieveClient());
		return fitRepository.findFirst(specification, Sort.by(Sort.Direction.DESC, IFOBTTest_.ANALYSE_DATUM));
	}

	@Override
	public Optional<IFOBTBestand> getIfobtBestand(String bestandsnaam)
	{
		return fitBestandRepository.findOne(
			heeftBestandsnaam(bestandsnaam).and(heeftStatussen(Arrays.asList(IFOBTBestandStatus.NIEUW, IFOBTBestandStatus.KAN_ORIG_BESTAND_VERWIJDEREN))));
	}

	@Override
	public boolean isVerwijderdeBarcode(String barcode)
	{
		var reader = AuditReaderFactory.get(sessionFactory.getCurrentSession());
		var auditQuery = reader.createQuery().forRevisionsOfEntity(IFOBTTest.class, false, true)
			.add(AuditEntity.property("barcode").eq(barcode))
			.addProjection(AuditEntity.id().count());

		return ((Long) auditQuery.getSingleResult()) > 0;
	}

	@Override
	public String getToonbareWaarde(IFOBTTest fit)
	{
		if (fit == null)
		{
			return null;
		}

		var betrouwbareLimiet = simplePreferenceService.getLong(PreferenceKey.COLON_BETROUWBARE_LIMIET_FIT.name(), 80L);
		var waardeHogerDanLimiet = fit.getUitslag() != null && fit.getUitslag().compareTo(BigDecimal.valueOf(betrouwbareLimiet)) >= 0;
		var heeftProFlag = UITSLAG_FLAG_PRO.equals(fit.getFlag());
		if (isDk2026Actief() && (waardeHogerDanLimiet || heeftProFlag))
		{
			return "> " + betrouwbareLimiet;
		}

		return fit.getUitslag() != null ? fit.getUitslag().toString() : "";
	}

	@Override
	public boolean isDk2026Actief()
	{
		var peilDatum = currentDateSupplier.getLocalDate();
		var startDk2026 = simplePreferenceService.getString(PreferenceKey.COLON_START_DK2026.name(), "20260101");
		return !peilDatum.isBefore(DateUtil.parseLocalDateForPattern(startDk2026, Constants.DATE_FORMAT_YYYYMMDD));
	}

	@Override
	public void koppelTestIndienMogelijk(String fitBarcode, IFOBTType fitType, ColonUitnodiging uitnodiging, Date datumVerstuurd, ColonScreeningRonde screeningRonde)
	{
		if (StringUtils.isBlank(fitBarcode))
		{
			return;
		}

		IFOBTTest fit;
		if (fitType.equals(IFOBTType.GOLD))
		{
			fit = uitnodiging.getGekoppeldeTest();
		}
		else
		{
			fit = uitnodiging.getGekoppeldeExtraTest();
		}

		if (fit != null && !fitBarcode.equals(fit.getBarcode()))
		{
			fit = null;
		}
		if (fit == null)
		{
			fit = new IFOBTTest();
			fit.setType(fitType);
			fit.setBarcode(fitBarcode);
		}
		fit.setColonScreeningRonde(screeningRonde);
		fit.setColonUitnodiging(uitnodiging);
		fit.setDatumVerstuurd(datumVerstuurd);

		if (fit.getStatus() == null)
		{
			if (uitnodiging.equals(screeningRonde.getLaatsteUitnodiging()))
			{
				setStatus(fit, IFOBTTestStatus.ACTIEF);
				if (fitType.equals(IFOBTType.GOLD))
				{
					screeningRonde.setLaatsteIFOBTTest(fit);
				}
				else
				{
					screeningRonde.setLaatsteIFOBTTestExtra(fit);
				}
			}
			else
			{
				setStatus(fit, IFOBTTestStatus.VERLOREN);
			}
			if (fitType.equals(IFOBTType.GOLD))
			{
				screeningRonde.setLaatsteIFOBTTestExtra(null);
			}
		}

		if (fitType.equals(IFOBTType.GOLD))
		{
			uitnodiging.setGekoppeldeTest(fit);
		}
		else
		{
			uitnodiging.setGekoppeldeExtraTest(fit);
		}
		fitRepository.save(fit);
	}
}
