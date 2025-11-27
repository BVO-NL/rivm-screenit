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

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.RetourzendingService;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineTimeService;
import nl.rivm.screenit.main.service.colon.ColonVervolgonderzoekKeuzesDto;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.service.colon.ColonStudieRegistratieService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.rivm.screenit.util.colon.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@RequiredArgsConstructor
@Service
public class ColonTestTimelineServiceImpl implements ColonTestTimelineService
{

	public static final int DEFAULT_TIJD_TUSSEN_VERSTUURD_EN_UITSLAG_ONTVANGEN = 8;

	private final HibernateService hibernateService;

	private final TestService testService;

	private final ColonTestTimelineTimeService testTimelineTimeService;

	private final ColonBaseFitService fitService;

	private final ColonStudieRegistratieService studieRegistratieService;

	private final BaseBriefService briefService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final DossierService dossierService;

	private final SimplePreferenceService preferenceService;

	private final BaseHoudbaarheidService houdbaarheidService;

	private final RetourzendingService retourzendingService;

	private final UitnodigingsDao uitnodigingsDao;

	private final ColonVerwerkVerslagService verwerkVerslagService;

	private final BaseVerslagService verslagService;

	private final ColonDossierBaseService colonDossierBaseService;

	private final ColonDossierService colonDossierService;

	private final OrganisatieParameterService organisatieParameterService;

	private final ColonAfspraakslotService afspraakslotService;

	private final ColonScreeningsrondeService screeningsrondeService;

	@Override
	public List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client)
	{
		var keuzes = new ArrayList<TestVervolgKeuzeOptie>();
		if (client.getColonDossier().getLaatsteScreeningRonde() != null
			&& client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
		{
			for (var ronde : client.getColonDossier().getScreeningRondes())
			{
				if (ronde.getLaatsteUitnodiging() != null)
				{
					keuzeAntwoordFormulierEnFit(keuzes, ronde);
					keuzeIntakeAfspraak(keuzes, ronde);
					keuzeIntakeAfspraakConclusie(keuzes, ronde);
					keuzeHerinneringFit(keuzes, ronde);
					keuzeRetourZending(keuzes, ronde);
				}
			}
			if (client.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getGekoppeldeFitRegistratie() == null)
			{
				keuzes.add(TestVervolgKeuzeOptie.UITNODIGING_POPUP);
			}
		}
		keuzes.add(TestVervolgKeuzeOptie.VERZET_TIJD);
		keuzes.add(TestVervolgKeuzeOptie.EINDE_SCREENINGSRONDE);
		return keuzes;
	}

	private void keuzeHerinneringFit(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.HERINNERING))
		{
			for (var test : ronde.getFitRegistraties())
			{
				if (!test.isHerinnering() && test.getStatus().equals(ColonFitRegistratieStatus.ACTIEF))
				{
					keuzes.add(TestVervolgKeuzeOptie.HERINNERING);
					return;
				}
			}
		}
	}

	private void keuzeAntwoordFormulierEnFit(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.FITREGISTRATIE))
		{
			for (var fitRegistratie : ronde.getFitRegistraties())
			{
				if (fitRegistratie.getUitslag() == null)
				{
					keuzes.add(TestVervolgKeuzeOptie.FITREGISTRATIE);
					return;
				}
			}
		}
	}

	private void keuzeRetourZending(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (!keuzes.contains(TestVervolgKeuzeOptie.RETOURZENDING))
		{
			for (var test : ronde.getFitRegistraties())
			{
				if (test.getType().equals(ColonFitType.GOLD) && retourzendingService.isValideColonUitnodiging(test.getUitnodiging()) == null)
				{
					keuzes.add(TestVervolgKeuzeOptie.RETOURZENDING);
					return;
				}
			}
		}
	}

	private void keuzeIntakeAfspraak(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		if (ColonScreeningRondeUtil.zijnErOngunstigeFitRegistraties(ronde))
		{
			if (ronde.getAfspraken().isEmpty())
			{
				keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK_BUITEN_ROOSTER);
				keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK_BINNEN_ROOSTER);
			}
			keuzes.add(TestVervolgKeuzeOptie.MDL_VERSLAG);
		}
	}

	private void keuzeIntakeAfspraakConclusie(List<TestVervolgKeuzeOptie> keuzes, ColonScreeningRonde ronde)
	{
		var afspraak = ronde.getLaatsteAfspraak();
		if (afspraak != null && afspraak.getConclusie() == null && !ColonScreeningRondeUtil.heeftAfgerondeVerslag(ronde, VerslagType.MDL))
		{
			keuzes.add(TestVervolgKeuzeOptie.INTAKE_AFSPRAAK_CONCLUSIE);
		}

	}

	@Override
	@Transactional
	public List<Client> maakOfVindClienten(TestTimelineModel model)
	{
		var clienten = new ArrayList<Client>();
		for (var bsn : model.getBsns())
		{
			clienten.add(maakOfVindClient(model, bsn));
		}
		return clienten;
	}

	private Client maakOfVindClient(TestTimelineModel model, String bsn)
	{
		var persoon = new Persoon();
		persoon.setBsn(bsn);
		persoon.setGeboortedatum(model.getGeboortedatum());
		persoon.setGeslacht(model.getGeslacht());

		var adres = new BagAdres();
		adres.setGbaGemeente(model.getGemeente());

		if (model.getGemeente() != null)
		{
			adres.setGemeenteCode(model.getGemeente().getCode());
			adres.setGemeente(model.getGemeente().getNaam());
		}
		persoon.setGbaAdres(adres);
		return testService.maakClient(persoon);
	}

	@Override
	@Transactional
	public List<Client> maakOfWijzigClienten(TestTimelineModel model)
	{
		var clienten = new ArrayList<Client>();
		for (var bsn : model.getBsns())
		{
			var client = maakOfVindClient(model, bsn);
			clienten.add(client);
			var persoon = client.getPersoon();
			persoon.setGeslacht(model.getGeslacht());
			persoon.setGeboortedatum(model.getGeboortedatum());

			hibernateService.saveOrUpdateAll(persoon);
		}
		return clienten;
	}

	private int getAantalUitnodigingen(Client client)
	{
		var aantalUitnodigingen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getUitnodigingen() != null && !screeningRonde.getUitnodigingen().isEmpty())
			{
				aantalUitnodigingen = screeningRonde.getUitnodigingen().size();
			}
		}
		return aantalUitnodigingen;
	}

	private int getAantalTestbuizen(Client client)
	{
		var aantalBuizen = 0;
		if (heeftLaatsteScreeningsRonde(client))
		{
			var screeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			if (screeningRonde.getFitRegistraties() != null && screeningRonde.getFitRegistraties().size() > 0)
			{
				aantalBuizen = screeningRonde.getFitRegistraties().size();
			}
		}
		return aantalBuizen;
	}

	private boolean heeftLaatsteScreeningsRonde(Client client)
	{
		return client.getColonDossier().getLaatsteScreeningRonde() != null;
	}

	@Override
	public List<String> validateTestClienten(List<Client> clienten)
	{

		if (clienten.size() == 1)
		{
			return new ArrayList<>();
		}
		Client eersteClient = null;
		var aantalUitnodigingen = 0;
		var aantalTestbuizen = 0;
		var errorMeldingen = new ArrayList<String>();
		for (var client : clienten)
		{
			if (eersteClient == null)
			{
				aantalUitnodigingen = getAantalUitnodigingen(client);
				aantalTestbuizen = getAantalTestbuizen(client);
				eersteClient = client;
			}

			if (aantalUitnodigingen != getAantalUitnodigingen(client))
			{
				var error = "Het aantal uitnodigingen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMeldingen.add(error);
			}
			if (aantalTestbuizen != getAantalTestbuizen(client))
			{
				var error = "Het aantal testbuizen voor client-bsn: " + client.getPersoon().getBsn() + "is niet gelijk aan de overige testcliënten.";
				LOG.error(error);
				errorMeldingen.add(error);
			}
		}
		return errorMeldingen;
	}

	@Override
	@Transactional
	public ColonDossier maakNieuweScreeningRonde(Client client, TestTimeLineDossierTijdstip tijdstip, ColonOnderzoeksVariant onderzoeksVariant)
	{
		if (client.getColonDossier().getLaatsteScreeningRonde() != null)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), TestTimeLineDossierTijdstip.EINDE_RONDE);
		}

		var dossier = client.getColonDossier();
		var ronde = newScreeningronde(dossier);
		maakOfGeefVooraankondiging(ronde);

		if (tijdstip == TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_ZONDER_FIT)
		{
			briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_ZONDER_FIT, currentDateSupplier.getDate());
		}
		else
		{
			if (new ArrayList<>(dossier.getScreeningRondes()).size() == 1)
			{
				maakUitnodiging(ronde, ColonUitnodigingscategorie.U1, onderzoeksVariant);
			}
			else
			{
				maakUitnodiging(ronde, ColonUitnodigingscategorie.U2, onderzoeksVariant);
			}

			bewerkUitnodiging(client, tijdstip);
		}
		return dossier;
	}

	@Override
	@Transactional
	public ColonDossier bewerkUitnodiging(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		var dossier = client.getColonDossier();
		var ronde = maakOfGeefLaatsteScreeningronde(dossier);
		var uitnodiging = maakOfGeefUitnodiging(ronde);

		if (TestTimeLineDossierTijdstip.DAG_UITNODIGING_VERSTUREN.equals(tijdstip) || TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN.equals(tijdstip))
		{
			var aantalDagen = controleerColonUitnodigingVersturen(dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging());
			if (aantalDagen > 0)
			{
				testTimelineTimeService.calculateBackwards(dossier, aantalDagen);
			}

			uitnodiging.setVerstuurdDatum(currentDateSupplier.getDate());
			uitnodiging.setVerstuurd(true);
		}

		if (TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN.equals(tijdstip))
		{

			testTimelineTimeService.calculateBackwards(dossier, 1);

			koppelFit(uitnodiging);

		}
		return dossier;
	}

	private int controleerColonUitnodigingVersturen(ColonUitnodiging uitnodiging)
	{
		return (int) ChronoUnit.DAYS.between(currentDateSupplier.getLocalDate(), DateUtil.toLocalDate(uitnodiging.getUitnodigingsDatum()));
	}

	private int bepaalAantalDagenVoorOntvangenAntwoordformulierOfFit(ColonUitnodiging uitnodiging)
	{

		var gold = uitnodiging.getGekoppeldeFitRegistratie();
		if (gold != null && (gold.getAnalyseDatum() != null || ColonFitRegistratieStatus.isUnmutableEindStatus(gold.getStatus()))
			|| uitnodiging.getAntwoordFormulier() != null && uitnodiging.getAntwoordFormulier().getScanDatum() != null)
		{
			return 0;
		}
		uitnodiging = uitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
		var verstuurdDatum = uitnodiging.getVerstuurdDatum();
		if (verstuurdDatum == null)
		{
			verstuurdDatum = uitnodiging.getCreatieDatum();
		}
		var datumVerstuurd = DateUtil.toLocalDate(verstuurdDatum).plusDays(DEFAULT_TIJD_TUSSEN_VERSTUURD_EN_UITSLAG_ONTVANGEN);
		return (int) ChronoUnit.DAYS.between(datumVerstuurd, currentDateSupplier.getLocalDate());
	}

	private ColonScreeningRonde newScreeningronde(ColonDossier dossier)
	{
		var ronde = dossier.getLaatsteScreeningRonde();
		var nu = currentDateSupplier.getDate();
		if (ronde != null)
		{
			screeningsrondeService.sluitRonde(ronde);
		}
		ronde = new ColonScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.LOPEND);
		ronde.setCreatieDatum(nu);
		ronde.setStatusDatum(nu);
		ronde.setAangemeld(true);
		ronde.setDossier(dossier);
		dossier.setLaatsteScreeningRonde(ronde);
		dossier.getScreeningRondes().add(ronde);
		hibernateService.saveOrUpdateAll(dossier, ronde);
		ronde = dossier.getLaatsteScreeningRonde();
		return ronde;
	}

	private ColonScreeningRonde maakOfGeefLaatsteScreeningronde(ColonDossier dossier)
	{
		var ronde = dossier.getLaatsteScreeningRonde();
		if (ronde == null)
		{
			ronde = newScreeningronde(dossier);
		}
		return ronde;
	}

	private ColonVooraankondiging maakOfGeefVooraankondiging(ColonScreeningRonde ronde)
	{
		var dossier = ronde.getDossier();
		var vooraankondiging = dossier.getVooraankondiging();
		if (vooraankondiging == null)
		{
			vooraankondiging = new ColonVooraankondiging();
			vooraankondiging.setClient(dossier.getClient());
			var nu = currentDateSupplier.getDate();
			vooraankondiging.setCreatieDatum(nu);
			dossier.setVooraankondiging(vooraankondiging);
			var brief = briefService.maakBvoBrief(ronde, BriefType.COLON_VOORAANKONDIGING);
			vooraankondiging.setBrief(brief);
			hibernateService.saveOrUpdateAll(vooraankondiging);
		}
		return vooraankondiging;
	}

	private void koppelFit(ColonUitnodiging uitnodiging)
	{
		if (uitnodiging != null && uitnodiging.getGekoppeldeFitRegistratie() == null)
		{
			var baseTestBarcode = ColonFitRegistratieUtil.getFitRegistratieBarcode(uitnodiging.getUitnodigingsId());
			var nu = currentDateSupplier.getDate();
			if (ColonOnderzoeksVariant.isOfType(uitnodiging.getOnderzoeksVariant(), ColonFitType.GOLD))
			{
				var ronde = uitnodiging.getScreeningRonde();
				var fitRegistratie = new ColonFitRegistratie();
				fitRegistratie.setDatumVerstuurd(nu);
				fitRegistratie.setStatus(ColonFitRegistratieStatus.ACTIEF);
				fitRegistratie.setStatusDatum(nu);
				fitRegistratie.setBarcode("TGD" + baseTestBarcode);
				fitRegistratie.setScreeningRonde(ronde);
				fitRegistratie.setUitnodiging(uitnodiging);
				fitRegistratie.setType(ColonFitType.GOLD);
				ronde.getFitRegistraties().add(fitRegistratie);
				ronde.setLaatsteFitRegistratie(fitRegistratie);
				ronde.setLaatsteExtraFitRegistratie(null);
				fitRegistratie.setUitnodiging(uitnodiging);
				uitnodiging.setGekoppeldeFitRegistratie(fitRegistratie);
				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				hibernateService.saveOrUpdateAll(fitRegistratie, uitnodiging, ronde);
			}
			if (ColonOnderzoeksVariant.isOfType(uitnodiging.getOnderzoeksVariant(), ColonFitType.STUDIE))
			{
				var ronde = uitnodiging.getScreeningRonde();
				var fitRegistratie = new ColonFitRegistratie();
				fitRegistratie.setDatumVerstuurd(nu);
				fitRegistratie.setStatus(ColonFitRegistratieStatus.ACTIEF);
				fitRegistratie.setStatusDatum(nu);
				fitRegistratie.setBarcode("TST" + baseTestBarcode);
				fitRegistratie.setScreeningRonde(ronde);
				fitRegistratie.setUitnodiging(uitnodiging);
				fitRegistratie.setType(ColonFitType.STUDIE);
				ronde.getFitRegistraties().add(fitRegistratie);
				ronde.setLaatsteExtraFitRegistratie(fitRegistratie);
				fitRegistratie.setUitnodiging(uitnodiging);
				uitnodiging.setGekoppeldeExtraFitRegistratie(fitRegistratie);
				uitnodiging.setVerstuurdDoorInpakcentrum(true);
				hibernateService.saveOrUpdateAll(fitRegistratie, uitnodiging, ronde);
			}
		}
	}

	private ColonUitnodiging maakUitnodiging(ColonScreeningRonde ronde, ColonUitnodigingscategorie cat, ColonOnderzoeksVariant onderzoeksVariant)
	{
		var uitnodiging = new ColonUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		if (ColonUitnodigingscategorie.U1.equals(cat) || ColonUitnodigingscategorie.U2.equals(cat))
		{
			uitnodiging.setUitnodigingscategorie(cat);
			uitnodiging.setUitnodigingsDatum(testTimelineTimeService.getVooraankondigingsPeriodeDatum());
		}
		else
		{
			uitnodiging.setUitnodigingsDatum(currentDateSupplier.getDate());
		}
		uitnodiging.setCreatieDatum(currentDateSupplier.getDate());
		uitnodiging.setScreeningRonde(ronde);
		uitnodiging.setOnderzoeksVariant(onderzoeksVariant);
		ronde.getUitnodigingen().add(uitnodiging);
		ronde.setLaatsteUitnodiging(uitnodiging);
		ronde.setLaatsteFitRegistratie(null);
		ronde.setLaatsteExtraFitRegistratie(null);
		hibernateService.saveOrUpdateAll(uitnodiging, ronde);
		colonDossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.UITNODIGING_ONTVANGEN);
		uitnodiging = ronde.getLaatsteUitnodiging();
		return uitnodiging;
	}

	private ColonUitnodiging maakOfGeefUitnodiging(ColonScreeningRonde ronde)
	{
		var uitnodiging = ronde.getLaatsteUitnodiging();
		if (uitnodiging == null)
		{
			if (ronde.getDossier().getScreeningRondes().size() == 1)
			{
				uitnodiging = maakUitnodiging(ronde, ColonUitnodigingscategorie.U1, ColonOnderzoeksVariant.STANDAARD);
			}
			else
			{
				uitnodiging = maakUitnodiging(ronde, ColonUitnodigingscategorie.U1, ColonOnderzoeksVariant.STANDAARD);
			}
		}
		return uitnodiging;
	}

	@Override
	public List<TestTimelineRonde> getTimelineRondes(Client client)
	{
		var onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		onderzoeken.add(Bevolkingsonderzoek.COLON);
		var rondes = dossierService.getScreeningRondeGebeurtenissen(client, new ClientDossierFilter(onderzoeken, false));

		return convertToTimelineRondes(rondes);
	}

	private List<TestTimelineRonde> convertToTimelineRondes(List<ScreeningRondeGebeurtenissen> rondeDossier)
	{
		Map<Integer, TestTimelineRonde> rondes = new HashMap<>();
		for (var ronde : rondeDossier)
		{
			var index = ronde.getRondenr() - 1;
			TestTimelineRonde testTimelineRonde;
			if (!rondes.containsKey(index))
			{
				testTimelineRonde = new TestTimelineRonde();
				testTimelineRonde.setRondeNummer(index + 1);
				rondes.put(index, testTimelineRonde);
			}
			else
			{
				testTimelineRonde = rondes.get(index);
			}
			testTimelineRonde.setColonScreeningRondeDossier(ronde);
		}
		return convertHashMapToList(rondes);
	}

	private List<TestTimelineRonde> convertHashMapToList(Map<Integer, TestTimelineRonde> map)
	{
		List<TestTimelineRonde> rondes = new ArrayList<>();
		for (var ronde : map.entrySet())
		{
			rondes.add(ronde.getKey(), ronde.getValue());
		}
		return rondes;
	}

	@Override
	@Transactional
	public void retourzendingOntvangen(ColonUitnodiging uitnodiging, String reden)
	{
		var dossier = uitnodiging.getScreeningRonde().getDossier();
		testTimelineTimeService.calculateBackwards(dossier, 5);

		retourzendingService.verwerkRetourzendingHandmatig(null, uitnodiging, reden);
	}

	@Override
	@Transactional
	public ColonFitRegistratie fitOntvangen(Client client, Boolean verlopen, ColonFitRegistratie fitRegistratie, int analyseDatumDiff)
	{

		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(fitRegistratie);
		var aantalDagen = bepaalAantalDagenVoorOntvangenAntwoordformulierOfFit(uitnodiging);
		if (aantalDagen >= 0)
		{
			testTimelineTimeService.calculateBackwards(client.getColonDossier(), aantalDagen);
		}

		var vandaag = currentDateSupplier.getLocalDate();

		if (fitRegistratie.getUitslag() != null || fitRegistratie.getGeinterpreteerdeUitslag() != null)
		{
			var fitHoudbaarheid = houdbaarheidService.getFitHoudbaarheidVoor(fitRegistratie.getBarcode());
			ColonHoudbaarheidFitReeks houdbaarheidFitReeks = null;
			if (fitHoudbaarheid == null && !Boolean.TRUE.equals(verlopen))
			{
				houdbaarheidFitReeks = new ColonHoudbaarheidFitReeks();
				houdbaarheidFitReeks.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				houdbaarheidFitReeks.setType(fitRegistratie.getType());
				houdbaarheidFitReeks.setLengthBarcode(fitRegistratie.getBarcode().length());
				houdbaarheidFitReeks.setBarcodeStart(fitRegistratie.getBarcode());
				houdbaarheidFitReeks.setBarcodeEnd(fitRegistratie.getBarcode());
				hibernateService.saveOrUpdate(houdbaarheidFitReeks);
			}
			else if (fitHoudbaarheid != null && (fitRegistratie.getBarcode().startsWith("TGD") || fitRegistratie.getBarcode().startsWith("TST")))
			{
				fitHoudbaarheid.setVervalDatum(DateUtil.toUtilDate(vandaag.plusDays(10)));
				hibernateService.saveOrUpdate(fitHoudbaarheid);
			}
			fitRegistratie.setAnalyseDatum(DateUtil.toUtilDate(vandaag.minusDays(analyseDatumDiff)));
			fitRegistratie.setVerwerkingsDatum(DateUtil.toUtilDate(vandaag));
			uitslagOntvangen(fitRegistratie);

			if (houdbaarheidFitReeks != null && (fitRegistratie.getBarcode().startsWith("TGD") || fitRegistratie.getBarcode().startsWith("TST"))
				&& (ColonFitRegistratieStatus.isUnmutableEindStatus(fitRegistratie.getStatus()) || fitRegistratie.getStatus() == ColonFitRegistratieStatus.UITGEVOERD))
			{
				hibernateService.delete(houdbaarheidFitReeks);
			}
		}
		return fitRegistratie;
	}

	private void uitslagOntvangen(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie.getType() != ColonFitType.STUDIE)
		{
			fitService.verwerkAnalyseResultaat(fitRegistratie);
		}
		else
		{
			studieRegistratieService.verwerkRegistratie(fitRegistratie);
		}
	}

	@Override
	@Transactional
	public ColonScreeningRonde naarEindeVanRonde(Client client)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.EINDE_RONDE);
		return dossier.getLaatsteScreeningRonde();
	}

	@Override
	@Transactional
	public void fitHerinneringVersturen(Client client, TestTimeLineDossierTijdstip tijdstip)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, tijdstip);
		if (TestTimeLineDossierTijdstip.DAG_NA_HERINNERING_VERSTUREN.equals(tijdstip))
		{
			var csr = dossier.getLaatsteScreeningRonde();
			var herinneringsBrief = briefService.maakBvoBrief(csr, BriefType.COLON_HERINNERING);
			for (var iTest : fitRegistratiesHerinneren(csr))
			{
				iTest.setHerinnering(Boolean.TRUE);
				hibernateService.saveOrUpdate(iTest);
				herinneringsBrief.setFitRegistratie(iTest);
				hibernateService.saveOrUpdate(herinneringsBrief);
			}
		}
	}

	private List<ColonFitRegistratie> fitRegistratiesHerinneren(ColonScreeningRonde csr)
	{
		return csr.getFitRegistraties().stream().filter(fit -> ColonFitRegistratieStatus.ACTIEF.equals(fit.getStatus()) && fit.getStatusDatum().before(getHerinneringspeildatum()))
			.collect(Collectors.toList());
	}

	private Date getHerinneringspeildatum()
	{
		var herinneringsperiode = preferenceService.getInteger(PreferenceKey.COLON_HERINNERINGS_PERIODE.name());
		return DateUtil.minDagen(currentDateSupplier.getDate(), herinneringsperiode);
	}

	@Override
	@Transactional
	public void verzetDossierAchteruitInTijd(Client client, int aantaldagen)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, aantaldagen);
	}

	@Override
	@Transactional
	public void maaktIntakeAfspraakVoorClient(Client client, ColonIntakelocatie intakelocatie, boolean binnenRooster)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(client.getColonDossier(), 1);

		var ronde = dossier.getLaatsteScreeningRonde();

		var brief = briefService.maakBvoBrief(ronde, BriefType.COLON_UITNODIGING_INTAKE);
		brief.setCreatieDatum(DateUtil.minusTijdseenheid(brief.getCreatieDatum(), 5, ChronoUnit.SECONDS));

		var intakeAfspraak = ronde.getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			LocalDateTime startDatum;
			ColonIntakekamer intakekamer;
			ColonAfspraakslot afspraakslot = null;
			var duurAfspraakInMinuten = organisatieParameterService.getOrganisatieParameter(intakelocatie, OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, 15);

			if (binnenRooster)
			{
				afspraakslot = vindEersteBeschikbareAfspraakslot(intakelocatie);
				startDatum = afspraakslot.getVanaf();
				intakekamer = afspraakslot.getKamer();
			}
			else
			{
				var eersteActieveKamer = intakelocatie.getKamers().stream().filter(kamer -> Boolean.TRUE.equals(kamer.getActief())).findFirst();
				if (eersteActieveKamer.isEmpty())
				{
					throw new IllegalStateException("Intakelocatie heeft geen actieve kamers");
				}
				startDatum = currentDateSupplier.getLocalDateTime().plusDays(1);
				intakekamer = eersteActieveKamer.get();
			}

			intakeAfspraak = new ColonIntakeAfspraak();
			intakeAfspraak.setAfstand(BigDecimal.valueOf(2.3));
			intakeAfspraak.setClient(client);
			intakeAfspraak.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
			intakeAfspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
			client.getAfspraken().add(intakeAfspraak);
			ronde.setLaatsteAfspraak(intakeAfspraak);
			ronde.getAfspraken().add(intakeAfspraak);
			intakeAfspraak.setScreeningRonde(ronde);
			intakeAfspraak.setKamer(intakekamer);
			intakeAfspraak.setVanaf(startDatum);
			intakeAfspraak.setTot(intakeAfspraak.getVanaf().plusMinutes(duurAfspraakInMinuten));
			intakeAfspraak.setAfspraakslot(afspraakslot);
		}
		intakeAfspraak.setBezwaar(false);
		intakeAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);
		brief.setIntakeAfspraak(intakeAfspraak);
		hibernateService.saveOrUpdate(brief);
		hibernateService.saveOrUpdate(intakeAfspraak);
		hibernateService.saveOrUpdate(ronde);
		hibernateService.saveOrUpdate(client);

		colonDossierBaseService.setDatumVolgendeUitnodiging(ronde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);
	}

	private ColonAfspraakslot vindEersteBeschikbareAfspraakslot(ColonIntakelocatie intakeLocatie)
	{
		var intakeNietWijzigbaar = preferenceService.getInteger(PreferenceKey.INTAKE_NIET_WIJZIGBAAR.name());
		var startDatum = currentDateSupplier.getLocalDate().plusDays(intakeNietWijzigbaar);
		var eindDatum = startDatum.plusYears(1);
		var beschikbareStatussen = List.of(ColonAfspraakslotStatus.GEBRUIKT_VOOR_CAPACITEIT, ColonAfspraakslotStatus.VRIJ_TE_VERPLAATSEN);
		var beschikbareAfspraakslot = afspraakslotService.getEerstBeschikbareAfspraakslot(startDatum, eindDatum, beschikbareStatussen, intakeLocatie);

		if (beschikbareAfspraakslot == null)
		{
			throw new IllegalStateException("Intakelocatie heeft geen beschikbare afspraakslots");
		}
		return beschikbareAfspraakslot;

	}

	@Override
	@Transactional
	public void maakIntakeAfspraakConclusieVoorClient(Client client, ColonConclusieType type)
	{
		var keuzes = new ColonVervolgonderzoekKeuzesDto();
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.INTAKE_AFSPRAAK_CONCLUSIE);
		var ronde = dossier.getLaatsteScreeningRonde();
		var intakeAfspraak = ronde.getLaatsteAfspraak();
		var conclusie = intakeAfspraak.getConclusie();
		var ingelogdeOrganisatieMedewerker = hibernateService.loadAll(OrganisatieMedewerker.class).get(0);
		keuzes.conclusie = type;
		if (conclusie == null)
		{
			conclusie = new ColonConclusie();
		}

		conclusie.setType(type);
		switch (type)
		{
		case COLOSCOPIE:
			keuzes.intakeConclusie = true;
			conclusie.setDatumColoscopie(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(10)));
		case CT_COLOGRAFIE:
			keuzes.intakeConclusie = true;
			break;
		case NO_SHOW:
		case ON_HOLD:
			keuzes.intakeConclusie = false;
			break;
		case DOORVERWIJZEN_NAAR_ANDER_CENTRUM:
			keuzes.intakeConclusie = false;
			keuzes.verwijzing = true;
			conclusie.setDoorverwijzingBevestigd(true);
			break;
		case CLIENT_WIL_ANDERE_INTAKELOKATIE:
			keuzes.intakeConclusie = false;
			keuzes.verwijzing = false;
			break;
		case GEEN_VERVOLGONDERZOEK:
			keuzes.intakeConclusie = true;
			keuzes.redenGeenVervolgOnderzoek = false;
			break;
		}
		intakeAfspraak.setConclusie(conclusie);

		conclusie.setOrganisatieMedewerker(ingelogdeOrganisatieMedewerker);

		colonDossierService.conclusieOpslaan(intakeAfspraak, keuzes, ingelogdeOrganisatieMedewerker, null);
	}

	@Override
	@Transactional
	public void maaktMdlVerslagVoorClient(Client client, ColoscopieLocatie locatie, MdlVervolgbeleid vervolgbeleid, Date datumOnderzoek)
	{
		var dossier = client.getColonDossier();
		testTimelineTimeService.calculateBackwards(dossier, TestTimeLineDossierTijdstip.MDL_VERSLAG);
		var ronde = dossier.getLaatsteScreeningRonde();

		var verslag = new MdlVerslag();
		verslag.setDatumOnderzoek(datumOnderzoek);
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verslag.setInvoerder(locatie.getOrganisatieMedewerkers().get(0));
		verslag.setScreeningRonde(ronde);
		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(VerslagType.MDL);
		verslag.setUitvoerderMedewerker(locatie.getOrganisatieMedewerkers().get(0).getMedewerker());
		verslag.setUitvoerderOrganisatie(locatie);
		verslag.setVervolgbeleid(vervolgbeleid);
		var content = new MdlVerslagContent();
		verslag.setVerslagContent(content);
		if (vervolgbeleid != null)
		{
			var coloscopieMedischeObservatie = new MdlColoscopieMedischeObservatie();
			content.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
			coloscopieMedischeObservatie.setVerslagContent(content);
			var definitiefVervolgbeleidVoorBevolkingsonderzoekg = new MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
			coloscopieMedischeObservatie.setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(definitiefVervolgbeleidVoorBevolkingsonderzoekg);

			definitiefVervolgbeleidVoorBevolkingsonderzoekg
				.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagService.getDsValue(vervolgbeleid.getCode(), vervolgbeleid.getCodeSystem(), "vs_vervolgbeleid"));
			definitiefVervolgbeleidVoorBevolkingsonderzoekg.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
		}
		content.setVerslag(verslag);
		content.setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.MDL));
		var verrichting = new MdlVerrichting();
		verrichting.setVerslagContent(content);
		verrichting.setAanvangVerrichting(currentDateSupplier.getDate());
		content.setVerrichting(verrichting);
		ronde.getVerslagen().add(verslag);
		hibernateService.saveOrUpdate(verslag);
		hibernateService.saveOrUpdate(ronde);
		verwerkVerslagService.verwerkInDossier(verslag);
		colonDossierBaseService.setVolgendeUitnodigingVoorVerslag(verslag);
	}
}
