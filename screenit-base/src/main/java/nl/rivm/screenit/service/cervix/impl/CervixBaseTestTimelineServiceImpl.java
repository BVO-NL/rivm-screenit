package nl.rivm.screenit.service.cervix.impl;

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

import java.security.SecureRandom;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvAnalyseresultaat;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorieOngestructureerdRegel;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieMonsterBmhk;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerrichting;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.service.cervix.CervixBaseTestTimelineHuisartsService;
import nl.rivm.screenit.service.cervix.CervixBaseTestTimelineService;
import nl.rivm.screenit.service.cervix.CervixBepaalVervolgService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixTestTimelineTimeService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
public class CervixBaseTestTimelineServiceImpl implements CervixBaseTestTimelineService
{
	private final HibernateService hibernateService;

	private final ICurrentDateSupplier dateSupplier;

	private final CervixFactory factory;

	private final CervixTestTimelineTimeService testTimelineTimeService;

	private final CervixBepaalVervolgService bepaalVervolgService;

	private final SimplePreferenceService preferenceService;

	private final CervixBaseMonsterService monsterService;

	private final CervixMonsterDao monsterDao;

	private final CervixVervolgService vervolgService;

	private final Cervix2023StartBepalingService cervix2023StartBepalingService;

	private final CervixBaseTestTimelineHuisartsService testTimelineHuisartsService;

	private final CervixBaseScreeningrondeService screeningrondeService;

	private final Random random = new SecureRandom();

	@Override
	@Transactional
	public CervixBaseTestTimelineService ontvangen(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.ONTVANGEN);

		CervixMonster monster = uitnodiging.getMonster();
		monster.setLaboratorium(laboratorium);
		monster.setOntvangstdatum(dateSupplier.getDate());
		monster.setOntvangstScreeningRonde(screeningrondeService.getOntvangstRondeVoorMonster(monster));
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
			uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.ONTVANGEN);
			uitstrijkje.setStatusDatum(dateSupplier.getDate());

			hibernateService.saveOrUpdate(uitstrijkje);
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			CervixZas zas = (CervixZas) monster;
			zas.setZasStatus(CervixZasStatus.ONTVANGEN);
			zas.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(zas);
		}

		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService nietAnalyseerbaar(CervixUitnodiging uitnodiging)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.NIET_ANALYSEERBAAR);

		CervixMonster monster = uitnodiging.getMonster();
		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
			uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR);
			uitstrijkje.setStatusDatum(dateSupplier.getDate());
			uitstrijkje.setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden.ONBEKEND);
			hibernateService.saveOrUpdate(uitstrijkje);
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			CervixZas zas = (CervixZas) monster;
			zas.setZasStatus(CervixZasStatus.NIET_ANALYSEERBAAR);
			zas.setStatusDatum(dateSupplier.getDate());
			zas.setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden.ONBEKEND);
			hibernateService.saveOrUpdate(zas);
		}
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService geanalyseerdOpHpv(CervixUitnodiging uitnodiging, CervixHpvBeoordelingWaarde hpvUitslag, BMHKLaboratorium laboratorium,
		List<CervixHpvResultValue> hpvResultValues)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.GEANALYSEERD_OP_HPV);

		CervixMonster monster = uitnodiging.getMonster();
		CervixScreeningRonde ronde = monster.getOntvangstScreeningRonde() != null ? monster.getOntvangstScreeningRonde() : uitnodiging.getScreeningRonde();
		CervixHpvBericht hpvBericht = factory.maakHpvBericht(laboratorium, "instrumentId", "hl7Bericht", Long.toString(System.currentTimeMillis()));
		hpvBericht.setStatus(BerichtStatus.VERWERKT);
		hibernateService.saveOrUpdate(hpvBericht);

		List<CervixHpvAnalyseresultaat> analyseresultaten = hpvResultValues
			.stream()
			.map(resultValue -> new CervixHpvAnalyseresultaat(resultValue, resultValue.getResultCode(), resultValue.getResultCode().getOrderCode()))
			.collect(Collectors.toList());
		factory.maakHpvBeoordeling(monster, hpvBericht, dateSupplier.getDate(), dateSupplier.getDate(), hpvUitslag, analyseresultaten);

		if (hpvUitslag != CervixHpvBeoordelingWaarde.ONGELDIG)
		{
			ronde.setMonsterHpvUitslag(monster);
			hibernateService.saveOrUpdate(ronde);
		}

		var bmhk2023Lab = cervix2023StartBepalingService.isBmhk2023Laboratorium(monster.getLaboratorium());

		if (uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
		{
			CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;
			if (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.ONTVANGEN && !bmhk2023Lab)
			{
				uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1);
			}
			else
			{
				uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
			}
			uitstrijkje.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(uitstrijkje);
			vervolgService.digitaalLabformulierKlaarVoorCytologie(uitstrijkje);
		}
		else if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS)
		{
			CervixZas zas = (CervixZas) monster;
			if (zas.getZasStatus() == CervixZasStatus.ONTVANGEN && !bmhk2023Lab)
			{
				zas.setZasStatus(CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1);
			}
			else
			{
				zas.setZasStatus(CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_2);
			}
			zas.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(zas);
		}
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService beoordeeldDoorCytologie(CervixUitnodiging uitnodiging, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.BEOORDEELD_DOOR_CYTOLOGIE);

		CervixMonster monster = uitnodiging.getMonster();
		CervixScreeningRonde ronde = uitnodiging.getScreeningRonde().getDossier().getLaatsteScreeningRonde();
		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) monster;

		OntvangenCdaBericht ontvangenCdaBericht = new OntvangenCdaBericht();
		ontvangenCdaBericht.setStatus(BerichtStatus.VERWERKT);
		ontvangenCdaBericht.setBerichtType(BerichtType.CERVIX_CYTOLOGIE_VERSLAG);

		CervixCytologieVerrichting cytologieVerrichting = new CervixCytologieVerrichting();
		cytologieVerrichting.setEindeVerrichting(dateSupplier.getDate());

		CervixCytologieMonsterBmhk cytologieMonsterBmhk = new CervixCytologieMonsterBmhk();
		cytologieMonsterBmhk.setMonsterIdentificatie(monster.getMonsterId());
		cytologieMonsterBmhk.setDatumAfnameMateriaal(dateSupplier.getDate());
		cytologieMonsterBmhk.setDatumAutorisatie(dateSupplier.getDate());
		cytologieMonsterBmhk.setDatumOntvangstMateriaal(dateSupplier.getDate());

		CervixCytologieCytologieUitslagBvoBmhk cytologieCytologieUitslagBvoBmhk = new CervixCytologieCytologieUitslagBvoBmhk();
		cytologieCytologieUitslagBvoBmhk.setMonsterBmhk(cytologieMonsterBmhk);
		cytologieCytologieUitslagBvoBmhk.setCnummerLaboratorium("Test c nummer");
		cytologieCytologieUitslagBvoBmhk.setVersieProtocol("Test versie protocol");

		cytologieMonsterBmhk.setCytologieUitslagBvoBmhk(cytologieCytologieUitslagBvoBmhk);

		CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts cytologieCytologieUitslagBvoBmhkTbvHuisarts = new CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts();
		cytologieCytologieUitslagBvoBmhkTbvHuisarts.setConclusie("Test conclusie");
		cytologieCytologieUitslagBvoBmhkTbvHuisarts.setProtocollairVerslag("Test protocolair verslag");

		CervixCytologieVerslagContent cytologieVerslagContent = new CervixCytologieVerslagContent();
		cytologieVerslagContent.setVerrichting(cytologieVerrichting);
		cytologieVerrichting.setVerslagContent(cytologieVerslagContent);
		cytologieVerslagContent.setCytologieUitslagBvoBmhk(cytologieCytologieUitslagBvoBmhk);
		cytologieCytologieUitslagBvoBmhk.setVerslagContent(cytologieVerslagContent);
		cytologieVerslagContent.setCytologieUitslagBvoBmhkTbvHuisarts(cytologieCytologieUitslagBvoBmhkTbvHuisarts);
		cytologieCytologieUitslagBvoBmhkTbvHuisarts.setVerslagContent(cytologieVerslagContent);

		CervixCytologieVerslag cytologieVerslag = new CervixCytologieVerslag();
		cytologieVerslag.setType(VerslagType.CERVIX_CYTOLOGIE);
		cytologieVerslag.setOntvangenBericht(ontvangenCdaBericht);
		cytologieVerslag.setVerslagContent(cytologieVerslagContent);
		cytologieVerslag.setDatumVerwerkt(dateSupplier.getDate());
		cytologieVerslag.setDatumOnderzoek(dateSupplier.getDate());
		cytologieVerslag.setStatus(VerslagStatus.AFGEROND);
		cytologieVerslag.setCytologieUitslag(cytologieUitslag);
		cytologieVerslag.setUitstrijkje(uitstrijkje);
		cytologieVerslag.setScreeningRonde(ronde);
		cytologieVerslag.setLaboratorium(laboratorium);
		cytologieVerslagContent.setVerslag(cytologieVerslag);
		cytologieVerslag.setPatholoogNaam("Dr. Patholoog");

		uitstrijkje.setCytologieVerslag(cytologieVerslag);
		uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.BEOORDEELD_DOOR_CYTOLOGIE);
		uitstrijkje.setStatusDatum(dateSupplier.getDate());

		ronde.getVerslagen().add(cytologieVerslag);
		if (cytologieUitslag != CervixCytologieUitslag.PAP0)
		{
			if (ronde.getInVervolgonderzoekDatum() == null)
			{
				if (ronde.getUitstrijkjeCytologieUitslag() == null)
				{
					ronde.setUitstrijkjeCytologieUitslag(uitstrijkje);
				}
			}
			else
			{
				if (ronde.getUitstrijkjeVervolgonderzoekUitslag() == null)
				{
					ronde.setUitstrijkjeVervolgonderzoekUitslag(uitstrijkje);
				}
			}
		}

		hibernateService.saveOrUpdate(ontvangenCdaBericht);
		hibernateService.saveOrUpdate(cytologieVerrichting);
		hibernateService.saveOrUpdate(cytologieCytologieUitslagBvoBmhk);
		hibernateService.saveOrUpdate(cytologieMonsterBmhk);
		hibernateService.saveOrUpdate(cytologieCytologieUitslagBvoBmhkTbvHuisarts);
		hibernateService.saveOrUpdate(cytologieVerslagContent);
		hibernateService.saveOrUpdate(cytologieVerslag);
		hibernateService.saveOrUpdate(uitstrijkje);
		hibernateService.saveOrUpdate(ronde);
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService labformulierGescand(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.LABFORMULIER_GESCAND);

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();

		if (uitstrijkje.getOntvangstScreeningRonde() != null && uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ONTVANGEN)
		{
			uitstrijkje.setOntvangstScreeningRonde(null);
		}

		var labformulier = new CervixLabformulier();
		labformulier.setUitstrijkje(uitstrijkje);
		labformulier.setLaboratorium(laboratorium);
		labformulier.setScanDatum(dateSupplier.getDate());
		labformulier.setObjid(Long.toString(random.nextInt(Integer.MAX_VALUE)));
		labformulier.setStatus(CervixLabformulierStatus.GESCAND);
		labformulier.setStatusDatum(dateSupplier.getDate());
		labformulier.setBarcode(uitstrijkje.getMonsterId());
		labformulier.setDatumUitstrijkje(DateUtil.toUtilDate(dateSupplier.getLocalDate().minusDays(1)));

		uitstrijkje.setLabformulier(labformulier);

		hibernateService.saveOrUpdateAll(labformulier, uitstrijkje);
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService labformulierGecontroleerd(CervixUitnodiging uitnodiging)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.LABFORMULIER_GECONTROLEERD);

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();

		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		labformulier.setStatus(CervixLabformulierStatus.GECONTROLEERD);
		labformulier.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(labformulier);

		if (uitstrijkje.getOntvangstScreeningRonde() == null)
		{
			uitstrijkje.setOntvangstScreeningRonde(screeningrondeService.getOntvangstRondeVoorMonster(uitstrijkje));
		}

		hibernateService.saveOrUpdate(uitstrijkje);
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService labformulierGecontroleerdVoorCytologie(CervixUitnodiging uitnodiging)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.LABFORMULIER_GECONTROLEERD_VOOR_CYTOLOGIE);

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();

		labformulier.setStatus(CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE);
		labformulier.setStatusDatum(dateSupplier.getDate());

		hibernateService.saveOrUpdate(labformulier);
		return this;
	}

	@Override
	@Transactional
	public CervixUitnodiging nieuweRonde(Client client)
	{
		return nieuweRonde(client, false);
	}

	@Override
	@Transactional
	public CervixUitnodiging nieuweRonde(Client client, boolean metVooraankondiging)
	{
		var dossier = client.getCervixDossier();
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			testTimelineTimeService.rekenDossierTerug(dossier, CervixTestTimeLineDossierTijdstip.NIEUWE_RONDE);
		}

		var geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
		int minimumLeeftijd = CervixLeeftijdcategorie.minimumLeeftijd();
		var jongerDanMinimumLeeftijd = DateUtil.getLeeftijd(geboortedatum, dateSupplier.getLocalDate()) < minimumLeeftijd;

		var ronde = factory.maakRonde(dossier, !jongerDanMinimumLeeftijd);
		if (jongerDanMinimumLeeftijd || metVooraankondiging)
		{
			maakVooraankondiging(ronde, metVooraankondiging);
			testTimelineTimeService.rekenDossierTerug(dossier, berekenAantalDagenDossierTerug(geboortedatum));
		}

		CervixUitnodiging uitnodiging = null;
		if (!jongerDanMinimumLeeftijd)
		{
			uitnodiging = factory.maakUitnodiging(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief(), true, false);
			verzendLaatsteBrief(ronde);
		}
		return uitnodiging;
	}

	@Override
	@Transactional
	public void uitstelVoorZwangerschap(CervixScreeningRonde ronde)
	{
		Date uitstellenTotDatum = DateUtil.toUtilDate(dateSupplier.getLocalDate().plusDays(preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name())));
		factory.maakUitstel(ronde, uitstellenTotDatum, CervixUitstelType.ZWANGERSCHAP);
	}

	@Override
	@Transactional
	public void nieuweCISRonde0(Client client, Date creatieDatum)
	{
		CervixDossier dossier = client.getCervixDossier();
		CervixCISHistorie cervixCisHistorie = createCISHistorie(dossier);
		CervixScreeningRonde ronde = factory.maakRonde(dossier, DateUtil.toLocalDateTime(creatieDatum), true);
		cervixCisHistorie.setScreeningRonde(ronde);
		hibernateService.saveOrUpdate(cervixCisHistorie);
	}

	@Override
	@Transactional
	public void nieuweCISHistorie(Client client)
	{
		CervixDossier dossier = client.getCervixDossier();
		testTimelineTimeService.rekenDossierTerug(dossier, CervixTestTimeLineDossierTijdstip.NIEUWE_RONDE);
		createCISHistorie(dossier);
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService orderVerstuurd(CervixUitnodiging uitnodiging)
	{
		testTimelineTimeService.rekenDossierTerug(uitnodiging.getScreeningRonde().getDossier(), CervixTestTimeLineDossierTijdstip.ORDER_VERSTUURD);

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) uitnodiging.getMonster();

		CervixCytologieOrder cytologieOrder = factory.maakCytologieOrder(uitstrijkje, getCytologieReden(uitstrijkje), "hl7Bericht");

		cytologieOrder.setStatus(CervixCytologieOrderStatus.VERSTUURD);
		cytologieOrder.setStatusDatum(dateSupplier.getDate());
		hibernateService.saveOrUpdate(cytologieOrder);
		return this;
	}

	@Override
	@Transactional
	public void verstuurUitnodiging(CervixScreeningRonde ronde)
	{
		var dagenVoorDeVooraankondiging = getDagenVoorDeVooraankondiging();
		var geboortedatum = DateUtil.toLocalDate(ronde.getDossier().getClient().getPersoon().getGeboortedatum());
		var dagenTot30isteVerjaardag = Math.abs(
			DateUtil.getPeriodeTussenTweeDatums(geboortedatum.plusYears(CervixLeeftijdcategorie.minimumLeeftijd()), dateSupplier.getLocalDate(), ChronoUnit.DAYS));

		testTimelineTimeService.rekenDossierTerug(ronde.getDossier(), Math.min(dagenVoorDeVooraankondiging, dagenTot30isteVerjaardag));
		factory.maakUitnodiging(ronde, ronde.getLeeftijdcategorie().getUitnodigingsBrief(), true, false);
		verzendLaatsteBrief(ronde);

	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService vervolgonderzoekBrief(CervixScreeningRonde ronde)
	{
		testTimelineTimeService.rekenDossierTerug(ronde.getDossier(), CervixTestTimeLineDossierTijdstip.VERVOLGONDERZOEK_BRIEF);

		CervixUitnodiging uitnodiging = factory.maakUitnodiging(ronde, BriefType.CERVIX_UITNODIGING_CONTROLEUITSTRIJKJE, true, false);
		ronde.setUitnodigingVervolgonderzoek(uitnodiging);
		hibernateService.saveOrUpdate(ronde);

		return verzendLaatsteBrief(ronde);
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService maakZasMonster(Client client, Account account, CervixTestTimeLineDossierTijdstip tijdstip, boolean isNieuwTypeZas)
	{
		CervixDossier dossier = client.getCervixDossier();
		CervixScreeningRonde ronde = dossier.getLaatsteScreeningRonde();
		CervixUitnodiging uitnodiging = ronde.getLaatsteZasUitnodiging();

		if (uitnodiging == null || uitnodiging.getMonster() != null)
		{
			uitnodiging = factory.maakZasUitnodiging(client, account, false, false);
		}

		switch (tijdstip)
		{
		case ZAS_AANVRAAG:
			zasAangevraagd(dossier);
			break;
		case ZAS_KLAARGEZET:
			zasAangevraagd(dossier);
			zasKlaargezet(uitnodiging);
			break;
		case ZAS_SAMENGESTELD:
			zasAangevraagd(dossier);
			zasKlaargezet(uitnodiging);
			zasSamengesteld(uitnodiging, isNieuwTypeZas);
			break;
		default:
		}
		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService zetMonsterId(CervixUitnodiging uitnodiging, String monsterId)
	{
		CervixMonster monster = uitnodiging.getMonster();

		if (monster != null)
		{
			monster.setMonsterId(monsterId);
			hibernateService.saveOrUpdate(monster);

			if (monster instanceof CervixUitstrijkje uitstrijkje)
			{
				CervixLabformulier labformulier = uitstrijkje.getLabformulier();
				if (labformulier != null)
				{
					labformulier.setBarcode(monsterId);
					hibernateService.saveOrUpdate(labformulier);
				}
			}
		}

		return this;
	}

	@Override
	@Transactional
	public CervixBaseTestTimelineService maakAanvraag(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium)
	{
		labformulierGescand(uitnodiging, laboratorium);
		labformulierGecontroleerd(uitnodiging);
		CervixLabformulier labformulier = CervixMonsterUtil.getUitstrijkje(uitnodiging.getMonster()).getLabformulier();
		labformulier.setDigitaal(true);

		var locaties = testTimelineHuisartsService.findFirstHuisartsLocatie();

		locaties.ifPresent(labformulier::setHuisartsLocatie);
		hibernateService.saveOrUpdate(labformulier);

		return this;
	}

	private int berekenAantalDagenDossierTerug(LocalDate geboortedatum)
	{
		var dagenVoorDeVooraankondiging = getDagenVoorDeVooraankondiging();
		var aantalDagenTotBereikenMinimaleLeeftijd = CervixLeeftijdcategorie.aantalDagenTotBereikenMinimaleLeeftijd(geboortedatum, dateSupplier.getLocalDate());
		return dagenVoorDeVooraankondiging - aantalDagenTotBereikenMinimaleLeeftijd;
	}

	private void maakVooraankondiging(CervixScreeningRonde ronde, boolean forceerVooraankondiging)
	{
		Integer dagenVoorDeVooraankondiging = getDagenVoorDeVooraankondiging();
		var geboortedatumMaximaal = dateSupplier.getLocalDate().minusYears(CervixLeeftijdcategorie.minimumLeeftijd());
		if (dagenVoorDeVooraankondiging != null)
		{
			geboortedatumMaximaal = geboortedatumMaximaal.plusDays(dagenVoorDeVooraankondiging);
		}
		var client = ronde.getDossier().getClient();
		var geboortedatum = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
		if (!geboortedatum.isAfter(geboortedatumMaximaal) || forceerVooraankondiging)
		{
			factory.maakVooraankondiging(ronde);

			verzendLaatsteBrief(ronde);
		}
	}

	private Integer getDagenVoorDeVooraankondiging()
	{
		return preferenceService.getInteger(PreferenceKey.CERVIX_VOORAANKONDIGINGS_PERIODE.name(), 30);
	}

	private CervixCISHistorie createCISHistorie(CervixDossier dossier)
	{
		CervixCISHistorie cervixCisHistorie = dossier.getCisHistorie();
		if (cervixCisHistorie == null)
		{
			cervixCisHistorie = new CervixCISHistorie();
			cervixCisHistorie.setDossier(dossier);
			dossier.setCisHistorie(cervixCisHistorie);
			hibernateService.saveOrUpdate(cervixCisHistorie);
			LocalDateTime localDateTime = dateSupplier.getLocalDateTime();
			addRegel(cervixCisHistorie, "1", "Test Test Test1", localDateTime.plusSeconds(1));
			addRegel(cervixCisHistorie, "1", "Test Test Test2", localDateTime.plusSeconds(2));
			addRegel(cervixCisHistorie, "2", "Test Test Test3", localDateTime.plusSeconds(3));
			addRegel(cervixCisHistorie, "3", "Test Test Test4", localDateTime.plusSeconds(4));
			hibernateService.saveOrUpdate(dossier);
		}
		return cervixCisHistorie;
	}

	private void addRegel(CervixCISHistorie cervixCisHistorie, String ronde, String tekst, LocalDateTime dateTime)
	{
		CervixCISHistorieOngestructureerdRegel regel = new CervixCISHistorieOngestructureerdRegel();
		regel.setRonde(ronde);
		regel.setTekst(tekst);
		regel.setDatum(DateUtil.toUtilDate(dateTime));
		regel.setCisHistorie(cervixCisHistorie);
		cervixCisHistorie.getCisHistorieRegels().add(regel);
		hibernateService.saveOrUpdateAll(regel, cervixCisHistorie);
	}

	private CervixCytologieReden getCytologieReden(CervixUitstrijkje uitstrijkje)
	{
		String stringStartdatumGenotypering = preferenceService.getString(PreferenceKey.CERVIX_START_AANLEVERING_GENOTYPERING_EN_INVOERING_TRIAGE.name());

		CervixBepaalVervolgContext vervolgContext = new CervixBepaalVervolgContext(uitstrijkje, false, dateSupplier.getLocalDateTime(),
			DateUtil.parseLocalDateForPattern(stringStartdatumGenotypering, Constants.DATE_FORMAT_YYYYMMDD), bepaalVervolgService, monsterService,
			preferenceService.getInteger(PreferenceKey.CERVIX_INTERVAL_CONTROLE_UITSTRIJKJE.name()));

		if (vervolgContext.inVervolgonderzoekDatum != null)
		{
			if (bepaalVervolgService.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
			{
				return CervixCytologieReden.HERHALING_VERVOLGONDERZOEK;
			}
			return CervixCytologieReden.VERVOLGONDERZOEK;
		}

		if (bepaalVervolgService.anderUitstrijkjeOnbeoordeelbaarCytologie(vervolgContext.huidigUitstrijkje))
		{
			return CervixCytologieReden.HERHALING_INITIEEL_NA_ONBEOORDEELBAARHEID;
		}

		if (vervolgContext.monsterHpvUitslag instanceof CervixUitstrijkje)
		{
			return CervixCytologieReden.INITIEEL_ZONDER_ZAS;
		}
		else
		{
			return CervixCytologieReden.INITIEEL_NA_ZAS;
		}
	}

	private CervixBaseTestTimelineService verzendLaatsteBrief(CervixScreeningRonde ronde)
	{
		CervixBrief brief = ronde.getLaatsteBrief();
		if (brief == null)
		{
			return null;
		}

		CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(ronde.getLeeftijdcategorie().getUitnodigingsBrief());
		mergedBrieven.setGeprint(true);
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		mergedBrieven.setScreeningOrganisatie(ronde.getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		brief.setGegenereerd(true);
		brief.setMergedBrieven(mergedBrieven);
		UploadDocument fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");
		hibernateService.saveOrUpdate(fakeMergeDocument);
		mergedBrieven.setMergedBrieven(fakeMergeDocument);
		hibernateService.saveOrUpdate(mergedBrieven);
		hibernateService.saveOrUpdate(brief);
		return this;
	}

	private void zasAangevraagd(CervixDossier dossier)
	{
		testTimelineTimeService.rekenDossierTerug(dossier, CervixTestTimeLineDossierTijdstip.ZAS_AANVRAAG);
	}

	private void zasKlaargezet(CervixUitnodiging uitnodiging)
	{
		CervixDossier dossier = uitnodiging.getScreeningRonde().getDossier();
		testTimelineTimeService.rekenDossierTerug(dossier, CervixTestTimeLineDossierTijdstip.ZAS_KLAARGEZET);

		uitnodiging.setVerstuurd(true);
		uitnodiging.setVerstuurdDatum(dateSupplier.getDate());
		uitnodiging.setTemplateNaam("8. Verzendbrief ZAS 20160706_etiket_rechts.docx");

		UploadDocument fakeMergeDocument = new UploadDocument();
		fakeMergeDocument.setActief(true);
		fakeMergeDocument.setNaam("dummy_testservice_brief_niet_openen");

		CervixBrief brief = uitnodiging.getBrief();

		CervixMergedBrieven mergedBrieven = new CervixMergedBrieven();
		mergedBrieven.setCreatieDatum(dateSupplier.getDate());
		mergedBrieven.setBriefType(uitnodiging.getScreeningRonde().getLeeftijdcategorie().getUitnodigingsBrief());
		mergedBrieven.setGeprint(true);
		mergedBrieven.setPrintDatum(dateSupplier.getDate());
		mergedBrieven.setVerwijderd(true);
		mergedBrieven.setScreeningOrganisatie(dossier.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie());
		mergedBrieven.setMergedBrieven(fakeMergeDocument);

		brief.setGegenereerd(true);
		brief.setMergedBrieven(mergedBrieven);
		mergedBrieven.getBrieven().add(brief);
		hibernateService.saveOrUpdateAll(fakeMergeDocument, mergedBrieven, brief, uitnodiging);
	}

	private void zasSamengesteld(CervixUitnodiging uitnodiging, boolean isNieuwTypeZas)
	{
		CervixDossier dossier = uitnodiging.getScreeningRonde().getDossier();
		testTimelineTimeService.rekenDossierTerug(dossier, CervixTestTimeLineDossierTijdstip.ZAS_SAMENGESTELD);

		String zasId = monsterDao.getNextMonsterId().toString();
		zasId = zasId.substring(Math.max(0, zasId.length() - 8));
		zasId = (isNieuwTypeZas ? "C" : "Z") + StringUtils.leftPad(zasId + "", 8, '0');
		CervixZas zas = factory.maakZasMonster(uitnodiging, zasId);
		uitnodiging.setVerstuurdDoorInpakcentrum(true);
		zas.setVerstuurd(dateSupplier.getDate());
		hibernateService.saveOrUpdate(zas);
	}
}
