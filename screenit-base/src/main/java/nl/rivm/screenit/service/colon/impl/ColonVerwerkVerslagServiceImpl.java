
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

import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag_;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlIncidentcomplicatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlLaesiecoloscopiecentrum;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlPoliep;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaPathologieProtocolColonbioptperPoliep;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.ComplicatieErnst;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.model.enums.ComplicatieSoort;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.repository.colon.ColonMdlVerslagRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.colon.ComplicatieService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftColonDossier;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftGeenOntvangenBericht;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftScreeningRondeInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftVerslagStatus;
import static org.springframework.data.domain.Sort.Direction.DESC;

@Service
@Slf4j
public class ColonVerwerkVerslagServiceImpl implements ColonVerwerkVerslagService
{

	private static final List<String> SURVAILLANCE_CODES = Arrays.asList("12", "13", "14", "15");

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private BaseVerslagService verslagService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ComplicatieService complicatieService;

	@Autowired
	private ColonMdlVerslagRepository mdlVerslagRepository;

	@Autowired
	private LogService logService;

	@Override
	@Transactional
	public void verwerkInDossier(MdlVerslag verslag)
	{
		verslag.setVervolgbeleid(dossierBaseService.getVervolgbeleid(verslag));
		hibernateService.saveOrUpdate(verslag);

		ColonScreeningRonde screeningRonde = verslag.getScreeningRonde();
		Date nu = currentDateSupplier.getDate();
		screeningRonde.setStatusDatum(nu);
		screeningRonde.setDefinitiefVervolgbeleid(null);
		ColonDossier dossier = screeningRonde.getDossier();

		if (dossier.getLaatsteScreeningRonde().equals(screeningRonde))
		{
			if (rondeHeeftDefinitiefMdlVervolgbeleid(screeningRonde))
			{
				if (!ScreeningRondeStatus.AFGEROND.equals(screeningRonde.getStatus()))
				{
					screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
					screeningRonde.setAfgerondReden("definitieve diagnose");
				}
			}
			else if (Boolean.FALSE.equals(dossier.getAangemeld()))
			{
				dossier.setStatus(DossierStatus.ACTIEF);
				dossier.setInactiveerReden(null);
				dossier.setInactiefVanaf(null);
			}
			hibernateService.saveOrUpdate(screeningRonde);
		}
	}

	@Override
	public boolean rondeHeeftDefinitiefMdlVervolgbeleid(ColonScreeningRonde screeningRonde)
	{
		var actueelsteVervolgbeleid = getActueelsteVervolgbeleid(screeningRonde);
		return MdlVervolgbeleid.isDefinitief(actueelsteVervolgbeleid);
	}

	private void bepaalEnSetUitnodigingsinterval(MdlVerslag verslag, ColonDossier dossier)
	{
		dossierBaseService.setVolgendeUitnodigingVoorVerslag(verslag);
		hibernateService.saveOrUpdate(dossier);
	}

	private MdlVervolgbeleid getActueelsteVervolgbeleid(ColonScreeningRonde screeningRonde)
	{
		var actueelsteVerslag = mdlVerslagRepository.findFirst(heeftScreeningRondeInMdlVerslag(screeningRonde).and(heeftVerslagStatus(VerslagStatus.AFGEROND)),
			Sort.by(Sort.Order.desc(MdlVerslag_.DATUM_ONDERZOEK)));
		return actueelsteVerslag.map(mdlVerslag -> dossierBaseService.getVervolgbeleid(mdlVerslag)).orElse(null);
	}

	private boolean complicatieUnkown(Client client, MdlVerslag mdlVerslag, Date complicatieDatum, ComplicatieErnst complicatieErnst, ComplicatieMoment complicatieMoment,
		ComplicatieSoort complicatieSoort)
	{
		boolean complicatieUnkown = false;
		if (complicatieDatum != null && complicatieErnst != null && complicatieMoment != null && complicatieSoort != null)
		{
			complicatieUnkown = true;
			for (Complicatie complicatie : client.getComplicaties())
			{
				if (complicatieDatum.equals(complicatie.getDatum()) && complicatieErnst.equals(complicatie.getErnst()) && complicatieMoment.equals(complicatie.getMoment())
					&& complicatieSoort.equals(complicatie.getSoort()))
				{
					boolean changed = false;
					if (complicatie.getMdlverslag() == null)
					{
						complicatie.setMdlverslag(mdlVerslag);
						changed = true;
					}
					if (!complicatie.isActief())
					{
						complicatie.setActief(true);
						changed = true;
					}
					if (changed)
					{
						hibernateService.saveOrUpdate(complicatie);
					}
					complicatieUnkown = false;
					break;
				}
			}
		}
		return complicatieUnkown;
	}

	@Override
	@Transactional
	public void onAfterVerwerkVerslagContent(MdlVerslag verslag)
	{

		MdlVerslagContent content = verslag.getVerslagContent();
		var aanvangVerrichting = content.getVerrichting().getAanvangVerrichting();
		if (content.getColoscopieMedischeObservatie() != null)
		{
			MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid = content.getColoscopieMedischeObservatie().getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();

			if (defVervolgbeleid != null)
			{
				fixSurveillanceColoscopie(defVervolgbeleid, DateUtil.toLocalDate(aanvangVerrichting));
				fixDefinitiefVervolgbeleid(content.getColoscopieMedischeObservatie());
			}
			fixAfbrekenColoscopie(content.getColoscopieMedischeObservatie());
			bepaalEnSetUitnodigingsinterval(verslag, verslag.getScreeningRonde().getDossier());
		}
		verslag.setDatumOnderzoek(aanvangVerrichting);

		if (verslag.getDatumOnderzoek() == null || complicatieService.magComplicatieVastleggen(verslag.getDatumOnderzoek()))
		{
			maakClientComplicatieUitMdlVerslag(verslag, content);
		}
		else if (content.getVerrichting() != null && CollectionUtils.isNotEmpty(content.getVerrichting().getIncidentcomplicatie()))
		{
			List<MdlIncidentcomplicatie> incidentcomplicaties = content.getVerrichting().getIncidentcomplicatie();
			if (incidentcomplicaties.get(0).getId() != null)
			{
				hibernateService.deleteAll(incidentcomplicaties);
			}
			incidentcomplicaties.clear();
			hibernateService.saveOrUpdate(content.getVerrichting());
		}
		converteerVolledigheidWegnameMateriaal(verslag);
	}

	@Override
	@Transactional
	public void onAfterVerwerkVerslagContent(PaVerslag verslag)
	{

		PaVerslagContent content = verslag.getVerslagContent();
		if (content.getVerrichting() != null)
		{
			verslag.setDatumOnderzoek(content.getVerrichting().getAanvangVerrichting());
			if (verslag.getDatumOnderzoek() == null)
			{
				verslag.setDatumOnderzoek(content.getVerrichting().getEindeVerrichting());
			}
		}
		DSValue consultrevisieMateriaalAangevraagd = verslag.getVerslagContent().getPathologieMedischeObservatie().getConsultrevisieMateriaalAangevraagd();

		if (consultrevisieMateriaalAangevraagd != null)
		{
			for (PaPathologieProtocolColonbioptperPoliep paPathologieProtocolColonbioptperPoliep : verslag.getVerslagContent().getPathologieProtocolColonbioptperPoliep())
			{
				paPathologieProtocolColonbioptperPoliep.setConsultMateriaalAangevraagd(consultrevisieMateriaalAangevraagd);
			}
			verslag.getVerslagContent().getPathologieMedischeObservatie().setConsultrevisieMateriaalAangevraagd(null);
		}
	}

	@Override
	public List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client)
	{
		var spec = heeftColonDossier(client.getColonDossier()).and(heeftVerslagStatus(VerslagStatus.AFGEROND));
		return mdlVerslagRepository.findAll(spec, Sort.by(DESC, ColonVerslag_.DATUM_ONDERZOEK));
	}

	@Override
	public Optional<MdlVerslag> getMdlVerslagUitRonde(ColonScreeningRonde ronde)
	{
		var spec = heeftScreeningRondeInMdlVerslag(ronde).and(heeftVerslagStatus(VerslagStatus.AFGEROND).and(heeftGeenOntvangenBericht()));
		return mdlVerslagRepository.findFirst(spec, Sort.by(Sort.Direction.DESC, MdlVerslag_.DATUM_VERWERKT));
	}

	@Override
	public MdlVerslag maakMdlVerslagVoorAfspraak(ColonIntakeAfspraak afspraak)
	{
		var ronde = afspraak.getColonScreeningRonde();

		var verslag = new MdlVerslag();
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verslag.setScreeningRonde(ronde);
		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(VerslagType.MDL);

		var content = new MdlVerslagContent();
		verslag.setVerslagContent(content);
		content.setVerslag(verslag);

		var coloscopieMedischeObservatie = new MdlColoscopieMedischeObservatie();
		content.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
		coloscopieMedischeObservatie.setVerslagContent(content);

		var definitiefVervolgbeleidVoorBevolkingsonderzoekg = new MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		coloscopieMedischeObservatie.setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(definitiefVervolgbeleidVoorBevolkingsonderzoekg);
		definitiefVervolgbeleidVoorBevolkingsonderzoekg.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);

		var verrichting = new MdlVerrichting();
		verrichting.setVerslagContent(content);
		content.setVerrichting(verrichting);

		ronde.getVerslagen().add(verslag);
		return verslag;
	}

	@Override
	@Transactional
	public void handmatigMdlVerslagOpslaan(MdlVerslag verslag, InstellingGebruiker ingelogdeGebruiker)
	{
		verslag.setInvoerder(ingelogdeGebruiker);
		verslag.getVerslagContent().setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.MDL));
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verwerkInDossier(verslag);
		onAfterVerwerkVerslagContent(verslag);
		var client = verslag.getScreeningRonde().getDossier().getClient();
		var organisatie = ingelogdeGebruiker.getOrganisatie();
		var melding = "Handmatige invoer eindconclusie en vervolgbeleid.";
		melding += " Datum onderzoek: " + DateUtil.formatShortDate(verslag.getVerslagContent().getVerrichting().getAanvangVerrichting());
		melding += " Coloscopie locatie: " + organisatie.getNaam();
		logService.logGebeurtenis(LogGebeurtenis.MDL_VERSLAG_VAST, ingelogdeGebruiker, client, melding,
			Bevolkingsonderzoek.COLON);
	}

	private void converteerVolledigheidWegnameMateriaal(MdlVerslag mdlVerslag)
	{
		if (mdlVerslag.getVerslagContent().getVersie().ordinal() < VerslagGeneratie.V4.ordinal())
		{
			DSValue inTotoCompleet = verslagService.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");
			DSValue piecemealCompleet = verslagService.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_verwijdering_compleet");
			DSValue incompleet = verslagService.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");

			DSValue inToto = verslagService.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_method_of_excision");
			DSValue piecemeal = verslagService.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_method_of_excision");
			DSValue radicaal = verslagService.getDsValue("255612005", "2.16.840.1.113883.6.96", "vs_extent");
			DSValue irradicaal = verslagService.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_extent");
			for (MdlLaesiecoloscopiecentrum mdlLaesiecoloscopiecentrum : mdlVerslag.getVerslagContent().getLaesiecoloscopiecentrum())
			{
				MdlPoliep poliep = mdlLaesiecoloscopiecentrum.getPoliep();
				DSValue volledigheidWegnameMateriaal = poliep.getVolledigheidWegnameMateriaal();
				if (volledigheidWegnameMateriaal != null)
				{
					if (volledigheidWegnameMateriaal.equals(inTotoCompleet))
					{
						poliep.setMethodeVanVerwijderen(inToto);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(piecemealCompleet))
					{
						poliep.setMethodeVanVerwijderen(piecemeal);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(incompleet))
					{
						poliep.setResultaatVerwijdering(irradicaal);
					}
					poliep.setVolledigheidWegnameMateriaal(null);
				}
			}
		}
	}

	private void fixAfbrekenColoscopie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		if (coloscopieMedischeObservatie.getRedenAfbrekingColoscopie() != null)
		{
			List<DSValue> oudeWaarden = new ArrayList<>();
			for (DSValue afbrekenColoscopie : coloscopieMedischeObservatie.getRedenAfbrekingColoscopie())
			{
				if (afbrekenColoscopie.getCode().equals("6") || afbrekenColoscopie.getCode().equals("7"))
				{
					oudeWaarden.add(afbrekenColoscopie);
				}
			}
			if (!oudeWaarden.isEmpty())
			{
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().removeAll(oudeWaarden);
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().add(verslagService.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
			}

		}
		DSValue redenCoecumNietBereikt = coloscopieMedischeObservatie.getRedenCoecumNietBereikt();
		if (redenCoecumNietBereikt != null && (redenCoecumNietBereikt.getCode().equals("6") || redenCoecumNietBereikt.getCode().equals("7")))
		{
			coloscopieMedischeObservatie.setRedenCoecumNietBereikt(verslagService.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
		}
	}

	private void fixDefinitiefVervolgbeleid(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidgroep = coloscopieMedischeObservatie.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		DSValue vervolgbeleidNavAfbrekingColoscopie = coloscopieMedischeObservatie.getVervolgbeleidNavAfbrekingColoscopie();
		if (vervolgbeleidNavAfbrekingColoscopie != null)
		{
			if (vervolgbeleidNavAfbrekingColoscopie.getCode().equals("2"))
			{

				definitiefVervolgbeleidgroep.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(
					verslagService.getDsValue("418714002", "2.16.840.1.113883.6.96", "vs_vervolgbeleid"));
			}
			else if (definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek() != null && (vervolgbeleidNavAfbrekingColoscopie.getCode().equals("1")
				&& definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek().getCode().equals("183851006")
				|| definitiefVervolgbeleidgroep.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek().getCode().equals("73761001")))
			{

				definitiefVervolgbeleidgroep
					.setDefinitiefVervolgbeleidVoorBevolkingsonderzoek(verslagService.getDsValue("73761001:260870009=64695001", "2.16.840.1.113883.6.96", "vs_vervolgbeleid"));
			}
		}
	}

	private void fixSurveillanceColoscopie(MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg defVervolgbeleid, LocalDate aanvangVerrichting)
	{
		MdlVerslagContent content = defVervolgbeleid.getColoscopieMedischeObservatie().getVerslagContent();

		Quantity surveillancecoloscopieCda = defVervolgbeleid.getPeriodeVervolgSurveillancecoloscopieCda();
		String value = null;
		String unit = null;
		if (surveillancecoloscopieCda != null)
		{
			value = surveillancecoloscopieCda.getValue();
			unit = surveillancecoloscopieCda.getUnit();
		}

		if (value != null && StringUtils.isBlank(unit) && aanvangVerrichting != null && content.getVersie().ordinal() < VerslagGeneratie.V3.ordinal())
		{
			try
			{
				LocalDate periodeDatum = null;
				if (value.length() == 6)
				{
					periodeDatum = DateUtil.parseLocalDateForPattern(value + "01", "yyyyMMdd");
				}
				else if (value.length() == 4)
				{
					periodeDatum = DateUtil.parseLocalDateForPattern(value, "yyyy").withMonth(aanvangVerrichting.getMonthValue());
				}
				if (periodeDatum != null)
				{
					periodeDatum = periodeDatum.withDayOfMonth(1);
					int months = DateUtil.getMonthsBetweenDates(DateUtil.toUtilDate(aanvangVerrichting), DateUtil.toUtilDate(periodeDatum));
					value = "" + months;
					unit = "maand";
					surveillancecoloscopieCda.setUnit(unit);
					surveillancecoloscopieCda.setValue(value);
				}
			}
			catch (DateTimeParseException e)
			{
				LOG.error("Fout bij vertalen surveillancecoloscopie naar waarde en unit " + value, e);
			}
		}

		boolean cdaBericht = content.getVerslag().getOntvangenBericht() != null;
		if (defVervolgbeleid.getPeriodeVervolgSurveillancescopie() == null)
		{

			if (StringUtils.isNotBlank(unit) && StringUtils.isNotBlank(value) && cdaBericht)
			{
				DSValue dsValue = null;
				try
				{
					int codeValue = Double.valueOf(value).intValue();
					String codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226";
					String valueSetName = "vs_periode_vervolg";
					String code2Jaar = "13";
					String code3Jaar = "14";
					String code5Jaar = "15";
					if (unit.equals("maand") || unit.equals("mo"))
					{
						if (codeValue > 0 && codeValue <= 12)
						{
							dsValue = verslagService.getDsValue(codeValue + "", codeSystem, valueSetName);
						}
						else if (codeValue == 24)
						{
							dsValue = verslagService.getDsValue(code2Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 36)
						{
							dsValue = verslagService.getDsValue(code3Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 60)
						{
							dsValue = verslagService.getDsValue(code5Jaar, codeSystem, valueSetName);
						}
					}
					else if (unit.equals("jaar"))
					{
						if (codeValue == 2)
						{
							dsValue = verslagService.getDsValue(code2Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 3)
						{
							dsValue = verslagService.getDsValue(code3Jaar, codeSystem, valueSetName);
						}
						else if (codeValue == 5)
						{
							dsValue = verslagService.getDsValue(code5Jaar, codeSystem, valueSetName);
						}
					}

					if (dsValue != null)
					{
						defVervolgbeleid.setPeriodeVervolgSurveillancescopie(dsValue);
					}
				}
				catch (NumberFormatException e)
				{
					LOG.error("Fout bij vertalen surveillancecoloscopie van cda waarde naar intvalue " + value);
				}
			}
			else
			{

				if (surveillancecoloscopieCda != null)
				{
					surveillancecoloscopieCda.setUnit(null);
					surveillancecoloscopieCda.setValue(null);
				}
			}
		}
		else
		{

			DSValue periodeVervolgSurveillancecoloscopie = defVervolgbeleid.getPeriodeVervolgSurveillancescopie();
			unit = "maand";
			String code = periodeVervolgSurveillancecoloscopie.getCode();
			switch (code)
			{
			case "1":
			case "2":
			case "3":
			case "4":
			case "5":
			case "6":
			case "7":
			case "8":
			case "9":
			case "10":
			case "12":
				value = Double.valueOf(code).intValue() + "";
				break;
			case "13":
				value = "24"; 
				break;
			case "14":
				value = "36"; 
				break;
			case "15":
				value = "60"; 
				break;
			default:
				unit = null;
				value = null;
			}

			if (surveillancecoloscopieCda == null)
			{
				surveillancecoloscopieCda = new Quantity();
				defVervolgbeleid.setPeriodeVervolgSurveillancecoloscopieCda(surveillancecoloscopieCda);
			}
			if (StringUtils.isNotBlank(unit) && StringUtils.isNotBlank(value))
			{
				surveillancecoloscopieCda.setUnit(unit);
				surveillancecoloscopieCda.setValue(value);
			}
		}
	}

	@Override
	@Transactional
	public void ontkoppelOfVerwijderComplicaties(MdlVerslag mdlVerslag)
	{
		Client client = mdlVerslag.getScreeningRonde().getDossier().getClient();
		List<Complicatie> complToDelete = new ArrayList<>();
		for (Complicatie complicatie : client.getComplicaties())
		{
			if (mdlVerslag.equals(complicatie.getMdlverslag()))
			{
				if (!Boolean.FALSE.equals(complicatie.getHandmatig()))
				{
					complicatie.setMdlverslag(null);
					hibernateService.saveOrUpdate(complicatie);
				}
				else
				{
					complToDelete.add(complicatie);
				}
			}
		}
		for (Complicatie complicatie : complToDelete)
		{
			client.getComplicaties().remove(complicatie);
			hibernateService.delete(complicatie);
		}
		hibernateService.saveOrUpdate(client);
	}

	private void maakClientComplicatieUitMdlVerslag(MdlVerslag mdlVerslag, MdlVerslagContent verslagContent)
	{
		InstellingGebruiker instellingGebruiker = mdlVerslag.getInvoerder();
		if (instellingGebruiker == null && mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers() != null)
		{
			for (InstellingGebruiker ig : mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers())
			{
				if (Boolean.TRUE.equals(ig.getActief()) && Boolean.TRUE.equals(ig.getOrganisatie().getActief())
					&& ig.getOrganisatie().equals(mdlVerslag.getUitvoerderOrganisatie()))
				{
					instellingGebruiker = ig;
					break;
				}
			}
			if (instellingGebruiker == null && OrganisatieType.ZORGINSTELLING.equals(mdlVerslag.getUitvoerderOrganisatie().getOrganisatieType()))
			{
				for (InstellingGebruiker ig : mdlVerslag.getUitvoerderMedewerker().getOrganisatieMedewerkers())
				{
					if (Boolean.TRUE.equals(ig.getActief()) && Boolean.TRUE.equals(ig.getOrganisatie().getActief()) && ig.getOrganisatie().getParent() != null
						&& Boolean.TRUE.equals(ig.getOrganisatie().getParent().getActief()) && ig.getOrganisatie().getParent().equals(mdlVerslag.getUitvoerderOrganisatie()))
					{
						instellingGebruiker = ig;
						break;
					}
				}
			}
		}

		Client client = mdlVerslag.getScreeningRonde().getDossier().getClient();
		if (verslagContent != null && verslagContent.getVerrichting() != null && CollectionUtils.isNotEmpty(verslagContent.getVerrichting().getIncidentcomplicatie()))
		{
			for (MdlIncidentcomplicatie incidentcomplicatie : verslagContent.getVerrichting().getIncidentcomplicatie())
			{
				Date complicatieDatum = mdlVerslag.getDatumOnderzoek();
				ComplicatieErnst complicatieErnst = ComplicatieErnst.getValue(incidentcomplicatie.getErnstIncidentcomplicatie());
				ComplicatieMoment complicatieMoment = ComplicatieMoment.BINNEN_24_UUR;
				ComplicatieSoort complicatieSoort = ComplicatieSoort.getValue(incidentcomplicatie.getTypeIncidentcomplicatie());

				if (complicatieUnkown(client, mdlVerslag, complicatieDatum, complicatieErnst, complicatieMoment, complicatieSoort))
				{
					Complicatie complicatie = new Complicatie();
					complicatie.setActief(true);
					complicatie.setClient(client);
					complicatie.setHandmatig(false);
					complicatie.setInstellingGebruiker(instellingGebruiker);
					complicatie.setDatum(complicatieDatum);
					complicatie.setErnst(complicatieErnst);
					complicatie.setMoment(complicatieMoment);
					complicatie.setMdlverslag(mdlVerslag);

					complicatie.setSoort(complicatieSoort);
					client.getComplicaties().add(complicatie);

					hibernateService.saveOrUpdate(complicatie);
				}
			}
			hibernateService.saveOrUpdate(client);
		}
	}

	@Override
	public ColonScreeningRonde getValideScreeningsRonde(Client client, Verslag oudeVersieVerslag, Date onderzoeksdatum)
	{
		ColonDossier dossier = client.getColonDossier();
		ColonScreeningRonde rondeVoorVerslag = null;
		if (oudeVersieVerslag instanceof ColonVerslag<?> verslag)
		{

			rondeVoorVerslag = verslag.getScreeningRonde();
		}
		if (rondeVoorVerslag == null)
		{
			List<ColonScreeningRonde> screeningRondes = new ArrayList<>(dossier.getScreeningRondes());
			Collections.sort(screeningRondes, new PropertyComparator<ColonScreeningRonde>("creatieDatum", false, false));

			boolean heeftOngunstigeUitslagOuderDanOnderzoeksdatum = onderzoeksdatum == null
				|| screeningRondes.stream().anyMatch(r -> isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(r, onderzoeksdatum));

			if (heeftOngunstigeUitslagOuderDanOnderzoeksdatum)
			{
				rondeVoorVerslag = screeningRondes.stream().filter(r -> ColonScreeningRondeUtil.getEersteOngunstigeTest(r) != null || r.getOpenUitnodiging() != null).findFirst()
					.orElse(null);
			}
		}
		return rondeVoorVerslag;
	}

	private boolean isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(ColonScreeningRonde ronde, Date onderzoeksdatum)
	{
		IFOBTTest eersteOngunstigeTest = ColonScreeningRondeUtil.getEersteOngunstigeTest(ronde);
		return eersteOngunstigeTest != null && eersteOngunstigeTest.getVerwerkingsDatum().compareTo(onderzoeksdatum) < 0;
	}
}
