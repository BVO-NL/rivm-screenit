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
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.persistence.criteria.From;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.colon.CapaciteitsPercWijziging;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.algemeen.PostcodeGebiedRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.colon.ColonUitnodigingsgebiedService;
import nl.rivm.screenit.specification.algemeen.PostcodeGebiedSpecification;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.PercentageUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static jakarta.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification.heeftCreatieDatum;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.clientUitnodigingBase;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.u1Base;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.u2Base;

@Slf4j
@Service
@RequiredArgsConstructor
public class ColonUitnodigingsgebiedServiceImpl implements ColonUitnodigingsgebiedService
{

	private final SimplePreferenceService simplePreferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final ColonUitnodigingService uitnodigingService;

	private final PostcodeGebiedRepository postcodeGebiedRepository;

	private final ClientRepository clientRepository;

	@Override
	public List<PostcodeGebied> findOverlappendePostcodeGebieden(PostcodeGebied postcodeGebied)
	{
		var specification = PostcodeGebiedSpecification.isAnderPostcodeGebied(postcodeGebied).and(PostcodeGebiedSpecification.heeftOverlappendePostcode(postcodeGebied));
		return postcodeGebiedRepository.findAll(specification);
	}

	@Override
	public List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(UitnodigingsGebied uitnodigingsGebied, Map<String, Integer> nieuweAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen)
	{
		var wijzigingen = new ArrayList<CapaciteitsPercWijziging>();
		var benodigdeIntakecapaciteiten = new HashMap<Long, BigDecimal>();

		var totaal = nieuweAdherentiePercentages.values().stream().reduce(0, Integer::sum);

		if (totaal != 10000 && totaal != 0)
		{
			throw new IllegalStateException("error.totaal.adherentie.niet.100.of.0.procent");
		}

		for (var koppelingMetIntakelocatie : uitnodigingsGebied.getVerdeling())
		{
			if (verwijderdeKoppelingen.contains(koppelingMetIntakelocatie)
				|| !koppelingMetIntakelocatie.getPercentageAdherentie().equals(nieuweAdherentiePercentages.get(getUniekIdOf(koppelingMetIntakelocatie))))
			{
				bepaalWijzigingen(nieuweAdherentiePercentages, verwijderdeKoppelingen, wijzigingen, benodigdeIntakecapaciteiten,
					koppelingMetIntakelocatie.getIntakelocatie().getCapaciteitVerdeling());
			}
		}

		if (wijzigingen.isEmpty() && uitnodigingsGebied.getVerdeling().size() > verwijderdeKoppelingen.size())
		{
			throw new IllegalStateException("error.adherentie.geen.wijzigingen");
		}

		wijzigingen.sort(Comparator.comparing(CapaciteitsPercWijziging::getUitnodigingsgebied).thenComparing(CapaciteitsPercWijziging::getIntakelocatie));
		return wijzigingen;
	}

	private BigDecimal berekenBenodigdeIntakecapaciteitVoorGebied(UitnodigingsGebied uitnodigingsGebied, Integer adherentie, Map<Long, BigDecimal> benodigdeIntakecapaciteiten)
	{
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

		var uitnodigingsInterval = simplePreferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsInterval == null)
		{
			throw new IllegalStateException("Spreidingsperiode op de parameterisatie pagina is niet gezet");
		}

		var totaalBenodigdeIntakecapaciteit = benodigdeIntakecapaciteiten.get(uitnodigingsGebied.getId());
		if (totaalBenodigdeIntakecapaciteit == null)
		{

			var vandaag = currentDateSupplier.getLocalDate();
			var laatsteDagVanHuidigJaar = vandaag.with(TemporalAdjusters.lastDayOfYear());
			var alleGeboortejarenTotMetHuidigJaar = uitnodigingService.getAlleGeboortejarenTotMetHuidigJaar();

			var aantalClienten = countPersonenInUitnodigingsGebied(uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd + 1,
				laatsteDagVanHuidigJaar, alleGeboortejarenTotMetHuidigJaar);
			var fitFactor = getFitFactorVoorGebied(uitnodigingsGebied);
			LOG.info("Uitnodigingsgebied {}: aantal clienten {}", uitnodigingsGebied.getNaam(), aantalClienten);
			totaalBenodigdeIntakecapaciteit = BigDecimal.valueOf(aantalClienten).divide(fitFactor, 4, RoundingMode.HALF_UP);
			benodigdeIntakecapaciteiten.put(uitnodigingsGebied.getId(), totaalBenodigdeIntakecapaciteit);
		}

		return totaalBenodigdeIntakecapaciteit.multiply(new BigDecimal(adherentie)).divide(BigDecimal.valueOf(10000), 4, RoundingMode.HALF_UP);
	}

	@Override
	public BigDecimal getFitFactorVoorGebied(UitnodigingsGebied uitnodigingsGebied)
	{
		var percLandelijkFitRetour = simplePreferenceService.getInteger(PreferenceKey.PERCENTAGEIFOBTRETOUR.name());
		if (percLandelijkFitRetour == null)
		{
			throw new IllegalStateException("Landelijk IfobtRetourPercentage is niet gezet");
		}

		var percLandelijkFitOngunstige = simplePreferenceService.getInteger(PreferenceKey.PERCENTGAGEIFOBTONGUSTIG.name());
		if (percLandelijkFitOngunstige == null)
		{
			throw new IllegalStateException("Landelijk IfobtOngunstigePercentage is niet gezet");
		}

		var percFitRetour = percLandelijkFitRetour;

		if (uitnodigingsGebied.getGemeente().getScreeningOrganisatie() != null && uitnodigingsGebied.getGemeente().getScreeningOrganisatie().getIfobtRetourPercentage() != null)
		{
			percFitRetour = uitnodigingsGebied.getGemeente().getScreeningOrganisatie().getIfobtRetourPercentage();
		}

		if (uitnodigingsGebied.getPercentageIFobtRetour() != null)
		{
			percFitRetour = uitnodigingsGebied.getPercentageIFobtRetour();
		}

		var fitRetourFactor = BigDecimal.ZERO;
		if (percFitRetour != null && percFitRetour > 0)
		{
			fitRetourFactor = berekenFITFactor(percFitRetour);
		}

		LOG.info("Uitnodigingsgebied {}: FIT Retour {} => factor {}", uitnodigingsGebied.getNaam(),
			PercentageUtil.percentageToString(percFitRetour),
			BigDecimalUtil.decimalToString(fitRetourFactor));

		var percFitOngunstig = percLandelijkFitOngunstige;

		if (uitnodigingsGebied.getPercentageOngunstigeIfobt() != null)
		{
			percFitOngunstig = uitnodigingsGebied.getPercentageOngunstigeIfobt();
		}

		var fitOngunstigFactor = BigDecimal.ZERO;
		if (percFitOngunstig != null && percFitOngunstig > 0)
		{
			fitOngunstigFactor = berekenFITFactor(percFitOngunstig);
		}

		LOG.info("Uitnodigingsgebied {}: FIT Ongunstig {} => factor {}", uitnodigingsGebied.getNaam(),
			PercentageUtil.percentageToString(percFitOngunstig),
			BigDecimalUtil.decimalToString(fitOngunstigFactor));

		var gebiedsFactor = fitRetourFactor.multiply(fitOngunstigFactor);
		LOG.info("Uitnodigingsgebied {}: Totaal factor {}", uitnodigingsGebied.getNaam(), BigDecimalUtil.decimalToString(gebiedsFactor));

		return gebiedsFactor;
	}

	@NotNull
	private static BigDecimal berekenFITFactor(Integer percFitRetour)
	{
		return BigDecimal.valueOf(10000).divide(BigDecimal.valueOf(percFitRetour), 4, RoundingMode.HALF_UP);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigingenDoorvoeren(UitnodigingsGebied uitnodigingsgebied, Map<String, Integer> nieuweAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen, List<CapaciteitsPercWijziging> wijzigingen, InstellingGebruiker ingelogdeGebruiker)
	{
		var melding = "Gewijzigde capaciteitspercentages:<br>";
		var adherentieMelding = "Adherentieverdeling gewijzigd voor gebied: " + uitnodigingsgebied.getNaam() + "<br>";
		for (var wijziging : wijzigingen)
		{
			var koppelingToChange = getKoppelingToChange(wijziging, uitnodigingsgebied);
			if (koppelingToChange != null)
			{
				melding = wijzigKoppeling(koppelingToChange, wijziging, melding);
			}
		}

		hibernateService.saveOrUpdate(uitnodigingsgebied);

		for (var verwijderdeKoppeling : verwijderdeKoppelingen)
		{
			var intakelocatie = verwijderdeKoppeling.getIntakelocatie();
			melding += verwijderdeKoppeling.getUitnodigingsGebied().getNaam() + "/" + intakelocatie.getNaam() + " verwijderd";
			uitnodigingsgebied.getVerdeling().remove(verwijderdeKoppeling);
			hibernateService.saveOrUpdate(uitnodigingsgebied);
			intakelocatie.getCapaciteitVerdeling().remove(verwijderdeKoppeling);
			hibernateService.delete(verwijderdeKoppeling);
			hibernateService.saveOrUpdate(intakelocatie);
		}

		logService.logGebeurtenis(LogGebeurtenis.ADHERENTIE_AANEGEPAST, ingelogdeGebruiker, adherentieMelding + melding, Bevolkingsonderzoek.COLON);
		hibernateService.getHibernateSession().flush();
		adherentieMelding = valideerAdherentieVanGewijzigdeGebieden(Set.of(uitnodigingsgebied));
		if (StringUtils.isNotBlank(adherentieMelding))
		{
			throw new IllegalStateException(adherentieMelding);
		}
	}

	@Override
	public String valideerAdherentieVanGewijzigdeGebieden(Set<UitnodigingsGebied> gewijzigdeGebieden)
	{
		var melding = "";
		for (var gebied : gewijzigdeGebieden.stream().sorted(Comparator.comparing(UitnodigingsGebied::getNaam)).collect(Collectors.toList()))
		{
			var koppelingen = gebied.getVerdeling();
			var totaalAdherentie = koppelingen.stream().map(ColoscopieCentrumColonCapaciteitVerdeling::getPercentageAdherentie).reduce(0, Integer::sum);
			var aantalVerdelingen = koppelingen.size();
			if ((aantalVerdelingen > 0 || totaalAdherentie > 0) && (aantalVerdelingen == 0 || totaalAdherentie < 10000))
			{
				melding += "<br>" + gebied.getNaam() + ": " + PercentageUtil.percentageToString(totaalAdherentie);
			}
		}
		if (!melding.isEmpty())
		{
			melding = "<br>Uitnodigingsgebieden minder dan 100% (>0 koppelingen) of meer dan 0% (0 koppelingen) adherentie:" + melding;
		}
		return melding;
	}

	private ColoscopieCentrumColonCapaciteitVerdeling getKoppelingToChange(CapaciteitsPercWijziging wijziging, UitnodigingsGebied uitnodigingsgebied)
	{
		ColoscopieCentrumColonCapaciteitVerdeling koppelingToChange = null;
		main:
		for (var koppeling : uitnodigingsgebied.getVerdeling())
		{
			koppelingToChange = getKoppelingToChange(wijziging, koppeling);
			if (koppelingToChange != null)
			{
				break;
			}
			for (var innerKoppeling : koppeling.getIntakelocatie().getCapaciteitVerdeling())
			{
				koppelingToChange = getKoppelingToChange(wijziging, innerKoppeling);
				if (koppelingToChange != null)
				{
					break main;
				}
			}
		}
		return koppelingToChange;
	}

	private ColoscopieCentrumColonCapaciteitVerdeling getKoppelingToChange(CapaciteitsPercWijziging wijziging, ColoscopieCentrumColonCapaciteitVerdeling koppeling)
	{
		ColoscopieCentrumColonCapaciteitVerdeling koppelingToChange = null;
		if (koppeling.getId() != null)
		{
			if (koppeling.getId().equals(wijziging.getIlUgId()))
			{
				koppelingToChange = koppeling;
			}
		}
		else if (koppeling.getUitnodigingsGebied().getId().equals(wijziging.getUgId()) && koppeling.getIntakelocatie().getId().equals(wijziging.getIlId()))
		{
			koppelingToChange = koppeling;
		}
		return koppelingToChange;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void wijzigingenDoorvoeren(ColonIntakelocatie intakelocatie, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen,
		List<CapaciteitsPercWijziging> wijzigingen, InstellingGebruiker ingelogdeGebruiker)
	{
		var melding = "";
		var adherentieMelding = "Verdeling gewijzigd voor intakelocatie: " + intakelocatie.getNaam() + "<br>";
		var uitnodigingsGebiedenTeControlerenOpAdherentie = new HashSet<UitnodigingsGebied>();
		for (var wijziging : wijzigingen)
		{
			ColoscopieCentrumColonCapaciteitVerdeling koppelingToChange = null;
			for (var koppeling : intakelocatie.getCapaciteitVerdeling())
			{
				koppelingToChange = getKoppelingToChange(wijziging, koppeling);

				if (koppelingToChange == null)
				{
					koppelingToChange = getKoppelingToChange(wijziging, koppeling.getUitnodigingsGebied());
				}
				if (koppelingToChange != null)
				{
					if (verwijderdeKoppelingen.contains(koppelingToChange))
					{
						koppelingToChange = null;
					}
					break;
				}
			}
			if (koppelingToChange != null)
			{
				melding = wijzigKoppeling(koppelingToChange, wijziging, melding);
				uitnodigingsGebiedenTeControlerenOpAdherentie.add(koppelingToChange.getUitnodigingsGebied());
			}
		}

		hibernateService.saveOrUpdate(intakelocatie);
		hibernateService.getHibernateSession().flush();

		for (var verwijderdeKoppeling : verwijderdeKoppelingen)
		{
			var uitnodigingsGebied = verwijderdeKoppeling.getUitnodigingsGebied();
			melding += uitnodigingsGebied.getNaam() + "/" + verwijderdeKoppeling.getIntakelocatie().getNaam() + " verwijderd";
			intakelocatie.getCapaciteitVerdeling().remove(verwijderdeKoppeling);
			hibernateService.saveOrUpdate(intakelocatie);
			uitnodigingsGebied.getVerdeling().remove(verwijderdeKoppeling);
			hibernateService.delete(verwijderdeKoppeling);
			hibernateService.saveOrUpdate(uitnodigingsGebied);
			uitnodigingsGebiedenTeControlerenOpAdherentie.add(uitnodigingsGebied);
		}

		logService.logGebeurtenis(LogGebeurtenis.ADHERENTIE_AANEGEPAST, ingelogdeGebruiker, adherentieMelding + melding, Bevolkingsonderzoek.COLON);
		hibernateService.getHibernateSession().flush();
		uitnodigingsGebiedenTeControlerenOpAdherentie.forEach(ug ->
		{
			var validatieMelding = valideerAdherentieVanGewijzigdeGebieden(Set.of(ug));
			if (StringUtils.isNotBlank(validatieMelding))
			{
				throw new IllegalStateException(validatieMelding);
			}
		});
	}

	private String wijzigKoppeling(ColoscopieCentrumColonCapaciteitVerdeling koppelingToChange, CapaciteitsPercWijziging wijziging, String melding)
	{
		var uitnodigingsGebied = koppelingToChange.getUitnodigingsGebied();
		var intakelocatieNaam = koppelingToChange.getIntakelocatie().getNaam();
		var oudeCapaciteitPer = koppelingToChange.getPercentageCapaciteit();
		if (wijziging.getNieuwCapPer().compareTo(oudeCapaciteitPer) != 0)
		{
			melding += "Capaciteit: " + uitnodigingsGebied.getNaam() + "/" + intakelocatieNaam + ": " + PercentageUtil.percentageToString(oudeCapaciteitPer) + "->";

			koppelingToChange.setPercentageCapaciteit(wijziging.getNieuwCapPer());

			melding = voegPercentageToeAanMelding(melding, koppelingToChange, oudeCapaciteitPer);
		}
		var oudeAdherentiePer = koppelingToChange.getPercentageAdherentie();
		if (wijziging.getNieuwAdhPer().compareTo(oudeAdherentiePer) != 0)
		{
			melding += "Adherentie: " + uitnodigingsGebied.getNaam() + "/" + intakelocatieNaam + ": " + PercentageUtil.percentageToString(oudeAdherentiePer) + "->";

			koppelingToChange.setPercentageAdherentie(wijziging.getNieuwAdhPer());

			melding = voegPercentageToeAanMelding(melding, koppelingToChange, oudeAdherentiePer);
		}
		hibernateService.saveOrUpdate(koppelingToChange);
		return melding;
	}

	@NotNull
	private static String voegPercentageToeAanMelding(String melding, ColoscopieCentrumColonCapaciteitVerdeling koppelingToChange, Integer percentage)
	{
		melding += PercentageUtil.percentageToString(percentage);
		if (koppelingToChange.getId() == null)
		{
			melding += "(nieuw)";
		}
		melding += "<br>";
		return melding;
	}

	@Override
	public List<CapaciteitsPercWijziging> bepaalCapaciteitsWijzigingen(ColonIntakelocatie intakelocatie, Map<String, Integer> nieuweAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen)
	{
		var wijzigingen = new ArrayList<CapaciteitsPercWijziging>();

		var benodigdeIntakecapaciteiten = new HashMap<Long, BigDecimal>();

		var eenGebied = intakelocatie.getCapaciteitVerdeling().size() == 1
			&& intakelocatie.getCapaciteitVerdeling().get(0).getUitnodigingsGebied().getVerdeling().size() == 1;
		var onzichtbareAdherentieWijzigingen = new ArrayList<String>();

		for (var koppelingMetGebied : intakelocatie.getCapaciteitVerdeling())
		{
			var nieuweAdherentie = getAdherentiePercentage(koppelingMetGebied, verwijderdeKoppelingen, nieuweAdherentiePercentages);

			if (eenGebied && nieuweAdherentie != 10000 && nieuweAdherentie != 0)
			{
				throw new IllegalStateException("error.totaal.adherentie.niet.100.of.0.procent");
			}
			var adherentieVerschil = koppelingMetGebied.getPercentageAdherentie() - nieuweAdherentie;
			if (adherentieVerschil != 0)
			{
				var uitnodigingsGebied = koppelingMetGebied.getUitnodigingsGebied();
				herberekenAdherentieVoorAndereIntakelocaties(intakelocatie, nieuweAdherentiePercentages, verwijderdeKoppelingen, wijzigingen,
					benodigdeIntakecapaciteiten, onzichtbareAdherentieWijzigingen, adherentieVerschil, uitnodigingsGebied);
			}
		}

		if (wijzigingen.isEmpty())
		{
			throw new IllegalStateException("error.adherentie.geen.wijzigingen");
		}

		Collections.sort(wijzigingen, Comparator.comparing(CapaciteitsPercWijziging::getUitnodigingsgebied).thenComparing(CapaciteitsPercWijziging::getIntakelocatie));
		for (String onzichtbareAdherentieWijziging : onzichtbareAdherentieWijzigingen)
		{
			nieuweAdherentiePercentages.remove(onzichtbareAdherentieWijziging);
		}
		return wijzigingen;
	}

	private void herberekenAdherentieVoorAndereIntakelocaties(ColonIntakelocatie intakelocatieRoot, Map<String, Integer> nieuweAdherentiePercentages,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen, List<CapaciteitsPercWijziging> wijzigingen,
		Map<Long, BigDecimal> benodigdeIntakecapaciteiten, List<String> onzichtbareAdherentieWijzigingen, int adherentieVerschil, UitnodigingsGebied uitnodigingsGebied)
	{
		var totaalOverigeAdherentie = bepaalAdherentieVanAndereIntakelocaties(intakelocatieRoot, adherentieVerschil, uitnodigingsGebied);
		var nieuwTotaalAdherentieGewijzigdGebied = 0;
		for (var koppelingNaarAndereIntakelocatie : uitnodigingsGebied.getVerdeling())
		{
			if (!intakelocatieRoot.equals(koppelingNaarAndereIntakelocatie.getIntakelocatie()))
			{
				var huidigeAdherentiePercentage = getAdherentiePercentage(koppelingNaarAndereIntakelocatie, verwijderdeKoppelingen, nieuweAdherentiePercentages);
				var nieuweAdherentiePercentage = BigDecimal.valueOf(huidigeAdherentiePercentage).add(BigDecimal.valueOf(huidigeAdherentiePercentage)
					.multiply(BigDecimal.valueOf(adherentieVerschil)).divide(BigDecimal.valueOf(totaalOverigeAdherentie), RoundingMode.HALF_UP));
				var uniekIdOfKoppelingAndereIntakelocatie = getUniekIdOf(koppelingNaarAndereIntakelocatie);
				nieuweAdherentiePercentages.put(uniekIdOfKoppelingAndereIntakelocatie, nieuweAdherentiePercentage.intValue());
				onzichtbareAdherentieWijzigingen.add(uniekIdOfKoppelingAndereIntakelocatie);
				nieuwTotaalAdherentieGewijzigdGebied += nieuweAdherentiePercentage.intValue();
			}
			else
			{
				nieuwTotaalAdherentieGewijzigdGebied += getAdherentiePercentage(koppelingNaarAndereIntakelocatie, verwijderdeKoppelingen, nieuweAdherentiePercentages);
			}
		}
		corrigeerAdherentie(nieuweAdherentiePercentages, verwijderdeKoppelingen, uitnodigingsGebied, nieuwTotaalAdherentieGewijzigdGebied);
		uitnodigingsGebied.getVerdeling().forEach(koppelingNaarIntakelocatie ->
			bepaalWijzigingen(nieuweAdherentiePercentages, verwijderdeKoppelingen, wijzigingen, benodigdeIntakecapaciteiten,
				koppelingNaarIntakelocatie.getIntakelocatie().getCapaciteitVerdeling()));
	}

	private void corrigeerAdherentie(Map<String, Integer> nieuweAdherentiePercentages, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen,
		UitnodigingsGebied uitnodigingsGebied, int nieuwTotaalAdherentieGewijzigdGebied)
	{
		if (nieuwTotaalAdherentieGewijzigdGebied != 0 && nieuwTotaalAdherentieGewijzigdGebied != 10000)
		{
			var afwijking = 10000 - nieuwTotaalAdherentieGewijzigdGebied;
			var koppelingMetHoogsteAdherentieOptional = uitnodigingsGebied.getVerdeling().stream()
				.max(Comparator.comparing(koppeling -> getAdherentiePercentage(koppeling, verwijderdeKoppelingen, nieuweAdherentiePercentages)));
			koppelingMetHoogsteAdherentieOptional.ifPresent(
				koppeling -> nieuweAdherentiePercentages.put(getUniekIdOf(koppeling),
					getAdherentiePercentage(koppeling, verwijderdeKoppelingen, nieuweAdherentiePercentages) + afwijking));
		}
	}

	private static int bepaalAdherentieVanAndereIntakelocaties(ColonIntakelocatie intakelocatieRoot, int adherentieVerschil, UitnodigingsGebied uitnodigingsGebied)
	{
		var totaalAdherentie = 0;
		for (var koppelingAndereIntakelocatie : uitnodigingsGebied.getVerdeling())
		{
			if (!intakelocatieRoot.equals(koppelingAndereIntakelocatie.getIntakelocatie()))
			{
				totaalAdherentie += koppelingAndereIntakelocatie.getPercentageAdherentie();
			}
		}
		if (totaalAdherentie == 0 && adherentieVerschil != -10000)
		{
			throw new IllegalStateException("error.adherentie.kan.niet.verdelen," + uitnodigingsGebied.getNaam());
		}
		LOG.info("Totaal overige adherentie in UG '{}' (behalve voor IL '{}'): {}", uitnodigingsGebied.getNaam(), intakelocatieRoot.getNaam(), totaalAdherentie);
		return totaalAdherentie;
	}

	private Integer getAdherentiePercentage(ColoscopieCentrumColonCapaciteitVerdeling koppeling,
		List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeVerdelingen, Map<String, Integer> nieuweAdherentiePercentages)
	{
		Integer adherentie = 0;
		if (!verwijderdeVerdelingen.contains(koppeling))
		{
			adherentie = nieuweAdherentiePercentages.get(getUniekIdOf(koppeling));
			if (adherentie == null)
			{
				adherentie = koppeling.getPercentageAdherentie();
			}
		}
		return adherentie;
	}

	private void bepaalWijzigingen(Map<String, Integer> nieuweAdherentiePercentages, List<ColoscopieCentrumColonCapaciteitVerdeling> verwijderdeKoppelingen,
		List<CapaciteitsPercWijziging> wijzigingen, Map<Long, BigDecimal> benodigdeIntakecapaciteiten, List<ColoscopieCentrumColonCapaciteitVerdeling> koppelingen)
	{
		var benodigdeIntakecapaciteitenPerGebied = new HashMap<Long, BigDecimal>();
		var totaalIntakecapaciteitIntakelocatie = BigDecimal.ZERO;
		for (var koppeling : koppelingen)
		{
			var adherentie = getAdherentiePercentage(koppeling, verwijderdeKoppelingen, nieuweAdherentiePercentages);
			var berekendeBenodigdeIntakecapaciteitVoorGebied = berekenBenodigdeIntakecapaciteitVoorGebied(koppeling.getUitnodigingsGebied(), adherentie,
				benodigdeIntakecapaciteiten);
			totaalIntakecapaciteitIntakelocatie = totaalIntakecapaciteitIntakelocatie.add(berekendeBenodigdeIntakecapaciteitVoorGebied);
			benodigdeIntakecapaciteitenPerGebied.put(koppeling.getUitnodigingsGebied().getId(), berekendeBenodigdeIntakecapaciteitVoorGebied);
		}

		for (var koppeling : koppelingen)
		{
			var intakelocatie = koppeling.getIntakelocatie();
			var uitnodigingsGebied = koppeling.getUitnodigingsGebied();

			var wijziging = new CapaciteitsPercWijziging();
			wijziging.setIlUgId(koppeling.getId());
			wijziging.setUgId(uitnodigingsGebied.getId());
			wijziging.setUitnodigingsgebied(uitnodigingsGebied.getNaam());
			wijziging.setIlId(intakelocatie.getId());
			wijziging.setIntakelocatie(intakelocatie.getNaam());
			wijziging.setOudCapPer(koppeling.getPercentageCapaciteit());

			wijziging.setOudBerekendeIntakes(berekenBenodigdeIntakecapaciteitVoorGebied(uitnodigingsGebied, koppeling.getPercentageAdherentie(),
				benodigdeIntakecapaciteiten));

			var berekendeBenodigdeIntakecapaciteitVoorGebied = benodigdeIntakecapaciteitenPerGebied.get(uitnodigingsGebied.getId());

			var nieuweCapaciteitspercentage = BigDecimal.ZERO;
			if (totaalIntakecapaciteitIntakelocatie.compareTo(BigDecimal.ZERO) > 0)
			{
				nieuweCapaciteitspercentage = berekendeBenodigdeIntakecapaciteitVoorGebied.multiply(BigDecimal.valueOf(10000)).divide(totaalIntakecapaciteitIntakelocatie, 4,
					RoundingMode.HALF_UP);
			}

			wijziging.setNieuwCapPer(nieuweCapaciteitspercentage.intValue());
			wijziging.setNieuwBerekendeIntakes(berekendeBenodigdeIntakecapaciteitVoorGebied);
			wijziging.setOudAdhPer(koppeling.getPercentageAdherentie());
			wijziging.setNieuwAdhPer(getAdherentiePercentage(koppeling, verwijderdeKoppelingen, nieuweAdherentiePercentages));
			Integer aantalGeprognostiseerdeAfspraakslots = intakelocatie.getAantalGeprognostiseerdeRoosterblokken();
			if (aantalGeprognostiseerdeAfspraakslots != null)
			{
				var prognoseVanRestVanJaar = bepaalPrognoseVoorRestVanJaar(aantalGeprognostiseerdeAfspraakslots);

				wijziging
					.setOudIntakesProg(
						prognoseVanRestVanJaar.multiply(BigDecimal.valueOf(koppeling.getPercentageCapaciteit())).divide(BigDecimal.valueOf(10000), 2, RoundingMode.HALF_UP));
				wijziging.setNieuwIntakesProg(prognoseVanRestVanJaar.multiply(nieuweCapaciteitspercentage).divide(BigDecimal.valueOf(10000), 2, RoundingMode.HALF_UP));
			}

			wijzigingen.remove(wijziging);
			wijzigingen.add(wijziging);
		}
		wijzigingen.forEach(w -> LOG.info(ToStringBuilder.reflectionToString(w, ToStringStyle.SHORT_PREFIX_STYLE)));
	}

	@NotNull
	private BigDecimal bepaalPrognoseVoorRestVanJaar(Integer aantalGeprognostiseerdeAfspraakslots)
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var eersteDagVanHuidigJaar = vandaag.with(TemporalAdjusters.firstDayOfYear());
		var aantalDagenInHuidigJaar = DateUtil.getPeriodeTussenTweeDatums(eersteDagVanHuidigJaar, eersteDagVanHuidigJaar.plusYears(1), ChronoUnit.DAYS);
		var aantalResterendeDagenInHuidigJaar = DateUtil.getPeriodeTussenTweeDatums(vandaag, eersteDagVanHuidigJaar.plusYears(1), ChronoUnit.DAYS);
		return new BigDecimal(aantalGeprognostiseerdeAfspraakslots).multiply(new BigDecimal(aantalResterendeDagenInHuidigJaar))
			.divide(new BigDecimal(aantalDagenInHuidigJaar), 2, RoundingMode.HALF_UP);
	}

	@Override
	public List<UitnodigingsGebied> getAllUitnodigingsgebieden()
	{
		return hibernateService.loadAll(UitnodigingsGebied.class, "naam", true);
	}

	@Override
	public long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd,
		LocalDate laatsteDagVanHuidigJaar, Set<Integer> geboortejaren)
	{
		var spec = clientUitnodigingBase(minimaleLeeftijd, maximaleLeeftijd, laatsteDagVanHuidigJaar, uitnodigingsGebied);

		if (laatsteDagVanHuidigJaar != null)
		{
			var vandaag = currentDateSupplier.getLocalDate();
			var u1Spec = u1Base(laatsteDagVanHuidigJaar, vandaag, new ArrayList<>(geboortejaren));
			var u2Spec = u2Base(laatsteDagVanHuidigJaar, vandaag, LEFT).with(Client_.colonDossier, LEFT);

			spec = spec.and(u1Spec.or(u2Spec));
		}

		return clientRepository.countDistinct(spec);
	}

	private Function<From<?, ? extends Client>, From<?, ? extends ColonScreeningRonde>> screeningRondeJoin()
	{
		return r ->
		{
			var dossierJoin = join(r, Client_.colonDossier);
			return join(dossierJoin, ColonDossier_.laatsteScreeningRonde);
		};
	}

	@Override
	public long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied)
	{
		return countPersonenInUitnodigingsGebied(uitnodigingsGebied, null, null, null, null);
	}

	@Override
	public long countClientenInUitnodigingsgebiedMetUitnodigingOpDatum(UitnodigingsGebied uitnodigingsGebied, LocalDate uitnodigingsDatum)
	{
		var spec = clientUitnodigingBase(null, null, null, uitnodigingsGebied)
			.and(heeftCreatieDatum(DateUtil.toUtilDate(uitnodigingsDatum)).with(screeningRondeJoin()));

		return clientRepository.count(spec);
	}

	@Override
	public String getUniekIdOf(ColoscopieCentrumColonCapaciteitVerdeling verdeling)
	{
		if (verdeling.getId() != null)
		{
			return verdeling.getId().toString();
		}
		else
		{
			String transientId = "";
			if (verdeling.getIntakelocatie() != null)
			{
				transientId += verdeling.getIntakelocatie().getId().toString();
			}
			if (verdeling.getUitnodigingsGebied() != null)
			{
				if (!transientId.isEmpty())
				{
					transientId += "_";
				}
				transientId += verdeling.getUitnodigingsGebied().getId().toString();
			}
			return transientId;
		}
	}
}
