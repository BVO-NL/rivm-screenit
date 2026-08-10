package nl.rivm.screenit.mamma.planning.wijzigingen;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashSet;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.planning.index.PlanningBlokkadeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningMelding;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

@Slf4j
enum PlanningDoorrekenenRoute
{
	;

	static void run(PlanningBlok blok)
	{
		LOG.trace("run blok: " + blok.getId());

		var totaal = new BigDecimal(blok.getAantalOnderzoeken());
		var blokType = blok.getCapaciteitBlokType();

		var beschikbaar = blok.getBeschikbaar();
		beschikbaar.clear();
		beschikbaar.add(totaal, blokType);
	}

	static void run(PlanningDag dag)
	{
		LOG.trace("run dag: " + dag.getDatum());

		var beschikbaar = dag.getBeschikbaar();
		beschikbaar.clear();

		for (var blok : dag.getBlokSet())
		{
			beschikbaar.add(blok.getBeschikbaar());
		}
	}

	static void run(PlanningWeek week)
	{
		LOG.trace("run week: " + week.getDatum());

		var beschikbaar = week.getBeschikbaar();
		beschikbaar.clear();

		for (var dag : week.getDagList())
		{
			beschikbaar.add(dag.getBeschikbaar());
		}
	}

	static void run(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		LOG.debug("run standplaatsPeriode: " + standplaatsPeriode.getId() + " volgnr" + standplaatsPeriode.getScreeningsEenheidVolgNr());

		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		var screeningsOrganisatieBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(screeningsEenheid.getScreeningsOrganisatie());
		var screeningsEenheidBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(screeningsEenheid);

		var context = new DoorrekenenStandplaatsPeriodeContext(standplaatsPeriode);
		var standplaatsPeriodeIterator = screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(standplaatsPeriode, false)
			.iterator();

		for (var datum = standplaatsPeriode.getVanaf(); datum.compareTo(PlanningConstanten.plannenTotEnMetDatum) <= 0; datum = datum.plusDays(1))
		{
			var dag = context.screeningsEenheid.getDagNavigableMap().get(datum);
			dag.setStandplaatsPeriode(standplaatsPeriode);

			dag.getBlokkadeSet().clear();
			addBlokkadeSet(dag, screeningsOrganisatieBlokkadeDatumMap);
			addBlokkadeSet(dag, screeningsEenheidBlokkadeDatumMap);
			addBlokkadeSet(dag, context.standplaatsBlokkadeDatumMap);

			if (standplaatsPeriode != null)
			{
				if (!standplaatsPeriode.gesplitst() && !context.oudCorrectieToegepast && !datum.isBefore(context.jaarovergang))
				{
					var benodigdEersteJaarStandplaatsRonde = context.standplaats.getBenodigd().get(context.jaarEersteStandplaatsPeriode);
					var benodigdStandplaatsTotaal = benodigdEersteJaarStandplaatsRonde.getTotaal();
					if (context.jaar > context.jaarEersteStandplaatsPeriode)
					{
						var benodigdJaar = context.standplaats.getBenodigd().get(context.jaar);
						benodigdStandplaatsTotaal = benodigdStandplaatsTotaal.add(benodigdJaar.getNieuw());
					}

					var benodigdVoorJaarovergangTotaal = BigDecimal.ZERO;
					for (var sp : context.standplaatsRonde.getStandplaatsPeriodeNavigableSet())
					{
						benodigdVoorJaarovergangTotaal = benodigdVoorJaarovergangTotaal.add(sp.getBeschikbaarVoorJaarovergangTotaal());
					}

					context.benodigdTotaalRestant = context.benodigdTotaalRestant
						.subtract(oudCorrectie(benodigdStandplaatsTotaal, benodigdVoorJaarovergangTotaal, benodigdEersteJaarStandplaatsRonde.getOud()));

					context.oudCorrectieToegepast = true;
				}

				var benodigdRestantNegatief = false;
				if (dag.getBlokkadeSet().isEmpty())
				{
					var dagBeschikbaarTotaal = dag.getBeschikbaar().getTotaal();
					context.benodigdTotaalRestant = context.benodigdTotaalRestant.subtract(dagBeschikbaarTotaal);
					benodigdRestantNegatief = context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) <= 0;
					standplaatsPeriode.add(datum.toEpochDay(), dagBeschikbaarTotaal, datum.isBefore(context.jaarovergang));
				}
				else
				{
					standplaatsPeriode.getBlokkadeNavigableSet().addAll(dag.getBlokkadeSet());
				}

				if (standplaatsPeriode.getPrognose() && benodigdRestantNegatief || !standplaatsPeriode.getPrognose() && standplaatsPeriode.getTotEnMet().equals(datum))
				{
					context.standplaatsPeriodeTotEnMet = datum;
					if (context.jaar != context.standplaatsPeriodeTotEnMet.getYear())
					{
						for (var j = context.jaar + 1; j <= datum.getYear(); j++)
						{
							var benodigdJaar = context.standplaats.getBenodigd().get(j);
							context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(benodigdJaar.getNieuw());
							if (context.isEersteStandplaatsRonde)
							{
								benodigdJaar = context.standplaats.getTransport().get(j);
								context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(benodigdJaar.getNieuw());
							}
							else
							{
								var eersteOnderzoekCorrectieRestant = benodigdJaar.getEersteOnderzoekCorrectie()
									.subtract(context.standplaats.getBenodigd().get(context.jaar).getEersteOnderzoekCorrectie());
								context.benodigdTotaalRestant = context.benodigdTotaalRestant.add(eersteOnderzoekCorrectieRestant);
							}
						}
						context.jaar = datum.getYear();
						benodigdRestantNegatief = context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) <= 0;
					}

					if (benodigdRestantNegatief || !standplaatsPeriode.getPrognose())
					{
						bepaalMeldingen(context, dag);

						standplaatsPeriode.setTotEnMet(context.standplaatsPeriodeTotEnMet);
						if (standplaatsPeriode.gesplitst())
						{
							standplaatsPeriode.unlock();
						}

						if (standplaatsPeriodeIterator.hasNext())
						{
							standplaatsPeriode = standplaatsPeriodeIterator.next();
							context = new DoorrekenenStandplaatsPeriodeContext(standplaatsPeriode, context.standplaatsPeriodeTotEnMet.plusDays(1));
						}
						else
						{
							standplaatsPeriode = null;
						}
					}
				}
			}
		}

		if (standplaatsPeriode != null)
		{
			standplaatsPeriode.setTotEnMet(standplaatsPeriode.getVanaf());
			var meldingList = context.standplaatsRonde.getMeldingList();
			meldingList.clear();
			meldingList.add(new PlanningMelding("Er zijn te weinig onderzoeken beschikbaar om een prognose te maken", MammaMeldingNiveau.PROBLEEM));
			while (standplaatsPeriodeIterator.hasNext())
			{
				var vorigeStandplaatsPeriode = standplaatsPeriode;

				if (standplaatsPeriode.gesplitst())
				{
					standplaatsPeriode.unlock();
				}
				standplaatsPeriode = standplaatsPeriodeIterator.next();
				standplaatsPeriode.setVanaf(vorigeStandplaatsPeriode.getTotEnMet().plusDays(1));
				standplaatsPeriode.setTotEnMet(standplaatsPeriode.getVanaf());
				meldingList = standplaatsPeriode.getStandplaatsRonde().getMeldingList();
				meldingList.clear();
				meldingList.add(new PlanningMelding("Zie voorgaande standplaatsperiode", MammaMeldingNiveau.PROBLEEM));
			}
		}
	}

	private static void addBlokkadeSet(PlanningDag dag, Map<LocalDate, Set<PlanningBlokkade>> blokkadeDatumMap)
	{
		if (blokkadeDatumMap != null)
		{
			var blokkadeSet = blokkadeDatumMap.get(dag.getDatum());
			if (blokkadeSet != null)
			{
				dag.getBlokkadeSet().addAll(blokkadeSet);
			}
		}
	}

	private static void bepaalMeldingen(DoorrekenenStandplaatsPeriodeContext context, PlanningDag laatsteDag)
	{
		var meldingList = context.standplaatsRonde.getMeldingList();
		meldingList.clear();

		var aantalBasisOnderzoeken = context.benodigdTotaalRestant.abs().setScale(1, BigDecimal.ROUND_UP);

		meldingList.add(new PlanningMelding(
			aantalBasisOnderzoeken + " " + (context.benodigdTotaalRestant.compareTo(BigDecimal.ZERO) < 0 ? "te veel" : "te weinig"),
			bepaalMeldingNiveau(context.benodigdTotaalRestant, laatsteDag.getBeschikbaar().getTotaal())));
	}

	private static MammaMeldingNiveau bepaalMeldingNiveau(BigDecimal benodigdRestant, BigDecimal beschikbaarLaatsteDag)
	{
		if (benodigdRestant.compareTo(BigDecimal.ZERO) <= 0)
		{
			if (benodigdRestant.add(beschikbaarLaatsteDag).compareTo(BigDecimal.ZERO) >= 0)
			{
				return MammaMeldingNiveau.INFO;
			}
			else
			{
				return MammaMeldingNiveau.WAARSCHUWING;
			}
		}
		else
		{
			return MammaMeldingNiveau.PROBLEEM;
		}
	}

	private static BigDecimal oudCorrectie(BigDecimal benodigd, BigDecimal benodigdVoorJaarovergang, BigDecimal benodigdOud)
	{
		if (benodigd.compareTo(BigDecimal.ZERO) != 0)
		{
			var percentageOud = benodigdOud.divide(benodigd, 6, RoundingMode.HALF_UP);
			var benodigdTotaalNaJaarovergang = benodigd.subtract(benodigdVoorJaarovergang);

			return benodigdTotaalNaJaarovergang.multiply(percentageOud);
		}
		else
		{
			return BigDecimal.ZERO;
		}

	}

	static void run(PlanningStandplaatsRonde standplaatsRonde)
	{
		LOG.debug("run standplaatsRonde: " + standplaatsRonde.getId());

		standplaatsRonde.setNiveau(MammaMeldingNiveau.INFO);
		for (var melding : standplaatsRonde.getMeldingList())
		{
			if (standplaatsRonde.getNiveau().compareTo(melding.getNiveau()) < 0)
			{
				standplaatsRonde.setNiveau(melding.getNiveau());
				if (standplaatsRonde.getNiveau() == MammaMeldingNiveau.PROBLEEM)
				{
					break;
				}
			}
		}

		var somGewogenDatum = BigDecimal.ZERO;
		var beschikbaarTotaal = BigDecimal.ZERO;
		for (var standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
		{
			somGewogenDatum = somGewogenDatum.add(standplaatsPeriode.getSomGewogenDatum());
			beschikbaarTotaal = beschikbaarTotaal.add(standplaatsPeriode.getBeschikbaarTotaal());
		}

		var gewogenGemiddeldeDatum = somGewogenDatum.divide(
			beschikbaarTotaal.compareTo(BigDecimal.ZERO) == 0 ? new BigDecimal(standplaatsRonde.getStandplaatsPeriodeNavigableSet().size()) : beschikbaarTotaal, 0,
			BigDecimal.ROUND_HALF_UP);
		var epochDay = gewogenGemiddeldeDatum.longValue();
		if (epochDay != 0)
		{
			var wekenVanTevorenUitnodigen = standplaatsRonde.getStandplaats().getScreeningsOrganisatie().getWekenVanTevorenUitnodigen();
			standplaatsRonde.setGewogenGemiddeldeDatum(LocalDate.ofEpochDay(epochDay - wekenVanTevorenUitnodigen * 7));
		}
		else
		{

			standplaatsRonde.setGewogenGemiddeldeDatum(standplaatsRonde.getStandplaatsPeriodeNavigableSet().first().getVanaf());
		}

		standplaatsRonde.setBeschikbaarTotaal(beschikbaarTotaal);

		LocalDate vorigeGewogenGemiddeldeDatum = null;
		var standplaats = standplaatsRonde.getStandplaats();
		if (standplaatsRonde.getId() != null)
		{
			var vorigeStandplaatsRonde = standplaats.getStandplaatsRondeNavigableSet().lower(standplaatsRonde);
			if (vorigeStandplaatsRonde != null)
			{
				vorigeGewogenGemiddeldeDatum = vorigeStandplaatsRonde.getGewogenGemiddeldeDatum();
			}
		}
		if (vorigeGewogenGemiddeldeDatum == null)
		{
			vorigeGewogenGemiddeldeDatum = standplaats.getVorigeGewogenGemiddeldeDatum();
		}
		if (vorigeGewogenGemiddeldeDatum != null)
		{
			standplaatsRonde.setInterval(new BigDecimal(ChronoUnit.DAYS.between(vorigeGewogenGemiddeldeDatum, standplaatsRonde.getGewogenGemiddeldeDatum())));
		}
		else
		{
			standplaatsRonde.setInterval(null);
		}
	}

	static void run(PlanningScreeningsEenheid screeningsEenheid)
	{
		LOG.debug("run screeningsEenheid: " + screeningsEenheid.getId());

		Set<PlanningStandplaatsRonde> standplaatsRondeSet = new HashSet<>();

		screeningsEenheid.setNiveau(MammaMeldingNiveau.INFO);
		for (var standplaatsPeriode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
		{
			var standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
			standplaatsRondeSet.add(standplaatsRonde);
			if (screeningsEenheid.getNiveau().compareTo(standplaatsRonde.getNiveau()) < 0)
			{
				screeningsEenheid.setNiveau(standplaatsRonde.getNiveau());
			}
		}

		var somGewogenInterval = BigDecimal.ZERO;
		var beschikbaarTotaal = BigDecimal.ZERO;
		for (var standplaatsRonde : standplaatsRondeSet)
		{
			var interval = standplaatsRonde.getInterval();
			if (interval != null)
			{
				somGewogenInterval = somGewogenInterval.add(interval.multiply(standplaatsRonde.getBeschikbaarTotaal()));
				beschikbaarTotaal = beschikbaarTotaal.add(standplaatsRonde.getBeschikbaarTotaal());
			}
		}
		if (!(beschikbaarTotaal.compareTo(BigDecimal.ZERO) == 0))
		{
			screeningsEenheid.setInterval(somGewogenInterval.divide(beschikbaarTotaal, 5, BigDecimal.ROUND_HALF_UP));
		}
		else
		{
			screeningsEenheid.setInterval(null);
		}
	}
}

class DoorrekenenStandplaatsPeriodeContext
{
	PlanningScreeningsEenheid screeningsEenheid;

	PlanningStandplaatsRonde standplaatsRonde;

	PlanningStandplaats standplaats;

	PlanningStandplaatsPeriode eersteStandplaatsPeriode;

	int jaarEersteStandplaatsPeriode;

	int jaar;

	boolean oudCorrectieToegepast;

	BigDecimal benodigdTotaalRestant;

	boolean isEersteStandplaatsRonde;

	NavigableSet<PlanningStandplaatsPeriode> voorgaandeStandplaatsPeriodeSet;

	Map<LocalDate, Set<PlanningBlokkade>> standplaatsBlokkadeDatumMap;

	LocalDate jaarovergang;

	LocalDate standplaatsPeriodeTotEnMet;

	DoorrekenenStandplaatsPeriodeContext(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		var vorigeStandplaatsPeriode = standplaatsPeriode.getScreeningsEenheid().getStandplaatsPeriodeNavigableSet().lower(standplaatsPeriode);
		var vanaf = vorigeStandplaatsPeriode != null ? vorigeStandplaatsPeriode.getTotEnMet().plusDays(1) : standplaatsPeriode.getVanaf();
		init(standplaatsPeriode, vanaf);
	}

	DoorrekenenStandplaatsPeriodeContext(PlanningStandplaatsPeriode standplaatsPeriode, LocalDate vanaf)
	{
		init(standplaatsPeriode, vanaf);
	}

	private void init(PlanningStandplaatsPeriode standplaatsPeriode, LocalDate vanaf)
	{
		screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
		standplaats = standplaatsRonde.getStandplaats();
		standplaatsPeriode.setVanaf(vanaf);

		eersteStandplaatsPeriode = standplaatsRonde.getStandplaatsPeriodeNavigableSet().first();
		jaarEersteStandplaatsPeriode = eersteStandplaatsPeriode.getVanaf()
			.minusWeeks(screeningsEenheid.getScreeningsOrganisatie().getWekenVanTevorenUitnodigen()).getYear();
		jaar = jaarEersteStandplaatsPeriode;
		oudCorrectieToegepast = false;

		var benodigdJaar = standplaats.getBenodigd().get(jaar);
		benodigdTotaalRestant = benodigdJaar.getTotaal();

		isEersteStandplaatsRonde = standplaatsRonde.equals(standplaats.getStandplaatsRondeNavigableSet().first());
		if (isEersteStandplaatsRonde)
		{
			benodigdJaar = standplaats.getTransport().get(jaar);
			benodigdTotaalRestant = benodigdTotaalRestant.add(benodigdJaar.getTotaal());
		}
		else
		{
			benodigdTotaalRestant = benodigdTotaalRestant.add(benodigdJaar.getEersteOnderzoekCorrectie());
		}

		voorgaandeStandplaatsPeriodeSet = standplaatsPeriode.getStandplaatsRonde().getStandplaatsPeriodeNavigableSet().headSet(standplaatsPeriode, false);
		if (!voorgaandeStandplaatsPeriodeSet.isEmpty())
		{
			voorgaandeStandplaatsPeriodeSet.last().await();

			for (var voorgaandeStandplaatsPeriode : voorgaandeStandplaatsPeriodeSet)
			{
				benodigdTotaalRestant = benodigdTotaalRestant.subtract(voorgaandeStandplaatsPeriode.getBeschikbaarTotaal());
			}
		}

		standplaatsBlokkadeDatumMap = PlanningBlokkadeIndex.getBlokkadeDatumMap(standplaats);
		standplaatsPeriode.clear();
		standplaatsPeriode.getBlokkadeNavigableSet().clear();

		jaarovergang = LocalDate.of(jaar + 1, 1, 1)
			.plusWeeks(screeningsEenheid.getScreeningsOrganisatie().getWekenVanTevorenUitnodigen());
	}
}
