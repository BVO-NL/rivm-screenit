package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;

import lombok.Getter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningDagDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.main.web.component.fullcalendar.event.EventSource;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.MammaCapaciteitOverviewPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.AantalRegulierOnderzoekenPerDagProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.AantalRegulierOnderzoekenPerWeekProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.BlokkadeEventsProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.StandplaatsEventsProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.WeekCapaciteitEventsProvider;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.collect.Range;

import static nl.rivm.screenit.util.DateUtil.plusDagen;
import static nl.rivm.screenit.util.DateUtil.toLocalDate;
import static nl.rivm.screenit.util.DateUtil.toLocalDateTime;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

public class ScreenITEventSourceFactory implements Serializable
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private final IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	@Getter
	private PlanningWeekDto weekDto;

	public ScreenITEventSourceFactory(IModel<MammaScreeningsEenheid> screeningsEenheid)
	{
		Injector.get().inject(this);
		screeningsEenheidModel = screeningsEenheid;
	}

	public EventSource getAantalOnderzoekenPerDagSource()
	{
		var aantalOnderzoekenPerDag = new EventSource();
		aantalOnderzoekenPerDag.setBackgroundColor("#FFFFFF");
		aantalOnderzoekenPerDag.setBorderColor("#FFFFFF");
		aantalOnderzoekenPerDag.setEventsProvider(new AantalRegulierOnderzoekenPerDagProvider(this));
		return aantalOnderzoekenPerDag;
	}

	public EventSource getAantalOnderzoekenPerWeekSource()
	{
		var aantalOnderzoekenPerWeek = new EventSource();
		aantalOnderzoekenPerWeek.setEventsProvider(new AantalRegulierOnderzoekenPerWeekProvider(this));
		return aantalOnderzoekenPerWeek;
	}

	public EventSource getWeekCapaciteitSource()
	{
		var capaciteit = new EventSource();
		capaciteit.setEventsProvider(new WeekCapaciteitEventsProvider(this));
		return capaciteit;
	}

	public EventSource getBlokkadesSource()
	{
		var blokkades = new EventSource();
		blokkades.setBackgroundColor("#f7e7e7");
		blokkades.setBorderColor("#df9f9f");
		blokkades.setEventsProvider(new BlokkadeEventsProvider(this));
		return blokkades;
	}

	public EventSource getStandplaatsSource(MammaCapaciteitOverviewPanel capaciteitOverviewPanel)
	{
		var blokkades = new EventSource();
		blokkades.setBackgroundColor("#f7f2ff");
		blokkades.setBorderColor("#d8beff");
		blokkades.setEventsProvider(new StandplaatsEventsProvider(this, capaciteitOverviewPanel));
		return blokkades;
	}

	public void resetCapaciteit(Date weekStart)
	{
		var screeningsEenheid = screeningsEenheidModel.getObject();
		weekDto = baseConceptPlanningsApplicatie.getWeek(screeningsEenheid, weekStart);

		var nu = dateSupplier.getLocalDateTime();
		var vandaag = nu.toLocalDate();
		if (nu.isAfter(toLocalDateTime(weekStart)))
		{
			var to = Collections.min(Arrays.asList(toUtilDate(nu), plusDagen(weekStart, 7)));
			var blokkenUitDatbase = baseCapaciteitsBlokService.getAlleCapaciteitBlokken(screeningsEenheid, Range.closed(weekStart, to));

			final var prognoseVanafDatum = nu.toLocalTime().isBefore(Constants.BK_EINDTIJD_DAG) ? vandaag : vandaag.plusDays(1);

			weekDto.dagen.stream()
				.filter(dagDto -> dagDto.datum.isBefore(prognoseVanafDatum) && planningModelHeeftGeenCapaciteitBlokOpDag(dagDto))
				.forEach(dagDto ->
				{
					final var totaalAantalOnderzoeken = new AtomicLong();
					blokkenUitDatbase.stream().filter(blok -> toLocalDate(blok.getVanaf()).equals(dagDto.datum)).forEach(blok ->
					{
						maakPlanningBlokEnVoegToeAanWeekDto(blok, screeningsEenheid, totaalAantalOnderzoeken);
					});
					dagDto.totaalAantalOnderzoeken = totaalAantalOnderzoeken.get();
				});
		}
	}

	private boolean planningModelHeeftGeenCapaciteitBlokOpDag(PlanningDagDto dagDto)
	{
		return weekDto.blokken.stream().noneMatch(blokDto -> dagDto.datum.equals(toLocalDate(blokDto.vanaf)));
	}

	private void maakPlanningBlokEnVoegToeAanWeekDto(MammaCapaciteitBlok blok, MammaScreeningsEenheid screeningsEenheid, AtomicLong totaalAantalOnderzoeken)
	{
		var blokDto = new PlanningCapaciteitBlokDto();
		blokDto.id = blok.getId();
		blokDto.blokType = blok.getBlokType();
		blokDto.vanaf = blok.getVanaf();
		blokDto.tot = blok.getTot();
		blokDto.screeningsEenheidId = screeningsEenheid.getId();
		blokDto.opmerkingen = blok.getOpmerkingen();
		blokDto.aantalOnderzoeken = blok.getAantalOnderzoeken();
		blokDto.minderValideAfspraakMogelijk = blok.getMinderValideAfspraakMogelijk();
		weekDto.blokken.add(blokDto);

		totaalAantalOnderzoeken.set(totaalAantalOnderzoeken.get() + blok.getBeschikbareCapaciteit().intValue());
	}

	public PlanningCapaciteitBlokDto getBlok(UUID conceptId)
	{
		return weekDto.blokken.stream().filter(b -> b.conceptId.equals(conceptId)).findFirst().orElse(null);
	}
}
