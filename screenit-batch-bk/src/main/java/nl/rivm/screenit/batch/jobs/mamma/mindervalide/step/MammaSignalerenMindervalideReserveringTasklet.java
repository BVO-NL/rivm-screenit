package nl.rivm.screenit.batch.jobs.mamma.mindervalide.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsPeriodeService;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptieAlgoritme;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.heeftVrijgegevenTotEnMetOpOfNaDatum;
import static nl.rivm.screenit.util.DateUtil.toLocalDate;

@Component
@RequiredArgsConstructor
public class MammaSignalerenMindervalideReserveringTasklet implements Tasklet
{
	@Qualifier("mammaMindervalideAfspraakOptieAlgoritme")
	private final MammaAfspraakOptieAlgoritme mindervalideAfspraakOptieAlgoritme;

	private final MammaScreeningsEenheidRepository screeningsEenheidRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaBaseStandplaatsPeriodeService standplaatsPeriodeService;

	private final LogService logService;

	private final static int MINIMUM_AANTAL_VRIJE_RESERVERINGEN = 5;

	@Override
	public @Nullable RepeatStatus execute(@NotNull StepContribution contribution, @NotNull ChunkContext chunkContext) throws Exception
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var alleScreeningEenhedenMetVrijgevenTotEnMetInDeToekomst = screeningsEenheidRepository.findAll(heeftVrijgegevenTotEnMetOpOfNaDatum(vandaag));

		var aantalVrijeMindervalideReserveringenPerScreeningsEenheid = alleScreeningEenhedenMetVrijgevenTotEnMetInDeToekomst.stream()
			.collect(Collectors.toMap(screeningsEenheid -> screeningsEenheid, this::bepaalAantalVrijeMindervalideReserveringen));

		aantalVrijeMindervalideReserveringenPerScreeningsEenheid.forEach(this::signaleerMindervalideReserveringenPerScreeningsEenheid);

		return RepeatStatus.FINISHED;
	}

	private int bepaalAantalVrijeMindervalideReserveringen(MammaScreeningsEenheid screeningsEenheid)
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var vrijgegevenTotEnMet = toLocalDate(screeningsEenheid.getVrijgegevenTotEnMet());
		var dossier = new MammaDossier();

		var standplaatsPerioden = standplaatsPeriodeService.getStandplaatsPeriodenVanScreeningsEenheid(screeningsEenheid, Range.closed(vandaag, vrijgegevenTotEnMet));
		var opkomstkans = BigDecimal.ONE;
		return standplaatsPerioden.stream()
			.mapToInt(standplaatsPeriode ->
			{

				var standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
				var standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());

				var vanafDatum = Collections.max(Arrays.asList(vandaag, standplaatsPeriodeVanaf));
				var totEnMetDatum = Collections.min(Arrays.asList(vrijgegevenTotEnMet, standplaatsPeriodeTotEnMet));
				return mindervalideAfspraakOptieAlgoritme.getAfspraakOpties(dossier, standplaatsPeriode, vanafDatum, totEnMetDatum, true, opkomstkans, null, false)
					.size();
			})
			.sum();
	}

	private void signaleerMindervalideReserveringenPerScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid, Integer aantalVrijeMindervalideReserveringen)
	{
		if (aantalVrijeMindervalideReserveringen < MINIMUM_AANTAL_VRIJE_RESERVERINGEN)
		{
			var logLevel = aantalVrijeMindervalideReserveringen == 0 ? Level.ERROR : Level.WARNING;
			var logEvent = new LogEvent(String.format("%s vrije mindervalidenreservering(en).", aantalVrijeMindervalideReserveringen), logLevel);

			List<Organisatie> screeningsOrganisaties = List.of(MammaScreeningsEenheidUtil.getScreeningsOrganisatie(screeningsEenheid));
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_WEINIG_MINDERVALIDE_RESERVERINGEN, screeningsEenheid, screeningsOrganisaties, logEvent, null, null,
				currentDateSupplier.getLocalDateTime(), Bevolkingsonderzoek.MAMMA);
		}
	}
}
