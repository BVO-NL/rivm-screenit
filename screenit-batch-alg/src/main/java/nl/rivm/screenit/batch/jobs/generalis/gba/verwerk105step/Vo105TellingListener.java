package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.Objects;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.repository.algemeen.GbaVraagRepository;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;

import org.jetbrains.annotations.NotNull;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.ItemProcessListener;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@RequiredArgsConstructor
public class Vo105TellingListener implements ItemProcessListener<Long, Vo105Bericht>, StepExecutionListener
{
	private final GbaVraagRepository gbaVraagRepository;

	private StepExecution stepExecution;

	@Override
	public void beforeStep(@NotNull StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		var verwerkingsLog = getVerwerkingsLog();
		verwerkingsLog.setAantalIndicatiesIngetrokkenBovengrensleeftijd(0);
		verwerkingsLog.setAantalIndicatiesIngetrokkenDefinitiefAfgemeld(0);
		verwerkingsLog.setAantalIndicatiesIngetrokkenSelectieblokkade(0);
		verwerkingsLog.setAantalIndicatiesIngetrokkenBezwaar(0);
		verwerkingsLog.setAantalIndicatiesIngetrokkenAfgevoerd(0);
	}

	@Override
	public ExitStatus afterStep(@NotNull StepExecution stepExecution)
	{
		return ExitStatus.COMPLETED;
	}

	@Override
	public void beforeProcess(@NotNull Long item)
	{
	}

	@Override
	public void afterProcess(@NotNull Long item, Vo105Bericht result)
	{
		var gbaVraag = gbaVraagRepository.getReferenceById(item);
		if (gbaVraag.getVraagType() != GbaVraagType.VERWIJDER_INDICATIE)
		{
			return;
		}

		updateTellingVoorReden(getVerwerkingsLog(), gbaVraag.getReden());
	}

	private void updateTellingVoorReden(GbaVerwerkingsLog verwerkingsLog, RedenGbaVraag reden)
	{
		switch (reden)
		{
		case BOVENGRENS_LEEFTIJD -> verwerkingsLog.setAantalIndicatiesIngetrokkenBovengrensleeftijd(
			verwerkingsLog.getAantalIndicatiesIngetrokkenBovengrensleeftijd() + 1);
		case AFGEMELD -> verwerkingsLog.setAantalIndicatiesIngetrokkenDefinitiefAfgemeld(
			verwerkingsLog.getAantalIndicatiesIngetrokkenDefinitiefAfgemeld() + 1);
		case SELECTIEBLOKKADE -> verwerkingsLog.setAantalIndicatiesIngetrokkenSelectieblokkade(
			verwerkingsLog.getAantalIndicatiesIngetrokkenSelectieblokkade() + 1);
		case BEZWAAR -> verwerkingsLog.setAantalIndicatiesIngetrokkenBezwaar(
			verwerkingsLog.getAantalIndicatiesIngetrokkenBezwaar() + 1);
		}
	}

	@Override
	public void onProcessError(@NotNull Long item, @NotNull Exception e)
	{
		LOG.error("Fout bij het verwerken van GbaVraag met id: {}", item, e);
	}

	private GbaVerwerkingsLog getVerwerkingsLog()
	{
		return (GbaVerwerkingsLog) Objects.requireNonNull(stepExecution, "Geen verwerkingslog gevonden in context")
			.getJobExecution().getExecutionContext().get(GbaConstants.RAPPORTAGEKEYGBA);
	}
}
