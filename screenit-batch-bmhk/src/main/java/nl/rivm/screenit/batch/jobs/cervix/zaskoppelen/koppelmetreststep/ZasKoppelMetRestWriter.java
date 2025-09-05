package nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.koppelmetreststep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import jakarta.persistence.EntityNotFoundException;
import jakarta.persistence.NonUniqueResultException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.ZasKoppelenConstants;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.ZasKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.repository.cervix.CervixUitnodigingRepository;
import nl.rivm.screenit.repository.cervix.CervixZasRepository;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification.heeftUitnodigingId;

@Component
@Slf4j
public class ZasKoppelMetRestWriter implements ItemWriter<InpakcentrumKoppelDataDto>
{

	@Autowired
	private CervixFactory factory;

	@Autowired
	private CervixUitnodigingRepository uitnodigingRepository;

	@Autowired
	private CervixZasRepository zasRepository;

	private StepExecution stepExecution;

	private ZasKoppelingBeeindigdLogEvent logEvent;

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	@Override
	public void write(Chunk<? extends InpakcentrumKoppelDataDto> chunk) throws Exception
	{
		logEvent = (ZasKoppelingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext().get(ZasKoppelenConstants.RAPPORTAGEKEYZASKOPPELEN);

		for (var verzondenUitnodiging : chunk.getItems())
		{

			String zasBarcode = null;

			try
			{

				zasBarcode = verzondenUitnodiging.getBarcode();
				validate(verzondenUitnodiging);

				var uitnodiging = uitnodigingRepository.findOne(heeftUitnodigingId(verzondenUitnodiging.getId()))
					.orElseThrow(() -> new EntityNotFoundException("Uitnodiging niet gevonden voor ID: " + verzondenUitnodiging.getId()));

				uitnodiging.setVerstuurdDoorInpakcentrum(true);

				if (StringUtils.isNotBlank(zasBarcode))
				{
					var zas = CervixMonsterUtil.getZAS(uitnodiging.getMonster());
					if (zas != null && !zasBarcode.equals(zas.getMonsterId()))
					{
						zas = null;
					}
					if (zas == null)
					{
						zas = factory.maakZasMonster(uitnodiging, zasBarcode);
					}
					var datumVerstuurd = DateUtil.parseDateForPattern(verzondenUitnodiging.getDatumVerzending(), Constants.DEFAULT_DATE_FORMAT);
					zas.setVerstuurd(datumVerstuurd);
					zasRepository.save(zas);
				}

				uitnodigingRepository.save(uitnodiging);

				logEvent.setAantalZasVerwerkt(logEvent.getAantalZasVerwerkt() + 1);
			}
			catch (NonUniqueResultException | EntityNotFoundException e)
			{
				LOG.error("Fout bij verwerken van koppel data regel ", e);
				var melding = String.format(KoppelConstants.CERVIX_ONBEKENDE_FOUT, verzondenUitnodiging.getId(),
					StringUtils.defaultIfBlank(zasBarcode, KoppelConstants.DEFAULT_STRING), e.getMessage());

				logEvent.setLevel(Level.ERROR);
				logEvent.setMelding(melding);
				throw new IllegalStateException(melding);
			}
		}
	}

	private void validate(InpakcentrumKoppelDataDto item)
	{
		if (item.getBarcode() == null)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + KoppelConstants.KOPPEL_BARCODE);
			throw new IllegalStateException("MatchingField not found: " + KoppelConstants.KOPPEL_BARCODE);
		}
	}
}
