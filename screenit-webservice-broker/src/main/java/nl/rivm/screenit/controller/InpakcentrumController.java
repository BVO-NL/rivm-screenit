package nl.rivm.screenit.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OngeldigeKoppelDataException;
import nl.rivm.screenit.model.algemeen.KoppelData;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataRequestDto;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataResponseDto;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.inpakcentrum.InpakcentrumKoppelDataParser;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("api/inpakcentrum/v2")
@RequiredArgsConstructor
public class InpakcentrumController
{
	private final JobService jobService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final InpakcentrumKoppelDataParser koppelDataParser;

	@PostMapping("/dk/mappingdata")
	public InpakcentrumKoppelDataResponseDto verwerktDKKoppelData(@RequestBody InpakcentrumKoppelDataRequestDto inpakcentrumKoppelDataRequestDto)
	{
		return verwerkResponse(Bevolkingsonderzoek.COLON, inpakcentrumKoppelDataRequestDto);
	}

	@PostMapping("/bmhk/mappingdata")
	public InpakcentrumKoppelDataResponseDto verwerkBmhkKoppelData(@RequestBody InpakcentrumKoppelDataRequestDto inpakcentrumKoppelDataRequestDto)
	{
		return verwerkResponse(Bevolkingsonderzoek.CERVIX, inpakcentrumKoppelDataRequestDto);
	}

	private InpakcentrumKoppelDataResponseDto verwerkResponse(Bevolkingsonderzoek bevolkingsonderzoek, InpakcentrumKoppelDataRequestDto inpakcentrumKoppelDataRequestDto)
	{
		if (Bevolkingsonderzoek.COLON.equals(bevolkingsonderzoek))
		{
			logService.logGebeurtenis(LogGebeurtenis.COLON_JOB_CONTROLE_FIT_KOPPELEN_GESTART, new LogEvent(), Bevolkingsonderzoek.COLON);
		}
		else if (Bevolkingsonderzoek.CERVIX.equals(bevolkingsonderzoek))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_GESTART, new LogEvent(), Bevolkingsonderzoek.CERVIX);
		}
		var response = new InpakcentrumKoppelDataResponseDto();
		var foutmelding = valideerRequest(bevolkingsonderzoek, inpakcentrumKoppelDataRequestDto);
		if (foutmelding == null)
		{
			response.setValid(true);
			var koppelData = new KoppelData();
			koppelData.setKoppelData(inpakcentrumKoppelDataRequestDto.getMappingData());
			koppelData.setFilename(inpakcentrumKoppelDataRequestDto.getFilename());
			koppelData.setOntvangen(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(koppelData);
			stuurNaarBatch(bevolkingsonderzoek, koppelData.getId());
		}
		else
		{
			response.setValid(false);
			response.setFoutmelding(foutmelding);
			LOG.error("Fout bij verwerken koppeldata: {}", foutmelding);
		}
		return response;
	}

	private String valideerRequest(Bevolkingsonderzoek bevolkingsonderzoek, InpakcentrumKoppelDataRequestDto inpakcentrumKoppelDataRequestDto)
	{
		if (inpakcentrumKoppelDataRequestDto == null || StringUtils.isBlank(inpakcentrumKoppelDataRequestDto.getFilename()))
		{
			return "filename is verplicht.";
		}
		var bestandsnaam = inpakcentrumKoppelDataRequestDto.getFilename();
		if (bevolkingsonderzoek.equals(Bevolkingsonderzoek.CERVIX) && !bestandsnaam.startsWith("BMHK")
			|| bevolkingsonderzoek.equals(Bevolkingsonderzoek.COLON) && !bestandsnaam.startsWith("DK") || !bestandsnaam.contains("_mergedata"))
		{
			return "Bestandsnaam is niet correct. Bestandsnaam moet beginnen met 'DK' of 'BMHK' gevolgd door '_mergedata' + uniek nummer per bestand.";
		}

		if (bestandsnaam.length() > 100)
		{
			return "Bestandsnaam mag maximaal 100 tekens bevatten.";
		}

		try
		{
			koppelDataParser.leesKoppelData(inpakcentrumKoppelDataRequestDto.getMappingData());
		}
		catch (OngeldigeKoppelDataException e)
		{
			LOG.error("Fout bij decoderen of verwerken van Base64 JSON: {}", ExceptionUtils.getRootCauseMessage(e));
			return "Mapping data is geen geldige Base64 gecodeerde JSON string.";
		}

		return null;
	}

	private void stuurNaarBatch(Bevolkingsonderzoek bevolkingsonderzoek, Long koppelDataId)
	{
		var batchJob = new BatchJob();
		if (Bevolkingsonderzoek.COLON.equals(bevolkingsonderzoek))
		{
			batchJob.setJobType(JobType.KOPPELDATA_VERWERKING);
		}
		if (Bevolkingsonderzoek.CERVIX.equals(bevolkingsonderzoek))
		{
			batchJob.setJobType(JobType.CERVIX_KOPPELDATA_VERWERKING);
		}

		batchJob.getJobParameters().put(JobStartParameter.KOPPEL_DATA.name(), koppelDataId);

		LOG.info("Geen fouten bij verwerking koppeldata, dus doorzetten naar batch voor verdere validatie.");
		jobService.startJob(batchJob, null);
	}
}
