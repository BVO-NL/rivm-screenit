package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Iterator;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.batch.service.InpakcentrumRestApplicatie;
import nl.rivm.screenit.model.algemeen.KoppelData;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumUploadRequestDto;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.LogEvent;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

@Slf4j
public abstract class BaseKoppelRestReader implements ItemStream
{
	@Autowired
	protected SessionFactory sessionFactory;

	@Autowired
	private InpakcentrumRestApplicatie inpakcentrumRestApplicatie;

	@Autowired
	private BarcodeValiderenService validerenService;

	protected Session hibernateSession;

	private StepExecution stepExecution;

	protected boolean unbindSessionFromThread = false;

	protected Iterator<InpakcentrumKoppelDataDto> koppeldata;

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	protected final StepExecution getStepExecution()
	{
		return stepExecution;
	}

	public InpakcentrumKoppelDataDto read()
	{
		if (koppeldata.hasNext())
		{
			return koppeldata.next();
		}
		return null;
	}

	@Override
	public final void open(ExecutionContext executionContext) throws ItemStreamException
	{
		hibernateSession = sessionFactory.openSession();
		doOpen();
	}

	protected void doOpen() throws ItemStreamException
	{
		var logEvent = (LogEvent) getStepExecution().getJobExecution().getExecutionContext()
			.get(getKoppelenConstant());

		List<String> semantischeFoutmeldingen = new ArrayList<>();

		try
		{
			var koppeldataLijst = getKoppeldataLijst();
			semantischeFoutmeldingen = validerenService.valideerOpSemantiek(koppeldataLijst);

			if (!semantischeFoutmeldingen.isEmpty())
			{
				voorkomVerwerkingKoppeldata(koppeldataLijst);
			}

			boolean versturenSuccesvol = verstuurSemantischeFoutmeldingen(semantischeFoutmeldingen);

			if (!versturenSuccesvol)
			{
				voorkomVerwerkingKoppeldata(koppeldataLijst);
			}
			koppeldata = koppeldataLijst.iterator();
		}
		catch (IOException e)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Er is een probleem opgetreden met een webservice, neem contact op met de helpdesk.");
			throw new ItemStreamException(e);
		}
		finally
		{
			var eindEvent = new LogEvent();
			eindEvent.setLevel(Level.INFO);

			if (!semantischeFoutmeldingen.isEmpty())
			{
				LOG.warn("Fouten gevonden: #{}", semantischeFoutmeldingen.size());
				eindEvent.setLevel(Level.ERROR);
				eindEvent.setMelding("De validatie heeft fouten gevonden en teruggekoppeld, Aantal fouten: #" + semantischeFoutmeldingen.size());
			}

			logEindValidatie(eindEvent);

			unbindSessionIfPossible();
		}
	}

	protected boolean verstuurSemantischeFoutmeldingen(List<String> foutmeldingen) throws IOException
	{
		LOG.info("Start versturen, semantische foutmeldingen size: {}", foutmeldingen.size());
		var request = new InpakcentrumUploadRequestDto();
		request.setContent(maakCsvStream(foutmeldingen).toString());
		request.setDatatype("csv");
		request.setFilename(getFileNaam() + ".csv");
		var response = inpakcentrumRestApplicatie.upload(request);
		var result = response.isUploadSucceeded();

		result &= inpakcentrumRestApplicatie.ready(result).isReady();

		if (!result)
		{
			var logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bestand met de semantische meldingen is niet succesvol naar het inpakcentrum verstuurd");

			logKoppelFout(logEvent);
		}

		LOG.info("Alles verstuurd met resultaat: {}", result);
		return result;
	}

	private String getFileNaam()
	{
		var koppelData = getKoppelData();
		var fileNaamVoorValidatie = koppelData.getFilename().replaceAll("mergedata", "validatie");
		var filenameParts = fileNaamVoorValidatie.split("\\.");
		return filenameParts[0];
	}

	private ByteArrayOutputStream maakCsvStream(List<String> foutmeldingen) throws IOException
	{
		var csvStream = new ByteArrayOutputStream();

		for (var foutmelding : foutmeldingen)
		{
			csvStream.write(foutmelding.getBytes());
			csvStream.write(System.lineSeparator().getBytes());
		}

		return csvStream;
	}

	protected List<InpakcentrumKoppelDataDto> getKoppeldataLijst() throws IOException
	{
		if (!TransactionSynchronizationManager.hasResource(sessionFactory))
		{
			TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
			unbindSessionFromThread = true;
		}
		var koppelData = getKoppelData();
		var objectMapper = new ObjectMapper();
		return objectMapper.readValue(Base64.getDecoder().decode(koppelData.getKoppelData()), new TypeReference<>()
		{
		});
	}

	private KoppelData getKoppelData()
	{
		var id = getStepExecution().getJobParameters().getLong(JobStartParameter.KOPPEL_DATA.name());
		return hibernateSession.get(KoppelData.class, id);
	}

	protected void voorkomVerwerkingKoppeldata(List<InpakcentrumKoppelDataDto> koppeldataLijst)
	{
		koppeldataLijst.clear();
	}

	protected abstract void logEindValidatie(LogEvent eindEvent);

	protected abstract void logKoppelFout(LogEvent logEvent);

	protected abstract String getKoppelenConstant();

	protected void unbindSessionIfPossible()
	{
		if (unbindSessionFromThread)
		{
			TransactionSynchronizationManager.unbindResource(sessionFactory);
			unbindSessionFromThread = false;
		}
	}
}
