package nl.rivm.screenit.batch.jobs.uitnodigingenversturen.versturenstep;

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
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;

import lombok.extern.slf4j.Slf4j;

import nl.dm_ict.photo._358.MERGEDATA;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS;
import nl.dm_ict.photo._358.MERGEDATA.UITNODIGING.MERGEFIELDS.MERGEFIELD;
import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.service.InpakcentrumRestApplicatie;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.util.WebservicePingUtil;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumMergeFieldDto;
import nl.rivm.screenit.model.inpakcentrum.naarinpakcentrum.InpakcentrumUitnodigingDto;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InpakcentrumService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernateSession;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.tempuri.IUpload;
import org.tempuri.UploadRequest;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;

@Slf4j
public abstract class AbstractUitnodigingenVersturenTasklet<U extends InpakbareUitnodiging<?>> implements Tasklet
{
	private static final int MINUTES_TO_WAIT_FOR_NEXT_TRY = 10;

	private static final int NR_OF_TRIES = 7;

	private static int NR_OF_RETRIES_ZIP_XML_VERZENDEN;

	private static int TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN;

	private static final String WSDL_QUESTION = "?wsdl";

	private StepExecution stepExecution;

	private JobExecution jobExecution;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private WebserviceInpakcentrumOpzettenService webserviceOpzettenService;

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	@Autowired
	@Qualifier(value = "inpakCentrumEndpointUrl")
	private String inpakCentrumEndpointUrl;

	@Autowired
	private InpakcentrumRestApplicatie inpakcentrumRestApplicatie;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private OrganisatieService organisatieService;

	@Autowired
	private InpakcentrumService inpakcentrumService;

	private ConcurrentLinkedQueue<UploadDocument> inpakcentrumBrieven;

	private ConcurrentLinkedQueue<Long> gegenereerdeUitnodigingIds;

	private AtomicInteger volgnummer;

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private MERGEDATA mergedata;

	private String bvoAfkorting;

	private Integer onsuccesvolleVerzendPogingenGehad = 0;

	private final List<InpakcentrumUitnodigingDto> gegenereerdeUitnodigingen = new ArrayList<>();

	protected abstract List<Long> getUitnodigingen();

	protected abstract BriefDefinitie getBriefDefinitie(U uitnodiging);

	protected abstract void setMergedBrieven(U uitnodiging, UploadDocument uploadDocument, BriefDefinitie briefDefinitie);

	protected abstract void setGegenereerd(U uitnodiging);

	protected abstract void setMergeContext(U uitnodiging, MailMergeContext mailMergeContext);

	protected abstract FileStoreLocation getFileStoreLocation();

	protected abstract void updateCounts(U uitnodiging);

	protected abstract void logMislukt(nl.rivm.screenit.model.Client client);

	protected abstract void logMislukt(Long uitnodigingId);

	protected abstract void logNietBereikbaar(LogEvent event);

	protected abstract String getBvoAfkorting();

	protected abstract void setUitnodigingVersturenTijd(List<Long> uitnodigingIds);

	protected abstract boolean uitzonderingGevonden(U uitnodiging);

	protected abstract U getUitnodigingById(Long uitnodigingId);

	protected void beforeProcessUitnodiging(U uitnodiging)
	{
	}

	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception
	{
		stepExecution = chunkContext.getStepContext().getStepExecution();
		jobExecution = stepExecution.getJobExecution();
		gegenereerdeUitnodigingen.clear();
		var gebruikNieuweInpakcentrum = inpakcentrumService.gebruikNieuweInpakcentrumKoppeling();
		controleerAlleBrieven();

		try
		{
			waitForAvailableEndpoint(inpakCentrumEndpointUrl.replace("/DataUpload", WSDL_QUESTION));
		}
		catch (Exception e)
		{
			return handleExceptionPingen(contribution, e);
		}

		onsuccesvolleVerzendPogingenGehad = 0;
		NR_OF_RETRIES_ZIP_XML_VERZENDEN = preferenceService.getInteger(PreferenceKey.RETRIES_VERZENDEN_INPAKCENTRUM.name());
		TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN = preferenceService.getInteger(PreferenceKey.TIME_BETWEEN_RETRIES_VERZENDEN_INPAKCENTRUM.name());

		gegenereerdeUitnodigingIds = new ConcurrentLinkedQueue<>();
		inpakcentrumBrieven = new ConcurrentLinkedQueue<>();
		volgnummer = new AtomicInteger(0);
		bvoAfkorting = getBvoAfkorting();

		if (gebruikNieuweInpakcentrum)
		{
			genereerUitnodigingenRest();
		}
		else
		{
			genereerUitnodigingenWsdl();
		}

		return RepeatStatus.FINISHED;
	}

	private void waitForAvailableEndpoint(String url)
	{
		int i = 0;
		if (!url.endsWith(WSDL_QUESTION))
		{
			url += WSDL_QUESTION;
		}
		var format = new SimpleDateFormat(Constants.DEFAULT_DATE_TIME_FORMAT);
		var message = new StringBuilder();
		message.append("\n<br>Poging 1 (").append(format.format(currentDateSupplier.getDate())).append("): ");

		while (!(inpakcentrumService.gebruikNieuweInpakcentrumKoppeling() ?
			getStatusRest() :
			WebservicePingUtil.ping(url, Arrays.asList("GetReady", "NumberOfRecords"), message)))
		{
			i++;
			var event = new LogEvent();

			var melding = "Na poging " + i;
			LOG.info(melding);
			event.setMelding(melding);
			if (i == NR_OF_TRIES)
			{
				event.setLevel(Level.ERROR);
			}
			else
			{
				event.setLevel(Level.WARNING);
			}
			logNietBereikbaar(event);
			if (i < NR_OF_TRIES)
			{
				try
				{
					Thread.sleep(MINUTES_TO_WAIT_FOR_NEXT_TRY * 60 * 1000);
				}
				catch (InterruptedException e)
				{
					LOG.error("Fout in sleep", e);
					Thread.currentThread().interrupt();
				}
				message.append("\n<br>Poging ").append(i + 1).append(" (").append(format.format(currentDateSupplier.getDate())).append("): ");
			}
			else
			{
				break;
			}
		}
		if (i == NR_OF_TRIES)
		{
			var emailAdressen = organisatieService
				.getOrganisatieByOrganisatieTypes(List.of(OrganisatieType.INPAKCENTRUM))
				.stream()
				.map(Organisatie::getEmail)
				.filter(StringUtils::isNotBlank)
				.collect(Collectors.toList());
			emailAdressen.addAll(getEmails(preferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name())));

			message.insert(0, "Na " + NR_OF_TRIES + " pogingen in het afgelopen uur is de Inpakcentrum endpoint " + url + " niet bereikbaar geweest. ");
			mailService.queueMailAanProfessional(String.join(";", emailAdressen), "Inpakcentrum endpoint niet bereikbaar", message.toString(), MailPriority.HIGH);
			throw new IllegalStateException(message.toString());
		}
	}

	private boolean getStatusRest()
	{
		try
		{
			return inpakcentrumRestApplicatie.status().isAvailable();
		}
		catch (Exception e)
		{
			LOG.error("Fout bij het ophalen van de status van de Inpakcentrum REST applicatie", e);
			return false;
		}
	}

	private void genereerUitnodigingenRest() throws Exception
	{
		var uitnodigingIds = getUitnodigingen();
		if (!uitnodigingIds.isEmpty())
		{
			var aantalBeschikbareCores = Runtime.getRuntime().availableProcessors();
			var aantalThreadsVoorGenereren = Math.min(aantalBeschikbareCores * BatchConstants.AANTAL_THREADS_PER_CORE, BatchConstants.MAXIMUM_AANTAL_THREADS);
			LOG.info("Gebruik {} threads om {} uitnodigingen te genereren.", aantalThreadsVoorGenereren, uitnodigingIds.size());
			var forkJoinPool = new ForkJoinPool(aantalThreadsVoorGenereren);

			var startMetGenererenTijd = System.currentTimeMillis();
			for (var uitnodigingId : uitnodigingIds)
			{
				forkJoinPool.submit(() -> OpenHibernateSession.withCommittedTransaction().run(() ->
					genereerDtoUitnodiging(uitnodigingId)));
			}
			forkJoinPool.shutdown();
			forkJoinPool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
			var stopMetGenererenTijd = System.currentTimeMillis();
			LOG.info("Klaar met genereren van {} uitnodigingen, dit duurde {} ms", gegenereerdeUitnodigingIds.size(), stopMetGenererenTijd - startMetGenererenTijd);
			verstuurUitnodigingen(Lists.newArrayList(gegenereerdeUitnodigingIds));
		}
		else
		{
			LOG.warn("Niets om te versturen");
		}
	}

	private void controleerAlleBrieven()
	{
		if (!checkBrieven())
		{
			String message = "Niet alle brieven hebben een template geupload. Upload de bijbehorende templates.";
			IllegalStateException exception = new IllegalStateException(message);
			crashMelding(message, exception);
			throw exception;
		}
	}

	private void verstuurUitnodigingen(List<Long> uitnodigingIds) throws JAXBException, IOException
	{
		if (!uitnodigingIds.isEmpty())
		{
			LOG.info("Start versturen, uitnodigingIds size: {}, id van eerste uitnodiging: {}", uitnodigingIds.size(), uitnodigingIds.get(0));
			var versturenGeslaagd = inpakcentrumService.gebruikNieuweInpakcentrumKoppeling() ? verstuurUitnodigingenMetRest() : verstuurUitnodigingenMetWsdl(uitnodigingIds);

			LOG.info("Alles verstuurd met resultaat: {}", versturenGeslaagd);
			if (versturenGeslaagd)
			{
				setUitnodigingVersturenTijd(uitnodigingIds);
			}
			else
			{
				crashMelding("Niet alles is correct verstuurd naar het inpakcentrum. Neem contact op met de helpdesk", null);
			}
		}
		else
		{
			LOG.warn("Niets om te versturen. Client(en) overgeslagen door onbekende/onvolledige retouradres.");
		}
	}

	private boolean verstuurUitnodigingenMetRest() throws IOException
	{
		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS);
		var filename = bvoAfkorting + "_mergedata" + dateFormat.format(currentDateSupplier.getDate()) + ".json";

		var mapper = new ObjectMapper();
		var content = mapper.createArrayNode();
		gegenereerdeUitnodigingen.forEach(uitnodiging ->
		{
			var contentNode = content.addObject();
			uitnodiging.getMergeFields().forEach(field ->
				contentNode.put(field.getName(), field.getValue()));
		});
		var jsonBytes = mapper.writeValueAsBytes(content);

		var response = inpakcentrumRestApplicatie.upload("json", filename, jsonBytes);
		var versturenGeslaagd = response.isUploadSucceeded();
		if (versturenGeslaagd)
		{
			var zips = zipInpakcentrumBrieven(bvoAfkorting);
			versturenGeslaagd = verstuurZipsMetRest(zips);
		}
		versturenGeslaagd &= inpakcentrumRestApplicatie.ready(versturenGeslaagd).isReady();
		return versturenGeslaagd;
	}

	private synchronized void updateUitnodigingen(InpakcentrumUitnodigingDto inpakcentrumUitnodigingDto)
	{
		gegenereerdeUitnodigingen.add(inpakcentrumUitnodigingDto);
	}

	private boolean verstuurZipsMetRest(Set<File> zips)
	{
		var versturenGeslaagd = true;
		for (var zip : zips)
		{
			LOG.info("{} grootte is: {} bytes", zip.getName(), zip.length());
			versturenGeslaagd &= uploadMetRest(zip, zip.getName());
			FileUtils.deleteQuietly(zip);
		}
		return versturenGeslaagd;
	}

	private boolean uploadMetRest(File file, String fileName)
	{
		if (file == null)
		{

			LOG.warn("UploadDocument was null wordt niet verstuurd naar inpakcentrum! fileName: {}", fileName);
			return true;
		}
		var extensie = FilenameUtils.getExtension(fileName);
		if (StringUtils.isBlank(extensie))
		{
			LOG.error("Kon geen extensie bepaald worden bij de fileName: {}", fileName);
			return false;
		}
		LOG.info("File met naam '{}' wordt naar inpakcentrum verstuurd", fileName);

		boolean verzendenGeslaagd;

		do
		{
			try
			{
				var template = FileUtils.readFileToByteArray(file);
				var response = inpakcentrumRestApplicatie.upload("zip", fileName, template);
				verzendenGeslaagd = response.isUploadSucceeded();
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden met uploaden naar het inpakcentrum", e);
				verzendenGeslaagd = false;
			}
		}
		while (bepaalMagNogmaalsProberen(verzendenGeslaagd));

		return verzendenGeslaagd;
	}

	private boolean bepaalMagNogmaalsProberen(boolean versturenGeslaagd)
	{
		if (versturenGeslaagd || onsuccesvolleVerzendPogingenGehad == NR_OF_RETRIES_ZIP_XML_VERZENDEN)
		{
			onsuccesvolleVerzendPogingenGehad = 0;
			return false;
		}

		onsuccesvolleVerzendPogingenGehad++;
		try
		{
			LOG.info("Retry nodig, wacht {} seconden", TimeUnit.MILLISECONDS.toSeconds(TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN));
			Thread.sleep(TIME_BETWEEN_RETRIES_ZIP_XML_VERZENDEN);
		}
		catch (InterruptedException e)
		{
			LOG.error("Fout in sleep", e);
			Thread.currentThread().interrupt();
		}

		return true;
	}

	private Set<File> zipInpakcentrumBrieven(String bvoAfkorting) throws IOException
	{
		var dateTime = LocalDateTime.now();
		var datumTijd = DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDDHHMMSS).format(dateTime);
		var baseZipNaam = bvoAfkorting.toLowerCase() + "_" + datumTijd;

		var maxBestandsGrootte = preferenceService.getInteger(PreferenceKey.INTERNAL_MAX_GROOTTE_ZIP.name());

		return ZipUtil.maakZips(Lists.newArrayList(inpakcentrumBrieven), baseZipNaam, maxBestandsGrootte);
	}

	private String maakPdfNaam(InpakbareUitnodiging<?> uitnodiging, int volgnummer)
	{
		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS);
		return String.format("%s_%s_%s.pdf", dateFormat.format(currentDateSupplier.getDate()), volgnummer, uitnodiging.getUitnodigingsId());
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	private List<String> getEmails(String emailreeks)
	{
		List<String> emails = new ArrayList<>();
		if (emailreeks != null)
		{
			if (emailreeks.contains(";"))
			{
				emails = Arrays.asList(emailreeks.split(";"));
			}
			else if (emailreeks.contains(","))
			{
				emails = Arrays.asList(emailreeks.split(","));
			}
			else
			{
				emails.add(emailreeks);
			}
		}
		return emails;
	}

	protected boolean checkBrieven()
	{
		for (var briefType : BriefType.values())
		{
			if (OrganisatieType.INPAKCENTRUM == briefType.getVerzendendeOrganisatieType() && briefService.getNieuwsteBriefDefinitie(briefType) == null)
			{
				return false;
			}

		}
		return true;
	}

	private void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		if (!getExecutionContext().containsKey(BatchConstants.MELDING) || !Level.ERROR.equals(getExecutionContext().get(BatchConstants.LEVEL)))
		{
			getExecutionContext().put(BatchConstants.MELDING, melding);
			getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
		}
	}

	private void genereerDtoUitnodiging(Long uitnodigingId)
	{
		U uitnodiging = getUitnodigingById(uitnodigingId);
		try
		{
			if (uitzonderingGevonden(uitnodiging))
			{
				return;
			}
			beforeProcessUitnodiging(uitnodiging);
			LOG.trace("Genereer uitnodiging voor uitnodigingId: {}", uitnodigingId);

			var inpakcentrumUitnodiging = new InpakcentrumUitnodigingDto();
			inpakcentrumUitnodiging.setId(uitnodiging.getUitnodigingsId());

			var briefDefinitie = getBriefDefinitie(uitnodiging);
			var client = uitnodiging.getScreeningRonde().getDossier().getClient();
			var briefActie = projectService.getProjectBriefActie(client, briefDefinitie.getBriefType());

			File briefTemplate;
			if (briefActie != null)
			{
				uitnodiging.setTemplateNaam(briefActie.getProject().getNaam() + ": " + briefActie.getDocument().getNaam());
				briefTemplate = uploadDocumentService.load(briefActie.getDocument());
			}
			else
			{
				var document = briefDefinitie.getDocument();
				uitnodiging.setTemplateNaam(document.getNaam());
				briefTemplate = uploadDocumentService.load(document);
			}

			var briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
			var mailMergeContext = new MailMergeContext();
			mailMergeContext.setClient(client);
			setMergeContext(uitnodiging, mailMergeContext);

			var document = asposeService.processDocument(briefTemplateBytes, mailMergeContext);
			var uitnodigingFile = briefService.genereerPdf(document, "uitnodiging", false);

			var uitnodigingVolgnummer = volgnummer.getAndIncrement();
			var uploadDocument = new UploadDocument();
			uploadDocument.setActief(Boolean.TRUE);
			uploadDocument.setContentType("application/pdf");
			uploadDocument.setNaam(maakPdfNaam(uitnodiging, uitnodigingVolgnummer));
			uploadDocument.setFile(uitnodigingFile);

			var timestamp = currentDateSupplier.getDate().getTime();
			uploadDocumentService.saveOrUpdate(uploadDocument, getFileStoreLocation(), timestamp, true);

			inpakcentrumBrieven.add(uploadDocument);

			setMergedBrieven(uitnodiging, uploadDocument, briefDefinitie);

			setGegenereerd(uitnodiging);

			vulMetaDataMetRest(inpakcentrumUitnodiging, briefActie, mailMergeContext, uitnodigingVolgnummer, uploadDocument);

			updateCounts(uitnodiging);
			uitnodiging.setVerstuurd(true);
			hibernateService.saveOrUpdate(uitnodiging);
			gegenereerdeUitnodigingIds.add(uitnodiging.getId());
		}
		catch (Exception e)
		{
			if (uitnodiging == null)
			{
				LOG.warn("Uitnodiging overgeslagen door een null uitnodiging", e);
				logMislukt(uitnodigingId);
			}
			else
			{
				nl.rivm.screenit.model.Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
				LOG.warn("Client (id: '{}') overgeslagen door een exception", client.getId(), e);
				logMislukt(client);
			}
		}
	}

	protected void vulMetaDataMetRest(InpakcentrumUitnodigingDto inpakcentrumUitnodigingDto, ProjectBriefActie briefActie, MailMergeContext mailMergeContext,
		int uitnodigingVolgnummer,
		UploadDocument uploadDocument)
	{
		var mergeFields = inpakcentrumUitnodigingDto.getMergeFields();
		Stream.of(MergeField.CLIENT_ADRES, MergeField.CLIENT_POSTCODE,
			MergeField.CLIENT_WOONPLAATS, MergeField.UITNODIGINGSID).filter(MergeField::naarInpakcentrum).forEach(mergeField ->
		{
			Object value = mergeField.getValue(mailMergeContext);
			addMergeField(mergeFields, mergeField.getFieldName(), value != null ? value.toString() : "");
		});

		addMergeField(mergeFields, "_VOLGNUMMER", Integer.toString(uitnodigingVolgnummer));
		addMergeField(mergeFields, "_BVO", bvoAfkorting);
		addMergeField(mergeFields, "_PROJECT", briefActie != null ? "P" + briefActie.getProject().getId() + ":" + briefActie.getProject().getNaam() : "");
		addMergeField(mergeFields, "_PDF", uploadDocument.getNaam());

		updateUitnodigingen(inpakcentrumUitnodigingDto);
	}

	protected void addMergeField(List<InpakcentrumMergeFieldDto> mergeFields, String key, String value)
	{
		var mergeField = new InpakcentrumMergeFieldDto();
		mergeField.setName(key);
		mergeField.setValue(value);
		mergeFields.add(mergeField);
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private void genereerUitnodigingenWsdl() throws Exception
	{
		var uitnodigingIds = getUitnodigingen();
		if (!uitnodigingIds.isEmpty())
		{
			mergedata = new MERGEDATA();

			var aantalBeschikbareCores = Runtime.getRuntime().availableProcessors();
			var aantalThreadsVoorGenereren = Math.min(aantalBeschikbareCores * BatchConstants.AANTAL_THREADS_PER_CORE, BatchConstants.MAXIMUM_AANTAL_THREADS);
			LOG.info("Gebruik {} threads om {} uitnodigingen te genereren.", aantalThreadsVoorGenereren, uitnodigingIds.size());
			var forkJoinPool = new ForkJoinPool(aantalThreadsVoorGenereren);

			var startMetGenererenTijd = System.currentTimeMillis();
			for (var uitnodigingId : uitnodigingIds)
			{
				forkJoinPool.submit(() -> OpenHibernateSession.withCommittedTransaction().run(() ->
					genereerXmlUitnodiging(uitnodigingId)));
			}
			forkJoinPool.shutdown();
			forkJoinPool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
			var stopMetGenererenTijd = System.currentTimeMillis();
			LOG.info("Klaar met genereren van {} uitnodigingen, dit duurde {}ms", gegenereerdeUitnodigingIds.size(), stopMetGenererenTijd - startMetGenererenTijd);
			verstuurUitnodigingen(Lists.newArrayList(gegenereerdeUitnodigingIds));
		}
		else
		{
			LOG.warn("Niets om te versturen");
		}
	}

	private RepeatStatus handleExceptionPingen(StepContribution contribution, Exception e)
	{
		LOG.error("Pingen levert niets op ", e);
		stepExecution.setExitStatus(ExitStatus.FAILED);
		stepExecution.setStatus(BatchStatus.FAILED);
		stepExecution.getJobExecution().addFailureException(e);
		contribution.setExitStatus(ExitStatus.FAILED);
		return RepeatStatus.FINISHED;
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private void genereerXmlUitnodiging(Long uitnodigingId)
	{
		U uitnodiging = getUitnodigingById(uitnodigingId);
		try
		{
			if (uitzonderingGevonden(uitnodiging))
			{
				return;
			}
			beforeProcessUitnodiging(uitnodiging);
			LOG.trace("Geneer uitnodiging voor uitnodigingId: {}", uitnodigingId);

			var inpakcentrumUitnodiging = new UITNODIGING();
			inpakcentrumUitnodiging.setID(uitnodiging.getUitnodigingsId());
			inpakcentrumUitnodiging.setMERGEFIELDS(new MERGEFIELDS());

			var briefDefinitie = getBriefDefinitie(uitnodiging);
			var client = uitnodiging.getScreeningRonde().getDossier().getClient();
			var briefActie = projectService.getProjectBriefActie(client, briefDefinitie.getBriefType());

			File briefTemplate;
			if (briefActie != null)
			{
				var inpakcentrumTemplateNaam = maakInpakcentrumTemplateNaam(briefActie);
				inpakcentrumUitnodiging.setTEMPLATE(inpakcentrumTemplateNaam);
				uitnodiging.setTemplateNaam(briefActie.getProject().getNaam() + ": " + briefActie.getDocument().getNaam());
				briefTemplate = uploadDocumentService.load(briefActie.getDocument());
			}
			else
			{
				var inpakcentrumTemplateNaam = maakInpakcentrumTemplateNaam(briefDefinitie);
				inpakcentrumUitnodiging.setTEMPLATE(inpakcentrumTemplateNaam);
				var document = briefDefinitie.getDocument();
				uitnodiging.setTemplateNaam(document.getNaam());
				briefTemplate = uploadDocumentService.load(document);
			}

			var briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
			var mailMergeContext = new MailMergeContext();
			mailMergeContext.setClient(client);
			setMergeContext(uitnodiging, mailMergeContext);

			var document = asposeService.processDocument(briefTemplateBytes, mailMergeContext);
			var uitnodigingFile = briefService.genereerPdf(document, "uitnodiging", false);

			int uitnodigingVolgnummer = volgnummer.getAndIncrement();
			var uploadDocument = new UploadDocument();
			uploadDocument.setActief(Boolean.TRUE);
			uploadDocument.setContentType("application/pdf");
			uploadDocument.setNaam(maakPdfNaam(uitnodiging, uitnodigingVolgnummer));
			uploadDocument.setFile(uitnodigingFile);

			var timestamp = currentDateSupplier.getDate().getTime();
			uploadDocumentService.saveOrUpdate(uploadDocument, getFileStoreLocation(), timestamp, true);

			inpakcentrumBrieven.add(uploadDocument);

			setMergedBrieven(uitnodiging, uploadDocument, briefDefinitie);

			setGegenereerd(uitnodiging);

			vulMetaDataMetWsdl(inpakcentrumUitnodiging, briefActie, mailMergeContext, uitnodigingVolgnummer, uploadDocument);

			updateCounts(uitnodiging);
			uitnodiging.setVerstuurd(true);
			hibernateService.saveOrUpdate(uitnodiging);
			gegenereerdeUitnodigingIds.add(uitnodiging.getId());
		}
		catch (Exception e)
		{
			if (uitnodiging == null)
			{
				LOG.warn("Uitnodiging overgeslagen door een null uitnodiging", e);
				logMislukt(uitnodigingId);
			}
			else
			{
				nl.rivm.screenit.model.Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
				LOG.warn("Client (id: '{}') overgeslagen door een exception", client.getId(), e);
				logMislukt(client);
			}
		}
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private String maakInpakcentrumTemplateNaam(ProjectBriefActie briefActie)
	{
		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS);
		String templateNaam = briefActie.getBriefType().name();
		if (briefActie.getProject() != null)
		{
			templateNaam += "_P" + briefActie.getProject().getId() + "_";
		}
		templateNaam += dateFormat.format(briefActie.getLaatstGewijzigd());
		return templateNaam;
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private String maakInpakcentrumTemplateNaam(BriefDefinitie briefDefinitie)
	{
		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS);
		return briefDefinitie.getBriefType().name() + "_" + dateFormat.format(briefDefinitie.getLaatstGewijzigd());
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	protected void vulMetaDataMetWsdl(UITNODIGING inpakcentrumUitnodiging, ProjectBriefActie briefActie, MailMergeContext mailMergeContext, int uitnodigingVolgnummer,
		UploadDocument uploadDocument)
	{
		var mergefieldContainer = inpakcentrumUitnodiging.getMERGEFIELDS().getMERGEFIELD();
		var teSturenMergefields = Arrays.asList(MergeField.SO_ID, MergeField.CLIENT_NAAM, MergeField.CLIENT_ADRES, MergeField.CLIENT_POSTCODE,
			MergeField.CLIENT_WOONPLAATS, MergeField.KIX_CLIENT);

		for (var mergeField : teSturenMergefields)
		{
			if (mergeField.naarInpakcentrum())
			{
				Object value = mergeField.getValue(mailMergeContext);
				addMergeFieldValue(mergefieldContainer, mergeField.getFieldName(), value != null ? value.toString() : "");
			}
		}

		addMergeFieldValue(mergefieldContainer, "_VOLGNUMMER", Integer.toString(uitnodigingVolgnummer));
		addMergeFieldValue(mergefieldContainer, "_BVO", bvoAfkorting);
		addMergeFieldValue(mergefieldContainer, "_PROJECT", briefActie != null ? "P" + briefActie.getProject().getId() + ":" + briefActie.getProject().getNaam() : "");
		addMergeFieldValue(mergefieldContainer, "_PDF", uploadDocument.getNaam());
		updateMergeData(inpakcentrumUitnodiging);
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	protected void addMergeFieldValue(List<MERGEFIELD> mergefieldContainer, String key, String value)
	{
		var mergeField = new MERGEFIELD();
		mergeField.setNAME(key);
		mergeField.setVALUE(value);
		mergefieldContainer.add(mergeField);
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private synchronized void updateMergeData(UITNODIGING inpakcentrumUitnodiging)
	{
		mergedata.getUITNODIGING().add(inpakcentrumUitnodiging);
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private boolean verstuurUitnodigingenMetWsdl(List<Long> uitnodigingIds) throws JAXBException, IOException
	{
		LOG.info("Webservice opzetten");
		var jaxbContext = JAXBContext.newInstance(MERGEDATA.class);
		var marshaller = jaxbContext.createMarshaller();
		var byteArrayOutputStream = new ByteArrayOutputStream();
		marshaller.marshal(mergedata, byteArrayOutputStream);

		var upload = webserviceOpzettenService.initialiseerWebserviceInpakcentrum();

		var dateFormat = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS);
		var uploadRequest = new UploadRequest();
		uploadRequest.setStream(byteArrayOutputStream.toByteArray());

		var versturenGeslaagd = verstuurXml(uitnodigingIds.size(), upload, dateFormat, uploadRequest);

		if (versturenGeslaagd)
		{
			var zips = zipInpakcentrumBrieven(bvoAfkorting);
			versturenGeslaagd = verstuurZipsMetWsdl(upload, zips);
		}

		versturenGeslaagd = upload.getReady(versturenGeslaagd);
		return versturenGeslaagd;
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private boolean verstuurXml(int aantalUitnodigingen, IUpload upload, SimpleDateFormat dateFormat, UploadRequest uploadRequest)
	{
		boolean versturenGeslaagd;

		do
		{
			versturenGeslaagd = upload
				.upload(uploadRequest, "xml", bvoAfkorting + "_mergedata" + dateFormat.format(currentDateSupplier.getDate()) + ".xml",
					aantalUitnodigingen)
				.isUploadSucceeded();
		}
		while (bepaalMagNogmaalsProberen(versturenGeslaagd));

		return versturenGeslaagd;
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private boolean verstuurZipsMetWsdl(IUpload upload, Set<File> zips)
	{
		var versturenGeslaagd = true;
		for (var zip : zips)
		{
			LOG.info("{} grootte is: {} bytes", zip.getName(), zip.length());
			versturenGeslaagd &= newUploadRequest(upload, zip, zip.getName());
			FileUtils.deleteQuietly(zip);
		}
		return versturenGeslaagd;
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private boolean newUploadRequest(IUpload upload, File file, String fileName)
	{
		if (file == null)
		{

			LOG.warn("UploadDocument was null wordt niet verstuurd naar inpakcentrum! fileName: {}", fileName);
			return true;
		}
		var extensie = FilenameUtils.getExtension(fileName);
		if (StringUtils.isBlank(extensie))
		{
			LOG.error("Kon geen extensie bepaald worden bij de fileName: {}", fileName);
			return false;
		}
		LOG.info("File met naam '{}' wordt naar inpakcentrum verstuurd", fileName);

		boolean verzendenGeslaagd;

		do
		{
			try
			{
				var uploadRequest = new UploadRequest();
				byte[] template = FileUtils.readFileToByteArray(file);
				uploadRequest.setStream(template);
				verzendenGeslaagd = upload.upload(uploadRequest, extensie, fileName, 1).isUploadSucceeded();
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden met uploaden naar het inpakcentrum", e);
				verzendenGeslaagd = false;
			}
		}
		while (bepaalMagNogmaalsProberen(verzendenGeslaagd));

		return verzendenGeslaagd;
	}
}
