package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.factory.algemeen.BriefFactory;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.BriefDefinitiesFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.preference.service.KeyPreferenceService;
import nl.rivm.screenit.repository.algemeen.BriefDefinitieRepository;
import nl.rivm.screenit.repository.algemeen.ClientBriefRepository;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.JavaScriptPdfHelper;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.apache.pdfbox.pdmodel.interactive.action.PDActionJavaScript;
import org.hibernate.Hibernate;
import org.jspecify.annotations.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;
import com.aspose.words.PdfCompliance;

import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftClient;

@Slf4j
@Service
public class BaseBriefServiceImpl implements BaseBriefService
{

	private static final int BYTES_TO_MBS = 1024 * 1024;

	@Autowired
	@Qualifier("locatieFilestore")
	private String locatieFilestore;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private FileService fileService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private LogService logService;

	@Autowired
	private MessageService messageService;

	@Autowired
	private KeyPreferenceService preferenceService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private OrganisatieService organisatieService;

	@Autowired
	private BriefDefinitieRepository briefDefinitieRepository;

	@Autowired
	private ClientBriefRepository clientBriefRepository;

	@Autowired
	private BriefFactory briefFactory;

	@Override
	public BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType)
	{
		return briefDefinitieRepository.findFirstByBriefTypeOrderByLaatstGewijzigdDesc(briefType);
	}

	@Override
	public List<BriefDefinitie> getBriefDefinities(BriefDefinitiesFilter criteria, Comparator<BriefType> comparator)
	{
		var result = new ArrayList<BriefDefinitie>();
		var bevolkingsonderzoeken = criteria.getBevolkingsonderzoeken();
		var briefTypes = BriefType.getBriefTypes(Boolean.TRUE.equals(criteria.getExactMatch()), Boolean.TRUE.equals(criteria.getBrievenNietMeerInGebruikOokTonen()),
			bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[0]));
		var naam = criteria.getNaam();
		briefTypes.sort(comparator);

		for (var briefType : briefTypes)
		{

			var briefDefinitiesVanDitBriefType = new ArrayList<BriefDefinitie>();
			var eersteOngebruikteVolgnummer = 1;
			var briefDefinities = briefDefinitieRepository.findByBriefTypeOrderByLaatstGewijzigdAsc(briefType);
			var matchMetNaamFilter = filterOpNaamMatchtMetEenBriefDefinitie(briefDefinities, naam);

			if (briefDefinities != null && matchMetNaamFilter)
			{
				for (var briefDefinitie : briefDefinities)
				{
					briefDefinitie.setVolgnummer(eersteOngebruikteVolgnummer++);
					if (!briefDefinitiesVanDitBriefType.isEmpty())
					{ 
						briefDefinitiesVanDitBriefType.getLast().setGeldigTot(briefDefinitie.getLaatstGewijzigd());
					}
					briefDefinitiesVanDitBriefType.add(briefDefinitie);
				}
			}

			if (briefDefinitiesVanDitBriefType.isEmpty() && StringUtils.isBlank(naam))
			{
				var legeBriefDefinitie = new BriefDefinitie();
				legeBriefDefinitie.setBriefType(briefType);
				legeBriefDefinitie.setVolgnummer(1);
				briefDefinitiesVanDitBriefType.add(legeBriefDefinitie);
			}

			for (var i = briefDefinitiesVanDitBriefType.size() - 1; i >= 0; i--)
			{
				result.add(briefDefinitiesVanDitBriefType.get(i));
			}
		}
		return result;
	}

	private boolean filterOpNaamMatchtMetEenBriefDefinitie(List<BriefDefinitie> briefDefinities, String naam)
	{
		if (StringUtils.isBlank(naam))
		{
			return true;
		}

		return briefDefinities.stream().anyMatch(briefDefinitie -> briefDefinitie.getDocument().getNaam().toLowerCase().contains(naam.toLowerCase()));
	}

	@Override
	@Transactional
	public void saveBriefDefinitie(BriefDefinitie nieuweBriefDefinitie, File uploadFile, String contentType, String filename) throws IOException
	{

		var uploadDocument = new UploadDocument();
		uploadDocument.setActief(Boolean.TRUE);
		uploadDocument.setContentType(contentType);
		uploadDocument.setFile(uploadFile);
		uploadDocument.setNaam(filename);

		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.BRIEF_TEMPLATES);

		nieuweBriefDefinitie.setDocument(uploadDocument);
		nieuweBriefDefinitie.setLaatstGewijzigd(currentDateSupplier.getDate());

		hibernateService.save(nieuweBriefDefinitie);

		logService.logGebeurtenis(LogGebeurtenis.BRIEF_TOEGEVOEGD, nieuweBriefDefinitie.getUploader(),
			"Brief geupload: " + nieuweBriefDefinitie.getDocument().getNaam() + ", Type: " + nieuweBriefDefinitie.getBriefType().getWeergaveNaam(),
			nieuweBriefDefinitie.getBriefType().getOnderzoeken());
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie, ProjectBrief origineleBrief)
	{
		return briefFactory.maakProjectBrief(pClient, actie, origineleBrief);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date)
	{
		return briefFactory.maakBezwaarBrief(client, type, date);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date date, boolean vragenOmHandtekening)
	{
		return briefFactory.maakBezwaarBrief(client, type, date, vragenOmHandtekening);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type)
	{
		return briefFactory.maakAlgemeneBrief(client, type);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment, boolean vervangendeProjectBrief)
	{
		return briefFactory.maakBvoBrief(afmelding, type, creatieMoment, vervangendeProjectBrief);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<?, A, ?>, A extends Afmelding<?, ?, B>> B maakBvoBrief(A afmelding, BriefType type, Date creatieMoment)
	{
		return briefFactory.maakBvoBrief(afmelding, type, creatieMoment, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type)
	{
		return briefFactory.maakBvoBrief(ronde, type, null, false, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, boolean gegenereerd)
	{
		return briefFactory.maakBvoBrief(ronde, type, null, gegenereerd, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment)
	{
		return briefFactory.maakBvoBrief(ronde, type, creatieMoment, false, false);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public <B extends ClientBrief<SR, ?, ?>, SR extends ScreeningRonde<?, B, ?, ?>> B maakBvoBrief(SR ronde, BriefType type, Date creatieMoment, boolean gegenereerd,
		boolean vervangendeProjectBrief)
	{
		return briefFactory.maakBvoBrief(ronde, type, creatieMoment, gegenereerd, vervangendeProjectBrief);
	}

	@Override
	@Transactional
	@Deprecated(forRemoval = true)
	public CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date date, CervixHuisarts arts)
	{
		return briefFactory.maakRegioBrief(so, type, date, arts);
	}

	@Override
	public void completePdf(MergedBrieven<?> mergedBrieven)
	{
		var file = uploadDocumentService.load(mergedBrieven.getMergedBrieven());

		try
		{
			var copy = Files.copy(file.toPath(), Path.of(file.toPath() + "-copy")).toFile();
			var javaScript = new PDActionJavaScript(JavaScriptPdfHelper.getPrintJavascript());
			try (var document = Loader.loadPDF(copy); var outputStream = new FileOutputStream(file))
			{
				document.getDocumentCatalog().setOpenAction(javaScript);
				document.save(outputStream);
				LOG.info("Mergedocument(id = {}) gegenereerd en klaar!", mergedBrieven.getId());
			}
			finally
			{
				verwijderBestandAlsMogelijk(copy);
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij toevoegen van javascript in PDF (voor automatische printpopup)", e);
			throw new IllegalStateException("Fout bij toevoegen van javascript in PDF (voor automatische printpopup)");
		}
	}

	@Override
	@Transactional
	public <B extends Brief, MB extends MergedBrieven<?>> void createOrAddMergedBrieven(List<? extends B> chunkItems, IBrievenGeneratorHelper<B, MB> briefGenerator)
		throws Exception
	{
		try
		{
			var automatischAfdrukkenViaParagon = briefGenerator.isAutomatischAfdrukkenViaParagon();
			var mergedBrieven = briefGenerator.getMergedBrieven();
			var documentDefinitie = briefGenerator.getDocumentDefinitie();
			var templateDocument = documentDefinitie.getDocument();
			Document chunkDocument = null;
			var brievenAanwezig = false;
			var succesvolleBrieven = new ArrayList<Brief>();
			for (var brief : chunkItems)
			{
				try
				{
					var client = getClientFromBrief(brief);
					if (client != null && !AdresUtil.isVolledigAdresVoorInpakcentrum(client))
					{
						var clientAdres = AdresUtil.getAdres(client.getPersoon(), currentDateSupplier.getLocalDate());
						var onvolledigAdresMelding = "De cliënt heeft een onvolledig adres, dit is geconstateerd bij het aanmaken van: " + brief.getBriefType()
							+ ". De volgende gegevens ontbreken: " + AdresUtil.bepaalMissendeAdresgegevensString(clientAdres) + ".";
						var dagen = preferenceService.getInteger(PreferenceKey.INTERNAL_HERINNERINGSPERIODE_LOGREGEL_ONVOLLEDIG_ADRES).orElse(0);
						if (logService.heeftGeenBestaandeLogregelBinnenPeriode(List.of(briefGenerator.getOnvolledigAdresLogGebeurtenis()), client.getPersoon().getBsn(),
							onvolledigAdresMelding, dagen))
						{
							var organisaties = new ArrayList<Organisatie>();
							organisaties.add(hibernateService.loadAll(Rivm.class).getFirst());
							if (mergedBrieven != null && mergedBrieven.getScreeningOrganisatie() != null)
							{
								organisaties.add(mergedBrieven.getScreeningOrganisatie());
							}
							logService.logGebeurtenis(briefGenerator.getOnvolledigAdresLogGebeurtenis(), organisaties, client, onvolledigAdresMelding,
								briefGenerator.getBevolkingsonderzoeken());
						}
					}
					else
					{
						if (automatischAfdrukkenViaParagon)
						{
							maakBriefAfdrukOpdrachtVoorParagon(brief, templateDocument, documentDefinitie, briefGenerator);
						}
						else
						{
							chunkDocument = appendDocument(templateDocument, brief, chunkDocument, briefGenerator);

							succesvolleBrieven.add(brief);
							setBriefGegenereerdInfo(brief, documentDefinitie);
							brievenAanwezig = true;
						}
					}
				}
				catch (Exception e)
				{
					LOG.error("Error bij aanmaken brief (brieftype: {}, briefId: {})", brief.getBriefType(), brief.getId(), e);
					var client = getClientFromBrief(brief);
					var dashboardOrganisaties = new ArrayList<Organisatie>();
					if (mergedBrieven != null && mergedBrieven.getScreeningOrganisatie() != null)
					{
						dashboardOrganisaties.add(mergedBrieven.getScreeningOrganisatie());
					}
					var melding = "Door technische reden kon de brief (brieftype: " + brief.getBriefType() + ") niet worden gegenereerd.";

					logService.logGebeurtenis(briefGenerator.getMergeProbleemLogGebeurtenis(), dashboardOrganisaties, client, melding, briefGenerator.getBevolkingsonderzoeken());
				}
			}
			if (!automatischAfdrukkenViaParagon && chunkDocument != null && brievenAanwezig)
			{
				maakPdfEnWerkInfoBij(briefGenerator, chunkDocument, mergedBrieven, succesvolleBrieven);
			}
			hibernateService.saveOrUpdateAll(chunkItems);
		}
		catch (Exception e)
		{
			briefGenerator.crashMelding("Er is een onbekende fout opgetreden, neem contact op met de helpdesk.", e);
			throw e;
		}
	}

	private <B extends Brief, MB extends MergedBrieven<?>> File maakPdfEnWerkInfoBij(IBrievenGeneratorHelper<B, MB> briefGenerator, Document mergedDocument, MB mergedBrieven,
		List<Brief> succesvolleBrieven) throws Exception
	{
		var pdfBestand = File.createTempFile("mergedBrieven", ".pdf");
		try (var output = new FileOutputStream(pdfBestand))
		{
			mergedDocument.save(output, asposeService.getPdfSaveOptions());
			mergedDocument.setWarningCallback(warning -> LOG.warn("Warning converting to pdf: {}", warning.getDescription()));
		}

		if (mergedBrieven != null)
		{
			setOrAppendPdf(mergedBrieven, pdfBestand, briefGenerator);

			for (var brief : succesvolleBrieven)
			{
				brief.setMergedBrieven(mergedBrieven);
				mergedBrieven.setAantalBrieven(mergedBrieven.getAantalBrieven() + 1);
			}
		}
		var screeningOrganisatieId = getScreeningOrganisatieId(mergedBrieven);
		briefGenerator.verhoogAantalBrievenVanScreeningOrganisatie(screeningOrganisatieId, succesvolleBrieven.size());

		return pdfBestand;
	}

	@Override
	public void pdfBestandOpslaanVoorVersturen(File pdfBestand, String bestandsNaam) throws IOException
	{
		fileService.save(getVolledigDocumentUitwisselingPath(bestandsNaam), pdfBestand);
	}

	@Override
	public InputStream getFileStreamVanPdfBestand(BriefafdrukopdrachtDto.Resource resource) throws IOException
	{
		return fileService.loadAsStream(getVolledigDocumentUitwisselingPath(resource.getPath()));
	}

	@Override
	public void verwijderPdfBestand(BriefafdrukopdrachtDto.Resource resource)
	{
		fileService.delete(getVolledigDocumentUitwisselingPath(resource.getPath()));
	}

	private @NonNull String getVolledigDocumentUitwisselingPath(String fileNaam)
	{
		return Path.of(locatieFilestore, FileStoreLocation.DOCUMENT_UITWISSELING.getPath(), fileNaam).toString();
	}

	private <MB extends MergedBrieven<?>> Long getScreeningOrganisatieId(MB mergedBrieven)
	{
		if (mergedBrieven != null && mergedBrieven.getScreeningOrganisatie() != null)
		{
			return mergedBrieven.getScreeningOrganisatie().getId();
		}
		return organisatieService.getLandelijkeScreeningsorganisatie().getId();
	}

	@Override
	public boolean isAutomatischAfdrukkenParagonActief()
	{
		var startAutomatischAfdrukkenParagon = preferenceService.getString(PreferenceKey.START_AUTOMATISCH_AFDRUKKEN_PARAGON).orElse("20261201");
		var startDatum = DateUtil.parseLocalDateForPattern(startAutomatischAfdrukkenParagon, Constants.DATE_FORMAT_YYYYMMDD);
		return !currentDateSupplier.getLocalDate().isBefore(startDatum);
	}

	private <B extends Brief, MB extends MergedBrieven<?>> void maakBriefAfdrukOpdrachtVoorParagon(B briefOpdracht, UploadDocument templateDocument,
		IDocument documentDefinitie, IBrievenGeneratorHelper<B, MB> briefGenerator) throws Exception
	{
		var mergedDocument = appendDocument(templateDocument, briefOpdracht, null, briefGenerator);
		File pdfBestand = null;
		try
		{
			pdfBestand = maakPdfEnWerkInfoBij(briefGenerator, mergedDocument, null, List.of(briefOpdracht));
			var bestandsNaam = UUID.randomUUID().toString();
			pdfBestandOpslaanVoorVersturen(pdfBestand, bestandsNaam);
			setBriefGegenereerdInfo(briefOpdracht, documentDefinitie);
			maakBriefafdrukopdrachtMessage(briefOpdracht, bestandsNaam, briefGenerator);
		}
		catch (Exception e)
		{
			if (pdfBestand != null && !pdfBestand.delete())
			{
				LOG.warn("Tijdelijk PDF-bestand {} kon niet verwijderd worden", pdfBestand.getAbsolutePath());
			}
			throw e;
		}
	}

	<B extends Brief> void maakBriefafdrukopdrachtMessage(B brief, String bestandsNaam, IBrievenGeneratorHelper<B, ?> briefGenerator)
	{
		var batchApplicationType = briefGenerator.getBatchApplicationType();
		if (batchApplicationType == null)
		{
			throw new IllegalArgumentException(
				"BatchApplicationType is null voor brief " + brief.getId() + ":" + Hibernate.getClass(brief) + ", er wordt geen BRIEF_AFDRUKKEN message aangemaakt");
		}

		var briefafdrukopdrachtDto = briefGenerator.maakBriefafdrukopdrachtVoorGegenereerdeBrief(brief, currentDateSupplier.getLocalDateTime());
		briefafdrukopdrachtDto.setResources(List.of(BriefafdrukopdrachtDto.Resource.builder().order(1).path(bestandsNaam).build()));
		var message = messageService.queueMessage(MessageType.BRIEF_AFDRUKKEN, briefafdrukopdrachtDto, batchApplicationType.name());
		LOG.info("Briefafdrukopdracht message id '{}' aangemaakt voor brief id '{}' in {}", message.getId(), briefafdrukopdrachtDto.getEntityId(),
			briefafdrukopdrachtDto.getEntityType().getSimpleName());
	}

	@Override
	@Transactional
	public <B extends Brief> void setBriefGegenereerd(B brief)
	{
		setBriefGegenereerdInfo(brief, getDefinitiveBriefDefinitie(brief));
	}

	private <B extends Brief> void setBriefGegenereerdInfo(B brief, IDocument documentDefinitie)
	{
		brief.setGegenereerd(true);
		if (brief instanceof ProjectBrief projectBrief && projectBrief.getBrief() != null)
		{
			projectBrief.getBrief().setGegenereerd(true);
		}
		else if (brief instanceof ClientBrief<?, ?, ?> clientBrief && clientBrief.getProjectBrief() != null)
		{
			clientBrief.getProjectBrief().setGegenereerd(true);
		}
		brief.setTemplateNaam(documentDefinitie.getDocument().getNaam());
		if (documentDefinitie instanceof BriefDefinitie definitie)
		{ 
			brief.setBriefDefinitie(definitie);
		}
		hibernateService.saveOrUpdate(brief);
	}

	private <B extends Brief, MB extends MergedBrieven<?>> Document appendDocument(UploadDocument briefTemplateDocument, B brief, Document chunkDocument,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws Exception
	{
		var briefTemplate = uploadDocumentService.load(briefTemplateDocument);
		var briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);

		var client = getClientFromBrief(brief);
		var context = getMailMergeContext(brief, client);
		briefGenerator.additionalMergedContext(context);

		Document document;
		var creator = briefGenerator.getDocumentCreator(context);
		if (creator == null)
		{
			document = asposeService.processDocument(briefTemplateBytes, context);
		}
		else
		{
			document = asposeService.processDocumentWithCreator(context, briefTemplate, creator, true);
		}

		if (chunkDocument == null)
		{
			chunkDocument = document;
		}
		else
		{
			chunkDocument.appendDocument(document, ImportFormatMode.USE_DESTINATION_STYLES);
		}

		briefGenerator.additionalActiesWithDocument(context, brief, chunkDocument);
		return chunkDocument;
	}

	private <B extends Brief, MB extends MergedBrieven<?>> void setOrAppendPdf(MB huidigeMergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws IOException
	{
		if (huidigeMergedBrieven.getMergedBrieven() == null)
		{
			setPdfInMergedBrievenEntiteit(huidigeMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator);
		}
		else
		{
			if (!startNieuwPdfIfNeeded(huidigeMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator))
			{
				joinPdfs(huidigeMergedBrieven, nieuwPdfMetMergedBrieven);
			}
		}
	}

	private <MB extends MergedBrieven<?>, B extends Brief> void setPdfInMergedBrievenEntiteit(MB mergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator) throws IOException
	{
		setPdfInMergedBrievenEntiteit(mergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator, true);
	}

	private <MB extends MergedBrieven<?>, B extends Brief> void setPdfInMergedBrievenEntiteit(MB mergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator, boolean verwijderTmpFile) throws IOException
	{
		LOG.info(briefGenerator.getTechnischeLoggingMergedBriefAanmaken(mergedBrieven));
		var mergedBrievenPdfContainer = new UploadDocument();
		mergedBrievenPdfContainer.setActief(Boolean.TRUE);
		mergedBrievenPdfContainer.setContentType("application/pdf");
		mergedBrievenPdfContainer.setNaam(briefGenerator.getMergedBrievenNaam(mergedBrieven));
		mergedBrievenPdfContainer.setFile(nieuwPdfMetMergedBrieven);

		uploadDocumentService.saveOrUpdate(mergedBrievenPdfContainer, briefGenerator.getFileStoreLocation(), briefGenerator.getFileStoreId(), verwijderTmpFile);
		mergedBrieven.setMergedBrieven(mergedBrievenPdfContainer);
		LOG.info("Mergedocument(id = " + mergedBrieven.getId() + ") nieuw aangemaakt op filestore: " + mergedBrievenPdfContainer.getPath());
	}

	private <B extends Brief, MB extends MergedBrieven<?>> boolean startNieuwPdfIfNeeded(MB mergedBrieven, File nieuwPdfMetMergedBrieven,
		IBrievenGeneratorHelper<B, MB> briefGenerator)
		throws IOException
	{
		var huidigePdfMetMergedBrievenContainer = mergedBrieven.getMergedBrieven();
		var huidigePdfMetMergedBrieven = uploadDocumentService.load(huidigePdfMetMergedBrievenContainer);
		Integer maxMergedBrievenPdfSizeMB = organisatieParameterService.getOrganisatieParameter(mergedBrieven.getScreeningOrganisatie(),
			OrganisatieParameterKey.MAX_MERGED_BRIEVEN_PDF_SIZE_MB);
		if (maxMergedBrievenPdfSizeMB != null && huidigePdfMetMergedBrieven.length() + nieuwPdfMetMergedBrieven.length() > (long) maxMergedBrievenPdfSizeMB * BYTES_TO_MBS)
		{
			var createdMergedBrieven = briefGenerator.createMergedBrieven(mergedBrieven.getCreatieDatum());
			if (createdMergedBrieven != null)
			{
				completePdf(mergedBrieven);
				correctPdfFileNameIfNeeded(huidigePdfMetMergedBrievenContainer);
				briefGenerator.increasePdfCounter();
				setPdfInMergedBrievenEntiteit(createdMergedBrieven, nieuwPdfMetMergedBrieven, briefGenerator);
				return true;
			}
		}
		return false;
	}

	private <MB extends MergedBrieven<?>> void joinPdfs(MB huidigeMergedBrieven, File nieuwPdfMetMergedBrieven) throws IOException
	{
		var huidigePdfMetMergeBrievenContainer = huidigeMergedBrieven.getMergedBrieven();
		var huidigePdfMetMergeBrieven = uploadDocumentService.load(huidigePdfMetMergeBrievenContainer);
		var copyHuidigePdfMetMergedBrieven = File.createTempFile("copyMergedBrieven", ".pdf");
		FileUtils.copyFile(huidigePdfMetMergeBrieven, copyHuidigePdfMetMergedBrieven);

		try (var outputStream = new FileOutputStream(huidigePdfMetMergeBrieven))
		{
			var pdfMergerUtility = new PDFMergerUtility();
			pdfMergerUtility.addSource(copyHuidigePdfMetMergedBrieven);
			pdfMergerUtility.addSource(nieuwPdfMetMergedBrieven);
			pdfMergerUtility.setDestinationStream(outputStream);
			pdfMergerUtility.mergeDocuments(IOUtils.createMemoryOnlyStreamCache());

			verwijderBestandAlsMogelijk(copyHuidigePdfMetMergedBrieven);
			verwijderBestandAlsMogelijk(nieuwPdfMetMergedBrieven);
		}
	}

	private void verwijderBestandAlsMogelijk(File bestand)
	{
		if (bestand != null && !bestand.delete())
		{
			LOG.warn("Bestand {} kon niet verwijderd worden", bestand.getAbsolutePath());
		}
	}

	private void correctPdfFileNameIfNeeded(UploadDocument huidigePdfMetMergedBrievenContainer)
	{
		var huidigePdfNaam = huidigePdfMetMergedBrievenContainer.getNaam();
		huidigePdfNaam = huidigePdfNaam.replace(".pdf", "");
		if (!Pattern.compile("_\\d{2}$").matcher(huidigePdfNaam).find())
		{
			huidigePdfMetMergedBrievenContainer.setNaam(huidigePdfNaam + "_01.pdf");
			hibernateService.saveOrUpdate(huidigePdfMetMergedBrievenContainer);
		}
	}

	@Override
	public <B extends Brief> File maakPdfAVanBrief(B brief) throws Exception
	{
		var client = getClientFromBrief(brief);
		var briefTemplate = getBriefDefinitieFile(brief);
		var context = getMailMergeContext(brief, client);
		var briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
		var document = asposeService.processDocument(briefTemplateBytes, context);
		return genereerPdf(document, brief.getBriefType().toString(), false, true);
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief, BaseDocumentCreator documentCreator,
		Consumer<MailMergeContext> mergeContextConsumer) throws Exception
	{
		var client = getClientFromBrief(brief);
		var briefTemplate = getBriefDefinitieFile(brief);
		var context = getMailMergeContext(brief, client);
		if (mergeContextConsumer != null)
		{
			mergeContextConsumer.accept(context);
		}
		var document = asposeService.processDocumentWithCreator(context, briefTemplate, documentCreator, true);
		return genereerPdf(document, brief.getBriefType().toString(), true);
	}

	@Override
	public <B extends Brief> File maakPdfVanBrief(B brief, Consumer<MailMergeContext> mergeContextConsumer) throws Exception
	{
		var client = getClientFromBrief(brief);
		var briefTemplate = getBriefDefinitieFile(brief);
		var context = getMailMergeContext(brief, client);
		if (mergeContextConsumer != null)
		{
			mergeContextConsumer.accept(context);
		}
		var briefTemplateBytes = FileUtils.readFileToByteArray(briefTemplate);
		var document = asposeService.processDocument(briefTemplateBytes, context);
		return genereerPdf(document, brief.getBriefType().toString(), true);
	}

	private <B extends Brief> File getBriefDefinitieFile(B brief)
	{
		var briefDefinitie = getDefinitiveBriefDefinitie(brief);
		var uploadDocument = briefDefinitie.getDocument();
		return uploadDocumentService.load(uploadDocument);
	}

	private <B extends Brief> MailMergeContext getMailMergeContext(B brief, Client client)
	{
		var context = new MailMergeContext();
		context.setClient(client);
		context.setBrief(brief);
		return context;
	}

	private <B extends Brief> Client getClientFromBrief(B brief)
	{
		Client client = null;
		if (ClientBrief.class.isAssignableFrom(Hibernate.getClass(brief)))
		{
			var clientBrief = (ClientBrief<?, ?, ?>) Hibernate.unproxy(brief);
			client = clientBrief.getClient();
		}
		return client;
	}

	private <B extends Brief> IDocument getDefinitiveBriefDefinitie(B brief)
	{
		IDocument briefDefinitie = getNieuwsteBriefDefinitie(brief.getBriefType());
		var briefClass = Hibernate.getClass(brief);
		if (ProjectBrief.class.isAssignableFrom(briefClass))
		{
			var projectBrief = (ProjectBrief) Hibernate.unproxy(brief);
			briefDefinitie = projectBrief.getDefinitie();
		}
		return briefDefinitie;
	}

	@Override
	public File genereerPdf(Document document, String fileNaam, boolean autoShowPrintdialog) throws Exception
	{
		return genereerPdf(document, fileNaam, autoShowPrintdialog, false);
	}

	private File genereerPdf(Document document, String fileNaam, boolean autoShowPrintdialog, boolean usePdfA) throws Exception
	{
		var tmpPdfFile = File.createTempFile(fileNaam, ".pdf");

		try (var tempStream = new ByteArrayOutputStream();
			var pdfOutStream = new FileOutputStream(tmpPdfFile))
		{
			var pdfSaveOptions = asposeService.getPdfSaveOptions();
			var tempPdfSaveOptions = pdfSaveOptions.deepClone();
			if (usePdfA)
			{
				tempPdfSaveOptions.setCompliance(PdfCompliance.PDF_A_2_U);
			}
			document.save(tempStream, tempPdfSaveOptions);
			var pdfBoxDocument = Loader.loadPDF(new ByteArrayInputStream(tempStream.toByteArray()).readAllBytes());
			if (autoShowPrintdialog)
			{
				var javaScript = new PDActionJavaScript(JavaScriptPdfHelper.getPrintJavascript());
				pdfBoxDocument.getDocumentCatalog().setOpenAction(javaScript);
			}
			pdfBoxDocument.save(pdfOutStream);
			pdfBoxDocument.close();
		}
		return tmpPdfFile;
	}

	@Override
	public <B extends Brief> List<B> getNietGegenereerdeBrievenVanBriefTypes(List<B> brieven, List<BriefType> brieftypes)
	{
		return brieven.stream()
			.filter(brief -> brieftypes.contains(brief.getBriefType()) && !BriefUtil.isGegenereerd(brief)).collect(Collectors.toList());
	}

	@Override
	@Transactional
	public void setNietGegenereerdeBrievenOpTegenhouden(ScreeningRonde<?, ?, ?, ?> screeningRonde, Collection<BriefType> brieftypes)
	{
		screeningRonde.getBrieven().stream().filter(brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isNietGegenereerdEnNietVervangen(brief))
			.forEach(brief -> hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true)));
	}

	@Override
	public boolean briefTypeWachtOpKlaarzettenInDezeRonde(ClientBrief<?, ?, ?> brief)
	{
		return briefTypeWachtOpKlaarzettenInDezeRonde(brief.getScreeningRonde(), Collections.singletonList(brief.getBriefType()));
	}

	@Override
	public boolean briefTypeWachtOpKlaarzettenInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes)
	{
		return ronde.getBrieven().stream().anyMatch(
			brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isNietGegenereerdEnNietVervangen(brief));
	}

	@Override
	public boolean briefTypeAlVerstuurdInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> brieftypes)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> brieftypes.contains(brief.getBriefType()) && BriefUtil.isGegenereerd(brief));
	}

	@Override
	public List<ClientBrief<?, ?, ?>> getClientBrieven(Client client)
	{
		return clientBriefRepository.findAll(heeftClient(client));
	}

	@Override
	@Transactional
	public void briefTegenhouden(ClientBrief brief, Account account)
	{
		hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_TEGENHOUDEN, account, brief.getClient(), BriefUtil.getBriefTypeNaam(brief) + " wordt tegengehouden.",
			BriefUtil.getOnderzoekenUitBriefType(brief));
	}

	@Override
	@Transactional
	public void briefNietMeerTegenhouden(ClientBrief brief, Account account)
	{
		hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, false));
		logService.logGebeurtenis(LogGebeurtenis.BRIEF_DOORVOEREN, account, brief.getClient(), BriefUtil.getBriefTypeNaam(brief) + " was tegengehouden en wordt nu doorgevoerd.",
			BriefUtil.getOnderzoekenUitBriefType(brief));
	}

	@Override
	public boolean briefTypeGemaaktInDezeRonde(ScreeningRonde<?, ?, ?, ?> ronde, Collection<BriefType> briefTypes)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> briefTypes.contains(brief.getBriefType()));
	}

	@Override
	public <B extends Brief> void verwijderBrief(B brief)
	{
		var mergedBrieven = brief.getMergedBrieven();
		if (mergedBrieven != null)
		{
			mergedBrieven.getBrieven().remove(brief);
			hibernateService.save(mergedBrieven);
		}
		hibernateService.delete(brief);
	}

	@Override
	@Transactional
	public <MB extends MergedBrieven<?>> void verwijderMergedBrieven(MB mergedBrieven)
	{
		mergedBrieven.setVerwijderd(true);
		var uploadDocument = mergedBrieven.getMergedBrieven();
		if (uploadDocument != null)
		{
			uploadDocumentService.delete(uploadDocument);
			mergedBrieven.setMergedBrieven(null);
		}
	}

	@Override
	@Transactional  
	public boolean isOverbruggingssituatieParagonStarted()
	{
		var startOvergangssituatieParagon = preferenceService.getString(PreferenceKey.START_OVERBRUGGINGSSITUATIE_PARAGON).orElse("20260701");
		var startDatum = DateUtil.parseLocalDateForPattern(startOvergangssituatieParagon, Constants.DATE_FORMAT_YYYYMMDD);
		return !currentDateSupplier.getLocalDate().isBefore(startDatum);
	}

	@Override
	public File maakPdfVanUploadDocument(UploadDocument document) throws Exception
	{
		var file = uploadDocumentService.load(document);
		var doc = asposeService.maakDocument(file);
		if (doc != null)
		{
			return genereerPdf(doc, "brieftemplate_inzien", false);
		}
		LOG.error("Aspose document kan niet aangemaakt worden voor upload document id: '{}'", document.getId());
		return null;
	}
}
