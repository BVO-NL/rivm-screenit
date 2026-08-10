package nl.rivm.screenit.batch.jobs.mamma.palga.csvexport.step;

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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvExportClientProjectie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaExportConfig;
import nl.rivm.screenit.model.batch.popupconfig.MammaPalgaGrondslag;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.rivm.screenit.util.CsvUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.StringUtil;
import nl.rivm.screenit.util.ZipUtil;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.ScrollableResults;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;

import au.com.bytecode.opencsv.CSVWriter;

@Slf4j
@Component
public class MammaPalgaCsvExportTasklet implements Tasklet
{

	@Autowired
	private String locatieFilestore;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaPalgaService palgaService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public RepeatStatus execute(@Nullable StepContribution contribution, ChunkContext chunkContext) throws IOException
	{
		var jobParameters = chunkContext.getStepContext().getStepExecution()
			.getJobExecution().getJobParameters();
		var exportConfig = objectMapper.readValue(jobParameters.getString(JobStartParameter.MAMMA_PALGA_EXPORT.name()), MammaPalgaExportConfig.class);

		var path = locatieFilestore + FileStoreLocation.MAMMA_PALGA_CSV_EXPORT.getPath();
		var filePrefix = getFilePrefix(exportConfig);
		var csvDocuments = genereerCsvDocuments(path, filePrefix, exportConfig);
		palgaService.deleteExports(null, null);
		zipExport(csvDocuments, filePrefix, path);

		return RepeatStatus.FINISHED;
	}

	private List<UploadDocument> genereerCsvDocuments(String path, String filePrefix, MammaPalgaExportConfig exportConfig) throws IOException
	{
		var aantalClienten = palgaService.getAantalClientenVoorPalgaExport(exportConfig);
		LOG.info("#clienten gevonden: {}", aantalClienten);
		List<UploadDocument> export = new ArrayList<>();
		if (aantalClienten > 0)
		{
			var aantalClientenPerFile = exportConfig.getMaxAantalPerFile();
			var aantalFiles = (int) Math.ceil((double) aantalClienten / aantalClientenPerFile);
			if (aantalFiles > 99)
			{
				throw new IllegalStateException("Te veel files aangemaakt: " + aantalFiles);
			}

			try (var clientExport = palgaService.getClientProjectieVoorPalgaExportScrollable(exportConfig))
			{
				var fileNummer = 0;
				while (clientExport.next())
				{
					export.add(genereerCsv(clientExport, aantalClientenPerFile, ++fileNummer, path, filePrefix));
				}
			}
		}
		else
		{
			LOG.warn("Geen clienten gevonden voor export.");
		}
		return export;
	}

	private UploadDocument genereerCsv(ScrollableResults<MammaPalgaCsvExportClientProjectie> exportGegevens, int aantalClientenPerFile, int fileNummer, String path,
		String prefix) throws IOException
	{
		var file = new File(path);
		file.mkdirs();
		var document = new UploadDocument();
		var fileName = getFileName(prefix, fileNummer) + ".csv";
		file = new File(path + fileName);
		try (var csvOutput = new CSVWriter(new FileWriter(file, false), ';', CSVWriter.NO_QUOTE_CHARACTER))
		{
			var aantalClienten = verwerkHuidigeBatch(exportGegevens, aantalClientenPerFile,
				clientProjectie -> csvOutput.writeNext(getCsvGegevens(clientProjectie).toArray(new String[] {})));
			LOG.info("CSV voor download gevuld: {}, aantal clienten: {}", fileName, aantalClienten);
		}
		CsvUtil.truncateLastLine(file);
		document.setFile(file);
		document.setNaam(fileName);
		document.setPath(file.getPath().replace(locatieFilestore, ""));
		return document;
	}

	private <T> int verwerkHuidigeBatch(ScrollableResults<T> resultaten, int maximaleBatchGrootte, Consumer<T> verwerker)
	{
		verwerker.accept(resultaten.get());
		var aantalVerwerkt = 1;
		while (aantalVerwerkt < maximaleBatchGrootte && resultaten.next())
		{
			verwerker.accept(resultaten.get());
			aantalVerwerkt++;
		}
		return aantalVerwerkt;
	}

	private String getFilePrefix(MammaPalgaExportConfig exportConfig)
	{
		var exportdatum = DateUtil.LOCAL_DATE_FORMAT_YYYYMMDD.format(currentDateSupplier.getLocalDate());
		return String.format("CHTRDS%s%s%02d", exportdatum, exportConfig.getGewensteUitslag().getCodeInFilePrefix(), leveringsnummerVoorFilePrefix(exportConfig));
	}

	private int leveringsnummerVoorFilePrefix(MammaPalgaExportConfig exportConfig)
	{
		return exportConfig.getGrondslag() == MammaPalgaGrondslag.KWALITEITSBORGING ? exportConfig.getVolgnummerKwaliteitsborging() : 0;
	}

	private String getFileName(String prefix, int fileNummer)
	{
		return prefix + String.format("%02d", fileNummer);
	}

	private List<String> getCsvGegevens(MammaPalgaCsvExportClientProjectie projectie)
	{
		List<String> gegevens = new ArrayList<>();

		gegevens.add(Long.toString(projectie.dossierId()));
		var voorlettersClient = NaamUtil.getVoorletters(projectie.voornaam());
		var voorletterClient = StringUtils.isNoneBlank(voorlettersClient) && !voorlettersClient.isEmpty() && StringUtil.isAlfabetKarakter(voorlettersClient.charAt(0))
			? voorlettersClient.substring(0, 1) : "";
		gegevens.add(voorletterClient);
		gegevens.add(projectie.achternaam().trim());
		gegevens.add(Constants.getDateYYYYMMDDFormat().format(projectie.geboortedatum()));
		gegevens.add(projectie.geslacht().getMnem());
		gegevens.add(projectie.bsn());
		return gegevens;
	}

	private void zipExport(List<UploadDocument> export, String prefix, String path) throws IOException
	{
		if (!export.isEmpty())
		{
			var fileName = getFileName(prefix, 0) + ".zip";
			var zipFile = ZipUtil.maakZips(export, path + fileName, 1048576).iterator().next();
			var zipDocument = new UploadDocument();
			zipDocument.setFile(zipFile);
			zipDocument.setNaam(fileName);
			zipDocument.setActief(true);
			zipDocument.setContentType("application/zip");
			for (var document : export)
			{
				uploadDocumentService.delete(document);
			}
			palgaService.saveOrUpdateExport(zipDocument);
		}
		else
		{
			LOG.warn("Geen export om te zippen.");
		}
	}
}
