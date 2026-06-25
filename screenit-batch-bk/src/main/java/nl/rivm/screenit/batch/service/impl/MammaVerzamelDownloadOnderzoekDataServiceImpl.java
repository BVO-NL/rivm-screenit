package nl.rivm.screenit.batch.service.impl;

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
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.ZipOutputStream;

import jakarta.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.MammaVerzamelDownloadOnderzoekDataService;
import nl.rivm.screenit.batch.service.MammaVerzamelOnderzoekDataService;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekenVerzoekRepository;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class MammaVerzamelDownloadOnderzoekDataServiceImpl implements MammaVerzamelDownloadOnderzoekDataService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaVerzamelOnderzoekDataService verzamelOnderzoekDataService;

	@Autowired
	private MammaBaseUitwisselportaalService uitwisselPortaalservice;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private MammaDownloadOnderzoekenVerzoekRepository downloadOnderzoekenVerzoekRepository;

	@Autowired
	private DatabaseRunner databaseRunner;

	@PostConstruct
	public void init()
	{
		berichtToBatchService.queueMammaVerzamelOnderzoeksDataBericht();
	}

	@Override
	public List<Long> getAlleVerzamelDownloadOnderzoekDataVerzoeken()
	{
		var resultList = new ArrayList<Long>();
		databaseRunner.runInSessionOnly(
			() -> resultList.addAll(downloadOnderzoekenVerzoekRepository.getMammaDownloadOnderzoekenVerzoekByStatus(BestandStatus.NOG_TE_VERWERKEN).stream()
				.map(MammaDownloadOnderzoekenVerzoek::getId).toList()));
		return resultList;
	}

	@Override
	public void verzamelOnderzoekData(Long verzoekId)
	{

		databaseRunner.runInSessionOnly(() ->
		{
			var success = new AtomicBoolean(true);
			MammaDownloadOnderzoekenVerzoek verzoekInner = null;
			try
			{
				verzoekInner = hibernateService.get(MammaDownloadOnderzoekenVerzoek.class, verzoekId);
				LOG.info("Start verzamelen van beelden en verslag voor verzoek '{}'", verzoekInner.getId());
				var finalVerzoekInner = verzoekInner;
				databaseRunner.runInNewTransaction(() ->
				{
					finalVerzoekInner.setGewijzigdOp(dateSupplier.getDate());
					finalVerzoekInner.setStatus(BestandStatus.BEZIG_MET_VERWERKEN);
				});
				for (var onderzoek : verzoekInner.getOnderzoeken())
				{
					Files.createDirectories(Paths.get(uitwisselPortaalservice.getOnderzoekRootPath(onderzoek)));
					databaseRunner.runInNewTransaction(() -> onderzoek.setStatus(BestandStatus.BEZIG_MET_VERWERKEN));
					success.compareAndSet(true, verzamelOnderzoekDataService.addBeeldenAanVerzoek(onderzoek));
					verzamelOnderzoekDataService.addVerslagAanVerzoek(onderzoek);
				}
				createEmptyZipFile(verzoekInner);
				uitwisselPortaalservice.zetFilesInZip(verzoekInner);
			}
			catch (Exception e)
			{
				success.set(false);
				LOG.error("Fout bij verzamelen van beelden of verslagen: ", e);
			}
			finally
			{
				if (verzoekInner != null)
				{
					var finalVerzoekInner = verzoekInner;
					databaseRunner.runInNewTransaction(() ->
					{
						finalVerzoekInner.setStatus(success.get() ? BestandStatus.VERWERKT : BestandStatus.CRASH);
						LOG.info("Klaar met verzamelen van beelden en verslag voor verzoek {}", finalVerzoekInner.getId());
					});
				}
			}
		});
	}

	private void createEmptyZipFile(MammaDownloadOnderzoekenVerzoek verzoek) throws IOException
	{
		var f = File.createTempFile("onderzoekData", ".zip");
		try (var out = new ZipOutputStream(new FileOutputStream(f)))
		{
			out.closeEntry();
		}
		var document = verzoek.getZipBestand();
		document.setFile(f);

		uploadDocumentService.update(document);
	}

}
