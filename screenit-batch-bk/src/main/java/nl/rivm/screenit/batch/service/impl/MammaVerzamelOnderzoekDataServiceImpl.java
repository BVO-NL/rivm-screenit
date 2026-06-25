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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MammaVerzamelOnderzoekDataService;
import nl.rivm.screenit.batch.service.impl.dicom.CMoveSCU;
import nl.rivm.screenit.batch.service.impl.dicom.DicomDir;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.dicom.CMoveConfig;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.dcm4che3.data.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class MammaVerzamelOnderzoekDataServiceImpl implements MammaVerzamelOnderzoekDataService
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseUitwisselportaalService uitwisselPortaalservice;

	@Override
	@Transactional
	public void addVerslagAanVerzoek(MammaDownloadOnderzoek onderzoek)
	{
		uitwisselPortaalservice.kopieerVerslagPdfNaarDownloadVerzoekMap(onderzoek);
	}

	@Override
	@Transactional
	public boolean addBeeldenAanVerzoek(MammaDownloadOnderzoek downloadOnderzoek)
	{
		boolean success;

		var connectionString = preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_IMS_DICOM_CMOVE_CONFIG.toString());
		var moveConfig = CMoveConfig.parse(connectionString);
		var moveSCU = new CMoveSCU();
		var onderzoek = downloadOnderzoek.getOnderzoek();
		var downloadOnderzoekId = downloadOnderzoek.getId();
		var uitnodigingsNr = onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getUitnodigingsNr();
		LOG.info("Start verzamelen van beelden voor download onderzoek {} (Creatiedatum {}, uitnodigingsNr: {})", downloadOnderzoekId, onderzoek.getCreatieDatum(), uitnodigingsNr);
		success = moveSCU.retrieve(moveConfig, uitnodigingsNr);
		var result = moveSCU.getResult();
		if (result != null)
		{
			var status = result.getInt(Tag.Status, 0);
			var nrOfCompeleteSuboperations = result.getInt(Tag.NumberOfCompletedSuboperations, 0);
			LOG.info("Response van Sectra op CMove: \n" + result);
			if (status > 0)
			{
				var errorComment = result.getString(Tag.ErrorComment);
				if (StringUtils.isNotBlank(errorComment))
				{
					LOG.error("Error from IMS: {} Download onderzoek: {}", errorComment, downloadOnderzoekId);
					downloadOnderzoek.setStatusMelding("Foutmelding ontvangen van IMS. Neem contact op met helpdesk.");
				}
				else
				{
					var nrOfFailedSuboperations = result.getInt(Tag.NumberOfFailedSuboperations, 0);
					var nrOfWarningSuboperations = result.getInt(Tag.NumberOfWarningSuboperations, 0);
					downloadOnderzoek
						.setStatusMelding("#" + (nrOfFailedSuboperations + nrOfWarningSuboperations) + " (E" + nrOfFailedSuboperations + "/W" + nrOfWarningSuboperations
							+ ") beelden van totaal #" + (nrOfFailedSuboperations + nrOfWarningSuboperations + nrOfCompeleteSuboperations) + " niet kunnen ophalen.");
					LOG.error("Status melding van IMS: {} Download onderzoek: {}", downloadOnderzoek.getStatusMelding(), downloadOnderzoekId);
				}
				downloadOnderzoek.setStatus(BestandStatus.CRASH);
				success = false;
			}
			else
			{
				downloadOnderzoek.setStatusMelding("#" + nrOfCompeleteSuboperations + " beelden opgehaald.");
				downloadOnderzoek.setStatus(BestandStatus.VERWERKT);
				LOG.info("{} Download onderzoek: {}", downloadOnderzoek.getStatusMelding(), downloadOnderzoekId);
				createDicomDirFile(uitwisselPortaalservice.getOnderzoekRootPath(downloadOnderzoek));
			}
		}
		else
		{
			downloadOnderzoek.setStatus(BestandStatus.CRASH);
			downloadOnderzoek.setStatusMelding("Geen reactie van IMS");
			LOG.error("{} Download onderzoek: {}", downloadOnderzoek.getStatusMelding(), downloadOnderzoekId);
			success = false;
		}
		return success;
	}

	private void createDicomDirFile(String onderzoekRootPath)
	{
		var dicomDir = new DicomDir(onderzoekRootPath);
		try
		{
			dicomDir.create();
		}
		catch (Exception e)
		{
			LOG.error("Fout tijdens maken van DICOMDIR voor map " + onderzoekRootPath, e);
		}
		finally
		{
			dicomDir.close();
		}
	}

}
