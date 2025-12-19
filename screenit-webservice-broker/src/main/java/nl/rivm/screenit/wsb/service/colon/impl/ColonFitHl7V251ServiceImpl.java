package nl.rivm.screenit.wsb.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.berichten.ColonFitAnalyseResultatenBericht;
import nl.rivm.screenit.model.colon.berichten.ColonHl7BerichtToFitAnalyseResultaatSetWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultatenBerichtRepository;
import nl.rivm.screenit.repository.colon.ColonFitLaboratoriumRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.wsb.service.BaseHL7v2Service;
import nl.rivm.screenit.wsb.service.colon.ColonFitHl7v251Service;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.UncategorizedJmsException;
import org.springframework.stereotype.Service;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Service
@Slf4j
public class ColonFitHl7V251ServiceImpl extends BaseHL7v2Service<OUL_R22> implements ColonFitHl7v251Service
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private ColonFitLaboratoriumRepository fitLaboratoriumRepository;

	@Autowired
	private ColonFitAnalyseResultatenBerichtRepository fitAnalyseResultatenBerichtRepository;

	@Override
	public Message processTypedMessage(OUL_R22 message)
	{
		ColonHl7BerichtToFitAnalyseResultaatSetWrapper wrapper = null;
		try
		{
			wrapper = new ColonHl7BerichtToFitAnalyseResultaatSetWrapper(message);
			verwerkBericht(wrapper);
			return message.generateACK();
		}
		catch (NullPointerException | HL7Exception ex)
		{
			return generateApplicationError(ex, message);
		}
		catch (Exception e)
		{
			return generateApplicationReject(e, message);
		}
		finally
		{
			if (wrapper != null)
			{
				try
				{
					verwerkBerichtService.queueFitAnalyseResultatenBericht(wrapper.getMessageId());
				}
				catch (UncategorizedJmsException e)
				{
					LOG.error("Er is een probleem met ActiveMQ.", e);
				}
			}
		}
	}

	private void verwerkBericht(ColonHl7BerichtToFitAnalyseResultaatSetWrapper wrapper) throws HL7Exception
	{
		var laboratorium = getLaboratorium(wrapper);

		if (fitAnalyseResultatenBerichtRepository.existsByMessageId(wrapper.getMessageId()))
		{
			var melding = "FIT HL7 Bericht (messageID: " + wrapper.getMessageId() + ") al eerder binnengekomen voor lab: " + laboratorium.getNaam();
			logging(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTATEN_SET_AANNEMEN, Level.WARNING, laboratorium, melding, Bevolkingsonderzoek.COLON);
			return;
		}

		logging(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTATEN_SET_AANNEMEN, Level.INFO, laboratorium,
			"Bericht (messageID: " + wrapper.getMessageId() + ") binnengekomen voor lab: " + laboratorium.getNaam(), Bevolkingsonderzoek.COLON);

		var analyseResultatenBericht = new ColonFitAnalyseResultatenBericht();
		analyseResultatenBericht.setMessageId(wrapper.getMessageId());
		analyseResultatenBericht.setStatus(BerichtStatus.NIEUW);
		analyseResultatenBericht.setLaboratorium(laboratorium);
		analyseResultatenBericht.setStatusDatum(dateSupplier.getDate());
		analyseResultatenBericht.setOntvangen(dateSupplier.getDate());
		analyseResultatenBericht.setHl7Bericht(wrapper.getMessage().toString());
		hibernateService.saveOrUpdate(analyseResultatenBericht);
	}

	private ColonFitLaboratorium getLaboratorium(ColonHl7BerichtToFitAnalyseResultaatSetWrapper message) throws HL7Exception
	{
		var labId = message.getLabId();
		var laboratorium = fitLaboratoriumRepository.findByLabId(labId);
		if (laboratorium != null)
		{
			return laboratorium;
		}
		throw new HL7Exception("FIT HL7 Bericht (messageID: " + message.getMessageId() + ") binnengekomen, geen lab gevonden met ID: " + labId);
	}

	private Message generateApplicationError(Exception ex, Message message)
	{
		var foutmelding = ex.getMessage();

		if (StringUtils.isBlank(foutmelding))
		{
			foutmelding = "Er is een (syntax)fout gevonden bij de verwerking van het FIT HL7 bericht. Het bericht zal niet verder worden verwerkt.";
		}
		LOG.error(foutmelding, ex);
		try
		{
			logging(LogGebeurtenis.COLON_FIT_ANALYSE_RESULTATEN_SET_FOUT, Level.ERROR, null, foutmelding, Bevolkingsonderzoek.COLON);
			return message.generateACK(AcknowledgmentCode.AE, new HL7Exception("Er is een fout opgetreden met de verwerking van het HL7 bericht."));
		}
		catch (HL7Exception | IOException e)
		{
			LOG.error("Er is gewoon helemaal niks meer mogelijk!", e);
		}
		return null;
	}

	private Message generateApplicationReject(Exception e, Message message)
	{
		try
		{
			var foutmelding = "Er is een onbekende fout opgetreden in de applicatie.";
			LOG.error(foutmelding, e);
			return message.generateACK(AcknowledgmentCode.AR, new HL7Exception(foutmelding));
		}
		catch (HL7Exception | IOException e1)
		{
			LOG.error("Er is gewoon helemaal niks meer mogelijk!", e1);
		}
		return null;
	}

}
