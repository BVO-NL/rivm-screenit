package nl.rivm.screenit.model.cervix.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Slf4j
public class CervixHpvBerichtWrapper
{
	private static String POSCONTROL = "POSCONTROL";

	private static String NEGCONTROL = "NEGCONTROL";

	@Getter
	private final OUL_R22 message;

	@Getter
	private final List<CervixHpvMonsterWrapper> results = new ArrayList<>();

	public CervixHpvBerichtWrapper(OUL_R22 message) throws HL7Exception
	{
		this.message = message;
		splitsResults();
	}

	private void splitsResults() throws HL7Exception
	{
		var allSpecimen = message.getSPECIMENAll();
		for (var specimen : allSpecimen)
		{
			var cervixHpvMonsterWrapper = new CervixHpvMonsterWrapper(specimen);
			if (!isControleWaarde(cervixHpvMonsterWrapper))
			{
				results.add(cervixHpvMonsterWrapper);
			}
		}
	}

	private boolean isControleWaarde(CervixHpvMonsterWrapper hpvMonsterWrapper)
	{
		return NEGCONTROL.equals(hpvMonsterWrapper.getControleWaarde()) || POSCONTROL.equals(hpvMonsterWrapper.getControleWaarde())
			|| hpvMonsterWrapper.getBarcode().toUpperCase().startsWith("Q");
	}

	public String getMessageId()
	{
		var header = message.getMSH();
		var messageId = header.getMsh10_MessageControlID();
		return messageId.getValue();
	}

	public String getInstrumentId()
	{
		var header = message.getMSH();
		var sendingApplication = header.getMsh3_SendingApplication();
		var instrumentId = sendingApplication.getHd2_UniversalID();
		return instrumentId.getValue();
	}

	public String getLabnaam()
	{
		var header = message.getMSH();
		var sendingFacility = header.getMsh4_SendingFacility();
		var labNaam = sendingFacility.getHd1_NamespaceID();
		return labNaam.getValue();
	}

	public boolean isValid()
	{
		LOG.debug("Start bericht validatie");
		LOG.debug("MessageID: " + getMessageId());
		LOG.debug("Labnaam: " + getLabnaam());
		LOG.debug("Instrument ID: " + getInstrumentId());
		LOG.debug("Aantal uitslagen: " + results.size());
		if (getMessageId() != null && getInstrumentId() != null)
		{
			for (var sample : results)
			{
				if (!sample.isValid())
				{
					LOG.debug("Bericht invalide");
					return false;
				}
			}
			LOG.debug("Bericht valide");
			return true;
		}
		LOG.debug("Bericht invalide");
		return false;
	}

}
