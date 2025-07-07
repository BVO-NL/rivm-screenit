package nl.rivm.screenit.model.berichten;

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

import org.apache.commons.lang.StringUtils;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.ACK;
import ca.uhn.hl7v2.util.StringUtil;

public class ScreenITResponseV251MessageWrapper implements ScreenITResponseHL7v2MessageWrapper
{

	private final AcknowledgmentCode acknowledgmentCode;

	private final String acknowledgmentCodeString;

	private final String melding;

	public ScreenITResponseV251MessageWrapper(Message message)
	{
		acknowledgmentCodeString = getAckCode(message);
		if (acknowledgmentCodeString != null)
		{
			acknowledgmentCode = AcknowledgmentCode.valueOf(acknowledgmentCodeString);
		}
		else
		{
			acknowledgmentCode = AcknowledgmentCode.AE;
		}
		var meldingUitBericht = getFoutmelding(message);
		if (StringUtil.isBlank(meldingUitBericht) && acknowledgmentCodeString == null)
		{
			melding = "Kon de acknowledgmentcode niet uit het bericht halen.";
		}
		else
		{
			melding = meldingUitBericht.trim();
		}
	}

	@Override
	public AcknowledgmentCode getAcknowledgmentCode()
	{
		return acknowledgmentCode;
	}

	@Override
	public String getMelding()
	{
		return melding;
	}

	private String getFoutmelding(Message response)
	{
		var ack = (ACK) response;
		var error = ack.getERR(0);
		var cwe = error.getErr3_HL7ErrorCode();
		var errorMelding = StringUtils.defaultIfBlank(cwe.getCwe9_OriginalText().getValue(), "");
		var ackMelding = StringUtils.defaultIfBlank(ack.getMSA().getMsa3_TextMessage().getValue(), "");
		return errorMelding + " " + ackMelding;
	}

	private String getAckCode(Message response)
	{
		var ack = (ACK) response;
		var msa = ack.getMSA();
		return msa.getAcknowledgmentCode().getValue();
	}

	@Override
	public String getAcknowledgmentCodeString()
	{
		return acknowledgmentCodeString;
	}

}
