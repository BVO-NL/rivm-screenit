package nl.rivm.screenit.wsb.servlet.hoh;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.handler.ColonFITHL7v251Handler;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.hoh.api.IMessageHandler;
import ca.uhn.hl7v2.hoh.api.IReceivable;
import ca.uhn.hl7v2.hoh.api.IResponseSendable;
import ca.uhn.hl7v2.hoh.raw.api.RawSendable;
import ca.uhn.hl7v2.hoh.raw.server.HohRawServlet;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

import static nl.rivm.screenit.util.Hl7v2BerichtUtil.createMessage;

@Slf4j
public class Hl7v2OverHttpServlet extends HohRawServlet
{

	public Hl7v2OverHttpServlet()
	{
		setMessageHandler(new HL7v2MessageHandler());
	}

	private static class HL7v2MessageHandler implements IMessageHandler<String>
	{

		@Override
		public IResponseSendable<String> messageReceived(IReceivable<String> theReceived)
			throws UnsupportedOperationException
		{
			var fithl7v251Handler = new ColonFITHL7v251Handler(OUL_R22.class);
			var incomingRawMsg = theReceived.getMessage();
			String ack;
			try
			{
				var oulBericht = createMessage(incomingRawMsg);
				ack = fithl7v251Handler.processTypedMessage(oulBericht).encode();
			}
			catch (ApplicationException | HL7Exception e)
			{
				LOG.error("Er is een probleem opgetreden met het verwerken van het HL7V2 bericht", e);
				throw new RuntimeException(e);
			}

			return new RawSendable(ack);
		}
	}
}
