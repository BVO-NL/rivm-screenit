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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.CommunicationHubClientConfig;
import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.service.DigitaalClientBerichtService;
import nl.topicuszorg.communicationhub.api.MessageServiceCommunicationHubClientApi;
import nl.topicuszorg.communicationhub.api.model.SmsMessage;

@Slf4j
@RequiredArgsConstructor
public abstract class DigitaalClientBerichtServiceImpl<CB extends DigitaalClientBericht<?>> implements DigitaalClientBerichtService<CB>
{

	private final MessageServiceCommunicationHubClientApi messageServiceApi;

	private final CommunicationHubClientConfig communicationHubClientConfig;

	@Override
	public String haalSmsBerichtOp(CB digitaalClientBericht)
	{
		var smsComHubGuid = digitaalClientBericht.getSmsComHubGuid();
		if (smsComHubGuid != null)
		{
			try
			{
				var response = messageServiceApi.getMessageChanges(communicationHubClientConfig.getTenant(), smsComHubGuid);
				if ((response.getMessage() instanceof SmsMessage smsMessage))
				{
					return smsMessage.getContent();
				}
			}
			catch (Exception e)
			{
				LOG.error("Fout bij ophalen SMS bericht met guid: {}", smsComHubGuid, e);
				return BERICHT_KON_NIET_WORDEN_OPGEHAALD_KEY;
			}
		}
		return GEEN_BERICHT_BESCHIKBAAR_KEY;
	}
}
