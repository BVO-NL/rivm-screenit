package nl.rivm.screenit.service;

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

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Base64;
import java.util.Optional;

import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.model.MailMergeContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static nl.rivm.screenit.Constants.INLINE_ID_SO_LOGO_EMAIL;

public interface BaseMailMergeService extends MergeMailAttachmentService
{
	Logger LOG = LoggerFactory.getLogger(BaseMailMergeService.class);

	default Optional<MailAttachmentDto> voegSoLogoToe(MailMergeContext mailMergeContext, UploadDocumentService uploadDocumentService)
	{
		var logoBrief = mailMergeContext.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie().getLogoBrief();
		var logoBestand = uploadDocumentService.load(logoBrief);

		try (InputStream inputStream = new FileInputStream(logoBestand))
		{
			var encodedImage = Base64.getEncoder().encodeToString(inputStream.readAllBytes());
			var mailAttachment = new MailAttachmentDto();
			mailAttachment.setContent(encodedImage);
			mailAttachment.setContentType("image/jpg");
			mailAttachment.setInlineContentId(INLINE_ID_SO_LOGO_EMAIL);
			mailAttachment.setFileName("logo.jpg");
			return Optional.of(mailAttachment);
		}
		catch (Exception e)
		{
			LOG.error("bvo logo omzetten naar bytes mislukt {}", e.getMessage());
		}

		return Optional.empty();
	}
}
