package nl.rivm.screenit.service.colon.impl;

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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.model.DigitaalBerichtTemplate;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.colon.ColonMergeMailAttachmentService;

import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ColonMergeMailAttachmentServiceImpl implements ColonMergeMailAttachmentService
{
	private final UploadDocumentService uploadDocumentService;

	@Override
	public List<MailAttachmentDto> maakMailAttachmentsAan(MailMergeContext mailMergeContext, DigitaalBerichtTemplate template)
	{
		var mailAttachmentList = new ArrayList<MailAttachmentDto>();
		var mergefields = template.getType().getMergeFields();
		var bodyTekst = template.getBody();
		String searchString = "{" + MergeField.SO_LOGO_EMAIL.getFieldName() + "}";

		if (mergefields.contains(MergeField.SO_LOGO_EMAIL) && bodyTekst.contains(searchString))
		{
			var logo = voegSoLogoToe(mailMergeContext, uploadDocumentService);
			logo.ifPresent(mailAttachmentList::add);
		}

		return mailAttachmentList;
	}
}
