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

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.dto.BriefafdrukopdrachtDto;
import nl.rivm.screenit.util.BriefUtil;

import org.codehaus.commons.nullanalysis.NotNull;
import org.hibernate.Hibernate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;

public interface IBrievenGeneratorHelper<B extends Brief, MB extends MergedBrieven<?>>
{
	Logger LOG = LoggerFactory.getLogger(IBrievenGeneratorHelper.class);

	default BaseDocumentCreator getDocumentCreator(MailMergeContext context)
	{
		return null;
	}

	default void additionalActiesWithDocument(MailMergeContext context, B brief, Document chunkDocument) throws Exception
	{

	}

	default String getTechnischeLoggingMergedBriefAanmaken(MB brieven)
	{
		return "Mergedocument(id = " + brieven.getId() + ") aangemaakt voor ScreeningOrganisatie " + brieven.getScreeningOrganisatie().getNaam()
			+ ", brieftype " + brieven.getBriefType().name();
	}

	String getMergedBrievenNaam(MB mergedBrieven);

	default BriefafdrukopdrachtDto maakBriefafdrukopdrachtVoorGegenereerdeBrief(@NotNull B brief, @NotNull LocalDateTime timestamp)
	{
		var briefType = BriefUtil.getOrigineleBrief(brief).getBriefType();
		return BriefafdrukopdrachtDto.builder()
			.code(briefType != null ? briefType.getBriefCode() : BriefType.FALLBACK_BRIEF_CODE)
			.kenmerk(BriefUtil.maakParagonKenmerk(brief))
			.timestamp(timestamp.format(DateTimeFormatter.ofPattern(Constants.DATE_FORMAT_YYYYMMDDHHMMSS)))
			.codeAddendum("")
			.entityId(brief.getId())
			.entityType(Hibernate.getClass(brief))
			.build();
	}

	default Long getFileStoreId()
	{
		return null;
	}

	BatchApplicationType getBatchApplicationType();

	boolean isAutomatischAfdrukkenViaParagon();

	default void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
	}

	default void verhoogAantalBrievenVanScreeningOrganisatie(Long soKey, Integer aantalToevoegen)
	{
	}

	default void additionalMergedContext(MailMergeContext context)
	{
	}

	Bevolkingsonderzoek[] getBevolkingsonderzoeken();

	LogGebeurtenis getMergeProbleemLogGebeurtenis();

	LogGebeurtenis getOnvolledigAdresLogGebeurtenis();

	FileStoreLocation getFileStoreLocation();

	BriefType getBriefType();

	IDocument getDocumentDefinitie();

	MB getMergedBrieven();

	default MB createMergedBrieven(Date aangemaaktOp)
	{
		return null;
	}

	default void increasePdfCounter()
	{
	}

}
