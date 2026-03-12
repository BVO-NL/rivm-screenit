package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Objects;

import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewer;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;

import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLabformulierPanel extends Panel
{
	@SpringBean
	private CervixLabformulierService labformulierService;

	private final static String BLANK_PAGE_PDF_PAD = "/static/assets/pdf/blank.pdf";

	public CervixLabformulierPanel(String id, String objid)
	{
		super(id);
		if (labformulierService.betreftEenS3Labformulier(objid))
		{
			add(new S3LabformulierViewerContainer("labformulier", objid));
		}
		else if (betreftEenTestTimelineLabformlier(objid))
		{
			add(new PdfViewer("labformulier", maakTijdelijkeBlankPdf()));
		}
		else
		{
			add(new EmptyPanel("labformulier"));
		}
	}

	private File maakTijdelijkeBlankPdf()
	{
		try (var blankPdfInputStream = CervixLabformulierPanel.class.getResourceAsStream(BLANK_PAGE_PDF_PAD))
		{
			Objects.requireNonNull(blankPdfInputStream, "Resource not found: " + BLANK_PAGE_PDF_PAD);
			var tempPdfFile = Files.createTempFile("screenit-blank-labformulier", ".pdf");
			Files.copy(blankPdfInputStream, tempPdfFile, StandardCopyOption.REPLACE_EXISTING);
			tempPdfFile.toFile().deleteOnExit();
			return tempPdfFile.toFile();
		}
		catch (IOException e)
		{
			throw new UncheckedIOException("Kon PDF template niet laden: " + BLANK_PAGE_PDF_PAD, e);
		}
	}

	private static boolean betreftEenTestTimelineLabformlier(String objid)
	{
		return objid.matches("\\d+");
	}
}
