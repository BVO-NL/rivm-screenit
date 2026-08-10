package nl.rivm.screenit.main.util;

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

import javax.imageio.ImageIO;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;

public class TiffUtil
{

	public static PDDocument tiffToPdfDocument(File tiffFile) throws IOException
	{

		var pdDocument = new PDDocument();

		try (var is = ImageIO.createImageInputStream(tiffFile))
		{
			if (is == null || is.length() == 0)
			{
				throw new IOException("Kon Tiff bestand niet vinden of tiff bestand is leeg.");
			}

			var iterator = ImageIO.getImageReaders(is);
			if (iterator == null || !iterator.hasNext())
			{
				throw new IOException("File format wordt niet ondersteund.");
			}
			var reader = iterator.next();
			reader.setInput(is);

			var aantalImages = reader.getNumImages(true);

			for (var i = 0; i < aantalImages; i++)
			{

				var image = reader.read(i);
				final var width = image.getWidth();
				final var height = image.getHeight();
				var page = new PDPage(new PDRectangle(width, height));
				pdDocument.addPage(page);
				final var imageXObject = LosslessFactory.createFromImage(pdDocument, image);
				try (var contentStream = new PDPageContentStream(pdDocument, page))
				{
					contentStream.drawImage(imageXObject, 0, 0, width, height);
				}
			}
		}

		return pdDocument;
	}
}
