package nl.rivm.screenit.util;

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

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import org.apache.fontbox.ttf.TTFParser;
import org.apache.pdfbox.io.RandomAccessReadBuffer;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDFont;
import org.apache.pdfbox.pdmodel.font.PDType0Font;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class WorkbookPdfUtil
{
	private static final float MARGIN = 40f;

	private static final float TITLE_FONT_SIZE = 14f;

	private static final float TITLE_LEADING = 18f;

	private static final float TEXT_FONT_SIZE = 9f;

	private static final float LEADING = 12f;

	private static final float CELL_PADDING_H = 4f;

	private static final float CELL_PADDING_V = 3f;

	private static final float RUIMTE_ONDER_TITEL = 6f;

	private static final float GRID_LINE_WIDTH = 0.5f;

	private static final float PAGE_WIDTH = PDRectangle.A4.getWidth();

	private static final float PAGE_HEIGHT = PDRectangle.A4.getHeight();

	private static final float USABLE_WIDTH = PAGE_WIDTH - 2 * MARGIN;

	private static final float CONTENT_TOP = PAGE_HEIGHT - MARGIN;

	private static final float CONTENT_BOTTOM = MARGIN;

	private static final float[] KOLOM_FRACTIES_TWEE = { 0.25f, 0.75f };

	private static final float[] KOLOM_FRACTIES_DRIE = { 0.20f, 0.55f, 0.25f };

	private static final String TITLE_FONT_RESOURCE = "/fonts/arialbd.ttf";

	private static final String TEXT_FONT_RESOURCE = "/fonts/arial.ttf";

	public static void schrijfAlsPdf(Workbook workbook, OutputStream outputStream) throws IOException
	{
		try (var document = new PDDocument())
		{
			var titleFont = laadFont(document, TITLE_FONT_RESOURCE);
			var textFont = laadFont(document, TEXT_FONT_RESOURCE);
			var formatter = new DataFormatter();
			var context = new PdfSchrijfContext(document, titleFont, textFont);
			for (var sheet : workbook)
			{
				context.nieuwePagina();
				context.schrijfTitel(sheet.getSheetName());
				schrijfTabel(context, sheet, formatter);
			}
			context.sluitHuidigePagina();
			document.save(outputStream);
		}
	}

	private static PDFont laadFont(PDDocument document, String klassenpadResource) throws IOException
	{
		try (var fontStream = WorkbookPdfUtil.class.getResourceAsStream(klassenpadResource))
		{
			var trueTypeFont = new TTFParser().parse(RandomAccessReadBuffer.createBufferFromStream(fontStream));
			trueTypeFont.setEnableGsub(false);
			return PDType0Font.load(document, trueTypeFont, true);
		}
	}

	private static void schrijfTabel(PdfSchrijfContext context, Sheet sheet, DataFormatter formatter) throws IOException
	{
		var kolomAantal = bepaalKolomAantal(sheet);
		var kolomBreedtes = berekenKolomBreedtes(kolomAantal);
		var kolomX = berekenKolomX(kolomBreedtes);
		for (var row : sheet)
		{
			var cellen = new String[kolomAantal];
			for (var kolomIndex = 0; kolomIndex < kolomAantal; kolomIndex++)
			{
				cellen[kolomIndex] = getCellTekst(row, kolomIndex, formatter);
			}
			context.schrijfTabelRij(cellen, kolomBreedtes, kolomX);
		}
	}

	private static int bepaalKolomAantal(Sheet sheet)
	{
		var kolomAantal = 0;
		for (var row : sheet)
		{
			kolomAantal = Math.max(kolomAantal, row.getLastCellNum());
		}
		return kolomAantal;
	}

	private static float[] berekenKolomBreedtes(int kolomAantal)
	{
		var fracties = bepaalKolomFracties(kolomAantal);
		var breedtes = new float[kolomAantal];
		for (var i = 0; i < kolomAantal; i++)
		{
			breedtes[i] = USABLE_WIDTH * fracties[i];
		}
		return breedtes;
	}

	private static float[] bepaalKolomFracties(int kolomAantal)
	{
		return switch (kolomAantal)
		{
			case 2 -> KOLOM_FRACTIES_TWEE;
			case 3 -> KOLOM_FRACTIES_DRIE;
			default -> gelijkeKolomFracties(kolomAantal);
		};
	}

	private static float[] gelijkeKolomFracties(int kolomAantal)
	{
		var fracties = new float[kolomAantal];
		Arrays.fill(fracties, kolomAantal == 0 ? 0f : 1f / kolomAantal);
		return fracties;
	}

	private static float[] berekenKolomX(float[] kolomBreedtes)
	{
		var kolomX = new float[kolomBreedtes.length];
		if (kolomX.length == 0)
		{
			return kolomX;
		}
		kolomX[0] = MARGIN;
		for (var i = 1; i < kolomBreedtes.length; i++)
		{
			kolomX[i] = kolomX[i - 1] + kolomBreedtes[i - 1];
		}
		return kolomX;
	}

	private static String getCellTekst(Row row, int kolom, DataFormatter formatter)
	{
		var cell = row.getCell(kolom);
		if (cell == null)
		{
			return null;
		}
		var tekst = formatter.formatCellValue(cell);
		return tekst.isEmpty() ? null : tekst;
	}

	private static List<String> regelsVoorCel(String tekst, float breedte, PDFont font) throws IOException
	{
		List<String> regels = new ArrayList<>();
		for (var deel : (tekst == null ? "" : tekst).split("\r\n|\r|\n", -1))
		{
			regels.addAll(breekAf(deel, font, TEXT_FONT_SIZE, breedte - 2 * CELL_PADDING_H));
		}
		return regels;
	}

	private static float breedte(String tekst, PDFont font, float fontSize) throws IOException
	{
		try
		{
			return font.getStringWidth(tekst) / 1000 * fontSize;
		}
		catch (IllegalArgumentException e)
		{
			return font.getStringWidth(veiligeTekst(font, tekst)) / 1000 * fontSize;
		}
	}

	private static List<String> breekAf(String tekst, PDFont font, float fontSize, float maxWidth) throws IOException
	{
		List<String> regels = new ArrayList<>();
		if (tekst.isEmpty())
		{
			regels.add(tekst);
			return regels;
		}

		var huidigeRegel = new StringBuilder();
		for (var woord : tekst.split(" "))
		{
			var kandidaat = huidigeRegel.isEmpty() ? woord : huidigeRegel + " " + woord;
			if (!huidigeRegel.isEmpty() && breedte(kandidaat, font, fontSize) > maxWidth)
			{
				regels.add(huidigeRegel.toString());
				huidigeRegel = new StringBuilder(woord);
			}
			else
			{
				huidigeRegel = new StringBuilder(kandidaat);
			}
		}
		regels.add(huidigeRegel.toString());
		return regels;
	}

	private static String veiligeTekst(PDFont font, String tekst)
	{
		var resultaat = new StringBuilder(tekst.length());
		for (var i = 0; i < tekst.length(); i++)
		{
			var karakter = tekst.charAt(i);
			resultaat.append(isEncodeerbaar(font, karakter) ? karakter : '?');
		}
		return resultaat.toString();
	}

	private static boolean isEncodeerbaar(PDFont font, char karakter)
	{
		try
		{
			font.encode(String.valueOf(karakter));
			return true;
		}
		catch (IOException | IllegalArgumentException e)
		{
			return false;
		}
	}

	private static final class PdfSchrijfContext
	{
		private final PDDocument document;

		private final PDFont titleFont;

		private final PDFont textFont;

		private PDPageContentStream contentStream;

		private float yPositie;

		private PdfSchrijfContext(PDDocument document, PDFont titleFont, PDFont textFont)
		{
			this.document = document;
			this.titleFont = titleFont;
			this.textFont = textFont;
		}

		private void nieuwePagina() throws IOException
		{
			sluitHuidigePagina();
			var page = new PDPage(PDRectangle.A4);
			document.addPage(page);
			contentStream = new PDPageContentStream(document, page);
			contentStream.setLineWidth(GRID_LINE_WIDTH);
			yPositie = CONTENT_TOP;
		}

		private void sluitHuidigePagina() throws IOException
		{
			if (contentStream != null)
			{
				contentStream.close();
				contentStream = null;
			}
		}

		private void schrijfTitel(String titel) throws IOException
		{
			ensureRuimte(TITLE_LEADING);
			schrijfTekst(MARGIN, yPositie - TITLE_FONT_SIZE, titel, titleFont, TITLE_FONT_SIZE);
			yPositie -= TITLE_LEADING;
			voegRuimteToe(RUIMTE_ONDER_TITEL);
		}

		private void voegRuimteToe(float hoogte) throws IOException
		{
			ensureRuimte(hoogte);
			yPositie -= hoogte;
		}

		private void schrijfTabelRij(String[] cellen, float[] kolomBreedtes, float[] kolomX) throws IOException
		{
			var kolomRegels = new List<?>[cellen.length];
			var maxRegels = 1;
			for (var i = 0; i < cellen.length; i++)
			{
				var regels = regelsVoorCel(cellen[i], kolomBreedtes[i], textFont);
				kolomRegels[i] = regels;
				maxRegels = Math.max(maxRegels, regels.size());
			}

			var regelIndex = 0;
			while (regelIndex < maxRegels)
			{
				ensureMinstEenRegelPast();
				var beschikbareHoogte = yPositie - CONTENT_BOTTOM;
				var regelsDiePassen = Math.max(1, (int) ((beschikbareHoogte - 2 * CELL_PADDING_V) / LEADING));
				var aantalRegelsDitSegment = Math.min(maxRegels - regelIndex, regelsDiePassen);
				var segmentHoogte = aantalRegelsDitSegment * LEADING + 2 * CELL_PADDING_V;
				var rijBovenY = yPositie;

				tekenRasterVoorRij(kolomBreedtes, kolomX, rijBovenY, rijBovenY - segmentHoogte);
				for (var i = 0; i < cellen.length; i++)
				{
					@SuppressWarnings("unchecked")
					var regels = (List<String>) kolomRegels[i];
					for (var j = 0; j < aantalRegelsDitSegment; j++)
					{
						var globaleIndex = regelIndex + j;
						if (globaleIndex < regels.size())
						{
							var baseline = rijBovenY - CELL_PADDING_V - TEXT_FONT_SIZE - j * LEADING;
							schrijfTekst(kolomX[i] + CELL_PADDING_H, baseline, regels.get(globaleIndex), textFont, TEXT_FONT_SIZE);
						}
					}
				}

				yPositie = rijBovenY - segmentHoogte;
				regelIndex += aantalRegelsDitSegment;
			}
		}

		private void tekenRasterVoorRij(float[] kolomBreedtes, float[] kolomX, float boven, float onder) throws IOException
		{
			for (var i = 0; i < kolomBreedtes.length; i++)
			{
				contentStream.addRect(kolomX[i], onder, kolomBreedtes[i], boven - onder);
			}
			contentStream.stroke();
		}

		private void schrijfTekst(float x, float y, String tekst, PDFont font, float fontSize) throws IOException
		{
			contentStream.beginText();
			contentStream.setFont(font, fontSize);
			contentStream.newLineAtOffset(x, y);
			try
			{
				contentStream.showText(tekst);
			}
			catch (IllegalArgumentException e)
			{
				contentStream.showText(veiligeTekst(font, tekst));
			}
			contentStream.endText();
		}

		private void ensureMinstEenRegelPast() throws IOException
		{
			ensureRuimte(LEADING + 2 * CELL_PADDING_V);
		}

		private void ensureRuimte(float benodigdeHoogte) throws IOException
		{
			if (yPositie - benodigdeHoogte < CONTENT_BOTTOM)
			{
				nieuwePagina();
			}
		}
	}
}
