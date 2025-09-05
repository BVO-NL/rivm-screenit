package nl.rivm.screenit.batch.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.regex.Pattern;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import software.amazon.awssdk.services.textract.model.Block;
import software.amazon.awssdk.services.textract.model.BlockType;

import static org.apache.commons.lang.StringUtils.replaceChars;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class TextractVerwerkenUtil
{
	private static final Pattern LOCATIE_ID_PATTERN = Pattern.compile("L[o0O]cat[iIl1]e-[iIl1]D:.*");

	public static boolean isValideHuisartsLocatieBlock(Block block)
	{
		var text = block.text();
		if (block.blockType() != BlockType.LINE || text == null)
		{
			return false;
		}
		return LOCATIE_ID_PATTERN.matcher(text).matches();
	}

	public static String vervangLettersMetCijfers(String tekst)
	{
		return replaceChars(tekst, "l1iIoO", "111100");
	}
}
