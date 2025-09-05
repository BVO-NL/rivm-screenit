package nl.rivm.screenit.model.enums;

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

import java.util.Optional;

import lombok.Getter;

@Getter
public enum RedenNietTeBeoordelen
{
	GEEN_MONSTER(Optional.empty()),

	BARCODE_ONLEESBAAR(Optional.empty()),

	BUIS_KAPOT(Optional.of("2")),

	GEEN_VLOEISTOF(Optional.of("3")),

	TE_WEINIG_ONTLASTING(Optional.empty()),

	TE_VEEL_ONTLASTING(Optional.empty()),

	TECHNISCH_ONMOGELIJK(Optional.of("1")),

	MANUELE_FOUT(Optional.empty()),

	AFWIJKENDE_MONSTERHOEVEELHEID(Optional.of("4"));

	private final Optional<String> code;

	RedenNietTeBeoordelen(Optional<String> code)
	{
		this.code = code;
	}

	public static RedenNietTeBeoordelen bepaalReden(String onbeoordeelbaarReden)
	{
		if (onbeoordeelbaarReden == null)
		{
			return RedenNietTeBeoordelen.TECHNISCH_ONMOGELIJK;
		}

		return switch (onbeoordeelbaarReden)
		{
			case "2" -> RedenNietTeBeoordelen.BUIS_KAPOT;
			case "3" -> RedenNietTeBeoordelen.GEEN_VLOEISTOF;
			case "4" -> RedenNietTeBeoordelen.AFWIJKENDE_MONSTERHOEVEELHEID;
			default -> RedenNietTeBeoordelen.TECHNISCH_ONMOGELIJK;
		};
	}

}
