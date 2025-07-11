/*-
 * ========================LICENSE_START=================================
 * se-proxy
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
export type OpschortenReden = "NIET_OPSCHORTEN" | "AANVULLENDE_BEELDEN_NODIG_SE" | "PRIORS_VAN_BUITEN_BVO";
export const AANVULLENDE_BEELDEN_NODIG_SE = "AANVULLENDE_BEELDEN_NODIG_SE"

export function getOpschortenRedenBeschrijving(reden: OpschortenReden): string {
	switch (reden) {
		case "NIET_OPSCHORTEN":
			return "Niet opschorten"
		case "AANVULLENDE_BEELDEN_NODIG_SE":
			return "Aanvullende beelden nodig van de SE"
		case "PRIORS_VAN_BUITEN_BVO":
			return "Priors van buiten bevolkingsonderzoek nodig"
		default:
			return ""
	}
}
