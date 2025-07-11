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
export const ucf = (s: string): string => s.length < 1 ? s : s[0].toUpperCase() + s.slice(1)
export const annotatieTitle = (s: string): string => {
	return ucf(s.toLowerCase().replace("signalering_", "").replace("legacy_", "").split("_").join(" "))
}

export const zelfdeTekst = (tekst: string | undefined, vergelijkTekst: string | undefined): boolean => {
	return tekst === vergelijkTekst || (!tekst && !vergelijkTekst)
}
