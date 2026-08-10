/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
export interface ClientZoekenFilterDto {
  geboortedatum?: Date | null
  bsn?: string | null
  postcode?: string | null
  huisnummer?: string | null
  briefkenmerk?: string | null
  bkUitnodigingsnummer?: number | null
  bmhkMonsterId?: string | null
  bmhkUitnodigingsId?: number | null
  dkBarcode?: string | null
  dkUitnodigingsId?: number | null
  anummer?: string | null
  mobielnummer?: string | null
  emailadres?: string | null
}
