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
export enum Aanspreekvorm {
  DHR = 'DHR',
  MEVR = 'MEVR',
  GEACHTE_MEVROUW = 'GEACHTE_MEVROUW',
  GEACHTE_HEER = 'GEACHTE_HEER',
  GEACHTE = 'GEACHTE',
}

export const aanspreekvormLabel: Record<Aanspreekvorm, string> = {
  [Aanspreekvorm.DHR]: 'Dhr.',
  [Aanspreekvorm.MEVR]: 'Mevr.',
  [Aanspreekvorm.GEACHTE_MEVROUW]: 'Geachte mevrouw',
  [Aanspreekvorm.GEACHTE_HEER]: 'Geachte heer',
  [Aanspreekvorm.GEACHTE]: 'Geachte',
}

export const clientAanspreekvormen = [Aanspreekvorm.GEACHTE, Aanspreekvorm.GEACHTE_HEER, Aanspreekvorm.GEACHTE_MEVROUW]
export const medewerkerAanspreekvormen = [Aanspreekvorm.DHR, Aanspreekvorm.MEVR]
