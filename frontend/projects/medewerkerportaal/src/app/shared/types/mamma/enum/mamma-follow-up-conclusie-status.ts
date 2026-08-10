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
export enum MammaFollowUpConclusieStatus {
  NIET_TE_VERWACHTEN = 'NIET_TE_VERWACHTEN',
  FALSE_NEGATIVE = 'FALSE_NEGATIVE',
  TRUE_NEGATIVE = 'TRUE_NEGATIVE',
  FALSE_POSITIVE = 'FALSE_POSITIVE',
  TRUE_POSITIVE = 'TRUE_POSITIVE',
}

export const mammaFollowUpConclusieStatusLabel: Record<MammaFollowUpConclusieStatus, string> = {
  [MammaFollowUpConclusieStatus.NIET_TE_VERWACHTEN]: 'Niet te verwachten',
  [MammaFollowUpConclusieStatus.FALSE_NEGATIVE]: 'False negative',
  [MammaFollowUpConclusieStatus.TRUE_NEGATIVE]: 'True negative',
  [MammaFollowUpConclusieStatus.FALSE_POSITIVE]: 'False positive',
  [MammaFollowUpConclusieStatus.TRUE_POSITIVE]: 'True positive',
}
