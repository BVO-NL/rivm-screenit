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
import { BezwaarMomentDto } from '@/shared/types/algemeen/dto/bezwaar-moment.dto'
import { AanvraagBriefStatus } from '@/shared/types/algemeen/enum/aanvraag-brief-status'
import { BezwaarType } from '@/shared/types/algemeen/enum/bezwaar-type'
import { Bevolkingsonderzoek } from '@/shared/types/autorisatie/bevolkingsonderzoek'

export const maakBezwaarMoment = (overrides: Partial<BezwaarMomentDto> = {}): BezwaarMomentDto => ({
  id: 1,
  status: AanvraagBriefStatus.VERWERKT,
  bezwaarDatum: new Date('2026-01-12T10:30:00.000Z'),
  bezwaarBriefId: 1,
  brieven: [],
  briefActies: [],
  bezwaren: [
    {
      type: BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK,
      bevolkingsonderzoek: undefined as unknown as Bevolkingsonderzoek,
      actief: true,
    },
    {
      type: BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES,
      bevolkingsonderzoek: Bevolkingsonderzoek.CERVIX,
      actief: true,
    },
  ],
  ...overrides,
})
