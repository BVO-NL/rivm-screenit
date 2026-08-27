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
import { AanvraagBriefStatus } from '../enum/aanvraag-brief-status'
import { BriefDto } from './brief.dto'
import { BezwaarDto } from './bezwaar.dto'
import { BriefActie } from '../enum/brief-actie'

export interface BezwaarMomentDto {
  id: number
  status: AanvraagBriefStatus
  bezwaarDatum: Date
  bezwaarBriefId: number
  brieven: BriefDto[]
  bezwaren: BezwaarDto[]
  briefActies: BriefActie[]
}
