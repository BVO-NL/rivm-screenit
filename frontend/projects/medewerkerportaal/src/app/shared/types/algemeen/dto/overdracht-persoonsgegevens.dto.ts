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
import { BriefDto } from '@shared/types/algemeen/dto/brief.dto'
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { GebeurtenisBron } from '@shared/types/algemeen/enum/gebeurtenis-bron'
import { OverdrachtType } from '@shared/types/algemeen/enum/overdracht-type'

export interface OverdrachtPersoonsgegevensVerwerkenDto {
  status: AanvraagBriefStatus | null
  type: OverdrachtType | null
  verstuurdeBrieven: BriefDto[]
  keuzes: OverdrachtPersoonsgegevensKeuzesDto
  heeftMammaDossier: boolean
  heeftCervixDossier: boolean
  heeftColonDossier: boolean
  ontvangenFormulierId: number | null
}

export interface OverdrachtPersoonsgegevensKeuzesDto {
  bkGegevens: boolean
  bkBeelden: boolean
  bmhkGegevens: boolean
  dkGegevens: boolean
}

export interface OverdrachtPersoonsgegevensDto {
  id: number
  status: AanvraagBriefStatus
  type: OverdrachtType
  statusDatum: Date
  bron: GebeurtenisBron
}
