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
import { NaamGebruik } from '@shared/types/algemeen/enum/naam-gebruik'
import { Geslacht } from '@shared/types/algemeen/enum/geslacht'
import { GbaStatus } from '@shared/types/algemeen/enum/gba-status'
import { OnderzoeksresultatenActieDto } from '@shared/types/algemeen/dto/onderzoeksresultaten-actie.dto'
import { BezwaarMomentDto } from '@shared/types/algemeen/dto/bezwaar-moment.dto'
import { PersoonDto } from '@shared/types/algemeen/dto/persoon.dto'

export interface ClientDto extends PersoonDto {
  titel: string
  geboortedatum: Date
  postcode: string
  straatnaam: string
  volledigeAdres: string | null
  plaats: string
  bsn: string
  naamGebruik: NaamGebruik
  partnerTussenvoegsel: string
  partnerAchternaam: string
  geslacht: Geslacht
  isTijdelijkAdres: boolean
  tijdelijkAdresVolledig?: string | null
  postadres?: string | null
  overlijdensdatum?: Date
  screeningsorganisatie?: string
  actief: boolean
  gbaStatus: GbaStatus
  onderzoeksresultatenActies: OnderzoeksresultatenActieDto[]
  bezwaarMomenten: BezwaarMomentDto[]
}
