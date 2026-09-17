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
import { Doelgroep } from '@shared/types/algemeen/enum/doelgroep'
import { PersoonDto } from '@shared/types/algemeen/dto/persoon.dto'

export interface ClientPaspoortDto extends PersoonDto {
  titel: string
  partnerTussenvoegsel: string
  partnerAchternaam: string
  naamGebruik: NaamGebruik
  geslacht: Geslacht
  bsn: string
  anummer: string | null
  geboortedatum: Date
  mobielNummer: string
  extraNummer: string
  emailAdres: string
  brpAdres: string | null
  brpPostcode: string | null
  brpWoonplaats: string | null
  adres: string | null
  postcode: string | null
  woonplaats: string | null
  isTijdelijkAdres: boolean
  isTijdelijkBrpAdres: boolean
  overlijdensdatum?: Date
  doelgroepen: Doelgroep[]
}
