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
import {NaamGebruik} from '../enum/naam-gebruik'
import {Geslacht} from '../enum/geslacht'
import {Doelgroep} from '@shared/types/algemeen/enum/doelgroep'
import {Aanspreekvorm} from '@shared/types/algemeen/enum/aanspreekvorm'

export interface ClientContactgegevensDto {
  clientId: number,
  voornaam: string
  achternaam: string
  tussenvoegsel: string
  titel: string
  geboortedatum: Date,
  bsn: string
  naamGebruik: NaamGebruik,
  partnerTussenvoegsel: string
  partnerAchternaam: string
  geslacht: Geslacht
  overlijdensdatum: Date,
  mobielNummer: string,
  extraNummer: string,
  emailAdres: string
  doelgroepen: Doelgroep[]
  dubbeleTijdReden: string
  aanspreekvorm: Aanspreekvorm
  heeftMammaAfspraak: boolean
}
