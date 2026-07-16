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
import { ClientDto } from '@shared/types/algemeen/dto/client.dto'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { Aanspreekvorm } from '@shared/types/algemeen/enum/aanspreekvorm'
import { NaamGebruik } from '@shared/types/algemeen/enum/naam-gebruik'
import { Geslacht } from '@shared/types/algemeen/enum/geslacht'
import { GbaStatus } from '@shared/types/algemeen/enum/gba-status'
import { ClientBrpGegevensDto } from '@shared/types/algemeen/dto/clientbrpgegevens.dto'

export const maakClient = (overrides: Partial<ClientDto> = {}): ClientDto => ({
  id: 1,
  voornaam: 'Jan',
  achternaam: 'Jansen',
  tussenvoegsel: '',
  titel: '',
  geboortedatum: new Date('1990-01-01'),
  postcode: '1234AB',
  straatnaam: 'Hoofdstraat 1',
  volledigeAdres: 'Hoofdstraat 1, 1234AB Amsterdam',
  plaats: 'Amsterdam',
  bsn: '123456789',
  naamGebruik: NaamGebruik.EIGEN,
  partnerTussenvoegsel: '',
  partnerAchternaam: '',
  geslacht: Geslacht.MAN,
  isTijdelijkAdres: false,
  screeningsorganisatie: 'Screeningsorganisatie',
  actief: true,
  gbaStatus: GbaStatus.INDICATIE_AANWEZIG,
  ...overrides,
})

export const maakClientContactgegevens = (overrides: Partial<ClientContactgegevensDto> = {}): ClientContactgegevensDto => {
  const client = maakClient()

  return {
    clientId: client.id,
    voornaam: client.voornaam,
    achternaam: client.achternaam,
    tussenvoegsel: client.tussenvoegsel,
    titel: client.titel,
    geboortedatum: client.geboortedatum,
    bsn: client.bsn,
    naamGebruik: client.naamGebruik,
    partnerTussenvoegsel: client.partnerTussenvoegsel,
    partnerAchternaam: client.partnerAchternaam,
    geslacht: client.geslacht,
    overlijdensdatum: client.overlijdensdatum as unknown as Date,
    mobielNummer: '0612345678',
    extraNummer: '0201234567',
    emailAdres: 'jan.jansen@example.org',
    doelgroepen: [],
    dubbeleTijdReden: '',
    aanspreekvorm: Aanspreekvorm.GEACHTE_HEER,
    heeftMammaAfspraak: false,
    ...overrides,
  }
}

export const maakBrpGegevens = (overrides: Partial<ClientBrpGegevensDto> = {}): ClientBrpGegevensDto => ({
  id: 1,
  indicatieStatus: 'ACTIEF',
  datumLaatsteBrpMutatie: '2025-01-01',
  laatstAangevraagdOp: '2025-01-01T10:00:00',
  tijdelijkBrpAdres: true,
  ...overrides,
})
