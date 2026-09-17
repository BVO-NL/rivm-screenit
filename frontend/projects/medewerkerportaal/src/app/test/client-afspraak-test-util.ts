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
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { maakAdres } from '@/test/adres-test-util'
import { ColonIntakeafspraakDto } from '@shared/types/colon/dto/colon-intakeafspraak.dto'

export const maakClientAfspraak = (overrides: Partial<ClientAfspraakDto> = {}): ClientAfspraakDto => ({
  id: 7,
  vanaf: '2026-09-01T09:00:00',
  locatie: {
    id: 2,
    naam: 'ColonIntakelocatie Utrecht',
    adres: maakAdres({ straat: 'Utrechtseweg', huisnummer: 1, postcode: '3511 AA', plaats: 'Utrecht' }),
  },
  locatieBeschrijving: null,
  ...overrides,
})

export const maakColonAfspraak = (overrides: Partial<ColonIntakeafspraakDto> = {}): ColonIntakeafspraakDto => ({
  id: 7,
  vanaf: '2026-09-01T09:00:00',
  locatie: {
    id: 2,
    naam: 'ColonIntakelocatie Utrecht',
    adres: maakAdres({ straat: 'Utrechtseweg', huisnummer: 1, postcode: '3511 AA', plaats: 'Utrecht' }),
  },
  locatieBeschrijving: null,
  digitaal: false,
  ...overrides,
})
