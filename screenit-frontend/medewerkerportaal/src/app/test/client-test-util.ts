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
import { NaamGebruik } from '@shared/types/algemeen/enum/naam-gebruik'
import { Geslacht } from '@shared/types/algemeen/enum/geslacht'

export const maakClient = (overrides: Partial<ClientDto> = {}): ClientDto => ({
  id: 1,
  voornaam: 'Jan',
  achternaam: 'Jansen',
  tussenvoegsel: '',
  titel: '',
  geboortedatum: new Date('1990-01-01'),
  postcode: '1234AB',
  straat: 'Hoofdstraat 1',
  volledigeAdres: 'Hoofdstraat 1, 1234AB Amsterdam',
  plaats: 'Amsterdam',
  bsn: '123456789',
  naamGebruik: NaamGebruik.EIGEN,
  partnerTussenvoegsel: '',
  partnerAchternaam: '',
  geslacht: Geslacht.MAN,
  tijdelijkAdres: false,
  screeningsorganisatie: 'Screeningsorganisatie',
  actief: true,
  ...overrides,
})
