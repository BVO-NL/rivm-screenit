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
import { ClientContactDto } from '@shared/types/algemeen/dto/client-contact.dto'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { maakMedewerker } from '@/test/medewerker-test-util'

export const maakClientContact = (overrides: Partial<ClientContactDto> = {}): ClientContactDto => ({
  id: 1,
  clientId: 123,
  datumTijd: new Date('2024-01-01T08:00:00.000Z'),
  notitie: 'Notitie',
  medewerker: maakMedewerker(),
  acties: [{ type: ClientContactActieType.GEEN }],
  ...overrides,
})
