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
import { ColonIntakelocatie } from '@shared/types/colon/colon-intakelocatie'
import { AdresDto } from '@shared/types/algemeen/dto/adres.dto'
import { maakAdres } from '@/test/adres-test-util'

export const maakColonIntakelocatie = (overrides: Partial<Omit<ColonIntakelocatie, 'adres'>> & { adres?: Partial<AdresDto> } = {}): ColonIntakelocatie => ({
  id: 1,
  naam: 'ColonIntakelocatie 1',
  aantalGeprognosticeerdeAfspraakslots: 200,
  huidigAantalAfspraakslots: 1,
  ...overrides,
  adres: maakAdres(overrides.adres),
})
