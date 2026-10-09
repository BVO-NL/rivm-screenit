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
import { Injectable } from '@angular/core'
import { DeepPartial } from '@shared/types/utility.types'
import { deepMerge } from '@shared/utils/deep-merge'
import { DEFAULT_STRINGS } from '@shared/strings/default.strings'

@Injectable({ providedIn: 'root' })
export class StringsService {
  getStrings(overrides?: DeepPartial<typeof DEFAULT_STRINGS>): typeof DEFAULT_STRINGS {
    return overrides ? deepMerge(DEFAULT_STRINGS, overrides) : DEFAULT_STRINGS
  }
}
