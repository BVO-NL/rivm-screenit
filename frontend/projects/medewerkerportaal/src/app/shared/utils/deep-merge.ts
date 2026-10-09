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
import { DeepPartial } from '@shared/types/utility.types'

export function deepMerge<T extends object>(base: T, overrides: DeepPartial<T>): T {
  return mergeDeep(base as Record<string, unknown>, overrides as Record<string, unknown>) as T
}

function mergeDeep(base: Record<string, unknown>, overrides: Record<string, unknown>): Record<string, unknown> {
  const result = { ...base }
  for (const key in overrides) {
    if (!Object.prototype.hasOwnProperty.call(overrides, key)) continue
    const override = overrides[key]
    if (override === undefined) continue
    const baseVal = base[key]
    if (override !== null && typeof override === 'object' && !Array.isArray(override) && baseVal !== null && typeof baseVal === 'object' && !Array.isArray(baseVal)) {
      result[key] = mergeDeep(baseVal as Record<string, unknown>, override as Record<string, unknown>)
    } else {
      result[key] = override
    }
  }
  return result
}
