/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { DebugElement, LOCALE_ID, Predicate, Provider } from '@angular/core'
import { By } from '@angular/platform-browser'
import { provideNoopAnimations } from '@angular/platform-browser/animations'
import { provideDsDateFormats, provideDsDateTimeFormats, provideDsTimeAdapter } from '@topicus-rgp-ds/web'
import { provideDateFnsAdapter } from '@angular/material-date-fns-adapter'
import { MAT_DATE_LOCALE } from '@angular/material/core'
import { nl } from 'date-fns/locale'
import localeNL from '@angular/common/locales/nl'
import { registerLocaleData } from '@angular/common'

export function byTestId(testId: string): Predicate<DebugElement> {
  return By.css(`[data-testid="${testId}"]`)
}

export function provideTestProviders(overrideProviders: Provider[] = []): Provider[] {
  registerLocaleData(localeNL)
  return [
    ...overrideProviders,
    provideNoopAnimations(),
    provideDsDateTimeFormats(),
    provideDsDateFormats(),
    provideDsTimeAdapter(),
    provideDateFnsAdapter(),
    { provide: MAT_DATE_LOCALE, useValue: nl },
    { provide: LOCALE_ID, useValue: 'nl' },
  ]
}
