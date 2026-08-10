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
import { ApplicationConfig, ErrorHandler, LOCALE_ID, provideBrowserGlobalErrorListeners } from '@angular/core'
import { provideRouter } from '@angular/router'

import { routes } from './app.routes'
import { provideAnimations } from '@angular/platform-browser/animations'
import { provideHttpClient, withFetch, withInterceptors } from '@angular/common/http'
import { authInterceptor } from './interceptors/auth/auth.interceptor'
import { provideDsDateTimeFormats } from '@topicus-rgp-ds/web'
import { provideDateFnsAdapter } from '@angular/material-date-fns-adapter'
import { GlobalErrorHandler } from './services/global-error-handler/global-error-handler'
import { nl } from 'date-fns/locale'
import { MAT_DATE_LOCALE } from '@angular/material/core'
import { registerLocaleData } from '@angular/common'
import localeNL from '@angular/common/locales/nl'
import { MatDatepickerIntl } from '@angular/material/datepicker'
import { DatepickerLocaleNl } from '../../../medewerkerportaal/src/app/config/mat-translations'

registerLocaleData(localeNL)

export const appConfig: ApplicationConfig = {
  providers: [
    provideRouter(routes),
    provideAnimations(),
    provideHttpClient(withFetch(), withInterceptors([authInterceptor])),
    provideDateFnsAdapter(),
    provideDsDateTimeFormats(),
    provideBrowserGlobalErrorListeners(),
    { provide: LOCALE_ID, useValue: 'nl' },
    { provide: MAT_DATE_LOCALE, useValue: nl },
    { provide: ErrorHandler, useClass: GlobalErrorHandler },
    { provide: MatDatepickerIntl, useClass: DatepickerLocaleNl },
  ],
}
