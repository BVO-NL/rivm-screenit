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
import { ApplicationConfig, ErrorHandler, importProvidersFrom, inject, LOCALE_ID, provideAppInitializer, provideBrowserGlobalErrorListeners } from '@angular/core'
import { provideHttpClient, withInterceptors } from '@angular/common/http'
import { provideAnimations } from '@angular/platform-browser/animations'
import { DEFAULT_DIALOG_CONFIG, DialogModule } from '@angular/cdk/dialog'
import { NoopScrollStrategy } from '@angular/cdk/overlay'
import { WINDOW } from '@shared/tokens/window.token'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { GlobalErrorHandler } from '@shared/services/global-error-handler/global-error-handler'
import { httpInterceptor } from '@shared/interceptors/http.interceptor'
import { ParameterService } from '@/algemeen/services/parameter/parameter.service'
import { forkJoin } from 'rxjs'
import { provideRouter } from '@angular/router'
import { HashLocationStrategy, LocationStrategy } from '@angular/common'
import { provideDsDateFormats, provideDsDateTimeFormats, provideDsLocalStorageNotificationAdapter, provideDsTimeAdapter } from '@topicus-rgp-ds/web'
import { provideDateFnsAdapter } from '@angular/material-date-fns-adapter'
import { nl } from 'date-fns/locale'
import { MAT_DATE_LOCALE } from '@angular/material/core'
import { DatepickerLocaleNl } from '@/config/mat-translations'
import { MatDatepickerIntl } from '@angular/material/datepicker'
import { routes } from './routes'

export const appConfig: ApplicationConfig = {
  providers: [
    provideHttpClient(withInterceptors([httpInterceptor])),
    provideAnimations(),
    provideRouter(routes),
    importProvidersFrom(DialogModule),
    provideAppInitializer(() => {
      const autorisatieService = inject(AutorisatieService)
      const parameterService = inject(ParameterService)
      return forkJoin([autorisatieService.getMedewerker(), parameterService.getParameters()])
    }),
    provideDsDateTimeFormats(),
    provideDsDateFormats(),
    provideDsTimeAdapter(),
    provideDateFnsAdapter(),
    provideDsLocalStorageNotificationAdapter(),
    provideBrowserGlobalErrorListeners(),
    { provide: LOCALE_ID, useValue: 'nl' },
    { provide: MAT_DATE_LOCALE, useValue: nl },
    { provide: MatDatepickerIntl, useClass: DatepickerLocaleNl },
    {
      provide: WINDOW,
      useFactory: windowFactory,
    },
    {
      provide: ErrorHandler,
      useClass: GlobalErrorHandler,
    },
    {
      provide: DEFAULT_DIALOG_CONFIG,
      useValue: {
        scrollStrategy: new NoopScrollStrategy(),
      },
    },
    {
      provide: LocationStrategy,
      useClass: HashLocationStrategy,
    },
  ],
}

function windowFactory() {
  return window
}
