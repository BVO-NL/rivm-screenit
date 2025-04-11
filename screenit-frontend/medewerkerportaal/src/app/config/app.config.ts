/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { ApplicationConfig, ErrorHandler, importProvidersFrom, inject, LOCALE_ID, provideAppInitializer } from '@angular/core'
import { provideHttpClient, withInterceptors } from '@angular/common/http'
import { provideAnimations } from '@angular/platform-browser/animations'
import { ClarityModule, ClrCommonStringsService } from '@clr/angular'
import { DEFAULT_DIALOG_CONFIG, DialogModule } from '@angular/cdk/dialog'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { WINDOW } from '@shared/tokens/window.token'
import { GlobalErrorHandler } from '@shared/services/global-error-handler/global-error-handler'
import { clarityTranslations } from './clarity-translations'
import { httpInterceptor } from '@shared/interceptors/http.interceptor'
import { NoopScrollStrategy } from '@angular/cdk/overlay'

export const appConfig: ApplicationConfig = {
  providers: [
    provideHttpClient(withInterceptors([httpInterceptor])),
    provideAnimations(),
    { provide: LOCALE_ID, useValue: 'nl' },
    importProvidersFrom(ClarityModule, DialogModule),
    provideAppInitializer(() => {
      const autorisatieService = inject(AutorisatieService)
      const commonStrings = inject(ClrCommonStringsService)

      commonStrings.localize(clarityTranslations)
      return autorisatieService.getGebruiker()
    }),
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
  ],
}

function windowFactory() {
  return window
}
