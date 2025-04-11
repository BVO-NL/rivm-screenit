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
import { bootstrapApplication } from '@angular/platform-browser'
import localeNL from '@angular/common/locales/nl'
import { appConfig } from '@/config/app.config'
import { AppComponent } from '@/app.component'
import { loadIcons } from '@/config/icons.config'
import setupWebApis from '@/config/web-apis'
import setupDatadog from '@/config/datadog'
import { registerLocaleData } from '@angular/common'

document.addEventListener('DOMContentLoaded', function () {
  loadIcons()
  setupWebApis()
  setupDatadog()
  registerLocaleData(localeNL)

  bootstrapApplication(AppComponent, appConfig).catch((err) => console.error(err))
})
