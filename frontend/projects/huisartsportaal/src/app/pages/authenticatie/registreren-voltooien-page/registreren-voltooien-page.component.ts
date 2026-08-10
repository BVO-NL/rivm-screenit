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
import { Component, inject } from '@angular/core'
import { DsCardComponent, DsModalConfig } from '@topicus-rgp-ds/web'
import { GegevensWijzigenComponent } from '../../../components/gegevens-wijzigigen/gegevens-wijzigen.component'
import { Router } from '@angular/router'

@Component({
  selector: 'app-registreren-voltooien-page',
  imports: [DsCardComponent, GegevensWijzigenComponent],
  templateUrl: './registreren-voltooien-page.component.html',
})
export class RegistrerenVoltooienPageComponent {
  config: DsModalConfig
  router = inject(Router)

  constructor() {
    this.config = new DsModalConfig()
    this.config.maxHeight = '100%'
  }

  onUpdate() {
    this.router.navigateByUrl('/')
  }
}
