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
import { DsCardComponent, DsCardConfig } from '@topicus-rgp-ds/web'
import { GegevensWijzigenComponent } from '../../components/gegevens-wijzigigen/gegevens-wijzigen.component'
import { Router } from '@angular/router'
import { AuthService } from '../../services/auth/auth.service'
import { Recht } from '../../models/Recht'
import { NotificatieService } from '../../services/notificatie/notificatie.service'

@Component({
  selector: 'app-gegevens-page',
  imports: [DsCardComponent, GegevensWijzigenComponent],
  templateUrl: './gegevens-page.component.html',
})
export class GegevensPageComponent {
  private readonly router = inject(Router)
  private readonly authService = inject(AuthService)
  private readonly notificatieService = inject(NotificatieService)

  dialogConfig: DsCardConfig = {
    maxHeight: 'calc(100vh - var(--header-height))',
  }

  constructor() {
    if (this.authService.heeftRecht(Recht.ROLE_OVEREENKOMST)) {
      this.notificatieService.info('De zakelijke voorwaarden zijn gewijzigd. U dient deze te accepteren voordat u verder kan gaan')
    }
  }

  onOpgeslagen() {
    this.router.navigateByUrl('/')
  }
}
