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
import { Component, computed, inject } from '@angular/core'
import { LocatieVerificatieDto } from '../../models/LocatieVerificatieDto'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { Dialog } from '@angular/cdk/dialog'
import { LocatieVerificatieDialogComponent } from '../locatie-verificatie-dialog/locatie-verificatie-dialog.component'
import { StateService } from '../../services/state/state.service'
import { AuthenticationScope } from '../../models/AuthenticationScope'
import { filter, take } from 'rxjs'
import { VerificatieService } from '../../services/verificatie/verificatie.service'

@Component({
  selector: 'app-locatie-verificatie-melding',
  imports: [DsButtonComponent],
  templateUrl: './locatie-verificatie-melding.component.html',
  styleUrl: './locatie-verificatie-melding.component.scss',
  host: {
    '[class.display-none]': `locatieVerificaties().length === 0 || !isLoginScope()`,
  },
})
export class LocatieVerificatieMeldingComponent {
  private readonly stateService = inject(StateService)
  private readonly verificatieService = inject(VerificatieService)
  private readonly dialog = inject(Dialog)

  locatieVerificaties = this.stateService.locatieVerificaties
  isLoginScope = computed(() => this.stateService.auth()?.scope === AuthenticationScope.LOGIN)

  openVerificatieDialog(locatie: LocatieVerificatieDto) {
    this.dialog
      .open(LocatieVerificatieDialogComponent, { data: locatie })
      .closed.pipe(
        take(1),
        filter((refresh: unknown) => refresh === true),
      )
      .subscribe(() => this.verificatieService.getLocatieVerificaties().subscribe())
  }
}
