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
import { DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective, DsInputComponent } from '@topicus-rgp-ds/web'
import { FormsModule, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { AuthService } from '../../../services/auth/auth.service'
import { NotificatieService } from '../../../services/notificatie/notificatie.service'
import { take } from 'rxjs'
import { WachtwoordWijzigenDto } from '../../../models/WachtwoordWijzigenDto'
import { CustomValidators } from '../../../utils/custom-validators'

@Component({
  selector: 'app-wachtwoord-vergeten-voltooien-page',
  imports: [DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective, DsInputComponent, FormsModule, ReactiveFormsModule],
  templateUrl: './wachtwoord-vergeten-voltooien-page.component.html',
})
export class WachtwoordVergetenVoltooienPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly authService = inject(AuthService)
  private readonly notificatieService = inject(NotificatieService)

  wachtwoordWijzigenForm = this.formBuilder.group({
    oudeWachtwoord: null,
    nieuweWachtwoord: ['', [Validators.required, CustomValidators.wachtwoordSterkteValidator]],
    nieuweWachtwoordControle: ['', [Validators.required, CustomValidators.wachtwoordSterkteValidator]],
  })

  wachtwoordWijzigen() {
    if (this.wachtwoordWijzigenForm.invalid) {
      return
    }
    this.authService
      .updateWachtwoord(this.wachtwoordWijzigenForm.value as WachtwoordWijzigenDto)
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('Uw wachtwoord is succesvol gewijzigd.')
        this.authService.logout()
      })
  }
}
