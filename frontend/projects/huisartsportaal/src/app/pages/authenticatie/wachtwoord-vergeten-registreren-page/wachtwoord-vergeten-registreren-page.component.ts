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
import { WachtwoordAanvragenDto } from '../../../models/WachtwoordAanvragenDto'
import { take } from 'rxjs'
import { AuthenticationScope } from '../../../models/AuthenticationScope'
import { Router } from '@angular/router'

@Component({
  selector: 'app-wachtwoord-vergeten-registreren-page',
  imports: [DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective, DsInputComponent, FormsModule, ReactiveFormsModule],
  templateUrl: './wachtwoord-vergeten-registreren-page.component.html',
})
export class WachtwoordVergetenRegistrerenPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly authService = inject(AuthService)
  private readonly notificatieService = inject(NotificatieService)
  private readonly router = inject(Router)

  wachtwoordAanvragenForm = this.formBuilder.group({
    emailOfGebruikersnaam: ['', Validators.required],
    inlogCode: ['', Validators.required],
    scope: AuthenticationScope.WACHTWOORDVERGETEN,
  })

  wachtwoordAanvragen() {
    if (this.wachtwoordAanvragenForm.invalid) {
      return
    }
    this.authService
      .wachtwoordAanvragen(this.wachtwoordAanvragenForm.value as WachtwoordAanvragenDto)
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('U kunt nu een nieuw wachtwoord aanmaken.')
        this.router.navigateByUrl('/wachtwoordvergeten/voltooien')
      })
  }
}
