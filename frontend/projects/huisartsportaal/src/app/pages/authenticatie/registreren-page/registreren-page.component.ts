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
import { RegistrationDto } from '../../../models/RegistrationDto'
import { Router } from '@angular/router'
import { NotificatieService } from '../../../services/notificatie/notificatie.service'

@Component({
  selector: 'app-registreren-page',
  imports: [DsButtonComponent, DsCardComponent, DsFooterActionsRightDirective, DsInputComponent, FormsModule, ReactiveFormsModule],
  templateUrl: './registreren-page.component.html',
})
export class RegistrerenPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly router = inject(Router)
  private readonly notificatieService = inject(NotificatieService)

  registrerenForm = this.formBuilder.group({
    agbCode: ['', Validators.required],
    registratieCode: ['', Validators.required],
  })
  private authService = inject(AuthService)

  registreren() {
    if (this.registrerenForm.invalid) {
      return
    }

    this.authService.registreren(this.registrerenForm.value as RegistrationDto).subscribe({
      next: () => this.router.navigate(['/registreren/voltooien']),
      error: () => this.notificatieService.error('AGB-code of registratieCode is onjuist.'),
    })
  }
}
