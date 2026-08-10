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
import { NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { Router, RouterLink } from '@angular/router'
import { AuthService } from '../../../services/auth/auth.service'
import { AuthenticationScope } from '../../../models/AuthenticationScope'
import { NotificatieService } from '../../../services/notificatie/notificatie.service'
import { Recht } from '../../../models/Recht'

@Component({
  selector: 'app-login-page',
  imports: [DsCardComponent, DsInputComponent, ReactiveFormsModule, DsFooterActionsRightDirective, DsButtonComponent, RouterLink],
  templateUrl: './login-page.component.html',
})
export class LoginPageComponent {
  private readonly formBuilder = inject(NonNullableFormBuilder)
  private readonly router = inject(Router)
  private readonly notificatieService = inject(NotificatieService)
  loginForm = this.formBuilder.group({
    gebruikersnaam: ['', Validators.required],
    wachtwoord: ['', Validators.required],
    scope: AuthenticationScope.LOGIN,
  })
  private authService = inject(AuthService)

  inloggen() {
    if (this.loginForm.invalid) {
      return
    }

    this.authService
      .login({
        gebruikersnaam: this.loginForm.value.gebruikersnaam!,
        wachtwoord: this.loginForm.value.wachtwoord!,
        scope: this.loginForm.value.scope!,
      })
      .subscribe({
        next: () => {
          let url = '/'
          if (this.authService.heeftRecht(Recht.ROLE_OVEREENKOMST)) {
            url = 'overeenkomst'
          }
          this.router.navigateByUrl(url)
        },
        error: () => this.notificatieService.error('Gebruikersnaam of wachtwoord is onjuist.'),
      })
  }
}
