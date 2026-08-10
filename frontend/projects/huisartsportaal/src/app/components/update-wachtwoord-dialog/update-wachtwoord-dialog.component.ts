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
import { DialogRef } from '@angular/cdk/dialog'
import { AuthService } from '../../services/auth/auth.service'
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { WachtwoordWijzigenDto } from '../../models/WachtwoordWijzigenDto'
import { take } from 'rxjs'
import { DsButtonComponent, DsFooterActionsRightDirective, DsModalComponent, DsModalConfig } from '@topicus-rgp-ds/web'
import { WachtwoordFormInputComponent } from '../wachtwoord-form-input/wachtwoord-form-input.component'
import { CustomValidators } from '../../utils/custom-validators'
import { NotificatieService } from '../../services/notificatie/notificatie.service'

@Component({
  selector: 'app-update-wachtwoord-dialog',
  imports: [ReactiveFormsModule, DsModalComponent, DsButtonComponent, DsFooterActionsRightDirective, WachtwoordFormInputComponent],
  templateUrl: './update-wachtwoord-dialog.component.html',
  styleUrl: './update-wachtwoord-dialog.component.scss',
})
export class UpdateWachtwoordDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly authService = inject(AuthService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly notificationService = inject(NotificatieService)

  updateWachtwoordForm: FormGroup = this.formBuilder.nonNullable.group({
    oudeWachtwoord: ['', Validators.required],
    nieuweWachtwoord: ['', [Validators.required, Validators.minLength(12)]],
    nieuweWachtwoordControle: ['', Validators.required],
  })

  modalConfig: DsModalConfig = {
    closeCallback: () => {
      this.annuleren()
      return true
    },
    contentPadding: '1rem',
    preventBackdropClose: true,
    minWidth: '600px',
    maxWidth: '800px',
    minHeight: '320px',
  }

  wachtwoordControlValidators = [Validators.required, CustomValidators.wachtwoordSterkteValidator]
  nieuweWachtwoordControlValidators = [Validators.required, CustomValidators.maakControleValidator(this.updateWachtwoordForm.get('nieuweWachtwoord')!)]

  opslaan() {
    if (this.updateWachtwoordForm.invalid) {
      return
    }
    this.authService
      .updateWachtwoord(this.updateWachtwoordForm.value as WachtwoordWijzigenDto)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success('Wachtwoord succesvol gewijzigd')
          this.dialogRef.close(true)
        },
        error: (err) => this.notificationService.error(err.error.message),
      })
  }

  annuleren() {
    this.dialogRef.close(false)
  }

  protected readonly Validators = Validators
}
