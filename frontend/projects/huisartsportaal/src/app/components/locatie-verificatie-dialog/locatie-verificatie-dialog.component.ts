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
import { BaseDialogComponent } from '../../../../../medewerkerportaal/src/app/shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { LocatieVerificatieDto } from '../../models/LocatieVerificatieDto'
import { VerificatieService } from '../../services/verificatie/verificatie.service'
import { NotificatieService } from '../../services/notificatie/notificatie.service'

@Component({
  selector: 'app-locatie-verificatie-dialog',
  imports: [BaseDialogComponent, DsInputComponent, ReactiveFormsModule, DsButtonComponent],
  templateUrl: './locatie-verificatie-dialog.component.html',
})
export class LocatieVerificatieDialogComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialogData: LocatieVerificatieDto = inject(DIALOG_DATA)
  private readonly verificatieService = inject(VerificatieService)
  private readonly dialogRef = inject(DialogRef)
  private readonly notificatieService = inject(NotificatieService)

  verificatieForm = this.formBuilder.group({
    huisartsportaalId: this.dialogData.huisartsportaalId,
    locatieNaam: [{ value: this.dialogData.locatieNaam, disabled: true }],
    zorgmailKlantnummer: [{ value: this.dialogData.zorgmailKlantnummer, disabled: true }],
    verificatieCode: this.formBuilder.control<number | null>(null, [Validators.required, Validators.minLength(4), Validators.maxLength(4), Validators.pattern('[0-9]*')]),
  })

  sluiten() {
    this.dialogRef.close()
  }

  verifieerLocatie() {
    this.verificatieService.verifieerLocatie(this.verificatieForm.getRawValue() as LocatieVerificatieDto).subscribe({
      next: (response) => {
        if (response.succes) {
          this.notificatieService.success(
            'Bedankt voor het verifiëren van uw Zorgmail klantnummer. U kunt nu voor de betreffende locatie labformulieren aanvragen en huisartsberichten ontvangen.',
          )
          this.dialogRef.close(true)
        }
      },
      error: () => {
        this.notificatieService.error(
          "Verificatie mislukt! Controleer of u de juiste verificatiecode bij de juiste locatie heeft ingevuld. U kunt de verificatiecode opnieuw aanvragen door op de knop 'Herzend verificatiemail' te klikken.",
        )
      },
    })
  }

  herzendMail() {
    this.verificatieService.herzendVerificatieCode(this.verificatieForm.getRawValue() as LocatieVerificatieDto).subscribe({
      next: () => this.notificatieService.success('Zorgmail klantnummer verificatiemail is opnieuw verzonden.'),
      error: () =>
        this.notificatieService.error('Er is een fout opgetreden bij het verzenden van de verificatiemail. Probeer het later nog eens of neem contact op met de servicedesk.'),
    })
  }
}
