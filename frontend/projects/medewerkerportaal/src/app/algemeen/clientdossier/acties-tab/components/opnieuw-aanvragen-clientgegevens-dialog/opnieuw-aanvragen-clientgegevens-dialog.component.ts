/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { take } from 'rxjs'
import { DsButtonComponent, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { RedenGbaVraag, redenGbaVraagLabels } from '@shared/types/algemeen/enum/reden-gba-vraag'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-opnieuw-aanvragen-clientgegevens-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsDropdownComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './opnieuw-aanvragen-clientgegevens-dialog.component.html',
})
export class OpnieuwAanvragenClientgegevensDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientId = inject<number>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly redenOpties = [RedenGbaVraag.ONJUIST_ADRES, RedenGbaVraag.ONJUISTE_PERSOONSGEGEVENS].map((waarde) => ({ waarde, naam: redenGbaVraagLabels[waarde] }))

  protected readonly aanvraagForm = this.formBuilder.group({
    reden: this.formBuilder.control<RedenGbaVraag | null>(null, Validators.required),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected aanvragen() {
    const reden = this.aanvraagForm.get('reden')?.value
    if (!reden) {
      return
    }
    this.contactActiesService
      .vraagClientgegevensOpnieuwAan(this.clientId, { reden, notitie: this.aanvraagForm.get('notitie')?.value ?? '' })
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.clientDossier.acties.opnieuwAanvragenClientgegevens.notifications.succes)
        this.dialogRef.close(true)
      })
  }
}
