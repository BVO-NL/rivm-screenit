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
import { DsButtonComponent, DsCheckboxComponent, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { BriefType, briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { StringsService } from '@shared/strings'

const KEUZE_GEBRUIK_GEGEVENS_BRIEVEN = [
  BriefType.CLIENT_BEZWAAR_AANVRAAG,
  BriefType.CLIENT_BEZWAAR_INTREKKEN,
  BriefType.CLIENT_BEZWAAR_VERWIJDEREN_ONDERZOEKRESULTATEN,
  BriefType.CLIENT_BEZWAAR_ALLES_VERWIJDEREN,
]

@Component({
  selector: 'app-keuze-gebruik-gegevens-brief-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsCheckboxComponent, DsDropdownComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './keuze-gebruik-gegevens-brief-dialog.component.html',
})
export class KeuzeGebruikGegevensBriefDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientId = inject<number>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly briefOpties = KEUZE_GEBRUIK_GEGEVENS_BRIEVEN.map((briefType) => ({ briefType, omschrijving: briefTypeLabels[briefType] }))

  protected readonly briefForm = this.formBuilder.group({
    briefType: this.formBuilder.control<BriefType | null>(null, Validators.required),
    handtekeningVergeten: this.formBuilder.nonNullable.control(false),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected aanvragen() {
    const { briefType, handtekeningVergeten = false, notitie = '' } = this.briefForm.value
    if (!briefType) {
      return
    }
    this.contactActiesService
      .verstuurKeuzeGebruikGegevensBrief(this.clientId, { briefType, handtekeningVergeten, notitie })
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.clientDossier.acties.keuzeGebruikGegevensBrief.notifications.succes)
        this.dialogRef.close(true)
      })
  }
}
