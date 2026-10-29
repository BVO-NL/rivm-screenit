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
import { DsButtonComponent, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { bepaalJuisteAanspreekvorm, clientAanspreekvormen } from '@shared/types/algemeen/enum/aanspreekvorm'
import { NaamUtils } from '@shared/utils/naam-utils'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-aanhef-wijzigen-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsDropdownComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './aanhef-wijzigen-dialog.component.html',
})
export class AanhefWijzigenDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly contactgegevens = inject<ClientContactgegevensDto>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly aanspreekvormOpties = clientAanspreekvormen.map((aanhef) => ({
    aanhef,
    omschrijving: NaamUtils.getGewensteAanspreekvorm(this.contactgegevens, aanhef),
  }))

  protected readonly aanhefForm = this.formBuilder.group({
    aanhef: this.formBuilder.control(bepaalJuisteAanspreekvorm(this.contactgegevens.aanspreekvorm, this.contactgegevens.geslacht), Validators.required),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected wijzigen() {
    const aanhef = this.aanhefForm.controls.aanhef.value
    if (!aanhef) {
      return
    }
    this.contactActiesService.wijzigAanhef(this.contactgegevens.clientId, { aanhef, notitie: this.aanhefForm.controls.notitie.value }).subscribe(() => {
      this.notificationService.success(this.strings.algemeen.clientDossier.acties.aanhefWijzigen.notifications.succes)
      this.dialogRef.close(true)
    })
  }
}
