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
import { FormControl, ReactiveFormsModule } from '@angular/forms'
import { take } from 'rxjs'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-heraanmelden-baarmoederhalskanker-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './heraanmelden-baarmoederhalskanker-dialog.component.html',
})
export class HeraanmeldenBaarmoederhalskankerDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly clientId = inject<number>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly notitie = new FormControl<string>('', { nonNullable: true })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected afronden() {
    this.contactActiesService
      .heraanmeldenBaarmoederhalskanker(this.clientId, { notitie: this.notitie.value })
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.clientDossier.acties.heraanmeldenBmhk.notifications.succes)
        this.dialogRef.close(true)
      })
  }
}
