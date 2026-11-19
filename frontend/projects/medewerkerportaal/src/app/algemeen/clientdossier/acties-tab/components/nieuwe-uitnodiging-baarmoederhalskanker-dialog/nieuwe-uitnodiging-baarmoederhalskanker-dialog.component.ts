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
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { FormControl, ReactiveFormsModule } from '@angular/forms'
import { filter, switchMap, take } from 'rxjs'
import { DsButtonComponent, DsDescriptionsComponent, DsSummaryPanelComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { CervixLaatsteUitnodigingDto } from '@shared/types/cervix/cervix-laatste-uitnodiging.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@shared/types/bevolkingsonderzoek'
import { typeGebeurtenisLabels } from '@shared/types/algemeen/enum/type-gebeurtenis'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'
import { LONG_TIME_FORMAT, NL_DATE_FORMAT } from '@shared/constants'
import { StringsService } from '@shared/strings'

export interface NieuweUitnodigingBaarmoederhalskankerDialogData {
  clientId: number
  laatsteUitnodiging: CervixLaatsteUitnodigingDto | null
}

@Component({
  selector: 'app-nieuwe-uitnodiging-baarmoederhalskanker-dialog',
  imports: [
    BaseDialogComponent,
    DatumTijdPipe,
    DsButtonComponent,
    DsDescriptionsComponent,
    DsSummaryPanelComponent,
    EnumLabelPipe,
    LegeWaardePipe,
    NotitieAanmakenVeldComponent,
    ReactiveFormsModule,
  ],
  templateUrl: './nieuwe-uitnodiging-baarmoederhalskanker-dialog.component.html',
})
export class NieuweUitnodigingBaarmoederhalskankerDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly dialog = inject(Dialog)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly data = inject<NieuweUitnodigingBaarmoederhalskankerDialogData>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly nieuweUitnodigingStrings = this.strings.algemeen.clientDossier.acties.nieuweUitnodigingBmhk
  protected readonly laatsteUitnodiging = this.data.laatsteUitnodiging
  protected readonly typeGebeurtenisLabels = typeGebeurtenisLabels
  protected readonly bevolkingsonderzoekLabels = bevolkingsonderzoekLabels
  protected readonly Bevolkingsonderzoek = Bevolkingsonderzoek
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly LONG_TIME_FORMAT = LONG_TIME_FORMAT

  protected readonly notitie = new FormControl<string>('', { nonNullable: true })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected aanvragen() {
    this.dialog
      .open<boolean>(ConfirmationDialogComponent, {
        data: {
          title: this.nieuweUitnodigingStrings.bevestigingTitel,
          body: this.nieuweUitnodigingStrings.bevestigingBody,
          neeButtonTekst: this.strings.algemeen.dialogs.bevestiging.neeButtonTekst,
          jaButtonTekst: this.nieuweUitnodigingStrings.bevestigingJaButtonTekst,
        },
        hasBackdrop: true,
      })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.contactActiesService.vraagNieuweUitnodigingBaarmoederhalskankerAan(this.data.clientId, { notitie: this.notitie.value })),
      )
      .subscribe(() => {
        this.notificationService.success(this.nieuweUitnodigingStrings.notifications.succes)
        this.dialogRef.close(true)
      })
  }
}
