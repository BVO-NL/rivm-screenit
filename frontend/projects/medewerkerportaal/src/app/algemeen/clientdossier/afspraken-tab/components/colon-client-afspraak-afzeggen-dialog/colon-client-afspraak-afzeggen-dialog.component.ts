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
import { DsButtonComponent, DsCheckboxComponent, DsDescriptionsComponent, DsSummaryPanelComponent } from '@topicus-rgp-ds/web'
import { filter, switchMap, take } from 'rxjs'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { NotificationService } from '@shared/services/notification/notification.service'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { StringsService } from '@shared/strings'
import { DatumNaarWeekPipe } from '@shared/pipes/datum-naar-week/datum-naar-week.pipe'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'

@Component({
  selector: 'app-colon-client-afspraak-afzeggen-dialog',
  imports: [
    AutorisatieDirective,
    BaseDialogComponent,
    DatumTijdPipe,
    DsButtonComponent,
    DsCheckboxComponent,
    DsDescriptionsComponent,
    DsSummaryPanelComponent,
    NotitieAanmakenVeldComponent,
    ReactiveFormsModule,
    DatumNaarWeekPipe,
  ],
  templateUrl: './colon-client-afspraak-afzeggen-dialog.component.html',
  styleUrl: './colon-client-afspraak-afzeggen-dialog.component.scss',
})
export class ColonClientAfspraakAfzeggenDialogComponent {
  private readonly dialogRef: DialogRef<boolean> = inject(DialogRef)
  private readonly dialog = inject(Dialog)
  private readonly colonAfspraakService = inject(ColonIntakeafspraakService)
  private readonly notificationService = inject(NotificationService)
  protected readonly strings = inject(StringsService).getStrings()
  private readonly dialogData = inject<{ clientId: number; afspraak: ClientAfspraakDto }>(DIALOG_DATA)
  protected readonly afspraak = this.dialogData.afspraak
  protected readonly briefTegenhoudenCtrl = new FormControl(false, { nonNullable: true })
  protected readonly notitieCtrl = new FormControl('', { nonNullable: true })

  protected readonly brievenTegenhoudenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN],
    actie: Actie.AANPASSEN,
    required: Required.ANY,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
  }

  protected isDigitaalAfspraak(afspraak: ClientAfspraakDto): boolean {
    return this.colonAfspraakService.isDigitaleAfspraak(afspraak)
  }

  protected annuleren() {
    this.dialogRef.close()
  }

  protected afzeggen() {
    const briefTegenhouden = this.briefTegenhoudenCtrl.value
    const notitie = this.notitieCtrl.value
    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: this.strings.algemeen.dialogs.bevestiging.titel,
          body: briefTegenhouden ? this.strings.algemeen.afspraken.afzeggen.briefTegenhouden : this.strings.algemeen.afspraken.afzeggen.briefVersturen,
          neeButtonTekst: this.strings.algemeen.dialogs.bevestiging.neeButtonTekst,
          jaButtonTekst: this.strings.algemeen.dialogs.bevestiging.jaButtonTekst,
        },
        hasBackdrop: true,
      })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.colonAfspraakService.zegAfspraakAf(this.afspraak.id, this.dialogData.clientId, briefTegenhouden, notitie)),
      )
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.afspraken.afzeggen.succesMessage)
        this.dialogRef.close(true)
      })
  }
}
