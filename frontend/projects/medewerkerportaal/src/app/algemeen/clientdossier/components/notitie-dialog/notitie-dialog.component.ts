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
import { Component, computed, inject } from '@angular/core'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsDescriptionsComponent, DsRadiobuttonOption, DsSummaryPanelComponent, DsTextareaComponent } from '@topicus-rgp-ds/web'
import { FormBuilder, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms'
import { NL_DATE_FORMAT, NL_DATE_TIME_FORMAT, NL_TIMEZONE } from '@shared/constants'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ClientContactDto } from '@shared/types/algemeen/dto/client-contact.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekAfkortingLabels } from '@shared/types/bevolkingsonderzoek'
import { Actie } from '@shared/types/autorisatie/actie'
import { Recht } from '@shared/types/autorisatie/recht'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Required } from '@shared/types/autorisatie/required'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { filter, switchMap, take } from 'rxjs'
import { DatePipe } from '@angular/common'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientService } from '@algemeen/services/client/client.service'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { NaamPipe, NaamTransform } from '@shared/pipes/naam/naam.pipe'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { isStringNullOfLeeg } from '@shared/utils/string-utils'
import { clientContactActieLabels } from '@shared/utils/client-contact-actie-utils'

@Component({
  selector: 'app-notitie-dialog',
  imports: [
    BaseDialogComponent,
    DsDescriptionsComponent,
    FormsModule,
    ReactiveFormsModule,
    DsTextareaComponent,
    DsButtonComponent,
    DatePipe,
    NaamPipe,
    DsSummaryPanelComponent,
    EnumLabelPipe,
  ],
  templateUrl: './notitie-dialog.component.html',
})
export class NotitieDialogComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly clientContactService = inject(ClientContactService)
  private readonly dialog = inject(Dialog)
  private readonly dialogRef = inject(DialogRef)
  private readonly notificatieService = inject(NotificationService)
  private readonly clientService = inject(ClientService)

  protected readonly dialogData: { contact: ClientContactDto; readonly: boolean; toonHistorieGegevens: boolean } = inject(DIALOG_DATA)
  protected readonly bestaandeContact: ClientContactDto = this.dialogData.contact
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  protected readonly NL_TIMEZONE = NL_TIMEZONE
  protected readonly isNieuweNotitie = computed(() => this.bestaandeContact?.id == null)
  protected readonly titel = computed(() => (this.isNieuweNotitie() ? 'Notitie toevoegen' : 'Notitie bewerken'))
  protected readonly algemeenBvo = { label: 'Algemeen', value: 'Algemeen' }
  protected readonly bevolkingsonderzoekLabels = bevolkingsonderzoekAfkortingLabels
  protected readonly actiesLabel = clientContactActieLabels(this.bestaandeContact?.acties ?? [])
  protected readonly bevolkingsonderzoeken: DsRadiobuttonOption<Bevolkingsonderzoek | string>[] = [
    this.algemeenBvo,
    ...Object.values(Bevolkingsonderzoek).map((bvo) => ({
      label: bevolkingsonderzoekAfkortingLabels[bvo],
      value: bvo.toString(),
    })),
  ]

  protected readonly magVerwijderen = computed(
    () =>
      this.autorisatieService.isToegestaan({
        actie: Actie.VERWIJDEREN,
        recht: [Recht.MEDEWERKER_CLIENT_CONTACT],
        bevolkingsonderzoekScopes: [Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA],
        level: ToegangLevel.LANDELIJK,
        required: Required.ALL,
      }) &&
      !this.isNieuweNotitie() &&
      !isStringNullOfLeeg(this.bestaandeContact.notitie?.trim()),
  )

  protected readonly notitieCtrl = this.formBuilder.control<string | null>(null, [Validators.required, Validators.maxLength(2048)])

  constructor() {
    if (this.dialogData.readonly) {
      this.notitieCtrl.disable()
      this.notitieCtrl.clearValidators()
    }

    if (this.dialogData.contact) {
      this.notitieCtrl.setValue(this.dialogData.contact.notitie)
    }
  }

  verwijderNotitie() {
    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Bevestiging',
          body: 'Weet u zeker dat u deze notitie wilt verwijderen?',
          jaTekst: 'Ja, verwijderen',
        },
      })
      .closed.pipe(
        take(1),
        filter((res: unknown) => res === true),
        switchMap(() => {
          const clientId = this.clientService.clientId()
          return this.dialogData.toonHistorieGegevens
            ? this.clientContactService.verwijderNotitieUitContact(this.bestaandeContact.id, clientId)
            : this.clientContactService.verwijderNotitie(this.bestaandeContact.id, clientId)
        }),
      )
      .subscribe(() => {
        this.notificatieService.success('Notitie succesvol verwijderd')
        this.dialogRef.close(true)
      })
  }

  wijzigingenOpslaan() {
    const contact = { ...this.bestaandeContact, notitie: this.notitieCtrl.value ?? '' }
    this.clientContactService
      .slaNotitieOp(contact.id, contact.notitie)
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('Notitie succesvol gewijzigd')
        this.dialogRef.close(true)
      })
  }

  notitieToevoegen() {
    const notitie = this.notitieCtrl.value ?? ''
    this.clientContactService
      .maakContact({
        clientId: this.clientService.clientId(),
        notitie,
        datumTijd: new Date(),
        acties: [{ type: ClientContactActieType.GEEN }],
      })
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('Notitie succesvol toegevoegd')
        this.dialogRef.close(true)
      })
  }

  sluitDialog() {
    this.dialogRef.close(false)
  }

  protected readonly NaamTransform = NaamTransform
}
