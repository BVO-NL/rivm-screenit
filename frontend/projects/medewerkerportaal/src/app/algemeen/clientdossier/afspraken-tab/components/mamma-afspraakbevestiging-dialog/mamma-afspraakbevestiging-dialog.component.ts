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
import { Component, DestroyRef, effect, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { format, parseISO } from 'date-fns'
import { Observable, take } from 'rxjs'
import {
  DsButtonComponent,
  DsContextualNotificationsComponent,
  DsContextualNotificationService,
  DsInputComponent,
  DsRadiobuttonComponent,
  DsRadiobuttonOption,
} from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { BriefAdresInformatiebalkComponent } from '@shared/components/brief-adres-informatiebalk/brief-adres-informatiebalk.component'
import { NotificationService } from '@shared/services/notification/notification.service'
import { StringsService } from '@shared/strings'
import { MAX_KARAKTERS, NL_LONG_DATE_FORMAT } from '@shared/constants'
import { MammaAfspraakDto } from '@shared/types/mamma/dto/mamma-afspraak.dto'
import { ClientService } from '@algemeen/services/client/client.service'
import { MammaClientafspraakService } from '@mamma/services/mamma-clientafspraak/mamma-clientafspraak.service'

export type BevestigingKeuze = 'BRIEF' | 'EMAIL'

export interface MammaAfspraakbevestigingDialogData {
  afspraak: MammaAfspraakDto
}

@Component({
  selector: 'app-mamma-afspraakbevestiging-dialog',
  imports: [
    BaseDialogComponent,
    BriefAdresInformatiebalkComponent,
    DsButtonComponent,
    DsContextualNotificationsComponent,
    DsInputComponent,
    DsRadiobuttonComponent,
    ReactiveFormsModule,
  ],
  templateUrl: './mamma-afspraakbevestiging-dialog.component.html',
})
export class MammaAfspraakbevestigingDialogComponent {
  private readonly dialogRef: DialogRef<BevestigingKeuze> = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly mammaClientafspraakService = inject(MammaClientafspraakService)
  private readonly notificationService = inject(NotificationService)
  private readonly contextualNotificationService = inject(DsContextualNotificationService)
  private readonly clientService = inject(ClientService)
  private readonly afspraak = inject<MammaAfspraakbevestigingDialogData>(DIALOG_DATA).afspraak
  private readonly client = this.clientService.select('client')()
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly context = 'mamma-afspraakbevestiging'
  protected readonly postadres = this.client.postadres ?? null
  protected readonly briefAlVerstuurd = this.afspraak.bevestigingsbriefAangevraagdOp !== null

  protected readonly form = this.formBuilder.group({
    keuze: this.formBuilder.nonNullable.control<BevestigingKeuze>(this.briefAlVerstuurd ? 'EMAIL' : 'BRIEF'),
    emailadres: this.formBuilder.nonNullable.control({ value: this.client.emailadres ?? '', disabled: !this.briefAlVerstuurd }, [
      Validators.required,
      Validators.email,
      Validators.maxLength(MAX_KARAKTERS.emailadres),
    ]),
  })

  protected readonly keuze = toSignal(this.form.get('keuze')!.valueChanges, { initialValue: this.form.get('keuze')!.value })

  private readonly emailOptie: DsRadiobuttonOption<BevestigingKeuze> = { label: this.strings.algemeen.labels.emailadres, value: 'EMAIL' }
  protected readonly keuzeOpties: DsRadiobuttonOption<BevestigingKeuze>[] = this.briefAlVerstuurd
    ? [this.emailOptie]
    : [{ label: this.strings.algemeen.labels.brief, value: 'BRIEF' }, this.emailOptie]

  protected readonly emailadresFouten = {
    required: this.strings.algemeen.errors.verplicht(this.strings.algemeen.labels.emailadres),
    email: this.strings.algemeen.errors.invalidEmail,
    maxlength: this.strings.algemeen.errors.maxKarakters(this.strings.algemeen.labels.emailadres, MAX_KARAKTERS.emailadres),
  }

  constructor() {
    this.toonBriefAlVerstuurdMelding()
    inject(DestroyRef).onDestroy(() => this.contextualNotificationService.removeNotificationByContext(this.context))

    effect(() => {
      if (this.keuze() === 'EMAIL') {
        this.emailadresCtrl.enable()
      } else {
        this.emailadresCtrl.disable()
      }
    })
  }

  private get emailadresCtrl() {
    return this.form.get('emailadres')!
  }

  protected annuleren() {
    this.dialogRef.close()
  }

  protected versturen() {
    const notifications = this.strings.algemeen.afspraken.mamma.afspraakbevestigingDialog.notifications
    const clientId = this.clientService.clientId()
    if (this.keuze() === 'BRIEF') {
      this.verstuur(this.mammaClientafspraakService.maakBevestigingsbrief(this.afspraak.id, clientId), notifications.briefAangemaakt)
    } else {
      this.form.markAllAsTouched()
      if (this.form.valid) {
        this.verstuur(this.mammaClientafspraakService.verstuurBevestigingsmail(this.afspraak.id, clientId, this.emailadresCtrl.value), notifications.emailVerstuurd)
      }
    }
  }

  private verstuur(verzoek: Observable<void>, melding: string) {
    const keuze = this.keuze()
    verzoek.pipe(take(1)).subscribe(() => {
      this.notificationService.success(melding)
      this.dialogRef.close(keuze)
    })
  }

  private toonBriefAlVerstuurdMelding() {
    const aangevraagdOp = this.afspraak.bevestigingsbriefAangevraagdOp
    if (aangevraagdOp) {
      const datum = format(parseISO(aangevraagdOp), NL_LONG_DATE_FORMAT)
      const message = this.strings.algemeen.afspraken.mamma.afspraakbevestigingDialog.briefAlVerstuurd(datum)
      this.contextualNotificationService.warning(this.context, { message, closeable: false }, 'page')
    }
  }
}
