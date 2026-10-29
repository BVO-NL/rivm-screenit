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
import { DsButtonComponent, DsDatepickerComponent, DsDescriptionsComponent, DsInputComponent, DsSummaryPanelComponent, DsValidators, MaxLengthDirective } from '@topicus-rgp-ds/web'
import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { Observable, take } from 'rxjs'
import { addDays, isValid } from 'date-fns'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { TijdelijkAdresDto } from '@shared/types/algemeen/dto/tijdelijk-adres.dto'
import { NotPersisted } from '@shared/types/not-presisted'
import { formatNLDate, isEinddatumVerlopen, normaliseerNaarDate, parseDate } from '@shared/utils/date-utils'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientService } from '@/algemeen/services/client/client.service'
import { huisletterValidator, trimmedValidator } from '@shared/validators/common-validators'
import { createEinddatumNaBegindatumValidator } from '@shared/validators/datum/datum.validator'
import { StringsService } from '@shared/strings'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { MAX_KARAKTERS } from '@shared/constants'

export interface AdresgegevensBewerkenModalData extends Partial<TijdelijkAdresDto> {
  clientId: string
  huidigAdres?: { volledigeAdres: string; postcode: string; plaats: string }
  alsContactActie?: boolean
}

@Component({
  selector: 'app-adresgegevens-bewerken-modal',
  imports: [
    BaseDialogComponent,
    DsButtonComponent,
    DsDatepickerComponent,
    ReactiveFormsModule,
    DsInputComponent,
    DsDescriptionsComponent,
    DsSummaryPanelComponent,
    NotitieAanmakenVeldComponent,
    MaxLengthDirective,
  ],
  templateUrl: './adresgegevens-bewerken-modal.component.html',
  styleUrl: './adresgegevens-bewerken-modal.component.scss',
})
export class AdresgegevensBewerkenModalComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialogData = inject<AdresgegevensBewerkenModalData>(DIALOG_DATA)
  private readonly tijdelijkAdres: Partial<TijdelijkAdresDto> & { clientId: string } = isEinddatumVerlopen(this.dialogData.einddatum)
    ? { clientId: this.dialogData.clientId }
    : this.dialogData
  protected readonly heeftTijdelijkAdres = this.tijdelijkAdres.id != null
  protected readonly huidigAdres = this.dialogData.huidigAdres
  protected readonly alsContactActie = this.dialogData.alsContactActie ?? false
  private readonly notificationService = inject(NotificationService)
  private readonly clientService = inject(ClientService)
  private readonly contactActiesService = inject(ContactActiesService)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly MAX_KARAKTERS = MAX_KARAKTERS

  adresgegevensForm: FormGroup = this.formBuilder.group(
    {
      straat: [this.tijdelijkAdres?.straat ?? '', Validators.required],
      huisnummer: [this.tijdelijkAdres?.huisnummer ?? null, [Validators.required, Validators.min(1)]],
      huisletter: [this.tijdelijkAdres?.huisletter ?? '', huisletterValidator],
      huisnummerToevoeging: [this.tijdelijkAdres?.huisnummerToevoeging ?? ''],
      huisnummerAanduiding: [this.tijdelijkAdres?.huisnummerAanduiding ?? ''],
      postcode: [this.tijdelijkAdres?.postcode ?? '', [Validators.required, trimmedValidator(DsValidators.postcode)]],
      plaats: [this.tijdelijkAdres?.plaats ?? '', Validators.required],
      begindatum: [this.tijdelijkAdres?.begindatum ?? null, Validators.required],
      einddatum: [this.tijdelijkAdres?.einddatum ?? null, Validators.required],
      notitie: [''],
    },
    { validators: createEinddatumNaBegindatumValidator() },
  )

  get begindatumCtrl(): FormControl {
    return this.adresgegevensForm.get('begindatum') as FormControl
  }

  get minEinddatum(): string {
    const begindatum = this.begindatumCtrl.value

    let begindatumDatum: Date | null = null
    if (begindatum instanceof Date) {
      begindatumDatum = begindatum
    } else if (typeof begindatum === 'string') {
      begindatumDatum = parseDate(begindatum)
    }

    return begindatumDatum != null && isValid(begindatumDatum) ? addDays(begindatumDatum, 1).toISOString() : ''
  }

  protected get naBegindatumMelding(): string {
    return this.strings.algemeen.errors.ingevuldeDatumNa(this.formatteerDatumVeld('begindatum'))
  }

  protected get voorEinddatumMelding(): string {
    return this.strings.algemeen.errors.ingevuldeDatumVoor(this.formatteerDatumVeld('einddatum'))
  }

  private formatteerDatumVeld(veld: 'begindatum' | 'einddatum'): string {
    const datum = normaliseerNaarDate(this.adresgegevensForm.get(veld)?.value)
    return datum != null && isValid(datum) ? formatNLDate(datum) : ''
  }

  protected cancel() {
    this.dialogRef.close()
  }

  protected save() {
    const tijdelijkAdresDto = this.naarTijdelijkAdresDto()

    this.slaTijdelijkAdresOp(tijdelijkAdresDto)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success(this.bepaalSuccesmelding(tijdelijkAdresDto))
          this.dialogRef.close(true)
        },
        error: () => {
          this.notificationService.error(this.strings.algemeen.adresgegevensBewerken.notifications.saveError)
        },
      })
  }

  private slaTijdelijkAdresOp(tijdelijkAdresDto: NotPersisted<TijdelijkAdresDto>): Observable<void> {
    if (this.alsContactActie) {
      return this.contactActiesService.wijzigTijdelijkAdres(Number(tijdelijkAdresDto.clientId), {
        tijdelijkAdres: tijdelijkAdresDto,
        notitie: this.adresgegevensForm.get('notitie')?.value,
      })
    }
    return this.clientService.updateTijdelijkAdres(tijdelijkAdresDto)
  }

  private bepaalSuccesmelding(tijdelijkAdresDto: NotPersisted<TijdelijkAdresDto>): string {
    return isEinddatumVerlopen(tijdelijkAdresDto.einddatum)
      ? this.strings.algemeen.adresgegevensBewerken.notifications.saveSuccessMetVerlopen
      : this.strings.algemeen.adresgegevensBewerken.notifications.saveSuccess
  }

  naarTijdelijkAdresDto(): NotPersisted<TijdelijkAdresDto> {
    const tijdelijkAdresForm = this.adresgegevensForm.value

    return {
      clientId: this.tijdelijkAdres.clientId,
      straat: tijdelijkAdresForm.straat || undefined,
      huisnummer: tijdelijkAdresForm.huisnummer || undefined,
      huisletter: tijdelijkAdresForm.huisletter || undefined,
      huisnummerToevoeging: tijdelijkAdresForm.huisnummerToevoeging || undefined,
      huisnummerAanduiding: tijdelijkAdresForm.huisnummerAanduiding || undefined,
      postcode: tijdelijkAdresForm.postcode || undefined,
      plaats: tijdelijkAdresForm.plaats || undefined,
      begindatum: tijdelijkAdresForm.begindatum || null,
      einddatum: tijdelijkAdresForm.einddatum || null,
    }
  }
}
