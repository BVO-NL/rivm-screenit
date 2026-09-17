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
import { DsButtonComponent, DsDatepickerComponent, DsDescriptionsComponent, DsInputComponent, DsSummaryPanelComponent, DsValidators } from '@topicus-rgp-ds/web'
import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { take } from 'rxjs'
import { addDays, isValid } from 'date-fns'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { TijdelijkAdresDto } from '@shared/types/algemeen/dto/tijdelijk-adres.dto'
import { isEinddatumVerlopen, parseDate } from '@shared/utils/date-utils'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientService } from '@/algemeen/services/client/client.service'
import { huisletterValidator, trimmedValidator } from '@shared/validators/common-validators'
import { createEinddatumNaBegindatumValidator } from '@shared/validators/datum/datum.validator'

@Component({
  selector: 'app-adresgegevens-bewerken-modal',
  imports: [BaseDialogComponent, DsButtonComponent, DsDatepickerComponent, ReactiveFormsModule, DsInputComponent, DsDescriptionsComponent, DsSummaryPanelComponent],
  templateUrl: './adresgegevens-bewerken-modal.component.html',
  styleUrl: './adresgegevens-bewerken-modal.component.scss',
})
export class AdresgegevensBewerkenModalComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialogData = inject(DIALOG_DATA) as TijdelijkAdresDto & {
    huidigAdres: { volledigeAdres: string; postcode: string; plaats: string }
  }
  private readonly tijdelijkAdres: Partial<TijdelijkAdresDto> & { clientId: string } = isEinddatumVerlopen(this.dialogData.einddatum)
    ? { clientId: this.dialogData.clientId }
    : this.dialogData
  protected readonly huidigAdres = this.dialogData.huidigAdres
  private readonly notificationService = inject(NotificationService)
  private readonly clientService = inject(ClientService)

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
    },
    { validators: createEinddatumNaBegindatumValidator() },
  )

  get begindatumCtrl(): FormControl {
    return this.adresgegevensForm.get('begindatum') as FormControl
  }

  get minEinddatum(): string {
    const begindatum = this.begindatumCtrl.value

    const begindatumDatum = begindatum instanceof Date ? begindatum : typeof begindatum === 'string' ? parseDate(begindatum) : null

    return begindatumDatum != null && isValid(begindatumDatum) ? addDays(begindatumDatum, 1).toISOString() : ''
  }

  protected cancel() {
    this.dialogRef.close()
  }

  protected save() {
    const tijdelijkAdresDto = this.naarTijdelijkAdresDto()

    this.clientService
      .updateTijdelijkAdres(tijdelijkAdresDto)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success(this.bepaalSuccesmelding(tijdelijkAdresDto))
          this.dialogRef.close(true)
        },
        error: () => {
          this.notificationService.error('Er is een fout opgetreden bij het opslaan van de adresgegevens. Probeer het later opnieuw.')
        },
      })
  }

  private bepaalSuccesmelding(tijdelijkAdresDto: TijdelijkAdresDto): string {
    return (
      'Adresgegevens succesvol opgeslagen.' +
      (isEinddatumVerlopen(tijdelijkAdresDto.einddatum) ? ' Dit tijdelijke adres heeft een einddatum in het verleden en is daarom niet zichtbaar in het overzicht.' : '')
    )
  }

  naarTijdelijkAdresDto(): TijdelijkAdresDto {
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
