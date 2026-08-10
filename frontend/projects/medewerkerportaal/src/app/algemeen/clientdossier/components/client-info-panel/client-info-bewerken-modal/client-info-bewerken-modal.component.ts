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
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsDescriptionsComponent, DsDropdownComponent, DsInputComponent, DsValidators } from '@topicus-rgp-ds/web'
import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { DatePipe } from '@angular/common'
import { Doelgroep, DoelgroepKiesbareOpties, doelgroepLabel } from '@shared/types/algemeen/enum/doelgroep'
import { aanspreekvormLabel, clientAanspreekvormen } from '@shared/types/algemeen/enum/aanspreekvorm'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientService } from '@/algemeen/services/client/client.service'
import { filter, take } from 'rxjs'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { enumNaarOpties } from '@shared/types/enum-optie'
import { geslachtLabel } from '@shared/types/algemeen/enum/geslacht'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { mobieleTelefoonValidator, normaalTelefoonnummerValidator } from '@shared/validators/telefoon-nummer/telefoon-nummer.validators'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { NL_DATE_FORMAT } from '@shared/constants'

@Component({
  selector: 'app-client-info-bewerken-modal',
  imports: [
    BaseDialogComponent,
    DsButtonComponent,
    ReactiveFormsModule,
    DsInputComponent,
    DsDescriptionsComponent,
    DsDropdownComponent,
    DatePipe,
    EnumLabelPipe,
    AutorisatieDirective,
  ],
  templateUrl: './client-info-bewerken-modal.component.html',
  styleUrl: './client-info-bewerken-modal.component.scss',
})
export class ClientInfoBewerkenModalComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialog = inject(Dialog)
  private readonly notificationService = inject(NotificationService)
  private readonly clientService = inject(ClientService)

  protected readonly clientData: ClientContactgegevensDto = inject(DIALOG_DATA)
  protected readonly Doelgroep = Doelgroep
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly doelgroepOpties = enumNaarOpties(
    Object.values(DoelgroepKiesbareOpties).reduce(
      (labels, waarde) => {
        labels[waarde] = doelgroepLabel[waarde]
        return labels
      },
      {} as Record<DoelgroepKiesbareOpties, string>,
    ),
  )
  protected readonly aanspreekvormOpties = enumNaarOpties(aanspreekvormLabel).filter((optie) => clientAanspreekvormen.includes(optie.waarde))
  protected readonly geslachtLabel = geslachtLabel
  private readonly oorspronkelijkeDoelgroep = this.kiesbareDoelgroepen()
  private readonly baseConstraint = {
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  protected readonly contactgegevensConstraint: SecurityConstraint = {
    ...this.baseConstraint,
    recht: [Recht.MEDEWERKER_CLIENT_CONTACTGEGEVENS_REGISTREREN],
    actie: Actie.AANPASSEN,
  }
  protected readonly aanspreekvormConstraint: SecurityConstraint = {
    ...this.baseConstraint,
    recht: [Recht.MEDEWERKER_CLIENT_GEGEVENS],
    actie: Actie.INZIEN,
  }
  protected readonly doelgroepConstraint: SecurityConstraint = {
    ...this.baseConstraint,
    recht: [Recht.MEDEWERKER_CLIENT_MAMMA_DOELGROEP_WIJZIGEN],
    actie: Actie.AANPASSEN,
  }
  protected readonly genderIdentiteitConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_TOON_GENDERINDETITEIT],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    required: Required.ANY,
  }
  contactgegevensForm: FormGroup = this.formBuilder.group({
    id: [this.clientData.clientId],
    mobielNummer: [this.clientData.mobielNummer, mobieleTelefoonValidator],
    extraNummer: [this.clientData.extraNummer, normaalTelefoonnummerValidator],
    emailadres: [this.clientData.emailAdres, DsValidators.email],
    aanspreekvorm: [this.clientData.aanspreekvorm],
    doelgroep: [this.oorspronkelijkeDoelgroep],
    dubbelTijdReden: [this.clientData.dubbeleTijdReden ?? '', Validators.maxLength(255)],
  })

  constructor() {
    this.contactgegevensForm
      .get('doelgroep')
      ?.valueChanges.pipe(takeUntilDestroyed())
      .subscribe(() => this.updateDubbelTijdRedenValidatie())
    this.updateDubbelTijdRedenValidatie()
  }

  get doelgroepCtrl(): FormControl {
    return this.contactgegevensForm.get('doelgroep') as FormControl
  }

  get selectedDoelgroep(): Doelgroep | undefined {
    return this.doelgroepCtrl.value
  }

  get dubbelTijdRedenCtrl(): FormControl<string> {
    return this.contactgegevensForm.get('dubbelTijdReden') as FormControl<string>
  }

  protected cancel() {
    this.dialogRef.close()
  }

  protected save() {
    if (!this.contactgegevensForm.valid) {
      return
    }

    const doelgroepGewijzigd = this.selectedDoelgroep != this.oorspronkelijkeDoelgroep
    if (doelgroepGewijzigd && this.clientData.heeftMammaAfspraak) {
      this.dialog
        .open(ConfirmationDialogComponent, {
          data: {
            title: 'Doelgroep gewijzigd',
            body: 'De doelgroep is gewijzigd. De client heeft echter nog een afspraak. U wordt geadviseerd om deze te verzetten of uit te stellen.',
            neeBtnTekst: 'Annuleren',
            jaBtnTekst: 'Begrepen',
          },
        })
        .closed.pipe(
          take(1),
          filter((doorgaan) => doorgaan === true),
        )
        .subscribe(() => this.saveContactgegevens())
    } else {
      this.saveContactgegevens()
    }
  }

  private saveContactgegevens() {
    this.clientService
      .saveContactgegevens(this.naarContactgegevensDto())
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success('Contactgegevens succesvol gewijzigd.')
          this.dialogRef.close(true)
        },
        error: () => {
          this.notificationService.error('Er is een fout opgetreden bij het opslaan van de contactgegevens. Probeer het later opnieuw.')
        },
      })
  }

  private naarContactgegevensDto(): ClientContactgegevensDto {
    const form = this.contactgegevensForm.value

    return {
      ...this.clientData,
      clientId: form.id,
      mobielNummer: form.mobielNummer,
      extraNummer: form.extraNummer,
      emailAdres: form.emailadres,
      aanspreekvorm: form.aanspreekvorm,
      doelgroepen: form.doelgroep ? [form.doelgroep] : [],
      dubbeleTijdReden: form.dubbelTijdReden,
    }
  }

  private kiesbareDoelgroepen(): Doelgroep | undefined {
    return this.clientData.doelgroepen.find((doelgroep) => doelgroep === Doelgroep.DUBBELE_TIJD || doelgroep === Doelgroep.MINDERVALIDE)
  }

  private updateDubbelTijdRedenValidatie() {
    if (this.selectedDoelgroep === Doelgroep.DUBBELE_TIJD) {
      this.dubbelTijdRedenCtrl.setValidators([Validators.required, Validators.maxLength(255)])
    } else {
      this.dubbelTijdRedenCtrl.setValidators([Validators.maxLength(255)])
    }
    this.dubbelTijdRedenCtrl.updateValueAndValidity({ emitEvent: false })
  }
}
