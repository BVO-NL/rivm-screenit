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
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import {
  DsButtonComponent,
  DsContextualNotificationsComponent,
  DsDescriptionsComponent,
  DsDropdownComponent,
  DsInputComponent,
  DsSummaryPanelComponent,
  DsValidators,
} from '@topicus-rgp-ds/web'
import { FormBuilder, FormControl, FormGroup, ReactiveFormsModule } from '@angular/forms'
import { ClientContactgegevensDto } from '@shared/types/algemeen/dto/clientcontactgegevens.dto'
import { DatePipe } from '@angular/common'
import { Doelgroep } from '@shared/types/algemeen/enum/doelgroep'
import { aanspreekvormLabel, clientAanspreekvormen } from '@shared/types/algemeen/enum/aanspreekvorm'
import { NotificationService } from '@shared/services/notification/notification.service'
import { ClientService } from '@/algemeen/services/client/client.service'
import { take } from 'rxjs'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { enumNaarOpties } from '@shared/types/enum-optie'
import { geslachtLabel } from '@shared/types/algemeen/enum/geslacht'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { mobieleTelefoonValidator, normaalTelefoonnummerValidator } from '@shared/validators/telefoon-nummer/telefoon-nummer.validators'
import { NL_DATE_FORMAT } from '@shared/constants'
import { DoelgroepKeuze, DoelgroepSelectorComponent } from '@/algemeen/clientdossier/components/doelgroep-selector/doelgroep-selector.component'
import { StringsService } from '@shared/strings'

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
    DsSummaryPanelComponent,
    DoelgroepSelectorComponent,
    DsContextualNotificationsComponent,
  ],
  templateUrl: './client-info-bewerken-dialog.component.html',
  styleUrl: './client-info-bewerken-dialog.component.scss',
})
export class ClientInfoBewerkenDialogComponent {
  private readonly dialogRef = inject(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly notificationService = inject(NotificationService)
  private readonly clientService = inject(ClientService)

  protected readonly strings = inject(StringsService).getStrings()
  protected readonly clientData: ClientContactgegevensDto = inject(DIALOG_DATA)
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly aanspreekvormOpties = enumNaarOpties(aanspreekvormLabel).filter((optie) => clientAanspreekvormen.includes(optie.waarde))
  protected readonly geslachtLabel = geslachtLabel
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
    doelgroep: [{ doelgroep: this.kiesbareDoelgroepen() ?? Doelgroep.REGULIER, dubbelTijdReden: this.clientData.dubbeleTijdReden ?? '' } satisfies DoelgroepKeuze],
  })

  get doelgroepCtrl(): FormControl {
    return this.contactgegevensForm.get('doelgroep') as FormControl
  }

  protected cancel() {
    this.dialogRef.close()
  }

  protected save() {
    if (!this.contactgegevensForm.valid) {
      return
    }

    this.clientService
      .saveContactgegevens(this.naarContactgegevensDto())
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success(this.strings.algemeen.clientInfoBewerken.notifications.saveSuccess)
          this.dialogRef.close(true)
        },
        error: () => {
          this.notificationService.error(this.strings.algemeen.clientInfoBewerken.notifications.saveError)
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
      doelgroepen: this.naarDoelgroepen(form.doelgroep?.doelgroep),
      dubbeleTijdReden: form.doelgroep?.dubbelTijdReden,
    }
  }

  private kiesbareDoelgroepen(): Doelgroep | undefined {
    return this.clientData.doelgroepen.find((doelgroep) => doelgroep === Doelgroep.DUBBELE_TIJD || doelgroep === Doelgroep.MINDERVALIDE || doelgroep === Doelgroep.REGULIER)
  }

  private naarDoelgroepen(doelgroep: Doelgroep | null | undefined): Doelgroep[] {
    return doelgroep ? [doelgroep] : []
  }
}
