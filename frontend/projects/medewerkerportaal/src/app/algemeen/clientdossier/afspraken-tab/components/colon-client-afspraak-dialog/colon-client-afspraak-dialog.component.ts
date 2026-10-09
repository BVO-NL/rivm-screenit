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
import { AfterViewInit, ChangeDetectorRef, Component, computed, inject, input, output, signal, viewChild, WritableSignal } from '@angular/core'
import { DsContextualNotificationService, DsFullscreenModalWizardComponent, DsWizardStepComponent, DsWizardStepControl } from '@topicus-rgp-ds/web'
import { ColonClientAfspraakZoekenComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken/colon-client-afspraak-zoeken.component'
import { format, parseISO } from 'date-fns'
import { NL_DATE_FORMAT, NL_DAY_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { ColonClientAfspraakBevestigingComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-bevestiging/colon-client-afspraak-bevestiging.component'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { ColonAfspraakSlotKeuze } from '@shared/types/colon/colon-afspraak-slot-keuze'
import { ClientService } from '@algemeen/services/client/client.service'
import { ColonClientAfspraakSamenvattingComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-samenvatting/colon-client-afspraak-samenvatting.component'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { take } from 'rxjs'
import { briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { StringsService } from '@shared/strings'
import { ColonIntakeafspraakAanmakenType } from '@shared/types/colon/enum/colon-intakeafspraak-aanmaken-type'
import { formatDatumVoorDigitaleAfspraak } from '@shared/utils/date-utils'

export type ColonClientAfspraakDialogMode = 'tijdstip_wijzigen' | 'locatie_wijzigen' | 'aanmaken' | null

@Component({
  selector: 'app-colon-client-afspraak-dialog',
  imports: [
    DsFullscreenModalWizardComponent,
    DsWizardStepComponent,
    ColonClientAfspraakZoekenComponent,
    ColonClientAfspraakBevestigingComponent,
    ColonClientAfspraakSamenvattingComponent,
  ],
  templateUrl: './colon-client-afspraak-dialog.component.html',
  styleUrl: './colon-client-afspraak-dialog.component.scss',
})
export class ColonClientAfspraakDialogComponent implements AfterViewInit {
  afspraak = input<ClientAfspraakDto | null>()
  afspraakDialogMode = input<ColonClientAfspraakDialogMode | null>(null)
  sluiten = output<boolean>()

  private readonly notificationService = inject(NotificationService)
  private readonly contextNotificationService = inject(DsContextualNotificationService)
  private readonly afspraakService = inject(ColonIntakeafspraakService)
  private readonly clientService = inject(ClientService)
  private readonly changeDetectorRef = inject(ChangeDetectorRef)
  private wizard = viewChild(DsFullscreenModalWizardComponent)
  private bevestiging = viewChild(ColonClientAfspraakBevestigingComponent)

  protected readonly strings = inject(StringsService).getStrings()
  protected aanmakenRequest: WritableSignal<ColonAfspraakMakenRequestDto | null> = signal(null)
  protected readonly geselecteerdSlot = computed(() => this.aanmakenRequest()?.afspraakslot ?? null)
  protected readonly gekozenAfspraakType: WritableSignal<ColonIntakeafspraakAanmakenType> = signal(ColonIntakeafspraakAanmakenType.STANDAARD)

  titel = computed(() => {
    switch (this.afspraakDialogMode()) {
      case 'tijdstip_wijzigen':
        return this.strings.algemeen.afspraken.verplaatsenDialog.titelTijdstipWijzigen
      case 'locatie_wijzigen':
        return this.strings.algemeen.afspraken.verplaatsenDialog.titelLocatieWijzigen
      case 'aanmaken':
        return this.strings.algemeen.afspraken.verplaatsenDialog.titelAanmaken
      default:
        return ''
    }
  })

  steps = computed(() => ({
    AFSPRAAK_KIEZEN: {
      titel: this.strings.algemeen.afspraken.verplaatsenDialog.afspraakZoekenStep.titel,
      subtitel: this.geselecteerdSlot()
        ? `${this.geselecteerdSlot()!.intakelocatie.naam}, \n${format(this.geselecteerdSlot()!.startTijd, NL_DATE_FORMAT)} om ${format(this.geselecteerdSlot()!.startTijd, TIME_FORMAT)}`
        : '',
      nextAction: { name: this.strings.algemeen.buttons.volgende, disabled: !this.geselecteerdSlot() },
      cancelAction: { name: this.strings.algemeen.buttons.annuleren },
      stepControl: this.createStepControl(
        () => this.aanmakenRequest() != null,
        () => this.aanmakenRequest.set(null),
      ),
    },
    AFSPRAAK_BEVESTIGEN: {
      titel: this.strings.algemeen.afspraken.verplaatsenDialog.bevestigingStep.titel,
      subtitel: this.aanmakenRequest() && this.aanmakenRequest()!.briefType ? briefTypeLabels[this.aanmakenRequest()!.briefType!] : '',
      nextAction: { name: this.strings.algemeen.buttons.volgende, callback: () => this.gaNaarSamenvatting() },
      cancelAction: { name: this.strings.algemeen.buttons.annuleren },
      previousAction: { name: this.strings.algemeen.buttons.vorige },
      stepControl: this.createStepControl(
        () => this.bevestiging()?.form.valid ?? false,
        () => this.bevestiging()?.herstelStandaardwaarden(),
      ),
    },
    SAMENVATTING: {
      titel: this.strings.algemeen.afspraken.verplaatsenDialog.samenvattingStep.titel,
      subtitel: '',
      cancelAction: { name: this.strings.algemeen.buttons.annuleren },
      previousAction: { name: this.strings.algemeen.buttons.vorige },
      completeAction: { name: this.strings.algemeen.buttons.bevestigen, callback: () => this.completeWizard() },
    },
  }))

  ngAfterViewInit() {
    this.contextNotificationService.removeNotificationByContext('ColonIntakeAfspraak')
    const afspraak = this.afspraak()
    if (afspraak) {
      let message = this.strings.algemeen.afspraken.verplaatsenDialog.afspraakMessage(
        format(afspraak.vanaf, NL_DAY_DATE_FORMAT),
        format(afspraak.vanaf, TIME_FORMAT),
        afspraak.locatie.naam,
      )
      if (this.afspraakService.isDigitaleAfspraak(afspraak)) {
        message = this.strings.algemeen.afspraken.verplaatsenDialog.afspraakMessageDigitaal(formatDatumVoorDigitaleAfspraak(afspraak.vanaf), afspraak.locatie.naam)
      }
      this.contextNotificationService.info('ColonIntakeAfspraak', { message, closeable: false }, 'page')
    }
  }

  sluitWizard() {
    this.sluiten.emit(false)
  }

  private gaNaarSamenvatting() {
    if (this.bevestiging()?.form.valid !== true) {
      this.bevestiging()?.toonValidatiefouten()
      return
    }
    this.wizard()?.next()
  }

  selecteerSlot(keuze: ColonAfspraakSlotKeuze) {
    if (!this.zetAanmakenRequest(keuze)) {
      return
    }
    this.changeDetectorRef.detectChanges()
    this.wizard()?.next()
  }

  wijzigDatumTijdBuitenRooster(datumTijdBuitenRooster: string | null) {
    if (!datumTijdBuitenRooster) {
      this.aanmakenRequest.set(null)
      return
    }

    const request = this.aanmakenRequest()
    if (request) {
      this.aanmakenRequest.set({
        ...request,
        datumTijdBuitenRooster,
        afspraakslot: { ...request.afspraakslot, startTijd: parseISO(datumTijdBuitenRooster) },
      })
      return
    }

    const afspraak = this.afspraak()
    if (afspraak && this.afspraakDialogMode() === 'tijdstip_wijzigen') {
      const startTijd = parseISO(datumTijdBuitenRooster)
      this.zetAanmakenRequest({
        afspraakslot: {
          startTijd,
          eindTijd: startTijd,
          intakelocatie: { id: afspraak.locatie.id, naam: afspraak.locatie.naam, adres: afspraak.locatie.adres },
          plaats: afspraak.locatie.adres.plaats,
          afstand: 0,
          locatieBeschrijving: afspraak.locatieBeschrijving,
        },
        datumTijdBuitenRooster,
      })
    }
  }

  private zetAanmakenRequest(keuze: ColonAfspraakSlotKeuze): boolean {
    const afspraak = this.afspraak()
    if (!afspraak && this.afspraakDialogMode() !== 'aanmaken') {
      return false
    }

    this.aanmakenRequest.set({
      afspraakslot: keuze.afspraakslot,
      datumTijdBuitenRooster: keuze.datumTijdBuitenRooster,
      briefType: null,
      notitie: null,
      verwezenMedischeRedenenDoorInfolijn: null,
      clientId: this.clientService.clientId(),
      afspraakId: afspraak?.id,
    })
    return true
  }

  private completeWizard() {
    if (this.afspraakDialogMode() === 'aanmaken') {
      this.maakAfspraak()
    } else {
      this.verplaatsAfspraak()
    }
  }

  private maakAfspraak() {
    this.afspraakService
      .maakAfspraak(this.aanmakenRequest()!)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.afspraken.verplaatsenDialog.succesMessageAangemaakt)
        this.sluiten.emit(true)
      })
  }

  private verplaatsAfspraak() {
    this.afspraakService
      .verplaatsAfspraak(this.aanmakenRequest()!)
      .pipe(take(1))
      .subscribe(() => {
        let message = ''
        switch (this.afspraakDialogMode()) {
          case 'locatie_wijzigen':
            message = this.strings.algemeen.afspraken.verplaatsenDialog.succesMessageLocatieGewijzigd
            break
          case 'tijdstip_wijzigen':
            message = this.strings.algemeen.afspraken.verplaatsenDialog.succesMessageTijdstipGewijzigd
            break
          case 'aanmaken':
            message = this.strings.algemeen.afspraken.verplaatsenDialog.succesMessageAangemaakt
            break
        }

        this.notificationService.success(message)
        this.sluiten.emit(true)
      })
  }

  private createStepControl(isValid: () => boolean, reset: () => void): DsWizardStepControl {
    return {
      get valid() {
        return isValid()
      },
      reset,
    }
  }
}
