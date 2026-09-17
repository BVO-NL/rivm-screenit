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
import { HttpErrorResponse } from '@angular/common/http'
import { DsContextualNotificationService, DsFullscreenModalWizardComponent, DsWizardStepComponent } from '@topicus-rgp-ds/web'
import { ColonClientAfspraakZoekenComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken/colon-client-afspraak-zoeken.component'
import { format } from 'date-fns'
import { NL_DAY_DATE_FORMAT, NL_LONG_DATE_FORMAT, TIME_FORMAT } from '@shared/constants'
import { ClientAfspraakDto } from '@shared/types/algemeen/dto/client-afspraak.dto'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { ColonClientAfspraakBevestigingComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-bevestiging/colon-client-afspraak-bevestiging.component'
import { ColonAfspraakMakenRequestDto } from '@shared/types/colon/dto/colon-afspraak-maken-request.dto'
import { ColonClientAfspraakSamenvattingComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-samenvatting/colon-client-afspraak-samenvatting.component'
import { ColonIntakeafspraakService } from '@colon/services/colon-intakeafspraak/colon-intakeafspraak.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { take } from 'rxjs'
import { briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'

@Component({
  selector: 'app-colon-client-afspraak-verplaatsen',
  imports: [
    DsFullscreenModalWizardComponent,
    DsWizardStepComponent,
    ColonClientAfspraakZoekenComponent,
    ColonClientAfspraakBevestigingComponent,
    ColonClientAfspraakSamenvattingComponent,
  ],
  templateUrl: './colon-client-afspraak-verplaatsen-dialog.component.html',
  styleUrl: './colon-client-afspraak-verplaatsen-dialog.component.scss',
})
export class ColonClientAfspraakVerplaatsenDialogComponent implements AfterViewInit {
  afspraak = input<ClientAfspraakDto | null>()
  sluiten = output<boolean>()

  private readonly notificationService = inject(NotificationService)
  private readonly contextNotificationService = inject(DsContextualNotificationService)
  private readonly afspraakService = inject(ColonIntakeafspraakService)
  private readonly changeDetectorRef = inject(ChangeDetectorRef)

  private wizard = viewChild(DsFullscreenModalWizardComponent)
  protected geselecteerdSlot: WritableSignal<VrijSlotZonderKamerDto | null> = signal(null)
  protected aanmakenRequest: WritableSignal<ColonAfspraakMakenRequestDto | null> = signal(null)

  steps = computed(() => ({
    AFSPRAAK_KIEZEN: {
      titel: 'Kies een afspraak',
      subtitel: this.geselecteerdSlot()
        ? `${this.geselecteerdSlot()!.intakelocatie.naam}, ${format(this.geselecteerdSlot()!.startTijd, NL_LONG_DATE_FORMAT)} om ${format(this.geselecteerdSlot()!.startTijd, TIME_FORMAT)}`
        : '',
      nextAction: { name: 'Volgende', disabled: !this.geselecteerdSlot() },
      cancelAction: { name: 'Annuleren' },
      stepControl: {
        valid: this.geselecteerdSlot() != null,
        reset: () => this.geselecteerdSlot.set(null),
      },
    },
    AFSPRAAK_BEVESTIGEN: {
      titel: 'Afspraakbevestiging',
      subtitel: this.aanmakenRequest() && this.aanmakenRequest()!.briefType ? briefTypeLabels[this.aanmakenRequest()!.briefType!] : '',
      nextAction: { name: 'Volgende', disabled: this.aanmakenRequest() == null },
      cancelAction: { name: 'Annuleren' },
      previousAction: { name: 'Vorige' },
      stepControl: {
        valid: this.aanmakenRequest() != null,
        reset: () => this.aanmakenRequest.set(null),
      },
    },
    SAMENVATTING: {
      titel: 'Samenvatting',
      subtitel: '',
      cancelAction: { name: 'Annuleren' },
      previousAction: { name: 'Vorige' },
      completeAction: { name: 'Bevestigen', callback: () => this.verplaatsAfspraak() },
    },
  }))

  ngAfterViewInit() {
    this.contextNotificationService.removeNotificationByContext('ColonIntakeAfspraak')
    if (this.afspraak()) {
      this.contextNotificationService.info(
        'ColonIntakeAfspraak',
        {
          message: `Huidige afspraak staat gepland op ${format(this.afspraak()!.vanaf, NL_DAY_DATE_FORMAT)} om ${format(this.afspraak()!.vanaf, TIME_FORMAT)} uur op locatie ${this.afspraak()!.locatie.naam}`,
          closeable: false,
        },
        'page',
      )
    }
  }

  sluitWizard() {
    this.sluiten.emit(false)
  }

  selecteerSlot(slot: VrijSlotZonderKamerDto | null) {
    this.geselecteerdSlot.set(slot)
    this.changeDetectorRef.detectChanges()
    this.wizard()?.next()
  }

  private verplaatsAfspraak() {
    this.afspraakService
      .verplaatsAfspraak(this.aanmakenRequest()!)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificationService.success('Tijdstip is gewijzigd')
          this.sluiten.emit(true)
        },
        error: (err: HttpErrorResponse) => {
          this.notificationService.error(err.error?.message ?? err.error?.foutmelding ?? 'Er is een fout opgetreden bij het verplaatsen van de afspraak. Probeer het opnieuw.')
        },
      })
  }
}
