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
import { Component, computed, inject, input, model, output, signal, WritableSignal } from '@angular/core'
import { FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonClientAfspraakZoekenFilterComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken-filter/colon-client-afspraak-zoeken-filter.component'
import { ColonClientAfspraakZoekenTabelComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken-tabel/colon-client-afspraak-zoeken-tabel.component'
import { DsContextualNotificationsComponent, DsDatetimepickerComponent, provideDsIsoStringDateAdapter } from '@topicus-rgp-ds/web'
import { VrijSlotZonderKamerFilter } from '@shared/types/colon/dto/vrij-slot-zonder-kamer-filter'
import { ColonAfspraakZoekopdracht } from '@shared/types/colon/colon-afspraak-zoekopdracht'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'
import { ColonIntakeafspraakAanmakenType } from '@shared/types/colon/enum/colon-intakeafspraak-aanmaken-type'
import { ColonAfspraakSlotKeuze } from '@shared/types/colon/colon-afspraak-slot-keuze'
import { parseISO } from 'date-fns'
import { ColonClientAfspraakDialogMode } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-dialog/colon-client-afspraak-dialog.component'
import { StringsService } from '@shared/strings'

@Component({
  selector: 'app-colon-client-afspraak-zoeken',
  imports: [ReactiveFormsModule, ColonClientAfspraakZoekenFilterComponent, ColonClientAfspraakZoekenTabelComponent, DsContextualNotificationsComponent, DsDatetimepickerComponent],
  templateUrl: './colon-client-afspraak-zoeken.component.html',
  styles: `
    .datetimepicker {
      display: block;
      width: clamp(275px, 33vw, 448px);
      max-width: 100%;
    }
  `,
  providers: [provideDsIsoStringDateAdapter()],
})
export class ColonClientAfspraakZoekenComponent {
  afspraakType = model<ColonIntakeafspraakAanmakenType>(ColonIntakeafspraakAanmakenType.STANDAARD)
  huidigeIntakelocatieId = input<number | null>(null)
  afspraakDialogMode = input<ColonClientAfspraakDialogMode>(null)
  toonAfstand = computed(() => this.afspraakDialogMode() === 'locatie_wijzigen')
  slotGekozen = output<ColonAfspraakSlotKeuze>()
  datumTijdBuitenRoosterChange = output<string | null>()

  protected readonly toonDatumTijd = computed(() => this.afspraakType() !== ColonIntakeafspraakAanmakenType.BUITEN)

  protected readonly isTijdstipWijzigenBuitenGeplandeSlots = computed(
    () => this.afspraakDialogMode() === 'tijdstip_wijzigen' && this.afspraakType() === ColonIntakeafspraakAanmakenType.BUITEN,
  )

  protected readonly afspraakslots: WritableSignal<VrijSlotZonderKamerDto[]> = signal([])
  protected query: WritableSignal<VrijSlotZonderKamerFilter | null> = signal(null)
  protected gezocht: WritableSignal<boolean> = signal(false)
  protected readonly datumTijdBuitenRoosterCtrl = new FormControl<string | null>(null, Validators.required)
  private datumTijdBuitenRooster: string | null = null
  protected readonly strings = inject(StringsService).getStrings()

  constructor() {
    this.datumTijdBuitenRoosterCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((datumTijd) => {
      this.datumTijdBuitenRoosterChange.emit(this.datumTijdBuitenRoosterCtrl.valid ? datumTijd : null)
    })
  }

  zoeken(zoekopdracht: ColonAfspraakZoekopdracht) {
    this.gezocht.set(true)
    this.query.set(zoekopdracht.filter)
    this.datumTijdBuitenRooster = zoekopdracht.datumTijdBuitenRooster
  }

  selecteer(slot: VrijSlotZonderKamerDto) {
    const datumTijdBuitenRooster = this.datumTijdBuitenRooster
    this.slotGekozen.emit({
      afspraakslot: datumTijdBuitenRooster ? { ...slot, startTijd: parseISO(datumTijdBuitenRooster) } : slot,
      datumTijdBuitenRooster,
    })
  }

  wijzigAfspraakType(type: ColonIntakeafspraakAanmakenType) {
    this.afspraakType.set(type)
    this.gezocht.set(false)
    if (!this.isTijdstipWijzigenBuitenGeplandeSlots()) {
      this.datumTijdBuitenRoosterChange.emit(null)
    }
  }

  valideerDatumTijdBuitenRooster() {
    this.datumTijdBuitenRoosterCtrl.markAsTouched()
    return this.datumTijdBuitenRoosterCtrl.valid
  }
}
