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
import { Component, model, signal, WritableSignal } from '@angular/core'
import { ColonClientAfspraakZoekenFilterComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken-filter/colon-client-afspraak-zoeken-filter.component'
import { ColonClientAfspraakZoekenTabelComponent } from '@algemeen/clientdossier/afspraken-tab/components/colon-client-afspraak-zoeken-tabel/colon-client-afspraak-zoeken-tabel.component'
import { DsContextualNotificationsComponent } from '@topicus-rgp-ds/web'
import { VrijSlotZonderKamerFilter } from '@shared/types/colon/dto/vrij-slot-zonder-kamer-filter'
import { VrijSlotZonderKamerDto } from '@shared/types/colon/dto/vrij-slot-zonder-kamer.dto'

@Component({
  selector: 'app-colon-client-afspraak-zoeken',
  imports: [ColonClientAfspraakZoekenFilterComponent, ColonClientAfspraakZoekenTabelComponent, DsContextualNotificationsComponent],
  templateUrl: './colon-client-afspraak-zoeken.component.html',
  styleUrl: './colon-client-afspraak-zoeken.component.scss',
})
export class ColonClientAfspraakZoekenComponent {
  geselecteerdSlot = model<VrijSlotZonderKamerDto | null>(null)
  protected readonly afspraakslots: WritableSignal<VrijSlotZonderKamerDto[]> = signal([])
  protected query: WritableSignal<VrijSlotZonderKamerFilter | null> = signal(null)
  protected gezocht: WritableSignal<boolean> = signal(false)

  zoeken(query: VrijSlotZonderKamerFilter) {
    this.gezocht.set(true)
    this.query.set(query)
  }
}
