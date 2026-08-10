/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { Component, inject, output } from '@angular/core'
import { VerrichtingenFilterEvent } from '../../../../models/VerrichtingenFilterEvent'
import { FormBuilder, ReactiveFormsModule } from '@angular/forms'
import { DsButtonComponent, DsDatepickerComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { StateService } from '../../../../services/state/state.service'
import { LocatieSelectorComponent } from '../../../../components/locatie-selector/locatie-selector.component'
import { LocatieDto } from '../../../../models/LocatieDto'

@Component({
  selector: 'app-verrichtingen-filter',
  imports: [ReactiveFormsModule, DsInputComponent, DsDatepickerComponent, DsButtonComponent, LocatieSelectorComponent],
  templateUrl: './verrichtingen-filter.component.html',
  styleUrl: './verrichtingen-filter.component.scss',
})
export class VerrichtingenFilterComponent {
  filterChanged = output<VerrichtingenFilterEvent>()
  formBuilder = inject(FormBuilder)
  stateService = inject(StateService)
  filterForm = this.formBuilder.group({
    clientNaam: '',
    datumUitstrijkje: this.formBuilder.control<Date | null>(null),
    locatie: this.formBuilder.control<LocatieDto | null>(null),
    verrichtingsdatumVanaf: this.formBuilder.control<Date | null>(null),
    verrichtingsdatumTotEnMet: this.formBuilder.control<Date | null>(null),
  })

  filteren() {
    if (this.filterForm.valid) {
      this.filterChanged.emit(this.filterForm.value as VerrichtingenFilterEvent)
    }
  }
}
