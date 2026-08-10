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
import { Component, effect, forwardRef, inject, Injector, input, model, runInInjectionContext, signal, WritableSignal } from '@angular/core'
import { WoonplaatsDto } from '../../models/WoonplaatsDto'
import { WoonplaatsenService } from '../../services/woonplaatsen/woonplaatsen.service'
import { DropdownValueModel, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { ControlValueAccessor, FormsModule, NG_VALUE_ACCESSOR } from '@angular/forms'
import { take } from 'rxjs'

@Component({
  selector: 'app-woonplaats-selector',
  imports: [DsDropdownComponent, FormsModule],
  template: `<div class="dropdown-wrapper">
    <ds-dropdown
      [disabled]="isDisabled"
      [items]="woonplaatsen()"
      [(ngModel)]="selectedWoonplaats"
      [label]="label()"
      (search)="onSearchChange($event)"
      [required]="required()"
      appendTo=".ds-modal"
    />
  </div>`,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => WoonplaatsSelectorComponent),
      multi: true,
    },
  ],
})
export class WoonplaatsSelectorComponent implements ControlValueAccessor {
  private woonplaatsenService = inject(WoonplaatsenService)
  private injector = inject(Injector)
  woonplaatsen: WritableSignal<DropdownValueModel<WoonplaatsDto>[]> = signal([])
  selectedWoonplaats = model<DropdownValueModel<WoonplaatsDto> | undefined>(undefined)
  required = input<boolean>(false)
  label = input<string>('Woonplaats')

  private onChange: ((value: WoonplaatsDto | undefined) => void) | undefined
  private onTouched: (() => void) | undefined
  isDisabled = false

  constructor() {
    effect(() => {
      if (this.selectedWoonplaats() && this.onChange) {
        this.onChange(this.selectedWoonplaats()!.value)
      }
    })
  }

  onSearchChange(event: { term: string; items: WoonplaatsDto[] }): void {
    this.woonplaatsenService
      .getWoonplaatsen(event.term)
      .pipe(take(1))
      .subscribe((woonplaatsen) => this.woonplaatsen.set(woonplaatsen.map((wp) => ({ value: wp!, label: `${wp.naam} (Gemeente ${wp.gemeente})` }))))
  }

  writeValue(woonplaats: WoonplaatsDto): void {
    if (!woonplaats) {
      return
    }

    runInInjectionContext(this.injector, () => {
      effect(() => {
        if (this.woonplaatsen().length > 0) {
          this.selectedWoonplaats.set(this.woonplaatsen().find((wp) => wp.value.huisartsportaalId === woonplaats.huisartsportaalId))
        }
      })
    })
    this.onSearchChange({ term: woonplaats.naam, items: [] })
  }

  registerOnChange(fn: (val: WoonplaatsDto | undefined) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled = isDisabled
  }
}
