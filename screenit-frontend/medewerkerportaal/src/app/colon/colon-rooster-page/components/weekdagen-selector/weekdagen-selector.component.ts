/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Component, forwardRef, inject, Input } from '@angular/core'

import { WEEK_DAGEN } from '@shared/constants'
import { AbbreviatePipe } from '@shared/pipes/abbreviate/abbreviate.pipe'
import { ClrCheckboxModule, ClrComboboxModule, ClrCommonFormsModule } from '@clr/angular'
import { ControlValueAccessor, FormArray, FormBuilder, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule } from '@angular/forms'
import { filter, identity } from 'rxjs'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { SelectOption } from '@shared/types/select-option'

@Component({
  selector: 'app-weekdagen-selector',
  imports: [AbbreviatePipe, ClrCheckboxModule, ClrCommonFormsModule, ReactiveFormsModule, ClrComboboxModule],
  styles: [
    `
      :host {
        display: block;
      }
    `,
  ],
  templateUrl: './weekdagen-selector.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => WeekdagenSelectorComponent),
      multi: true,
    },
  ],
})
export class WeekdagenSelectorComponent implements ControlValueAccessor {
  private onChange: ((value: number[]) => void) | undefined
  private onTouched: (() => void) | undefined
  private formBuilder: FormBuilder = inject(FormBuilder)

  @Input() metLabel: boolean = false
  @Input() type: 'select' | 'checkbox' = 'checkbox'

  protected readonly dagen = WEEK_DAGEN

  weekdagenForm = this.formBuilder.group({
    dagenSelect: [],
    dagenCheckbox: this.formBuilder.array(this.maakDagControls()),
  })
  alleDagen = { label: 'Alle dagen', value: 0 }

  get dagenCheckboxCtrl(): FormArray {
    return this.weekdagenForm.get('dagenCheckbox') as FormArray
  }

  get dagenSelectCtrl(): FormControl {
    return this.weekdagenForm.get('dagenSelect') as FormControl
  }

  get heeftDagGeselecteerd(): boolean {
    return this.type === 'checkbox' ? this.heeftCheckboxGeselecteerd : this.heeftSelectGeselecteerd
  }

  get heeftSelectGeselecteerd() {
    return this.dagenSelectCtrl.dirty ? this.dagenCheckboxCtrl.value : true
  }

  get heeftCheckboxGeselecteerd(): boolean {
    return this.dagenCheckboxCtrl.dirty ? this.dagenCheckboxCtrl.value.some(identity) : true
  }

  constructor() {
    this.dagenCheckboxCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.onChange != undefined),
      )
      .subscribe((value) => {
        const dagen = value != null ? this.dagen.filter((_, index) => value[index]).map((option) => option.value) : []
        this.onChange!(dagen)
      })

    this.dagenSelectCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.onChange != undefined),
      )
      .subscribe((value) => {
        let dagen = value ?? []
        if (dagen.includes(this.alleDagen)) {
          dagen = [...this.dagen]
          this.dagenSelectCtrl.setValue(dagen)
        }
        this.onChange!(dagen.map((dag: SelectOption<number>) => dag.value))
      })
  }

  private maakDagControls() {
    return this.dagen.map(() => this.formBuilder.control(false))
  }

  writeValue(value: number[]): void {
    if (value) {
      if (this.type === 'select') {
        this.writeValueSelect(value)
      } else {
        this.writeValueCheckbox(value)
      }
    }
  }

  private writeValueSelect(value: number[]) {
    const weekDagen = value[0] === this.alleDagen.value ? [...this.dagen] : this.dagen.filter((dag) => value.includes(dag.value))
    this.dagenSelectCtrl.setValue(weekDagen, { emitEvent: false })
  }

  private writeValueCheckbox(value: number[]) {
    const weekDagen = value[0] === this.alleDagen.value ? [...this.dagen] : this.dagen.map((dag) => value.includes(dag.value))
    this.dagenCheckboxCtrl.setValue(weekDagen, { emitEvent: false })
  }

  registerOnChange(fn: (value: number[]) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    if (isDisabled) {
      this.dagenSelectCtrl.disable()
      this.dagenCheckboxCtrl.disable()
    } else {
      this.dagenSelectCtrl.enable()
      this.dagenCheckboxCtrl.enable()
    }
  }
}
