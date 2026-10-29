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
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { FormControl, ReactiveFormsModule, ValidationErrors } from '@angular/forms'
import { DsDatetimepickerComponent } from '@topicus-rgp-ds/web'
import { provideScreenitIsoStringDateAdapter } from '@shared/adapters/screenit-iso-string-date-adapter'

@Component({
  selector: 'app-datum-tijd-picker',
  imports: [ReactiveFormsModule, DsDatetimepickerComponent],
  providers: [provideScreenitIsoStringDateAdapter()],
  changeDetection: ChangeDetectionStrategy.OnPush,
  template: `
    <ds-datetimepicker
      [formControl]="control()"
      [label]="label()"
      [datePlaceholder]="datePlaceholder()"
      [timePlaceholder]="timePlaceholder()"
      [errorMessage]="errorMessage()"
      [maxVisibleErrors]="maxVisibleErrors()"
      [required]="required()"
    />
  `,
  styles: `
    :host ::ng-deep .datepicker {
      flex: 1 1 auto;
    }

    :host ::ng-deep .timepicker {
      flex: 0 0 6.5rem;
    }
  `,
})
export class DatumTijdPickerComponent {
  control = input.required<FormControl<string | null>>()
  label = input<string>('')
  datePlaceholder = input<string>('')
  timePlaceholder = input<string>('')
  required = input<boolean>(false)
  errorMessage = input<ValidationErrors>({})
  maxVisibleErrors = input<number>(1)
}
