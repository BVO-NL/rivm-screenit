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
import { ChangeDetectorRef, Component, forwardRef, inject, input, viewChild } from '@angular/core'
import { ControlValueAccessor, NG_VALUE_ACCESSOR } from '@angular/forms'
import { DsButtonComponent, DsButtonMenuDirective, DsContextMenuItem, DsIconComponent, DsMenuItem } from '@topicus-rgp-ds/web'
import { faAngleDown } from '@fortawesome/pro-light-svg-icons'
import { filter, fromEvent, take } from 'rxjs'

@Component({
  selector: 'app-select-button',
  imports: [DsButtonComponent, DsButtonMenuDirective, DsIconComponent],
  template: `<button
    ds-button-secondary
    dsButtonMenu
    [menu]="options()"
    (menuItemClicked)="menuItemClicked($event)"
    [disabled]="disabled"
    (menuOpened)="setCloseOnClick()"
    data-testid="calendar-view-select-button"
  >
    {{ selectedRange }}
    <ds-icon [icon]="faAngleDown"></ds-icon>
  </button>`,
  styles: [
    `
      .dropdown-toggle {
        justify-content: flex-start;
      }
    `,
  ],
  providers: [{ provide: NG_VALUE_ACCESSOR, useExisting: forwardRef(() => SelectButtonComponent), multi: true }],
})
export class SelectButtonComponent<CalendarView> implements ControlValueAccessor {
  options = input<DsContextMenuItem[][]>([])
  cdr = inject(ChangeDetectorRef)
  private buttonMenuDirective = viewChild(DsButtonMenuDirective)
  onChange: ((value: CalendarView) => void) | undefined
  onTouch: ((value: CalendarView) => void) | undefined
  disabled = false
  value: CalendarView | undefined

  protected readonly faAngleDown = faAngleDown

  get selectedRange(): string {
    return (
      this.options()
        .flat()
        .find((optie: DsContextMenuItem) => optie.label.toLowerCase() === this.value)?.label ?? 'Onbekend'
    )
  }

  registerOnChange(fn: (value: CalendarView) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: (value: CalendarView) => void): void {
    this.onTouch = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.disabled = isDisabled
  }

  menuItemClicked($event: DsMenuItem) {
    this.value = $event.label === 'Dag' ? ('dag' as CalendarView) : $event.label === 'Werkweek' ? ('werkweek' as CalendarView) : ('week' as CalendarView)
    if (this.onChange) {
      this.onChange(this.value)
    }
  }

  writeValue(obj: CalendarView): void {
    this.value = obj
  }

  setCloseOnClick(): void {
    fromEvent<MouseEvent>(document, 'mousedown')
      .pipe(
        filter((event: MouseEvent) => {
          const clickTarget = event.target as HTMLElement
          return !clickTarget.closest('.ds-context-menu') && !clickTarget.closest(`#${this.buttonMenuDirective()!.elementClass}`)
        }),
        take(1),
      )
      .subscribe(() => this.cdr.markForCheck())
  }
}
