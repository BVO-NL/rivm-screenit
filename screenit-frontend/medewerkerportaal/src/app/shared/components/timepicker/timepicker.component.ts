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
import { Component, DestroyRef, ElementRef, forwardRef, inject, Injector, input, OnInit, signal, viewChild } from '@angular/core'
import { CallToActionDirective, DsButtonComponent, DsIconComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { AbstractControl, ControlValueAccessor, FormControl, NG_VALIDATORS, NG_VALUE_ACCESSOR, NgControl, ReactiveFormsModule, ValidationErrors, Validator } from '@angular/forms'
import { faClock } from '@fortawesome/pro-light-svg-icons'
import { formatCompleteTijdString, formatTijdString, isValideTijd, normaliseerTijdInvoer } from '@shared/utils/date-utils'
import { merge, of } from 'rxjs'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { CdkConnectedOverlay, CdkOverlayOrigin, ConnectedPosition } from '@angular/cdk/overlay'

const STANDAARD_FOUTMELDINGEN: Record<string, string> = {
  required: 'De tijd is verplicht',
  noValidTime: 'De ingevoerde tijd is niet valide',
}

@Component({
  selector: 'app-timepicker',
  imports: [ReactiveFormsModule, DsInputComponent, CallToActionDirective, CdkConnectedOverlay, CdkOverlayOrigin, DsButtonComponent, DsIconComponent],
  templateUrl: './timepicker.component.html',
  styleUrls: ['./timepicker.component.scss'],
  host: {
    '(document:keydown)': 'onToetsIndrukken($event)',
  },
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => TimepickerComponent),
      multi: true,
    },
    {
      provide: NG_VALIDATORS,
      useExisting: forwardRef(() => TimepickerComponent),
      multi: true,
    },
  ],
})
export class TimepickerComponent implements ControlValueAccessor, Validator, OnInit {
  label = input<string>('')
  inForm = input<boolean>(true)
  required = input<boolean>(false)

  foutmeldingen = signal<Record<string, string>>({ ...STANDAARD_FOUTMELDINGEN })

  tijdCtrl = new FormControl('', { nonNullable: true })
  toonKeuzelijst = signal(false)
  klokIcoon = faClock

  geselecteerdUur = signal<number | null>(null)
  geselecteerdMinuut = signal<number | null>(null)
  actieveKolom = signal<'uren' | 'minuten'>('uren')

  readonly uurOpties = Array.from({ length: 24 }, (_, i) => i)
  readonly minutenOpties = Array.from({ length: 12 }, (_, i) => i * 5)

  readonly overlayPosities: ConnectedPosition[] = [{ originX: 'start', originY: 'bottom', overlayX: 'start', overlayY: 'top', offsetY: 4 }]

  private uurKolomRef = viewChild<ElementRef<HTMLElement>>('uurKolom')
  private minutenKolomRef = viewChild<ElementRef<HTMLElement>>('minutenKolom')

  private readonly elementRef = inject(ElementRef<HTMLElement>)
  private readonly injector = inject(Injector)
  private readonly destroyRef = inject(DestroyRef)
  private ngControl: NgControl | null = null
  private interneSynchronisatie = false
  private onChange?: (value: string) => void
  private onTouched?: () => void

  constructor() {
    this.tijdCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((waarde) => {
      if (this.interneSynchronisatie) {
        return
      }

      this.onChange?.(waarde)
    })
  }

  ngOnInit(): void {
    this.ngControl = this.injector.get(NgControl, null, { self: true, optional: true })

    const buitensteControl = this.ngControl?.control
    if (buitensteControl) {
      merge(of(null), buitensteControl.statusChanges, buitensteControl.valueChanges)
        .pipe(takeUntilDestroyed(this.destroyRef))
        .subscribe(() => this.syncMetBuitensteControl())
    }
  }

  writeValue(value: string | null): void {
    this.synchroniseerIntern(() => {
      this.tijdCtrl.setValue(value ?? '', { emitEvent: false })
    })
    this.syncMetBuitensteControl()
  }

  registerOnChange(fn: (value: string) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.synchroniseerIntern(() => {
      if (isDisabled) {
        this.tijdCtrl.disable({ emitEvent: false })
        this.toonKeuzelijst.set(false)
      } else {
        this.tijdCtrl.enable({ emitEvent: false })
      }
    })
  }

  validate(control: AbstractControl): ValidationErrors | null {
    const waarde = control.value
    if (!waarde) {
      return null
    }

    return isValideTijd(String(waarde).trim()) ? null : { noValidTime: true }
  }

  openKeuzelijst(): void {
    if (!this.tijdCtrl.disabled && !this.toonKeuzelijst()) {
      this.initialiseerSelectie()
      this.actieveKolom.set('uren')
      this.toonKeuzelijst.set(true)
      setTimeout(() => this.scrollNaarGeselecteerdeTijd(), 0)
    }
  }

  wisselKeuzelijst(): void {
    if (this.tijdCtrl.disabled) {
      return
    }

    if (!this.toonKeuzelijst()) {
      this.initialiseerSelectie()
      this.actieveKolom.set('uren')
      this.toonKeuzelijst.set(true)
      setTimeout(() => this.scrollNaarGeselecteerdeTijd(), 0)
    } else {
      this.toonKeuzelijst.set(false)
    }

    this.focusOpInvoer()
  }

  sluitKeuzelijst(): void {
    this.toonKeuzelijst.set(false)
  }

  selecteerUur(event: MouseEvent, uur: number): void {
    event.preventDefault()
    this.geselecteerdUur.set(uur)
    this.actieveKolom.set('uren')
    if (this.geselecteerdMinuut() !== null) {
      this.bevestigTijdSelectie()
    }
  }

  selecteerMinuut(event: MouseEvent, minuut: number): void {
    event.preventDefault()
    this.geselecteerdMinuut.set(minuut)
    this.actieveKolom.set('minuten')
    if (this.geselecteerdUur() !== null) {
      this.bevestigTijdSelectie()
    }
  }

  formatTijd(n: number): string {
    return formatTijdString(String(n))
  }

  voorkomFocusVerlies(event: MouseEvent): void {
    event.preventDefault()
  }

  onToetsIndrukken(event: KeyboardEvent): void {
    if (!this.toonKeuzelijst()) {
      return
    }

    switch (event.key) {
      case 'ArrowDown':
        event.preventDefault()
        this.navigeerOptie(1)
        break
      case 'ArrowUp':
        event.preventDefault()
        this.navigeerOptie(-1)
        break
      case 'ArrowRight':
        event.preventDefault()
        this.actieveKolom.set('minuten')
        break
      case 'ArrowLeft':
        event.preventDefault()
        this.actieveKolom.set('uren')
        break
      case 'Enter':
        event.preventDefault()
        if (this.geselecteerdUur() !== null && this.geselecteerdMinuut() !== null) {
          this.bevestigTijdSelectie()
        }
        this.toonKeuzelijst.set(false)
        this.focusOpInvoer()
        break
      case 'Escape':
        event.preventDefault()
        this.toonKeuzelijst.set(false)
        this.focusOpInvoer()
        break
      case 'Tab':
        this.toonKeuzelijst.set(false)
        break
    }
  }

  onFocusUit(event: FocusEvent): void {
    const volgendElement = event.relatedTarget as Node | null
    if (volgendElement && this.elementRef.nativeElement.contains(volgendElement)) {
      return
    }

    const genormaliseerdeWaarde = normaliseerTijdInvoer(this.tijdCtrl.value)
    if (genormaliseerdeWaarde !== this.tijdCtrl.value) {
      this.tijdCtrl.setValue(genormaliseerdeWaarde)
    }

    this.toonKeuzelijst.set(false)
    this.onTouched?.()
    this.syncMetBuitensteControl()
  }

  private bevestigTijdSelectie(): void {
    const uur = this.geselecteerdUur()!
    const minuut = this.geselecteerdMinuut()!
    const tijdString = formatCompleteTijdString(uur, minuut)
    this.tijdCtrl.markAsDirty()
    this.tijdCtrl.setValue(tijdString)
    this.onTouched?.()
    this.syncMetBuitensteControl()
  }

  private navigeerOptie(richting: number): void {
    if (this.actieveKolom() === 'uren') {
      const huidigUur = this.geselecteerdUur() ?? 0
      const nieuwUur = (huidigUur + richting + this.uurOpties.length) % this.uurOpties.length
      this.geselecteerdUur.set(nieuwUur)
    } else {
      const huidigeIndex = this.geselecteerdMinuut() !== null ? this.minutenOpties.indexOf(this.geselecteerdMinuut()!) : 0
      const normIndex = huidigeIndex < 0 ? 0 : huidigeIndex
      const nieuweIndex = (normIndex + richting + this.minutenOpties.length) % this.minutenOpties.length
      this.geselecteerdMinuut.set(this.minutenOpties[nieuweIndex])
    }
    setTimeout(() => this.scrollNaarGeselecteerdeTijd(), 0)
  }

  private initialiseerSelectie(): void {
    const waarde = normaliseerTijdInvoer(this.tijdCtrl.value)
    if (isValideTijd(waarde)) {
      const [uurStr, minuutStr] = waarde.split(':')
      const minuut = parseInt(minuutStr, 10)
      this.geselecteerdUur.set(parseInt(uurStr, 10))
      this.geselecteerdMinuut.set(this.minutenOpties.includes(minuut) ? minuut : null)
    } else {
      const nu = new Date()
      this.geselecteerdUur.set(nu.getHours())
      this.geselecteerdMinuut.set((Math.round(nu.getMinutes() / 5) * 5) % 60)
    }
  }

  private scrollNaarGeselecteerdeTijd(): void {
    const uurKolom = this.uurKolomRef()?.nativeElement ?? null
    const minutenKolom = this.minutenKolomRef()?.nativeElement ?? null

    const scrollUurIndex = this.geselecteerdUur() ?? new Date().getHours()
    const minuutIndex = this.geselecteerdMinuut() !== null ? this.minutenOpties.indexOf(this.geselecteerdMinuut()!) : -1
    const scrollMinuutIndex = minuutIndex >= 0 ? minuutIndex : Math.floor(new Date().getMinutes() / 5)

    this.scrollNaarOptie(uurKolom, scrollUurIndex)
    this.scrollNaarOptie(minutenKolom, scrollMinuutIndex)
  }

  private scrollNaarOptie(container: HTMLElement | null, index: number): void {
    if (!container) {
      return
    }
    const knoppen = container.querySelectorAll<HTMLElement>('button')
    if (index < 0 || index >= knoppen.length) {
      return
    }
    const knop = knoppen[index]
    container.scrollTop = knop.offsetTop - container.clientHeight / 2 + knop.offsetHeight / 2
  }

  private bepaalFoutmeldingen(buitensteControl: AbstractControl): Record<string, string> {
    if (!buitensteControl.errors) {
      return STANDAARD_FOUTMELDINGEN
    }

    const extraFoutmeldingen: Record<string, string> = {}
    for (const [sleutel, waarde] of Object.entries(buitensteControl.errors)) {
      if (typeof waarde === 'string' && !(sleutel in STANDAARD_FOUTMELDINGEN)) {
        extraFoutmeldingen[sleutel] = waarde
      }
    }

    return { ...STANDAARD_FOUTMELDINGEN, ...extraFoutmeldingen }
  }

  private syncMetBuitensteControl(): void {
    const buitensteControl = this.ngControl?.control
    if (!buitensteControl) {
      return
    }

    this.foutmeldingen.set(this.bepaalFoutmeldingen(buitensteControl))

    this.synchroniseerIntern(() => {
      this.tijdCtrl.setErrors(buitensteControl.errors, { emitEvent: false })

      if (buitensteControl.dirty) {
        this.tijdCtrl.markAsDirty({ onlySelf: true })
      } else {
        this.tijdCtrl.markAsPristine({ onlySelf: true })
      }

      if (buitensteControl.touched) {
        this.tijdCtrl.markAsTouched({ onlySelf: true })
      } else {
        this.tijdCtrl.markAsUntouched({ onlySelf: true })
      }
    })
  }

  private synchroniseerIntern(actie: () => void): void {
    this.interneSynchronisatie = true
    try {
      actie()
    } finally {
      this.interneSynchronisatie = false
    }
  }

  private focusOpInvoer(): void {
    const inputElement = this.elementRef.nativeElement.querySelector('input')
    inputElement?.focus()
  }
}
