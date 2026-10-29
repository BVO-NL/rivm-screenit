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
import { formatDate } from '@angular/common'
import { Component, forwardRef, inject, input, LOCALE_ID, OnDestroy, signal } from '@angular/core'
import { takeUntilDestroyed, toObservable, toSignal } from '@angular/core/rxjs-interop'
import { switchMap } from 'rxjs'
import { ControlValueAccessor, FormControl, FormGroup, NG_VALIDATORS, NG_VALUE_ACCESSOR, ReactiveFormsModule, ValidationErrors, Validator, Validators } from '@angular/forms'
import { DsContextualNotificationService, DsDropdownComponent, DsInputComponent, MaxLengthDirective } from '@topicus-rgp-ds/web'
import { Doelgroep, DoelgroepKiesbareOpties, doelgroepLabel } from '@shared/types/algemeen/enum/doelgroep'
import { enumNaarOpties } from '@shared/types/enum-optie'
import { MAX_KARAKTERS, NL_DATE_FORMAT } from '@shared/constants'
import { StringsService } from '@shared/strings'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'

export interface DoelgroepKeuze {
  doelgroep: Doelgroep | null
  dubbelTijdReden: string
}

@Component({
  selector: 'app-doelgroep-selector',
  imports: [ReactiveFormsModule, DsDropdownComponent, DsInputComponent, MaxLengthDirective],
  templateUrl: './doelgroep-selector.component.html',
  styleUrl: './doelgroep-selector.component.scss',
  providers: [
    { provide: NG_VALUE_ACCESSOR, useExisting: forwardRef(() => DoelgroepSelectorComponent), multi: true },
    { provide: NG_VALIDATORS, useExisting: forwardRef(() => DoelgroepSelectorComponent), multi: true },
  ],
})
export class DoelgroepSelectorComponent implements ControlValueAccessor, Validator, OnDestroy {
  static readonly CONTEXT = 'doelgroep-wijzigen'

  readonly clientId = input.required<number>()
  readonly appendTo = input('.ds-modal')
  readonly mammaAfspraakDatumTijd = input<Date | undefined>()

  private readonly locale = inject(LOCALE_ID)
  private readonly contextualNotificationService = inject(DsContextualNotificationService)
  private readonly contactActiesService = inject(ContactActiesService)

  protected readonly magDoelgroepWijzigen = toSignal(
    toObservable(this.clientId).pipe(switchMap((clientId) => this.contactActiesService.heeftBeschikbareActie(clientId, ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN))),
    { initialValue: false },
  )

  protected readonly strings = inject(StringsService).getStrings()
  protected readonly MAX_KARAKTERS = MAX_KARAKTERS
  protected readonly doelgroepOpties = enumNaarOpties(
    Object.values(DoelgroepKiesbareOpties).reduce(
      (labels, waarde) => {
        labels[waarde] = doelgroepLabel[waarde]
        return labels
      },
      {} as Record<DoelgroepKiesbareOpties, string>,
    ),
  )
  protected readonly dubbeleTijdGekozen = signal(false)

  protected readonly form = new FormGroup({
    doelgroep: new FormControl<Doelgroep | null>(Doelgroep.REGULIER),
    dubbelTijdReden: new FormControl<string>('', { nonNullable: true, validators: [Validators.maxLength(MAX_KARAKTERS.dubbeleTijdReden)] }),
  })

  private oorspronkelijkeDoelgroep: Doelgroep | null | undefined

  private onChange: ((waarde: DoelgroepKeuze) => void) | null = null
  private onTouched: (() => void) | null = null

  constructor() {
    this.form.valueChanges.pipe(takeUntilDestroyed()).subscribe(() => {
      this.updateRedenValidatie()
      this.updateDoelgroepWijzigenMelding()
      this.onChange?.(this.form.getRawValue())
      this.onTouched?.()
    })
  }

  writeValue(waarde: DoelgroepKeuze | null): void {
    this.form.reset({ doelgroep: waarde?.doelgroep ?? Doelgroep.REGULIER, dubbelTijdReden: waarde?.dubbelTijdReden ?? '' }, { emitEvent: false })
    this.oorspronkelijkeDoelgroep ??= this.form.get('doelgroep')?.value
    this.updateRedenValidatie()
  }

  registerOnChange(fn: (waarde: DoelgroepKeuze) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    if (isDisabled) {
      this.form.disable({ emitEvent: false })
    } else {
      this.form.enable({ emitEvent: false })
    }
  }

  validate(): ValidationErrors | null {
    return this.form.valid ? null : { doelgroepKeuze: this.form.get('dubbelTijdReden')?.errors ?? true }
  }

  ngOnDestroy(): void {
    this.contextualNotificationService.removeNotificationByContext(DoelgroepSelectorComponent.CONTEXT)
  }

  private updateRedenValidatie() {
    const dubbeleTijd = this.form.get('doelgroep')?.value === Doelgroep.DUBBELE_TIJD
    this.dubbeleTijdGekozen.set(dubbeleTijd)

    const redenCtrl = this.form.get('dubbelTijdReden')
    redenCtrl?.setValidators(dubbeleTijd ? [Validators.required, Validators.maxLength(MAX_KARAKTERS.dubbeleTijdReden)] : [Validators.maxLength(MAX_KARAKTERS.dubbeleTijdReden)])
    redenCtrl?.updateValueAndValidity({ emitEvent: false })
  }

  private updateDoelgroepWijzigenMelding() {
    this.contextualNotificationService.removeNotificationByContext(DoelgroepSelectorComponent.CONTEXT)

    const doelgroepGewijzigd = this.form.get('doelgroep')?.value !== this.oorspronkelijkeDoelgroep
    const mammaAfspraakDatumTijd = this.mammaAfspraakDatumTijd()
    if (doelgroepGewijzigd && mammaAfspraakDatumTijd) {
      this.contextualNotificationService.warning(
        DoelgroepSelectorComponent.CONTEXT,
        { message: this.strings.algemeen.clientInfoBewerken.doelgroepWijzigenMelding.body(formatDate(mammaAfspraakDatumTijd, NL_DATE_FORMAT, this.locale)), closeable: false },
        'page',
      )
    }
  }
}
