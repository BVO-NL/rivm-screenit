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
import { BezwaarService } from '@/algemeen/services/bezwaar/bezwaar.service'
import { BezwaarGroupViewWrapper } from '@/shared/types/algemeen/bezwaar-group-view-wrapper'
import { BezwaarViewWrapper } from '@/shared/types/algemeen/bezwaar-view-wrapper'
import { BezwaarMomentDto } from '@/shared/types/algemeen/dto/bezwaar-moment.dto'
import { BezwaarKeuze } from '@/shared/types/algemeen/dto/bezwaar-verwerken.dto'
import { Bevolkingsonderzoek, bevolkingsonderzoekLabels } from '@/shared/types/bevolkingsonderzoek'
import { EmptyStatePanelComponent } from '@shared/components/empty-state-panel/empty-state-panel.component'
import { NgClass } from '@angular/common'
import { Component, computed, forwardRef, inject, input, linkedSignal, signal } from '@angular/core'
import { ControlValueAccessor, FormsModule, NG_VALUE_ACCESSOR } from '@angular/forms'
import { faFile } from '@fortawesome/pro-solid-svg-icons'
import { DsCheckboxComponent } from '@topicus-rgp-ds/web'
import { StringsService } from '@shared/strings'
import { bevatBezwaar, isZelfdeBezwaar } from '@shared/utils/bezwaar-utils'

const GROEP_VOLGORDE: Record<string, number> = {
  ALGEMEEN: 0,
  [Bevolkingsonderzoek.CERVIX]: 1,
  [Bevolkingsonderzoek.MAMMA]: 2,
}

@Component({
  selector: 'app-bezwaar-lijst',
  imports: [NgClass, EmptyStatePanelComponent, DsCheckboxComponent, FormsModule],
  templateUrl: './bezwaar-lijst.component.html',
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => BezwaarLijstComponent),
      multi: true,
    },
  ],
  styles: `
    .groep-label {
      font-weight: 600;
      color: var(--ds-secondary-text-color);
    }

    ul {
      list-style-type: disc;
    }
  `,
})
export class BezwaarLijstComponent implements ControlValueAccessor {
  private readonly bezwaarService = inject(BezwaarService)
  protected readonly faFile = faFile
  protected readonly strings = inject(StringsService).getStrings()

  horizontaal = input<boolean>(false)
  bezwaarMoment = input<BezwaarMomentDto | undefined>()
  subtekst = input<string>('')
  bewerkbaar = input<boolean>(false)
  bezwaarOpties = input<BezwaarKeuze[]>([])

  bezwaarGroupViewWrappers = computed(() => {
    const groepen = this.bewerkbaar() ? this.bezwaarService.groepeerBezwaren(this.bezwaarOpties()) : this.bezwaarService.getBezwaarGroupViewWrappers(this.bezwaarMoment(), true)
    return [...groepen].sort(
      (a: BezwaarGroupViewWrapper, b: BezwaarGroupViewWrapper) => (GROEP_VOLGORDE[a.key] ?? Number.MAX_SAFE_INTEGER) - (GROEP_VOLGORDE[b.key] ?? Number.MAX_SAFE_INTEGER),
    )
  })

  private readonly geselecteerdeBezwaren = linkedSignal<BezwaarKeuze[]>(() => this.bezwaarMoment()?.bezwaren ?? [])
  protected readonly isDisabled = signal(false)
  private onChange: ((bezwaren: BezwaarKeuze[]) => void) | undefined
  private onTouched: (() => void) | undefined

  protected getGroepLabel(groep: { key: string; bevolkingsonderzoek?: Bevolkingsonderzoek }) {
    if (!groep.bevolkingsonderzoek) {
      return this.strings.algemeen.bezwaar.bezwaarLijst.groep.algemeen
    }

    return bevolkingsonderzoekLabels[groep.bevolkingsonderzoek] ?? groep.key
  }

  protected isGeselecteerd(bezwaar: BezwaarViewWrapper) {
    return bevatBezwaar(this.geselecteerdeBezwaren(), bezwaar)
  }

  protected wisselBezwaar(bezwaar: BezwaarViewWrapper, geselecteerd: boolean) {
    const zonderBezwaar = this.geselecteerdeBezwaren().filter((huidig) => !isZelfdeBezwaar(huidig, bezwaar))
    const bezwaarKeuze: BezwaarKeuze = { type: bezwaar.type, bevolkingsonderzoek: bezwaar.bevolkingsonderzoek }
    const nieuweSelectie = geselecteerd ? [...zonderBezwaar, bezwaarKeuze] : zonderBezwaar
    this.geselecteerdeBezwaren.set(nieuweSelectie)
    this.onChange?.(nieuweSelectie)
    this.onTouched?.()
  }

  writeValue(bezwaren: BezwaarKeuze[] | null): void {
    if (bezwaren) {
      this.geselecteerdeBezwaren.set(bezwaren)
    }
  }

  registerOnChange(fn: (bezwaren: BezwaarKeuze[]) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    this.isDisabled.set(isDisabled)
  }
}
