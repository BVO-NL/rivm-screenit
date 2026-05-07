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
import { ChangeDetectorRef, Component, effect, inject, input, model, output, viewChild } from '@angular/core'
import { FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms'
import { CalendarView } from '@/shared/types/calendar-view'
import { SelectButtonComponent } from '@shared/components/select-button/select-button.component'
import { SelectOption } from '@shared/types/select-option'
import { Dialog } from '@angular/cdk/dialog'
import { AfspraakslotBewerkenDialogComponent } from '@/colon/colon-rooster-page/components/afspraakslot-bewerken-dialog/afspraakslot-bewerken-dialog.component'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@/shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { Recht } from '@shared/types/autorisatie/recht'
import { BlokkadeBewerkenDialogComponent } from '@/colon/colon-rooster-page/components/blokkade-bewerken-dialog/blokkade-bewerken-dialog.component'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { RoosterLegendaComponent } from '@/colon/colon-rooster-page/components/rooster-legenda/rooster-legenda.component'
import { TijdslotsVerwijderenDialogComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-dialog/tijdslots-verwijderen-dialog.component'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { DsButtonComponent, DsButtonMenuDirective, DsContextMenuItem, DsDatepickerComponent, DsIconComponent, DsMenuItem } from '@topicus-rgp-ds/web'
import { faAdd, faAngleDown, faAngleLeft, faAngleRight, faMagnifyingGlassMinus, faMagnifyingGlassPlus, faTrashCan } from '@fortawesome/pro-light-svg-icons'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ColonTijdslotType } from '@shared/types/colon/colon-tijdslot-type'
import { isValid } from 'date-fns'
import { parseDate } from '@shared/utils/date-utils'
import { filter, fromEvent, take } from 'rxjs'

@Component({
  selector: 'app-rooster-controls',
  templateUrl: './rooster-controls.component.html',
  imports: [
    FormsModule,
    SelectButtonComponent,
    AutorisatieDirective,
    RoosterLegendaComponent,
    DsButtonComponent,
    DsDatepickerComponent,
    DsIconComponent,
    DsButtonMenuDirective,
    ReactiveFormsModule,
  ],
})
export class RoosterControlsComponent {
  viewRange = model<CalendarView>('werkweek')
  selectedDate = input<Date | undefined>()
  currentDate = input<Date | undefined>()

  viewRangeChange = output<CalendarView>()
  gotoNext = output<void>()
  gotoPrevious = output<void>()
  gotoToday = output<void>()
  selectedDateChange = output<Date>()
  zoomLevelChanged = output<number>()
  getCurrentDate = output<void>()
  datumCtrl: FormControl = new FormControl<Date | null>(null)
  zoomLevel = 30
  isNavLinkDayClick = false
  viewRanges: DsContextMenuItem[][] = [[new DsContextMenuItem({ label: 'Week' }), new DsContextMenuItem({ label: 'Werkweek' }), new DsContextMenuItem({ label: 'Dag' })]]
  tijdslotAanmakenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_LOCATIE_ROOSTER],
    actie: Actie.TOEVOEGEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.ORGANISATIE,
    organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
    required: Required.ANY,
  }
  tijdslotVerwijderenConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_LOCATIE_ROOSTER],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.ORGANISATIE,
    organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
    required: Required.ANY,
  }
  protected minDate: string = parseDate('2000-01-01').toISOString()
  protected maxDate: string = parseDate('2200-12-31').toISOString()

  protected readonly faAdd = faAdd
  protected readonly faTrashCan = faTrashCan
  protected readonly faAngleLeft = faAngleLeft
  protected readonly faAngleRight = faAngleRight
  protected readonly faAngleDown = faAngleDown
  protected readonly faMagnifyingGlassMinus = faMagnifyingGlassMinus
  protected readonly faMagnifyingGlassPlus = faMagnifyingGlassPlus
  protected readonly ColonTijdslotType = ColonTijdslotType
  private buttonMenuDirective = viewChild(DsButtonMenuDirective)
  private readonly cdr = inject(ChangeDetectorRef)

  protected verwijderMenu: DsContextMenuItem[][] = [
    [
      new DsContextMenuItem({
        label: ColonTijdslotType.AFSPRAAKSLOT + 's',
        isDeleteItem: true,
      }),
      new DsContextMenuItem({
        label: ColonTijdslotType.BLOKKADE + 's',
        isDeleteItem: true,
      }),
    ],
  ]
  private readonly dialog: Dialog = inject(Dialog)
  private zoomLevels = [5, 10, 15, 20, 30, 60, 120]

  constructor() {
    this.datumCtrl.valueChanges.pipe(takeUntilDestroyed()).subscribe((value) => {
      if (value && value instanceof Date) {
        this.selectedDateChange.emit(value)
      }
    })

    effect(() => {
      const value = this.selectedDate()
      if (value && isValid(value)) {
        const currentValue = this.datumCtrl.value
        if (!currentValue || currentValue.getTime() !== value.getTime()) {
          this.datumCtrl.setValue(value, { emitEvent: false })
        }
      } else if (!value) {
        this.datumCtrl.setValue(null, { emitEvent: false })
      }
    })
  }

  get isZoomOutDisabled(): boolean {
    const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
    return currentZoomIndex === this.zoomLevels.length - 1
  }

  get isZoomInDisabled(): boolean {
    const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
    return currentZoomIndex === 0
  }

  zoomIn(): void {
    if (!this.isZoomInDisabled) {
      const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
      const nextZoomIndex = currentZoomIndex - 1
      this.zoomLevel = this.zoomLevels[nextZoomIndex]
      this.zoomLevelChanged.emit(this.zoomLevel)
    }
  }

  zoomOut(): void {
    if (!this.isZoomOutDisabled) {
      const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
      const nextZoomIndex = currentZoomIndex + 1

      this.zoomLevel = this.zoomLevels[nextZoomIndex]
      this.zoomLevelChanged.emit(this.zoomLevel)
    }
  }

  openAfspraakslotAanmakenDialog() {
    this.dialog.open<ColonAfspraakslot>(AfspraakslotBewerkenDialogComponent)
  }

  openBulkTijdslotVerwijderenDialog(typeTijdslotSelectie: SelectOption<string>) {
    this.dialog.open<ColonTijdslot>(TijdslotsVerwijderenDialogComponent, { data: typeTijdslotSelectie })
  }

  openBlokkadeDialog() {
    this.dialog.open<ColonBlokkade>(BlokkadeBewerkenDialogComponent)
  }

  setViewRange(viewRange: CalendarView) {
    this.viewRange.set(viewRange)
    this.isNavLinkDayClick = false
    this.viewRangeChange.emit(viewRange)
  }

  verwijderItemClicked($event: DsMenuItem) {
    const option: SelectOption<string> = {
      label: $event.label,
      value: $event.label.toLowerCase(),
    }
    this.openBulkTijdslotVerwijderenDialog(option)
  }

  toevoegItemClicked(tijdslotType: ColonTijdslotType) {
    if (tijdslotType === ColonTijdslotType.AFSPRAAKSLOT) {
      this.openAfspraakslotAanmakenDialog()
    } else if (tijdslotType === ColonTijdslotType.BLOKKADE) {
      this.openBlokkadeDialog()
    }
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
