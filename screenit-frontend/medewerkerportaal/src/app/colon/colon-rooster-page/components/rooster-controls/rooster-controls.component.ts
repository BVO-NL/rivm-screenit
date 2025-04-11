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
import { Component, CUSTOM_ELEMENTS_SCHEMA, EventEmitter, inject, Input, LOCALE_ID, Output } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { CalendarView } from '@/shared/types/calendar-view'
import { ClrButtonGroupModule, ClrDatepickerModule, ClrDropdownModule, ClrIconModule } from '@clr/angular'
import { SelectButtonComponent } from '@shared/components/select-button/select-button.component'
import { SelectOption } from '@shared/types/select-option'
import { Dialog } from '@angular/cdk/dialog'
import { AfspraakslotEditDialogComponent } from '@/colon/colon-rooster-page/components/afspraakslot-edit-dialog/afspraakslot-edit-dialog.component'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/autorisatie/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { Recht } from '@shared/types/autorisatie/recht'
import { BlokkadeEditDialogComponent } from '@/colon/colon-rooster-page/components/blokkade-edit-dialog/blokkade-edit-dialog.component'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { RoosterLegendaComponent } from '@/colon/colon-rooster-page/components/rooster-legenda/rooster-legenda.component'
import { TijdslotsVerwijderenDialogComponent } from '@/colon/colon-rooster-page/components/tijdslots-verwijderen-dialog/tijdslots-verwijderen-dialog.component'
import { NgForOf } from '@angular/common'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { format, isValid } from 'date-fns'
import { parseDate } from '@shared/date-utils'
import { NL_DATE_FORMAT } from '@shared/constants'

@Component({
  selector: 'app-rooster-controls',
  templateUrl: './rooster-controls.component.html',
  imports: [
    FormsModule,
    ClrDatepickerModule,
    ClrButtonGroupModule,
    SelectButtonComponent,
    AutorisatieDirective,
    RoosterLegendaComponent,
    ClrDropdownModule,
    ClrIconModule,
    NgForOf,
  ],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
  providers: [{ provide: LOCALE_ID, useValue: 'nl' }],
  styles: [
    `
      .btn-group .btn[disabled] + .btn {
        border-left: 1px solid;
      }

      .dropdown .btn {
        border-right-width: 0;
      }
    `,
  ],
})
export class RoosterControlsComponent {
  @Input() viewRange: CalendarView = 'workWeek'
  @Input() timeRange = 'morning'
  @Input() selectedDate: Date | undefined
  @Input() currentDate: Date | undefined

  @Output() viewRangeChange: EventEmitter<CalendarView> = new EventEmitter<CalendarView>()
  @Output() timeRangeChanged: EventEmitter<string> = new EventEmitter<string>()
  @Output() gotoNext: EventEmitter<void> = new EventEmitter<void>()
  @Output() gotoPrevious: EventEmitter<void> = new EventEmitter<void>()
  @Output() gotoToday: EventEmitter<void> = new EventEmitter<void>()
  @Output() selectedDateChange: EventEmitter<Date> = new EventEmitter<Date>()
  @Output() zoomLevelChanged: EventEmitter<number> = new EventEmitter<number>()
  @Output() getCurrentDate: EventEmitter<void> = new EventEmitter<void>()

  private dialog: Dialog = inject(Dialog)

  zoomLevel = 30
  isNavLinkDayClick: boolean = false
  private zoomLevels = [5, 10, 15, 20, 30, 60, 120]
  viewRanges: SelectOption<string>[] = [
    { label: 'Week', value: 'week' },
    { label: 'Werkweek', value: 'workWeek' },
    { label: 'Dag', value: 'day' },
  ]
  verwijderOpties: SelectOption<string>[] = [
    { label: 'Afspraakslots', value: 'afspraakslots' },
    { label: 'Blokkades', value: 'blokkades' },
  ]
  tijdslotAanmakenConstraint: SecurityConstraint = {
    recht: [Recht.GEBRUIKER_LOCATIE_ROOSTER],
    actie: Actie.TOEVOEGEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.INSTELLING,
    organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
    required: Required.ANY,
  }

  tijdslotVerwijderenConstraint: SecurityConstraint = {
    recht: [Recht.GEBRUIKER_LOCATIE_ROOSTER],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.INSTELLING,
    organisatieTypeScopes: [OrganisatieType.INTAKELOCATIE],
    required: Required.ANY,
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

  get isZoomOutDisabled(): boolean {
    const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
    return currentZoomIndex === this.zoomLevels.length - 1
  }

  get isZoomInDisabled(): boolean {
    const currentZoomIndex = this.zoomLevels.indexOf(this.zoomLevel)
    return currentZoomIndex === 0
  }

  openAfspraakslotAanmakenDialog() {
    this.dialog.open<ColonAfspraakslot>(AfspraakslotEditDialogComponent)
  }

  openBulkTijdslotVerwijderenDialog(typeTijdslotSelectie: SelectOption<string>) {
    this.dialog.open<ColonTijdslot>(TijdslotsVerwijderenDialogComponent, { data: typeTijdslotSelectie })
  }

  openBlokkadeDialog() {
    this.dialog.open<ColonBlokkade>(BlokkadeEditDialogComponent)
  }

  setViewRange(viewRange: CalendarView) {
    this.viewRange = viewRange
    this.isNavLinkDayClick = false
    this.viewRangeChange.emit(viewRange)
  }

  checkInput(event: Event): void {
    const input = event.target as HTMLInputElement
    const inputDatum = parseDate(input.value)
    if (!isValid(inputDatum) || inputDatum.getFullYear() < 2000 || inputDatum.getFullYear() > 2200) {
      if (this.viewRange === 'day') {
        input.value = this.currentDate ? format(this.currentDate, NL_DATE_FORMAT) : ''
      } else {
        this.selectedDate = undefined
        input.value = ''
      }
    }
  }
}
