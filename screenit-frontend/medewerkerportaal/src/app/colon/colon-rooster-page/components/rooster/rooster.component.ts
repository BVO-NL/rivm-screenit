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
import { AfterViewInit, Component, ElementRef, inject, ViewChild } from '@angular/core'
import resourceTimeGridPlugin from '@fullcalendar/resource-timegrid'
import { CalendarApi, CalendarOptions, DateSelectArg, EventSourceInput, ViewApi } from '@fullcalendar/core'
import { FullCalendarComponent, FullCalendarModule } from '@fullcalendar/angular'
import interactionPlugin from '@fullcalendar/interaction'
import nlLocale from '@fullcalendar/core/locales/nl'
import { filter, identity, Observable, of, switchMap, tap } from 'rxjs'
import { RoosterService } from '@/colon/colon-rooster-page/services/rooster.service'
import { CommonModule } from '@angular/common'
import { RoosterControlsComponent } from '@/colon/colon-rooster-page/components/rooster-controls/rooster-controls.component'
import { CalendarView } from '@shared/types/calendar-view'
import { format, isValid } from 'date-fns'
import { CalendarKamer } from '@shared/types/calendar-kamer'
import { WINDOW } from '@shared/tokens/window.token'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Dialog } from '@angular/cdk/dialog'
import { AfspraakslotEditDialogComponent } from '@/colon/colon-rooster-page/components/afspraakslot-edit-dialog/afspraakslot-edit-dialog.component'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'
import { ColonTijdslotStatus } from '@shared/types/colon/colon-tijdslot-status'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { isColonAfspraakslot, isColonBlokkade } from '@shared/type-guards'
import { BlokkadeEditDialogComponent } from '@/colon/colon-rooster-page/components/blokkade-edit-dialog/blokkade-edit-dialog.component'
import { NotPersisted } from '@shared/types/not-presisted'
import tippy from 'tippy.js'
import { ResourceApi } from '@fullcalendar/resource'
import { TIME_FORMAT } from '@shared/constants'
import { environment } from '../../../../../environments/environment'

@Component({
  selector: 'app-rooster',
  templateUrl: './rooster.component.html',
  imports: [FullCalendarModule, RoosterControlsComponent, CommonModule],
})
export class RoosterComponent implements AfterViewInit {
  @ViewChild('calendar')
  private calendarComponent: FullCalendarComponent | undefined
  @ViewChild('calendar', { static: true, read: ElementRef })
  private calendarElement: ElementRef<HTMLDivElement> | undefined
  private window: Window = inject(WINDOW)
  private calendarApi: CalendarApi | undefined
  private roosterService: RoosterService = inject(RoosterService)
  selectedDate: Date | undefined
  viewRange: CalendarView = 'workWeek'
  currentDate: Date | undefined
  private dialog: Dialog = inject(Dialog)

  private eventClasses: Record<string, string> = {
    [ColonTijdslotStatus.BLOKKADE]: 'event_status_blocked',
    [ColonTijdslotStatus.GEBRUIKT_VOOR_CAPACITEIT]: 'event_status_capacity',
    [ColonTijdslotStatus.INTAKE_GEPLAND]: 'event_status_intake',
    [ColonTijdslotStatus.VRIJ_TE_VERPLAATSEN]: 'event_status_free',
    [ColonTijdslotStatus.BEPERKING]: 'event_status_beperking',
    [ColonTijdslotStatus.FEESTDAG]: 'event_status_feestdag',
  }

  get toonIconen(): boolean {
    const zoomLevel = this.calendarApi?.getOption('slotDuration') as Duration
    return zoomLevel.minutes !== undefined && zoomLevel.minutes <= 15
  }

  tijdsloten$: Observable<EventSourceInput> = of([])
  kamers: CalendarKamer[] = []
  calendarOptions: CalendarOptions = {
    timeZone: 'CET',
    locale: nlLocale,
    weekNumbers: true,
    initialView: 'workWeek',
    datesAboveResources: true,
    headerToolbar: false,
    nowIndicator: true,
    selectable: true,
    dateAlignment: 'week',
    views: {
      week: {
        type: 'resourceTimeGridWeek',
        duration: { days: 7 },
      },
      day: {
        type: 'resourceTimeGridDay',
        dayHeaderFormat: { weekday: 'long', month: 'long', day: 'numeric' },
      },
      workWeek: {
        type: 'resourceTimeGridWeek',
        duration: { days: 5 },
      },
    },
    eventMinHeight: 10,
    contentHeight: 1000,
    slotDuration: { minutes: 60 },
    expandRows: true,
    allDaySlot: false,
    plugins: [resourceTimeGridPlugin, interactionPlugin],
    schedulerLicenseKey: environment.schedulerLicenseKey,
    stickyHeaderDates: true,
    slotLabelFormat: {
      hour: 'numeric',
      minute: '2-digit',
      omitZeroMinute: false,
      meridiem: 'short',
    },
    scrollTime: { hours: 8 },
    resourceOrder: 'order',
    datesSet: ({ view }) => this.handleDatesSet(view),
    eventClassNames: ({ event }) => this.getEventClasses(event.extendedProps as ColonAfspraakslot),
    windowResize: () => this.updateCalendarHeight(),
    eventClick: ({ event }) => this.editTijdslot(event.extendedProps as ColonAfspraakslot),
    select: (event: DateSelectArg) => this.handleSelect(event.startStr, event.endStr, event.resource?.extendedProps?.['kamerId']),
    resourceLabelDidMount: ({ resource, el }) => this.handleResourceDidMount(resource, el),
    resourceLabelContent: ({ resource }) => resource.extendedProps['order'],
    navLinks: true,
    navLinkDayClick: (date) => this.handleNavLinkDayClick(date),
  }

  constructor() {
    this.roosterService
      .getKamers()
      .pipe(
        takeUntilDestroyed(),
        tap((kamers) => {
          this.kamers = kamers.map((kamer: ColonKamer) => ({
            id: kamer.id.toString(),
            kamerId: kamer.id,
            title: kamer.naam,
            extendedProps: kamer,
          }))
        }),
        switchMap(() => this.roosterService.getInstellingen()),
      )
      .subscribe((instellingen) => {
        this.gotoDate(instellingen.geprognosticeerdeVanafDatum)
      })

    this.roosterService.onRefresh.pipe(takeUntilDestroyed(), filter(identity)).subscribe(() => this.getTijdslots())
  }

  ngAfterViewInit() {
    this.calendarApi = this.calendarComponent?.getApi()
    setTimeout(() => {
      this.updateCalendarHeight()
      this.setZoomLevel(30)
    }, 100)
  }

  updateCalendarHeight() {
    const fixedDiv = this.calendarElement?.nativeElement
    const yPosition = fixedDiv?.getBoundingClientRect().top ?? 0
    const windowHeight = this.window.innerHeight

    const newHeight = windowHeight - yPosition - 80
    this.calendarApi?.setOption('height', newHeight + 'px')
  }

  getTijdslots() {
    const startDate = this.calendarApi?.view.activeStart
    const endDate = this.calendarApi?.view.activeEnd
    if (!startDate || !endDate) {
      return
    }

    this.tijdsloten$ = this.roosterService.getTijdslots(startDate, endDate)
  }

  editTijdslot(tijdslot: ColonTijdslot) {
    if (isColonBlokkade(tijdslot)) {
      this.dialog.open(BlokkadeEditDialogComponent, { data: tijdslot })
    } else if (isColonAfspraakslot(tijdslot)) {
      this.dialog.open(AfspraakslotEditDialogComponent, { data: tijdslot })
    }
  }

  getEventClasses(afspraakslot: ColonAfspraakslot): string {
    if (!afspraakslot.status || !this.eventClasses[afspraakslot.status]) {
      return this.eventClasses[ColonTijdslotStatus.VRIJ_TE_VERPLAATSEN]
    }

    return this.eventClasses[afspraakslot.status]
  }

  onToggleView(view: CalendarView) {
    this.calendarApi?.changeView(view)
    this.viewRange = view
    this.calendarApi?.setOption('dateAlignment', view === 'day' ? 'day' : 'week')
    this.getTijdslots()
    this.getCurrentDate()
  }

  handleSelect(startDatumTijd: string, eindDatumTijd: string, kamerId: number) {
    const nieuwTijdslot: NotPersisted<ColonTijdslot> = {
      tot: eindDatumTijd,
      vanaf: startDatumTijd,
      kamerId,
    }
    this.dialog.open(AfspraakslotEditDialogComponent, { data: nieuwTijdslot })
  }

  handleResourceDidMount(resource: ResourceApi, element: HTMLElement) {
    tippy(element, { content: resource.title, arrow: true, placement: 'bottom' })
  }

  gotoNext() {
    this.navigateCalendar('next')
  }

  gotoPrevious() {
    this.navigateCalendar('prev')
  }

  private navigateCalendar(direction: 'next' | 'prev') {
    if (direction === 'next') {
      this.calendarApi?.next()
    } else {
      this.calendarApi?.prev()
    }
    if (this.viewRange !== 'day') {
      this.selectedDate = undefined
    } else {
      this.selectedDate = this.calendarApi?.getDate()
      this.currentDate = this.calendarApi?.getDate()
    }
    this.getTijdslots()
  }

  gotoToday() {
    this.calendarApi?.today()
    this.getTijdslots()
  }

  gotoDate(date: Date) {
    if (isValid(date)) {
      this.calendarApi?.gotoDate(format(date, 'yyyy-MM-dd'))
      this.getTijdslots()
    }
  }

  updateSelectedDate(date: Date) {
    this.selectedDate = date
  }

  getCurrentDate() {
    this.currentDate = this.calendarApi?.view.currentStart
  }

  handleSelectedDateChange(date: Date) {
    this.gotoDate(date)
    this.updateSelectedDate(date)
  }

  handleDatesSet(view: ViewApi) {
    if (view.type === 'day') {
      this.selectedDate = view.currentStart
    } else {
      this.selectedDate = undefined
    }
  }

  handleNavLinkDayClick(date: Date) {
    this.onToggleView('day')
    this.gotoDate(date)
  }

  setZoomLevel(zoomLevel: number): void {
    this.calendarApi?.setOption('slotDuration', { minutes: zoomLevel })
    this.calendarApi?.scrollToTime({ hours: 8 })
  }

  protected readonly ColonTijdslotStatus = ColonTijdslotStatus
  protected readonly TIME_FORMAT = TIME_FORMAT
}
