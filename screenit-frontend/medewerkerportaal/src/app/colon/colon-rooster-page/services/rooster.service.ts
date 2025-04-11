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
import { inject, Injectable, Signal } from '@angular/core'
import { forkJoin, map, Observable, of, Subject, switchMap, take, tap } from 'rxjs'

import { ApiService } from '@shared/services/api/api.service'
import { ColonAfspraakslot } from '@shared/types/colon/colon-afspraakslot'
import { ColonKamer } from '@shared/types/colon/colon-kamer'
import { convertNLDateStringToDateString, formatDate, formatDateAsISO, formatNLDate, formatTimeAsISO, parseDate } from '@shared/date-utils'
import { ColonBlokkade } from '@shared/types/colon/colon-blokkade'
import { ColonRoosterBeperkingenDto } from '@shared/types/colon/colon-rooster-beperkingen-dto'
import { ColonRoosterInstellingen, ColonRoosterInstellingenDto } from '@shared/types/colon/colon-rooster-instellingen'
import { BaseService } from '@shared/services/base/base.service'
import { EventInput, EventSourceInput } from '@fullcalendar/core'
import { NotPersisted } from '@shared/types/not-presisted'
import { Signaleringstermijn } from '@shared/types/signaleringstermijn'
import { addDays, differenceInDays, endOfDay, startOfDay } from 'date-fns'
import { ColonFeestdagDto } from '@shared/types/colon/colon-feestdag'
import { ColonTijdslotStatus } from '@shared/types/colon/colon-tijdslot-status'
import { ColonTijdslotFilter } from '@shared/types/colon/colon-tijdslot-filter'
import { ColonTijdslot } from '@shared/types/colon/colon-tijdslot'
import { ColonService } from '@/colon/services/colon.service'

interface RoosterState {
  instellingen: ColonRoosterInstellingen
  kamers: ColonKamer[]
  signaleringstermijn: Signaleringstermijn | null
}

@Injectable({
  providedIn: 'root',
})
export class RoosterService extends BaseService<RoosterState> {
  private api: ApiService = inject(ApiService)
  private colonService: ColonService = inject(ColonService)
  private roosterBaseUrl = '/api/colon/rooster'
  private roosterAfspraakslotBaseUrl = '/api/colon/rooster/afspraakslot'
  private roosterBlokkadeBaseUrl = '/api/colon/rooster/blokkade'
  onRefresh: Subject<boolean> = new Subject<boolean>()

  get instellingen(): Signal<ColonRoosterInstellingen> {
    return this.select('instellingen')
  }

  get kamers(): Signal<ColonKamer[]> {
    return this.select('kamers')
  }

  getAfspraakslots(startDate: Date, endDate: Date): Observable<ColonAfspraakslot[]> {
    const url = this.roosterAfspraakslotBaseUrl
    const params: { startDate?: string; endDate?: string } = {
      startDate: formatDate(startDate),
      endDate: formatDate(endDate),
    }
    return this.api.get<ColonAfspraakslot[]>(url, params)
  }

  get signaleringstermijn(): Signal<Signaleringstermijn | null> {
    return this.select('signaleringstermijn')
  }

  get heeftGeenCapaciteitBinnenSignaleringstermijn(): boolean {
    return this.signaleringstermijn()?.heeftGeenCapaciteitBinnenSignaleringsTermijn ?? false
  }

  fetchSignaleringstermijn(): Observable<Signaleringstermijn> {
    return this.api.get<Signaleringstermijn>('/api/colon/intakelocatie/signaleringstermijn').pipe(
      map((signaleringstermijn) => ({
        ...signaleringstermijn,
        signaleringsTermijnDeadline: formatNLDate(parseDate(signaleringstermijn.signaleringsTermijnDeadline)),
      })),
      tap((signaleringstermijn) => {
        this.set('signaleringstermijn', signaleringstermijn)
      }),
    )
  }

  getTijdslots(startDate: Date, endDate: Date): Observable<EventSourceInput> {
    return forkJoin([
      this.getAfspraakslots(startDate, endDate).pipe(map((items: ColonAfspraakslot[]) => items.map((item: ColonAfspraakslot) => this.afspraakslotToTijdslot(item)))),
      this.getBlokkades(startDate, endDate).pipe(map((items: ColonBlokkade[]) => items.map((item: ColonBlokkade) => this.blokkadeToTijdslot(item)))),
      this.getBeperkingslots(startDate, endDate),
      this.getFeestdagslots(startDate, endDate),
    ]).pipe(
      take(1),
      map(([afspraakslots, blokkades, beperkingen, feestdagen]) => [...afspraakslots, ...blokkades, ...beperkingen, ...feestdagen]),
    )
  }

  createAfspraakslots(afspraakslot: NotPersisted<ColonAfspraakslot>, alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.post<ColonAfspraakslot>(this.roosterAfspraakslotBaseUrl, { ...afspraakslot, alleenValidatie }), 'toevoegen')
  }

  updateAfspraakslot(afspraakslot: ColonAfspraakslot, alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(
      this.api.put<ColonAfspraakslot>(`${this.roosterAfspraakslotBaseUrl}/${afspraakslot.id}`, {
        ...afspraakslot,
        alleenValidatie,
      }),
      'aanpassen',
    )
  }

  deleteAfspraakslot(id: number): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.del(`${this.roosterAfspraakslotBaseUrl}/${id}`), 'verwijderen')
  }

  getBlokkades(startDate: Date, endDate: Date): Observable<ColonBlokkade[]> {
    const url = this.roosterBlokkadeBaseUrl
    const params: { startDate?: string; endDate?: string } = {
      startDate: formatDate(startDate),
      endDate: formatDate(endDate),
    }
    return this.api.get<ColonBlokkade[]>(url, params)
  }

  createBlokkades(blokkade: NotPersisted<ColonBlokkade>, alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.post<ColonBlokkade>(this.roosterBlokkadeBaseUrl, { ...blokkade, alleenValidatie }))
  }

  updateBlokkade(blokkade: ColonBlokkade, alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.put<ColonBlokkade>(`${this.roosterBlokkadeBaseUrl}/${blokkade.id}`, { ...blokkade, alleenValidatie }))
  }

  deleteBlokkade(id: number): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.del(`${this.roosterBlokkadeBaseUrl}/${id}`))
  }

  searchTijdslots(filter: ColonTijdslotFilter, type: string): Observable<ColonTijdslot[]> {
    const params: Record<string, string> = {
      startDatum: convertNLDateStringToDateString(filter.startDatum),
      eindDatum: convertNLDateStringToDateString(filter.eindDatum),
      startTijd: filter.startTijd,
      eindTijd: filter.eindTijd,
      dagen: filter.dagen.join(','),
    }
    if (filter.kamerId) {
      params.kamerId = filter.kamerId.toString()
    }
    if (type === 'afspraakslots') {
      return this.searchAfspraakslots(params)
    } else {
      return this.searchBlokkades(params)
    }
  }

  private searchBlokkades(params: Record<string, string>): Observable<ColonTijdslot[]> {
    const url = `${this.roosterBlokkadeBaseUrl}/search`
    return this.api.get<ColonTijdslot[]>(url, params)
  }

  private searchAfspraakslots(params: Record<string, string>): Observable<ColonTijdslot[]> {
    const url = `${this.roosterAfspraakslotBaseUrl}/search`
    return this.api.get<ColonTijdslot[]>(url, params)
  }

  bulkDeleteTijdslots(tijdslotIds: number[], alleenValidatie: boolean, type: string): Observable<unknown> {
    if (type === 'afspraakslots') {
      return this.bulkDeleteAfspraakslots(tijdslotIds, alleenValidatie)
    } else {
      return this.bulkDeleteBlokkades(tijdslotIds, alleenValidatie)
    }
  }

  private bulkDeleteAfspraakslots(afspraakslotIds: number[], alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(
      this.api.del(`${this.roosterAfspraakslotBaseUrl}/${afspraakslotIds.join(',')}?alleenValidatie=${alleenValidatie}&bulk=true`),
      'verwijderen',
    )
  }

  private bulkDeleteBlokkades(blokkadeIds: number[], alleenValidatie: boolean): Observable<unknown> {
    return this.handleTijdslotChangeResponse(this.api.del(`${this.roosterBlokkadeBaseUrl}/${blokkadeIds.join(',')}?alleenValidatie=${alleenValidatie}&bulk=true`))
  }

  getKamers(): Observable<ColonKamer[]> {
    return this.api.get<ColonKamer[]>(`${this.roosterBaseUrl}/kamers`).pipe(
      take(1),
      map((kamers) => kamers.map((kamer: ColonKamer, index: number) => ({ ...kamer, order: index + 1 }))),
      tap((kamers) => this.set('kamers', kamers)),
    )
  }

  getBeperkingen(): Observable<ColonRoosterBeperkingenDto> {
    return this.api.get<ColonRoosterBeperkingenDto>(`${this.roosterBaseUrl}/beperkingen`)
  }

  getBeperkingslots(startDate: Date, endDate: Date): Observable<EventInput[]> {
    return this.getBeperkingen().pipe(
      map((beperkingen) => {
        const difference = differenceInDays(endDate, startDate)
        const tijdslots = []
        for (let i = 0; i < difference + 1; i++) {
          const datum = addDays(startDate, i)
          const startEersteDeelNacht = formatDateAsISO(startOfDay(datum))
          const eindEersteDeelNacht = formatTimeAsISO(beperkingen.nachtBeperkingEind, datum)
          const eersteDeelNachtTijdslot = this.beperkingToTijdslot(beperkingen, startEersteDeelNacht, eindEersteDeelNacht)

          const startTweedeDeelNacht = formatTimeAsISO(beperkingen.nachtBeperkingBegin, datum)
          const eindTweedeDeelNacht = formatDateAsISO(endOfDay(datum))
          const tweedeDeelNachtTijdslot = this.beperkingToTijdslot(beperkingen, startTweedeDeelNacht, eindTweedeDeelNacht)

          let weekendTijdslot: EventInput | undefined
          if (datum.getDay() == 0 || datum.getDay() == 6) {
            weekendTijdslot = this.beperkingToTijdslot(beperkingen, formatDateAsISO(startOfDay(datum)), formatDateAsISO(endOfDay(datum)))
          }

          for (const kamer of this.kamers()) {
            if (weekendTijdslot) {
              tijdslots.push({ ...weekendTijdslot, resourceId: kamer.id.toString() })
            } else {
              tijdslots.push({ ...eersteDeelNachtTijdslot, resourceId: kamer.id.toString() })
              tijdslots.push({ ...tweedeDeelNachtTijdslot, resourceId: kamer.id.toString() })
            }
          }
        }
        return tijdslots
      }),
    )
  }

  getFeestdagslots(startDatum: Date, eindDatum: Date): Observable<EventInput[]> {
    return this.api
      .get<ColonFeestdagDto[]>(`/api/colon/feestdag`, {
        startDatum: formatNLDate(startDatum),
        eindDatum: formatNLDate(eindDatum),
      })
      .pipe(
        map((feestdagen: ColonFeestdagDto[]) => {
          const feestdagSlots: EventInput[] = []
          this.kamers().forEach((kamer) => feestdagen.forEach((feestdag: ColonFeestdagDto) => feestdagSlots.push(this.feestdagToTijdslot(feestdag, kamer.id.toString()))))
          return feestdagSlots
        }),
      )
  }

  updateBeperkingen(beperkingen: ColonRoosterBeperkingenDto): Observable<unknown> {
    return this.api.put(`${this.roosterBaseUrl}/beperkingen`, beperkingen)
  }

  getInstellingen(): Observable<ColonRoosterInstellingen> {
    return this.api.get<ColonRoosterInstellingenDto>(`/api/colon/rooster/instellingen`).pipe(
      take(1),
      map<ColonRoosterInstellingenDto, ColonRoosterInstellingen>((response) => ({
        geprognosticeerdeVanafDatum: new Date(response.geprognosticeerdeVanafDatum),
        duurAfspraakInMinuten: response.duurAfspraakInMinuten,
      })),
      tap((instellingen) => this.set('instellingen', instellingen)),
    )
  }

  private handleTijdslotChangeResponse(response: Observable<unknown>, actie: 'toevoegen' | 'aanpassen' | 'verwijderen' | null = null): Observable<unknown> {
    return response.pipe(
      switchMap((response) => {
        if (
          (actie === 'toevoegen' && this.heeftGeenCapaciteitBinnenSignaleringstermijn) ||
          (actie === 'verwijderen' && !this.heeftGeenCapaciteitBinnenSignaleringstermijn) ||
          actie === 'aanpassen'
        ) {
          return this.fetchSignaleringstermijn()
        }
        return of(response)
      }),
      switchMap(() => this.colonService.fetchIntakelocatie()),
      tap(() => this.onRefresh.next(true)),
    )
  }

  private afspraakslotToTijdslot(item: ColonAfspraakslot): EventInput {
    return {
      ...item,
      extendedProps: item,
      id: item.id?.toString(),
      start: item.vanaf,
      end: item.tot,
      resourceId: item.kamerId.toString(),
    }
  }

  private blokkadeToTijdslot(item: ColonBlokkade): EventInput {
    return {
      ...item,
      extendedProps: item,
      id: item.id?.toString(),
      start: item.vanaf,
      end: item.tot,
      resourceId: item.kamerId.toString(),
      status: 'BLOKKADE',
    }
  }

  private beperkingToTijdslot(item: ColonRoosterBeperkingenDto, startTime: string, endTime: string): EventInput {
    return {
      extendedProps: item,
      id: Math.random().toString(),
      start: startTime,
      end: endTime,
      status: 'BEPERKING',
      display: 'background',
    }
  }

  private feestdagToTijdslot(item: ColonFeestdagDto, resourceId: string): EventInput {
    return {
      extendedProps: item,
      id: `${resourceId}_${item.id}`,
      start: formatDateAsISO(startOfDay(new Date(item.datum))),
      end: formatDateAsISO(endOfDay(new Date(item.datum))),
      status: ColonTijdslotStatus.FEESTDAG,
      resourceId,
      display: 'background',
    }
  }
}
