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
import { inject, Injectable } from '@angular/core'
import { catchError, connectable, map, Observable, of, switchMap, tap } from 'rxjs'
import { ApiService } from '@shared/services/api/api.service'
import { ClientContactService } from '@algemeen/services/client-contact/client-contact.service'
import { BaseService } from '@shared/services/base/base.service'
import { BaseClientContactDto } from '@shared/types/algemeen/dto/base-client-contact.dto'
import { AanhefWijzigenDto } from '@shared/types/algemeen/dto/aanhef-wijzigen.dto'
import { ContactActieDto } from '@shared/types/algemeen/dto/contact-actie.dto'
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'
import { KeuzeGebruikGegevensBriefDto } from '@shared/types/algemeen/dto/keuze-gebruik-gegevens-brief.dto'
import { BezwaarVerwerkenDto } from '@shared/types/algemeen/dto/bezwaar-verwerken.dto'
import { TijdelijkAdresWijzigenDto } from '@shared/types/algemeen/dto/tijdelijk-adres-wijzigen.dto'
import { DeelnamewensenDto, DeelnamewensenRegistrerenDto } from '@shared/types/algemeen/dto/deelnamewensen.dto'
import { OpnieuwAanvragenClientgegevensDto } from '@shared/types/algemeen/dto/opnieuw-aanvragen-clientgegevens.dto'
import { DsActionItem, DsDefaultActionList, ListCategoryAccentColor } from '@topicus-rgp-ds/web'
import { alleenBaarmoederhalskanker, alleenBorstkanker, alleenDarmkanker, heeftAlleBevolkingsonderzoeken } from '@shared/utils/bevolkingsonderzoek-utils'
import { bepaalContactActieOpties, ContactActieOptie, contactActieOptieLabels } from '@shared/types/algemeen/enum/contact-actie-optie'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import {
  faBuildingCircleArrowRight,
  faCalendarDays,
  faFilePlus,
  faFileShield,
  faHand,
  faHouseCircleCheck,
  faKitMedical,
  faPaperPlane,
  faPhone,
  faPrint,
  faStethoscope,
  faUserDoctor,
  faUserMinus,
  faUserPen,
  faUserPlus,
  faUsersGear,
  faUserShield,
  faVial,
  faWheelchair,
} from '@fortawesome/pro-solid-svg-icons'
import { StringsService } from '@shared/strings'

type ActieHandler = (actieOptie: ContactActieOptie) => void

interface ContactActiesState {
  contactActies: ContactActieDto[]
}

const ALGEMEEN_ACCENT_KLEUR: ListCategoryAccentColor = 'blue'
const COLON_ACCENT_KLEUR: ListCategoryAccentColor = 'purple'
const CERVIX_ACCENT_KLEUR: ListCategoryAccentColor = 'red'
const MAMMA_ACCENT_KLEUR: ListCategoryAccentColor = 'orange'

@Injectable({
  providedIn: 'root',
})
export class ContactActiesService extends BaseService<ContactActiesState> {
  private readonly apiService = inject(ApiService)
  private readonly clientContactService = inject(ClientContactService)
  private readonly strings = inject(StringsService).getStrings().algemeen.clientDossier.acties.categorieen
  private readonly baseUrl = '/api/contact-acties'
  private readonly clientContactUrl = '/api/client-contact'

  getContactActies(clientId: number): Observable<ContactActieDto[]> {
    const contactActies$ = connectable(this.apiService.get<ContactActieDto[]>(`${this.baseUrl}/${clientId}`).pipe(tap((contactActies) => this.set('contactActies', contactActies))))
    contactActies$.connect()
    return contactActies$
  }

  getBeschikbareActies(clientId: number, actieTypes: ClientContactActieType[]): Observable<ContactActieDto[]> {
    return this.apiService.get<ContactActieDto[]>(`${this.baseUrl}/${clientId}`, { actieTypes: actieTypes.join(',') })
  }

  heeftBeschikbareActie(clientId: number, actieType: ClientContactActieType): Observable<boolean> {
    return this.getBeschikbareActies(clientId, [actieType]).pipe(
      map((acties) => acties.some((actie) => actie.actieType === actieType)),
      catchError(() => of(false)),
    )
  }

  wijzigAanhef(clientId: number, wijziging: AanhefWijzigenDto): Observable<void> {
    return this.clientContactService.ververstAantalContactenMetMeldingNa(this.apiService.post<void>(`${this.baseUrl}/${clientId}/aanhef-wijzigen`, wijziging), () => clientId)
  }

  getDeelnamewensen(clientId: number): Observable<DeelnamewensenDto> {
    return this.apiService.get<DeelnamewensenDto>(`${this.baseUrl}/${clientId}/deelnamewensen`)
  }

  registreerDeelnamewensen(clientId: number, registratie: DeelnamewensenRegistrerenDto): Observable<void> {
    return this.clientContactService.ververstAantalContactenMetMeldingNa(this.apiService.post<void>(`${this.baseUrl}/${clientId}/deelnamewensen`, registratie), () => clientId)
  }

  registreerDeelnameBuitenBvoBmhk(clientId: number): Observable<void> {
    return this.clientContactService.ververstAantalContactenMetMeldingNa(this.apiService.post<void>(`${this.baseUrl}/${clientId}/deelname-buiten-bvo-bmhk`, null), () => clientId)
  }

  heraanmeldenBaarmoederhalskanker(clientId: number, heraanmelding: BaseClientContactDto): Observable<ContactActieDto[]> {
    return this.clientContactService
      .ververstAantalContactenMetMeldingNa(this.apiService.post<void>(`${this.baseUrl}/${clientId}/heraanmelden-baarmoederhalskanker`, heraanmelding), () => clientId)
      .pipe(switchMap(() => this.getContactActies(clientId)))
  }

  wijzigTijdelijkAdres(clientId: number, wijziging: TijdelijkAdresWijzigenDto): Observable<void> {
    return this.clientContactService.ververstAantalContactenMetMeldingNa(
      this.apiService.post<void>(`${this.baseUrl}/${clientId}/tijdelijk-adres-wijzigen`, wijziging),
      () => clientId,
    )
  }

  verstuurKeuzeGebruikGegevensBrief(clientId: number, brief: KeuzeGebruikGegevensBriefDto): Observable<void> {
    return this.clientContactService.ververstAantalContactenMetMeldingNa(
      this.apiService.post<void>(`${this.baseUrl}/${clientId}/keuze-gebruik-gegevens-brief`, brief),
      () => clientId,
    )
  }

  verwerkBezwaar(clientId: number, gegevens: BezwaarVerwerkenDto, bestand: File): Observable<void> {
    return this.verstuurBezwaarFormulier(`${this.clientContactUrl}/${clientId}/bezwaar-verwerken`, clientId, gegevens, bestand)
  }

  verwijderGegevens(clientId: number, gegevens: BezwaarVerwerkenDto, bestand: File): Observable<void> {
    return this.verstuurBezwaarFormulier(`${this.clientContactUrl}/${clientId}/gegevens-verwijderen`, clientId, gegevens, bestand)
  }

  private verstuurBezwaarFormulier(url: string, clientId: number, gegevens: BezwaarVerwerkenDto, bestand: File): Observable<void> {
    const formData = new FormData()
    formData.append('gegevens', new Blob([JSON.stringify(gegevens)], { type: 'application/json' }))
    formData.append('bestand', bestand)
    return this.clientContactService.ververstAantalContactenMetMeldingNa(this.apiService.post<void>(url, formData), () => clientId)
  }

  vraagClientgegevensOpnieuwAan(clientId: number, aanvraag: OpnieuwAanvragenClientgegevensDto): Observable<ContactActieDto[]> {
    return this.clientContactService
      .ververstAantalContactenMetMeldingNa(this.apiService.post<void>(`${this.baseUrl}/${clientId}/opnieuw-aanvragen-clientgegevens`, aanvraag), () => clientId)
      .pipe(switchMap(() => this.getContactActies(clientId)))
  }

  maakAlgemeneActies(contactActies: ContactActieDto[], onActieGekozen: ActieHandler): DsDefaultActionList {
    return {
      title: this.strings.algemeen,
      accentColor: ALGEMEEN_ACCENT_KLEUR,
      actionItems: contactActies.filter((actie) => heeftAlleBevolkingsonderzoeken(actie.bevolkingsonderzoeken)).flatMap((actie) => this.maakActieItems(actie, onActieGekozen)),
    }
  }

  maakColonActies(contactActies: ContactActieDto[], onActieGekozen: ActieHandler): DsDefaultActionList {
    return {
      title: this.strings.colon,
      accentColor: COLON_ACCENT_KLEUR,
      actionItems: contactActies.filter((actie) => alleenDarmkanker(actie.bevolkingsonderzoeken)).flatMap((actie) => this.maakActieItems(actie, onActieGekozen)),
    }
  }

  maakCervixActies(contactActies: ContactActieDto[], onActieGekozen: ActieHandler): DsDefaultActionList {
    return {
      title: this.strings.cervix,
      accentColor: CERVIX_ACCENT_KLEUR,
      actionItems: contactActies.filter((actie) => alleenBaarmoederhalskanker(actie.bevolkingsonderzoeken)).flatMap((actie) => this.maakActieItems(actie, onActieGekozen)),
    }
  }

  maakMammaActies(contactActies: ContactActieDto[], onActieGekozen: ActieHandler): DsDefaultActionList {
    return {
      title: this.strings.mamma,
      accentColor: MAMMA_ACCENT_KLEUR,
      actionItems: contactActies.filter((actie) => alleenBorstkanker(actie.bevolkingsonderzoeken)).flatMap((actie) => this.maakActieItems(actie, onActieGekozen)),
    }
  }

  private maakActieItems(actie: ContactActieDto, onActieGekozen: ActieHandler): DsActionItem[] {
    return bepaalContactActieOpties(actie.actieType).map((actieOptie) => ({
      title: contactActieOptieLabels[actieOptie],
      action: {
        callback: () => onActieGekozen(actieOptie),
        type: 'callback',
      },
      icon: this.bepaalIcoon(actieOptie),
    }))
  }

  private bepaalIcoon(actieOptie: ContactActieOptie): IconDefinition {
    switch (actieOptie) {
      case ContactActieOptie.GEEN:
      case ContactActieOptie.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
        return faUserPlus
      case ContactActieOptie.TIJDELIJK_ADRES:
        return faHouseCircleCheck
      case ContactActieOptie.DEELNAMEWENSEN:
        return faHand
      case ContactActieOptie.AANPASSEN_AANHEF:
        return faUserPen
      case ContactActieOptie.CERVIX_DEELNAME_BUITEN_BVO_BMHK:
        return faBuildingCircleArrowRight
      case ContactActieOptie.COLON_AANVRAGEN_NIEUWE_FIT:
        return faVial
      case ContactActieOptie.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN:
        return faCalendarDays
      case ContactActieOptie.COLON_NIEUWE_AFSPRAAK_AANMAKEN:
        return faCalendarDays
      case ContactActieOptie.COLON_AFMELDEN:
        return faUserMinus
      case ContactActieOptie.COLON_HERAANMELDEN:
        return faUserPlus
      case ContactActieOptie.COLON_HUISARTS_WIJZIGEN:
        return faUserDoctor
      case ContactActieOptie.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
        return faFilePlus
      case ContactActieOptie.COLON_OPEN_UITNODIGING:
      case ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN:
        return faPaperPlane
      case ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_VERWERKEN:
        return faUserShield
      case ContactActieOptie.INZAGE_PERSOONSGEGEVENS:
        return faFileShield
      case ContactActieOptie.CERVIX_AFMELDEN:
        return faUserMinus
      case ContactActieOptie.CERVIX_HERAANMELDEN:
        return faUserPlus
      case ContactActieOptie.CERVIX_UITSTEL:
      case ContactActieOptie.CERVIX_ZAS_AANVRAGEN:
        return faKitMedical
      case ContactActieOptie.CERVIX_HERDRUK:
        return faPrint
      case ContactActieOptie.CERVIX_FRISSE_START:
      case ContactActieOptie.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN:
        return faFilePlus
      case ContactActieOptie.MAMMA_RONDE_FORCEREN:
      case ContactActieOptie.MAMMA_AFSPRAAK_MAKEN:
        return faCalendarDays
      case ContactActieOptie.MAMMA_AFSPRAAK_MAKEN_FORCEREN:
        return faCalendarDays
      case ContactActieOptie.MAMMA_AFSPRAAK_WIJZIGEN:
        return faCalendarDays
      case ContactActieOptie.MAMMA_UITSTELLEN:
      case ContactActieOptie.MAMMA_AFMELDEN:
        return faUserMinus
      case ContactActieOptie.MAMMA_HERAANMELDEN:
        return faUserPlus
      case ContactActieOptie.MAMMA_HERBEOORDELEN:
      case ContactActieOptie.MAMMA_DOELGROEP_WIJZIGEN:
        return faUsersGear
      case ContactActieOptie.MAMMA_HUISARTS_WIJZIGEN:
        return faUserDoctor
      case ContactActieOptie.MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS:
        return faWheelchair
      case ContactActieOptie.MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS:
        return faWheelchair
      case ContactActieOptie.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK:
        return faStethoscope
      case ContactActieOptie.MAMMA_VERZOEK_CLIENT_CONTACT:
        return faPhone
      case ContactActieOptie.MAMMA_INFOBRIEF_PROTHESEN:
        return faFilePlus
    }
  }
}
