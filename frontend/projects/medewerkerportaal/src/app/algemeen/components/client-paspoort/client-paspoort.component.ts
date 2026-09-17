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
import { Component, computed, inject } from '@angular/core'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import { DatePipe } from '@angular/common'
import { DsBadgeComponent, DsDescriptionCustomTemplateDirective, DsDescriptionsComponent, DsHorizontalInformationBarComponent } from '@topicus-rgp-ds/web'
import { switchMap } from 'rxjs'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ClientPaspoortDto } from '@shared/types/algemeen/dto/client-paspoort.dto'
import { NL_DATE_FORMAT } from '@shared/constants'
import { LegeWaardePipe } from '@shared/pipes/lege-waarde/lege-waarde.pipe'
import { berekenLeeftijd } from '@shared/utils/date-utils'
import { DoelgroepBadgesComponent } from '@shared/components/doelgroep-badges/doelgroep-badges.component'
import { NaamPipe, NaamTransform } from '@shared/pipes/naam/naam.pipe'
import { GeslachtAfkortingPipe } from '@shared/pipes/geslacht-afkorting/geslacht-afkorting.pipe'

@Component({
  selector: 'app-client-paspoort',
  imports: [
    DatePipe,
    DsDescriptionsComponent,
    DsDescriptionCustomTemplateDirective,
    DsBadgeComponent,
    LegeWaardePipe,
    NaamPipe,
    GeslachtAfkortingPipe,
    DoelgroepBadgesComponent,
    DsHorizontalInformationBarComponent,
  ],
  templateUrl: './client-paspoort.component.html',
  styleUrl: './client-paspoort.component.scss',
  providers: [DatePipe],
})
export class ClientPaspoortComponent {
  private readonly clientService = inject(ClientService)
  protected readonly clientId = this.clientService.clientId
  private readonly datePipe = inject(DatePipe)

  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
  protected readonly NaamTransform = NaamTransform
  protected readonly paspoort = toSignal<ClientPaspoortDto | undefined>(toObservable(this.clientId).pipe(switchMap((clientId) => this.clientService.getClientPaspoort(clientId))), {
    initialValue: undefined,
  })
  protected readonly telefoonnummer = computed(() => {
    const paspoort = this.paspoort()
    return paspoort?.mobielNummer || paspoort?.extraNummer || '-'
  })
  protected readonly telefoonLabel = computed(() => {
    const paspoort = this.paspoort()
    if (!paspoort) {
      return 'Telefoonnummer'
    }
    if (paspoort.mobielNummer) {
      return 'Mobiel nummer'
    }
    if (paspoort.extraNummer) {
      return 'Extra nummer'
    }
    return 'Telefoonnummer'
  })
  protected readonly brpAdres = computed(() => {
    const paspoort = this.paspoort()
    if (!paspoort) {
      return '-'
    }
    return this.stelAdresSamen(paspoort.brpAdres, paspoort.brpPostcode, paspoort.brpWoonplaats)
  })
  protected readonly adres = computed(() => {
    const paspoort = this.paspoort()
    if (!paspoort) {
      return '-'
    }
    return this.stelAdresSamen(paspoort.adres, paspoort.postcode, paspoort.woonplaats)
  })
  protected readonly postadresLabel = computed(() => {
    const paspoort = this.paspoort()
    if (paspoort?.isTijdelijkAdres) {
      return 'Postadres (tijdelijk adres)'
    }
    if (paspoort?.isTijdelijkBrpAdres) {
      return 'Postadres (tijdelijk BRP-adres)'
    }
    return 'Postadres'
  })
  protected readonly adresGelijkAanBrp = computed(() => {
    const paspoort = this.paspoort()
    return !!paspoort && !paspoort.isTijdelijkAdres && !paspoort.isTijdelijkBrpAdres
  })
  protected readonly leeftijd = computed(() => berekenLeeftijd(this.paspoort()?.geboortedatum))
  protected readonly geboortedatumMetLeeftijd = computed(() => {
    const geboortedatum = this.datePipe.transform(this.paspoort()?.geboortedatum, NL_DATE_FORMAT) ?? ''
    const leeftijd = this.leeftijd()
    return leeftijd !== undefined ? `${geboortedatum} (${leeftijd})` : geboortedatum
  })
  protected readonly bsnMetANummer = computed(() => {
    const paspoort = this.paspoort()
    if (!paspoort) {
      return '-'
    }
    if (paspoort.bsn && paspoort.anummer) {
      return `${paspoort.bsn} (A-nr: ${paspoort.anummer})`
    }
    return paspoort.bsn ?? '-'
  })

  private stelAdresSamen(adres: string | null, postcode: string | null, woonplaats: string | null): string {
    const delen = [adres, postcode, woonplaats].map((deel) => deel?.trim()).filter((deel): deel is string => !!deel)
    return delen.length > 0 ? delen.join(' ') : '-'
  }
}
