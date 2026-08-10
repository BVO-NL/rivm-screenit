/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { computed, effect, Injectable, signal, WritableSignal } from '@angular/core'
import { AppState } from '../../models/AppState'
import { TokenDto } from '../../models/TokenDto'
import { UserDto } from '../../models/UserDto'
import { HuisartsDto } from '../../models/HuisartsDto'
import { AanvraagDto } from '../../models/AanvraagDto'
import { LocatieDto } from '../../models/LocatieDto'
import { LocatieVerificatieDto } from '../../models/LocatieVerificatieDto'
import { AuthenticationScope } from '../../models/AuthenticationScope'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  private readonly state: WritableSignal<AppState> = signal({
    loading: false,
    locaties: [],
    auth: null,
    toasts: [],
    huisarts: null,
    locatieVerificaties: [],
    user: null,
    labformulieren: [],
  })
  readonly appState = this.state.asReadonly()
  readonly auth = computed(() => this.state().auth)
  readonly user = computed(() => this.state().user)
  readonly huisarts = computed(() => this.state().huisarts)
  readonly locatieVerificaties = computed(() => this.state().locatieVerificaties)
  readonly locaties = computed(() => this.state().locaties)
  readonly toasts = computed(() => this.state().toasts)
  readonly loading = computed(() => this.state().loading)
  readonly labformulieren = computed(() => this.state().labformulieren)

  constructor() {
    this.state.update((state) => {
      const savedState = sessionStorage.getItem('hpState')
      if (savedState) {
        return { ...state, ...JSON.parse(savedState) }
      }
      return state
    })

    effect(() => {
      if (this.state()) {
        sessionStorage.setItem('hpState', JSON.stringify(this.state()))
      }
    })
  }
  setAuth(auth: TokenDto | null) {
    this.state.update((state: AppState) => ({ ...state, auth }))
  }

  setScope(scope: AuthenticationScope) {
    this.state.update((state: AppState) => ({ ...state, auth: { ...state.auth, scope } }))
  }

  setUser(user: UserDto) {
    this.state.update((state: AppState) => ({ ...state, user }))
  }

  setHuisarts(huisarts: HuisartsDto) {
    this.state.update((state: AppState) => ({ ...state, huisarts }))
  }

  setLabformulieren(labformulieren: AanvraagDto[]) {
    this.state.update((state: AppState) => ({ ...state, labformulieren }))
  }

  setLocaties(locaties: LocatieDto[]): void {
    this.state.update((state: AppState) => ({ ...state, locaties }))
  }

  updateLocatie(locatie: LocatieDto): void {
    this.state.update((state: AppState) => {
      const locaties = state.locaties
      const index = locaties.findIndex((l) => l.huisartsportaalId === locatie.huisartsportaalId)
      if (index !== -1) {
        locaties[index] = locatie
      }
      return { ...state, locaties }
    })
  }

  setLocatieVerificaties(locatieVerificaties: LocatieVerificatieDto[]): void {
    this.state.update((state: AppState) => ({ ...state, locatieVerificaties }))
  }
}
