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
import { computed, inject, Injectable } from '@angular/core'
import { HttpClient } from '@angular/common/http'
import { TokenDto } from '../../models/TokenDto'
import { RegistrationDto } from '../../models/RegistrationDto'
import { CredentialsDto } from '../../models/CredentialsDto'
import { WachtwoordWijzigenDto } from '../../models/WachtwoordWijzigenDto'
import { Observable, switchMap, take, tap } from 'rxjs'
import { StateService } from '../state/state.service'
import { Router } from '@angular/router'
import { HuisartsService } from '../huisarts/huisarts.service'
import { LocatieService } from '../locatie/locatie.service'
import { LocatieStatus } from '../../models/LocatieDto'
import { WachtwoordVergetenDto } from '../../models/WachtwoordVergetenDto'
import { WachtwoordAanvragenDto } from '../../models/WachtwoordAanvragenDto'
import { VerificatieService } from '../verificatie/verificatie.service'
import { UserDto } from '../../models/UserDto'
import { Recht } from '../../models/Recht'
import { HuisartsDto } from '../../models/HuisartsDto'
import { AuthenticationScope } from '../../models/AuthenticationScope'

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private readonly http = inject(HttpClient)
  private readonly stateService = inject(StateService)
  private readonly router = inject(Router)
  private readonly huisartsService = inject(HuisartsService)
  private readonly locatieService = inject(LocatieService)
  private readonly verificatieService = inject(VerificatieService)
  private readonly baseUrl = 'api/v1/auth'
  isLoggedIn = computed(() => this.stateService.auth()?.token != null)

  login(credentials: CredentialsDto) {
    return this.http.post<TokenDto>(`${this.baseUrl}/inloggen`, credentials).pipe(
      take(1),
      tap((response: TokenDto) => this.stateService.setAuth(response)),
      switchMap(() => this.huisartsService.getHuisarts()),
      switchMap(() => this.verificatieService.getLocatieVerificaties()),
      switchMap(() => this.getCurrentUser()),
    )
  }

  getCurrentUser(): Observable<UserDto> {
    return this.http.get<UserDto>(`api/v1/huisarts/currentuser`).pipe(
      tap((response: UserDto) => {
        this.stateService.setUser(response)
      }),
    )
  }

  heeftRecht(recht: Recht): boolean {
    const user = this.stateService.user()
    return user?.rollen.includes(recht) ?? false
  }

  logout() {
    this.stateService.setAuth(null)
    this.router.navigateByUrl('login')
  }

  getToken(): string | undefined {
    return this.stateService.auth()?.token
  }

  getScope(): string | undefined {
    return this.stateService.auth()?.scope
  }

  heeftScope(scope: AuthenticationScope): boolean {
    return this.getScope() === scope
  }

  updateWachtwoord(wachtwoord: WachtwoordWijzigenDto): Observable<void> {
    return this.http.post<void>('api/v1/huisarts/wachtwoord-wijzigen', wachtwoord)
  }

  registreren(payload: RegistrationDto) {
    return this.http.post<TokenDto>(`${this.baseUrl}/registreren`, payload).pipe(
      take(1),
      tap((response: TokenDto) => this.stateService.setAuth(response)),
      switchMap(() => this.huisartsService.getHuisarts()),
      switchMap(() => this.locatieService.getLocaties(0, 1000, LocatieStatus.ACTIEF)),
      switchMap(() => this.getCurrentUser()),
    )
  }

  registratieVoltooien(payload: HuisartsDto) {
    return this.huisartsService.updateHuisarts(payload).pipe(
      take(1),
      tap(() => this.stateService.setScope(AuthenticationScope.LOGIN)),
      switchMap(() => this.verificatieService.getLocatieVerificaties()),
      switchMap(() => this.getCurrentUser()),
    )
  }

  wachtwoordOpvragen(payload: WachtwoordVergetenDto): Observable<void> {
    return this.http.post<void>(`${this.baseUrl}/wachtwoord-vergeten`, payload)
  }

  wachtwoordAanvragen(payload: WachtwoordAanvragenDto): Observable<TokenDto> {
    return this.http.post<TokenDto>(`${this.baseUrl}/wachtwoord-aanvragen`, payload).pipe(tap((response: TokenDto) => this.stateService.setAuth(response)))
  }

  getCsrfToken() {
    return this.getCookie('XSRF-TOKEN')
  }

  private getCookie(key: string): string | null | undefined {
    const results = RegExp(`(^|;)\\s*${key}\\s*=\\s*([^;]+)`).exec(document.cookie)
    return results ? results.pop() : null
  }
}
