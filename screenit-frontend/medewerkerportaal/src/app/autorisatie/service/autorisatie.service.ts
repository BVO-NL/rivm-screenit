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
import { inject, Injectable } from '@angular/core'
import { ApiService } from '@shared/services/api/api.service'
import { BaseService } from '@shared/services/base/base.service'
import { Observable, take, tap } from 'rxjs'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Gebruiker } from '@shared/types/autorisatie/gebruiker'
import { Actie } from '@shared/types/autorisatie/actie'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'

interface AutorisatieState {
  gebruiker: Gebruiker
}

@Injectable({
  providedIn: 'root',
})
export class AutorisatieService extends BaseService<AutorisatieState> {
  private api: ApiService = inject(ApiService)
  private actieOrder = [Actie.INZIEN, Actie.AANPASSEN, Actie.TOEVOEGEN, Actie.VERWIJDEREN]
  private levelOrder = [ToegangLevel.EIGEN, ToegangLevel.INSTELLING, ToegangLevel.REGIO, ToegangLevel.LANDELIJK]

  getGebruiker(): Observable<Gebruiker> {
    return this.api.get<Gebruiker>('/api/autorisatie/gebruiker').pipe(
      take(1),
      tap((res) => {
        this.set('gebruiker', res)
      }),
    )
  }

  isToegestaan(constraint: SecurityConstraint): boolean {
    const gebruiker = this.select('gebruiker')
    const inScope = this.inScope(gebruiker(), constraint)
    const heeftRecht = this.heeftRecht(gebruiker(), constraint)

    return inScope && heeftRecht
  }

  private heeftRecht(gebruiker: Gebruiker, constraint: SecurityConstraint): boolean {
    const rollenBinnenOnderzoeken = gebruiker.rollen
      .map((gebruikerRol) => gebruikerRol.rol)
      .filter((rol) => rol.bevolkingsonderzoeken.some((onderzoek) => constraint.bevolkingsonderzoekScopes.includes(onderzoek)) && rol.actief)
    const permissiesBinnenRollen = rollenBinnenOnderzoeken.map((rol) => rol.permissies).flat()
    return permissiesBinnenRollen.some(
      (permissie) =>
        constraint.recht.includes(permissie.recht) &&
        this.actieOrder.indexOf(permissie.actie as Actie) >= this.actieOrder.indexOf(constraint.actie) &&
        this.levelOrder.indexOf(permissie.toegangLevel as ToegangLevel) >= this.levelOrder.indexOf(constraint.level),
    )
  }

  private inScope(gebruiker: Gebruiker, constraint: SecurityConstraint): boolean {
    if (constraint.bevolkingsonderzoekScopes) {
      let valtBinnenOrganisatieTypeScopes = false
      for (const type of constraint.organisatieTypeScopes) {
        if (gebruiker.organisatie.organisatieType === type) {
          valtBinnenOrganisatieTypeScopes = true
          break
        }
      }
      return valtBinnenOrganisatieTypeScopes
    }
    return true
  }
}
