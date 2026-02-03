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
import { HttpParams } from '@angular/common/http'
import { WicketProperties } from './wicket-properties'

declare let wicketProperties: WicketProperties

declare let Wicket

export class WicketActionBuilder {
  private readonly url: string
  private request: { u: string } = { u: '' }

  private params: HttpParams = new HttpParams()

  constructor() {
    if ('undefined' !== typeof wicketProperties) {
      this.url = wicketProperties.callbackUrl
    } else {
      this.url = ''
    }
  }

  setVisitatieId(visitatieId: number): this {
    this.params = this.params.append('visitatie', visitatieId)
    return this
  }

  setAction(action: string): this {
    this.params = this.params.append('action', action)
    return this
  }

  build(): this {
    this.request = { u: `${this.url}&${this.params.toString()}` }
    return this
  }

  send(): void {
    new Wicket.Ajax.Call().ajax(this.request)
  }
}
