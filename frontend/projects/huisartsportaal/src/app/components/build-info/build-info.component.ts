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
import { Component, inject } from '@angular/core'
import { BuildInfoService } from '../../services/build-info/build-info.service'
import { toSignal } from '@angular/core/rxjs-interop'
import { NL_DATE_FORMAT } from '../../utils/constants'

@Component({
  selector: 'app-build-info',
  template: `@if (buildInfo()) {
    <div class="build-info" data-testid="build-info">
      <div>ScreenIT - Huisartsenportaal {{ buildInfo()!.version }} | {{ buildInfo()!.timestamp }}</div>
      <div>Instance: {{ buildInfo()!.instance }}</div>
    </div>
  } `,
  styles: `
    .build-info {
      position: fixed;
      bottom: 5px;
      right: 5px;
      color: var(--grijs);
      font-size: 0.75rem;
      user-select: none;
    }
  `,
})
export class BuildInfoComponent {
  private readonly buildInfoService = inject(BuildInfoService)
  buildInfo = toSignal(this.buildInfoService.getBuildInfo())
  protected readonly NL_DATE_FORMAT = NL_DATE_FORMAT
}
