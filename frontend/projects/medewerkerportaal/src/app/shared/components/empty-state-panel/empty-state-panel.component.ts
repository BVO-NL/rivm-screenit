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
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import { DsIconComponent } from '@topicus-rgp-ds/web'
import { faFile } from '@fortawesome/pro-solid-svg-icons'

@Component({
  selector: 'app-empty-state-panel',
  imports: [DsIconComponent],
  templateUrl: './empty-state-panel.component.html',
  styleUrl: './empty-state-panel.component.scss',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EmptyStatePanelComponent {
  defaultIcon = faFile
  icon = input<IconDefinition>(this.defaultIcon)
  titel = input.required<string>()
  subtekst = input('')
}
