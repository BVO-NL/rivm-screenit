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
import { Component, inject, signal, WritableSignal } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { take } from 'rxjs'
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
import { MammaDense2Configuratie } from '@shared/types/mamma/mamma-dense2-configuratie'
import { Dense2ImportComponent } from '@/mamma/mamma-dense2-uitwisseling-page/components/dense2-import/dense2-import.component'
import { Dense2ExportComponent } from '@/mamma/mamma-dense2-uitwisseling-page/components/dense2-export/dense2-export.component'
import { Dense2ConfiguratieComponent } from '@/mamma/mamma-dense2-uitwisseling-page/components/dense2-configuratie/dense2-configuratie.component'
import { PageComponent } from '@shared/components/page/page.component'

@Component({
  selector: 'app-mamma-dense2-uitwisseling-page',
  imports: [ReactiveFormsModule, Dense2ImportComponent, Dense2ExportComponent, Dense2ConfiguratieComponent, PageComponent],
  template: ` <app-page titel="DENSE-2 uitwisseling">
    <app-dense2-export class="mb-3" [exportBestandsnaam]="exportBestandsnaam()" />
    <app-dense2-import class="mb-3" />
    <app-dense2-configuratie [configuratie]="configuratie()" />
  </app-page>`,
  styles: `
    .pagina {
      max-width: var(--page-width-sm);
    }
  `,
})
export class MammaDense2UitwisselingPageComponent {
  private dense2Service: Dense2Service = inject(Dense2Service)
  configuratie: WritableSignal<MammaDense2Configuratie | undefined> = signal(undefined)
  exportBestandsnaam: WritableSignal<string> = signal('')

  constructor() {
    this.dense2Service
      .getConfiguratie()
      .pipe(take(1))
      .subscribe((configuratie: MammaDense2Configuratie) => {
        if (configuratie.exportBestandsnaam) {
          this.exportBestandsnaam.set(configuratie.exportBestandsnaam)
        }
        this.configuratie.set(configuratie)
      })
  }
}
