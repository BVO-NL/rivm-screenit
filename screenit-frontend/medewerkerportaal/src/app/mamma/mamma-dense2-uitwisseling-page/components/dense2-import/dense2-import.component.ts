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
import { Component, inject } from '@angular/core'
import { CardComponent } from '@shared/components/card/card.component'
import { ToastService } from '@/toast/service/toast.service'
import { FormsModule } from '@angular/forms'
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'
import { catchError, of, throwError } from 'rxjs'

@Component({
  selector: 'app-dense2-import',
  imports: [CardComponent, FormsModule],
  template: `
    <app-card>
      <div header>Import</div>
      <div class="clr-row clr-align-items-center" body>
        <div class="clr-col-3">Bestandsnaam</div>
        <div class="clr-col-9 upload-file">
          <label for="uploadFile" class="btn">Bladeren</label>
          <input type="file" id="uploadFile" accept="text/csv" (change)="changeFile($event)" />
        </div>
      </div>
      <button footer class="btn btn-primary" [disabled]="!file" (click)="import()">Verwerken</button>
    </app-card>
  `,
  styleUrl: './dense2-import.component.scss',
})
export class Dense2ImportComponent {
  file: File | undefined
  private toastService: ToastService = inject(ToastService)
  private dense2Service: Dense2Service = inject(Dense2Service)

  changeFile(event: Event) {
    const target = event.target as HTMLInputElement
    if (target.files) {
      this.file = target.files[0]
    }
  }

  import() {
    if (this.file) {
      this.dense2Service
        .import(this.file)
        .pipe(
          catchError((err) => {
            if (err.status === 403) {
              this.toastService.error('Type bestand kan niet verwerkt worden.')
              return of(null)
            }
            return throwError(() => err)
          }),
        )
        .subscribe((melding) => {
          if (melding) {
            this.toastService.success(melding)
          }
        })
    }
  }
}
