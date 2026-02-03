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
import { Component, inject } from '@angular/core'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { MammaVisitatieDto } from '@shared/types/mamma/dto/mamma-visitatie.dto'

@Component({
  selector: 'app-mamma-visitatie-verwijderen-dialog',
  imports: [BaseDialogComponent],
  template: `<app-base-dialog titel="Visistatie verwijderen">
    <div body class="body">
      <div class="mb-2">Onderstaande visitatie definitief verwijderen?</div>
      <table>
        <tr>
          <td>Omschrijving</td>
          <td>{{ visitatie.omschrijving }}</td>
        </tr>
        <tr>
          <td>Aangemaakt door</td>
          <td>{{ visitatie.aangemaaktDoor }}</td>
        </tr>
      </table>
    </div>
    <div buttons class="btn-group">
      <button type="button" class="btn" (click)="sluitDialog()" data-testid="annuleren_button">Annuleren</button>
      <button type="button" class="btn btn-danger" (click)="bevestigVerwijderen()" data-testid="verwijderen_button">Verwijderen</button>
    </div>
  </app-base-dialog>`,
  styles: `
    .body table td {
      padding-right: 1rem;
    }
  `,
})
export class MammaVisitatieVerwijderenDialogComponent {
  protected readonly visitatie = inject(DIALOG_DATA) as MammaVisitatieDto
  private readonly dialogRef = inject(DialogRef)

  sluitDialog() {
    this.dialogRef.close(false)
  }

  bevestigVerwijderen() {
    this.dialogRef.close(true)
  }
}
