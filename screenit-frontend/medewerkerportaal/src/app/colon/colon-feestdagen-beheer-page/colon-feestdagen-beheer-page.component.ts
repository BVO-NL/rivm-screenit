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
import { Component, CUSTOM_ELEMENTS_SCHEMA, inject } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ClrDatagridModule, ClrFormsModule } from '@clr/angular'
import { Observable, take } from 'rxjs'
import { ColonFeestdag } from '@shared/types/colon/colon-feestdag'
import { Dialog } from '@angular/cdk/dialog'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ReactiveFormsModule } from '@angular/forms'
import { FeestdagEditDialogComponent } from '@/colon/colon-feestdagen-beheer-page/components/feestdag-edit-dialog/feestdag-edit-dialog.component'
import { FeestdagenService } from '@/colon/colon-feestdagen-beheer-page/services/feestdagen.service'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@shared/types/autorisatie/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'

@Component({
  selector: 'app-colon-feestdagen-beheer-page',
  imports: [CommonModule, ClrDatagridModule, ReactiveFormsModule, ClrFormsModule, AutorisatieDirective],
  templateUrl: './colon-feestdagen-beheer-page.component.html',
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
  styles: [
    `
      .header-btn {
        margin-top: 1.2rem;
      }
    `,
  ],
})
export class ColonFeestdagenBeheerPageComponent {
  feestdagen$: Observable<ColonFeestdag[]> | undefined
  private feestdagenService: FeestdagenService = inject(FeestdagenService)
  private dialogService: Dialog = inject(Dialog)
  toevoegenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.TOEVOEGEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  bewerkenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }
  verwijderenConstraint: SecurityConstraint = {
    recht: ['COLON_FEESTDAGEN_BEHEER'],
    actie: Actie.VERWIJDEREN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM],
    required: Required.ANY,
  }

  constructor() {
    this.getFeestdagen()
  }

  getFeestdagen() {
    this.feestdagen$ = this.feestdagenService.getFeestdagen()
  }

  openFeestdagDialog(feestdag?: ColonFeestdag) {
    this.dialogService
      .open(FeestdagEditDialogComponent, { data: feestdag })
      .closed.pipe(take(1))
      .subscribe((res: unknown) => {
        if (res) {
          this.getFeestdagen()
        }
      })
  }

  confirmDeleteFeestdag(feestdag: ColonFeestdag) {
    this.dialogService
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Waarschuwing',
          body: 'Weet u zeker dat u deze feestdag wilt verwijderen?',
        },
        minWidth: '300px',
      })
      .closed.pipe(take(1))
      .subscribe((res: unknown) => {
        if (res === true) {
          this.removeFeestdag(feestdag)
        }
      })
  }

  private removeFeestdag(feestdag: ColonFeestdag) {
    this.feestdagenService
      .removeFeestdag(feestdag)
      .pipe(take(1))
      .subscribe(() => {
        this.getFeestdagen()
      })
  }
}
