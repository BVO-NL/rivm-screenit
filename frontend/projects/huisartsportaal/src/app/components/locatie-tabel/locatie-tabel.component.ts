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
import { Component, computed, inject, input, signal, WritableSignal } from '@angular/core'
import {
  DsButtonComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsConfirmationDialogComponent,
  DsConfirmationDialogConfigModel,
  DsConfirmationDialogModel,
  DsDropdownComponent,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsLabelTemplateDirective,
  DsOptionTemplateDirective,
  DsPaginatorComponent,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSort, MatSortHeader } from '@angular/material/sort'
import { LocatieService } from '../../services/locatie/locatie.service'
import { LocatieDto, LocatieStatus } from '../../models/LocatieDto'
import { faPencil, faTrash } from '@fortawesome/pro-solid-svg-icons'
import { Dialog } from '@angular/cdk/dialog'
import { LocatieBewerkenDialogComponent } from '../locatie-bewerken-dialog/locatie-bewerken-dialog.component'
import { switchMap, take } from 'rxjs'
import { LocatieStatusComponent } from '../locatie-status/locatie-status.component'
import { NotificatieService } from '../../services/notificatie/notificatie.service'
import { AdresDto } from '../../models/AdresDto'
import { FormsModule } from '@angular/forms'
import { VerificatieService } from '../../services/verificatie/verificatie.service'

@Component({
  selector: 'app-locatie-tabel',
  imports: [
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCellDef,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsPaginatorComponent,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    MatSort,
    MatSortHeader,
    DsButtonComponent,
    DsIconComponent,
    DsConfirmationDialogComponent,
    LocatieStatusComponent,
    DsDropdownComponent,
    FormsModule,
    DsOptionTemplateDirective,
    DsLabelTemplateDirective,
  ],
  templateUrl: './locatie-tabel.component.html',
  styleUrl: './locatie-tabel.component.scss',
})
export class LocatieTabelComponent {
  private readonly locatieService = inject(LocatieService)
  private readonly dialogController = inject(Dialog)
  private readonly notificationService = inject(NotificatieService)
  private readonly verificatieService = inject(VerificatieService)
  private alleKolommen = ['naam', 'adres', 'postcode', 'plaats', 'status', 'wijzigen', 'verwijderen']
  protected readonly faPencil = faPencil
  protected readonly faTrash = faTrash
  protected readonly LocatieStatus = LocatieStatus

  private selectedVerwijderLocatie: WritableSignal<LocatieDto | null> = signal(null)
  registreren = input<boolean>(false)
  controleren = input<boolean>(false)
  postAdres = input<AdresDto | null>(null)
  displayedColumns = computed(() => (this.registreren() ? this.alleKolommen.filter((col) => col !== 'wijzigen') : this.alleKolommen))
  dataSource: WritableSignal<LocatieDto[]> = signal([])
  showConfirmationDialog = computed(() => this.selectedVerwijderLocatie() !== null)
  filter: LocatieStatus = LocatieStatus.ACTIEF
  statussen = [LocatieStatus.ACTIEF, LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD, LocatieStatus.INACTIEF]

  confirmationModel: DsConfirmationDialogModel = {
    title: 'Locatie verwijderen',
    actionLeft: {
      title: 'Annuleren',
      callback: (): void => this.selectedVerwijderLocatie.set(null),
    },
    actionRight: {
      title: 'Locatie verwijderen',
      buttonType: 'delete',
      callback: (): void => {
        if (this.selectedVerwijderLocatie()) {
          this.locatieService
            .verwijderLocatie(this.selectedVerwijderLocatie()!)
            .pipe(
              take(1),
              switchMap(() => this.verificatieService.getLocatieVerificaties()),
            )
            .subscribe(() => {
              this.getLocaties()
              this.selectedVerwijderLocatie.set(null)
            })
        }
      },
    },
  }
  confirmationConfig: DsConfirmationDialogConfigModel = {
    closeCallback: () => {
      this.selectedVerwijderLocatie.set(null)
      return true
    },
    enableClose: true,
    contentPadding: '1rem',
    preventBackdropClose: true,
  }

  constructor() {
    this.getLocaties()
  }

  getLocaties() {
    this.locatieService
      .getLocaties(0, 10, this.filter)
      .pipe(take(1))
      .subscribe((locaties) => this.dataSource.set(locaties))
  }

  wijzigLocatie(locatie?: LocatieDto) {
    this.dialogController
      .open(LocatieBewerkenDialogComponent, { data: { locatie, postAdres: this.postAdres() } })
      .closed.pipe(take(1))
      .subscribe((opgeslagen: unknown) => {
        if (opgeslagen) {
          if (locatie && locatie.huisartsportaalId && locatie.status === LocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD) {
            this.notificationService.warning(
              'Om uw Zorgmail klantnummer te bevestigen is een bericht met een verificatiecode naar uw zorgmailadres (HIS) gestuurd. Volg de instructies in de mail om uw Zorgmail klantnummer te bevestigen.',
            )
          } else {
            this.notificationService.success('De locatie is succesvol opgeslagen')
          }

          this.getLocaties()
        }
      })
  }

  verwijderLocatie(locatie: LocatieDto) {
    this.selectedVerwijderLocatie.set(locatie)
  }
}
