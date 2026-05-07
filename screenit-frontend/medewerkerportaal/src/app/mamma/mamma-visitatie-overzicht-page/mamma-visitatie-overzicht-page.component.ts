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
import { AfterViewInit, Component, DestroyRef, inject, signal, viewChild } from '@angular/core'
import { DatePipe, TitleCasePipe } from '@angular/common'
import { Dialog } from '@angular/cdk/dialog'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Actie } from '@shared/types/autorisatie/actie'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { OrganisatieType } from '@/shared/types/algemeen/organisatie-type'
import { Required } from '@shared/types/autorisatie/required'
import { ReactiveFormsModule } from '@angular/forms'
import { MammaVisitatieService } from '@/mamma/mamma-visitatie-overzicht-page/services/mamma-visitatie.service'
import { NL_DATE_TIME_FORMAT } from '@shared/constants'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { MammaVisitatieBewerkenDialogComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatie-bewerken-dialog/mamma-visitatie-bewerken-dialog.component'
import { SorteerRichting } from '@shared/types/sorteer-richting'
import { MammaVisitatieWerklijstFilter } from '@shared/types/mamma/mamma-visitatie-werklijst-filter'
import { MammaVisitatieDto } from '@shared/types/mamma/dto/visitatie/mamma-visitatie.dto'
import { PagineringDto } from '@shared/types/paginering'
import { MammaVisitatieFilterComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatie-filter/mamma-visitatie-filter.component'
import { filter, switchMap, take } from 'rxjs'
import { WicketUtils } from '@shared/utils/wicket/wicket-utils'
import { MammaVisitatieVerwijderenDialogComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatie-verwijderen-dialog/mamma-visitatie-verwijderen-dialog.component'
import { Recht } from '@shared/types/autorisatie/recht'
import { DocumentService } from '@/shared/services/document/document.service'
import { PagedResponse } from '@/shared/types/paged-response'
import { SorteerParameterDto } from '@/shared/types/sort-param'
import { NotificationService } from '@shared/services/notification/notification.service'
import { MammaVisitatieStatus } from '@/shared/types/mamma/mamma-visitatie-status'
import {
  DsButtonComponent,
  DsButtonMenuDirective,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsContextMenuItem,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsPageEvent,
  DsPaginatorComponent,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { MatSort, MatSortHeader, Sort } from '@angular/material/sort'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import { faAdd, faDownload, faList, faTrashCan, faUpRightFromSquare } from '@fortawesome/pro-light-svg-icons'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { MammaVisitatielijstDialogData } from '@shared/types/mamma/dto/visitatie/mamma-visitatielijst-dialog-data'
import { MammaVisitatielijstGenererenDialogComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatielijst-genereren-dialog/mamma-visitatielijst-genereren-dialog.component'
import { MammaVisitatielijstRapportDialogComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatielijst-rapport-dialog/mamma-visitatielijst-rapport-dialog.component'
import { PageComponent } from '@shared/components/page/page.component'

@Component({
  selector: 'app-mamma-visitatie-overzicht-page',
  imports: [
    DatePipe,
    ReactiveFormsModule,
    TitleCasePipe,
    MammaVisitatieFilterComponent,
    DsIconComponent,
    DsButtonComponent,
    DsTableComponent,
    MatSort,
    DsHeaderCellDef,
    DsColumnDef,
    DsCellDef,
    DsCell,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsRowDef,
    DsHeaderRowDef,
    DsRowComponent,
    DsButtonMenuDirective,
    DsPaginatorComponent,
    MatSortHeader,
    AutorisatieDirective,
    PageComponent,
  ],
  templateUrl: './mamma-visitatie-overzicht-page.component.html',
  styleUrl: './mamma-visitatie-overzicht-page.component.scss',
})
export class MammaVisitatieOverzichtPageComponent implements AfterViewInit {
  private readonly dialog = inject(Dialog)
  private readonly visitatieService = inject(MammaVisitatieService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly documentenService = inject(DocumentService)
  private readonly notificationService = inject(NotificationService)
  private readonly destroyRef = inject(DestroyRef)

  private sort = viewChild(MatSort)

  protected readonly faAddIcon: IconDefinition = faAdd
  protected readonly faListIcon: IconDefinition = faList
  protected readonly faDownloadIcon: IconDefinition = faDownload
  protected readonly faPopoutIcon: IconDefinition = faUpRightFromSquare
  protected readonly faDeleteIcon: IconDefinition = faTrashCan

  displayedColumns = [
    'omschrijving',
    'gestartOp',
    'afgerondOp',
    'aangemaaktOp',
    'aangemaaktDoor',
    'status',
    'beoordelingsEenheid.naam',
    'rapportage',
    'vragenlijst',
    'inzien',
    'verwijderen',
  ]
  toevoegenMenuItems: DsContextMenuItem[][] = [[new DsContextMenuItem({ label: 'BE' }), new DsContextMenuItem({ label: 'Insteltechniek' })]]

  protected inzienConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE],
    required: Required.ANY,
  }

  protected insteltechniekAanpassenConstraint: SecurityConstraint = {
    ...this.inzienConstraint,
    recht: [Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK],
    actie: Actie.AANPASSEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
    organisatieTypeScopes: [OrganisatieType.RIVM],
  }

  protected toevoegenConstraint: SecurityConstraint = {
    ...this.inzienConstraint,
    organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.SCREENINGSORGANISATIE],
    actie: Actie.TOEVOEGEN,
  }

  protected verwijderenConstraint: SecurityConstraint = {
    ...this.inzienConstraint,
    actie: Actie.VERWIJDEREN,
  }

  protected alleVisitaties = signal<MammaVisitatieDto[]>([])
  protected paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 20, totaal: 0 })
  protected sortering = signal<SorteerParameterDto>({ veld: 'omschrijving', richting: SorteerRichting.ASC })
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  private selectedFilter: MammaVisitatieWerklijstFilter | undefined

  get isKwaliteitsplatform(): boolean {
    return this.autorisatieService.heeftOrganisatieType(OrganisatieType.KWALITEITSPLATFORM)
  }

  get magInzien(): boolean {
    return !this.autorisatieService.heeftOrganisatieType(OrganisatieType.RIVM)
  }

  get magToevoegen(): boolean {
    return !this.isKwaliteitsplatform && this.autorisatieService.isToegestaan(this.toevoegenConstraint)
  }

  get magVerwijderen(): boolean {
    return !this.isKwaliteitsplatform && this.autorisatieService.isToegestaan(this.verwijderenConstraint)
  }

  get magDetailsInzien(): boolean {
    return this.autorisatieService.isToegestaan(this.inzienConstraint) && !this.isKwaliteitsplatform
  }

  openVisitatieToevoegenDialog(visitatieType: string): void {
    this.dialog
      .open(MammaVisitatieBewerkenDialogComponent, { data: { visitatie: {} as MammaVisitatieDto, visitatieType } })
      .closed.pipe(
        take(1),
        filter((res) => !!res),
      )
      .subscribe((res) => this.alleVisitaties.update((visitaties) => [...visitaties, res as MammaVisitatieDto]))
  }

  openVisitatieEditDialog(visitatie: MammaVisitatieDto) {
    if (this.autorisatieService.isToegestaan(this.inzienConstraint) && !this.isKwaliteitsplatform) {
      if (visitatie.status !== MammaVisitatieStatus.UITGEVOERD) {
        this.dialog
          .open(MammaVisitatieBewerkenDialogComponent, { data: { visitatie, visitatieType: visitatie.beoordelingseenheid ? 'be' : 'insteltechniek' } })
          .closed.pipe(
            take(1),
            filter((res) => !!res),
          )
          .subscribe((res) => {
            this.alleVisitaties.update((visitaties) =>
              visitaties.map((visitatieItem) => {
                if (visitatieItem.id === (res as MammaVisitatieDto).id) {
                  return res as MammaVisitatieDto
                }
                return visitatieItem
              }),
            )
          })
      } else {
        this.notificationService.warning('Visitatie is al uitgevoerd.')
      }
    }
  }

  openVerwijderenDialog(visitatie: MammaVisitatieDto): void {
    if (!this.magVerwijderen) {
      return
    }

    this.dialog
      .open(MammaVisitatieVerwijderenDialogComponent, { data: visitatie })
      .closed.pipe(
        take(1),
        filter((res) => !!res),
        switchMap(() => this.visitatieService.verwijderVisitatie(visitatie)),
      )
      .subscribe(() => this.alleVisitaties.update((visitaties) => visitaties.filter((visitatieItem) => visitatieItem.id !== visitatie.id)))
  }

  openVisitatielijstGenererenDialog() {
    this.dialog
      .open<MammaVisitatielijstDialogData>(MammaVisitatielijstGenererenDialogComponent)
      .closed.pipe(
        take(1),
        filter((res) => !!res),
      )
      .subscribe((res: MammaVisitatielijstDialogData) => this.openVisitatielijstRapportDialog(res))
  }

  private openVisitatielijstRapportDialog(data: MammaVisitatielijstDialogData): void {
    this.dialog
      .open(MammaVisitatielijstRapportDialogComponent, { data })
      .closed.pipe(
        take(1),
        filter((res) => !!res),
      )
      .subscribe((res) => this.alleVisitaties.update((visitaties) => [...visitaties, ...(res as MammaVisitatieDto[])]))
  }

  navigeerNaarVisitatie(visitatieId: number) {
    WicketUtils.toVisitatie(visitatieId)
  }

  filterValidaties(filter: MammaVisitatieWerklijstFilter) {
    this.selectedFilter = filter
    this.paginering.update((paginering) => ({ ...paginering, paginaNummer: 1 }))
    this.getVisitaties()
  }

  ngAfterViewInit(): void {
    this.sort()
      ?.sortChange.pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe((sortEvent: Sort) => {
        if (!sortEvent.direction) {
          this.sortering.set({ veld: 'omschrijving', richting: SorteerRichting.ASC })
        } else {
          this.sortering.set({
            veld: sortEvent.active,
            richting: sortEvent.direction === 'asc' ? SorteerRichting.ASC : SorteerRichting.DESC,
          })
        }

        this.paginering.update((paginering) => ({ ...paginering, paginaNummer: 1 }))

        this.getVisitaties()
      })
  }

  private getVisitaties() {
    if (!this.selectedFilter) {
      return
    }

    this.visitatieService
      .getVisitaties(this.selectedFilter, this.sortering(), this.paginering())
      .pipe(take(1))
      .subscribe((response: PagedResponse<MammaVisitatieDto[]>) => {
        this.paginering.set(response.paginering)
        this.alleVisitaties.set(response.data)
      })
  }

  downloadDocument(documentId: number, bestandsnaam: string) {
    this.documentenService.download(documentId, bestandsnaam)
  }

  handlePageChange($event: DsPageEvent): void {
    const huidigPaginaGrootte = this.paginering().paginaGrootte
    const paginaGrootteGewijzigd = $event.pageSize !== huidigPaginaGrootte

    const paginaIndex = paginaGrootteGewijzigd ? 0 : $event.pageIndex

    this.paginering.update((paginering) => ({
      ...paginering,
      paginaNummer: paginaIndex + 1,
      paginaGrootte: $event.pageSize,
    }))

    this.getVisitaties()
  }
}
