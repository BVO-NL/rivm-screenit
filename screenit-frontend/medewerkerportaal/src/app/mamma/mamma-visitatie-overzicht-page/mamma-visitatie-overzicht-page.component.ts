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
import { Component, computed, inject, signal } from '@angular/core'
import { DatePipe, TitleCasePipe } from '@angular/common'
import { ClrDatagridModule, ClrDatagridStateInterface, ClrDropdownModule, ClrIconModule } from '@clr/angular'
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
import { MammaVisitatieDto } from '@shared/types/mamma/dto/mamma-visitatie.dto'
import { PagineringDto } from '@shared/types/paginering'
import { MammaVisitatieFilterComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatie-filter/mamma-visitatie-filter.component'
import { filter, switchMap, take } from 'rxjs'
import { WicketUtils } from '@shared/utils/wicket/wicket-utils'
import { MammaVisitatieVerwijderenDialogComponent } from '@/mamma/mamma-visitatie-overzicht-page/components/mamma-visitatie-verwijderen-dialog/mamma-visitatie-verwijderen-dialog.component'
import { Recht } from '@shared/types/autorisatie/recht'
import { DocumentService } from '@/shared/services/document/document.service'
import { PagedResponse } from '@/shared/types/paged-response'
import { SorteerParameterDto } from '@/shared/types/sort-param'
import { ToastService } from '@/shared/toast/service/toast.service'
import { MammaVisitatieStatus } from '@/shared/types/mamma/mamma-visitatie-status'

@Component({
  selector: 'app-mamma-visitatie-overzicht-page',
  imports: [ClrDatagridModule, DatePipe, ClrDatagridModule, ReactiveFormsModule, ClrIconModule, TitleCasePipe, MammaVisitatieFilterComponent, ClrDropdownModule],
  templateUrl: './mamma-visitatie-overzicht-page.component.html',
  styleUrl: './mamma-visitatie-overzicht-page.component.scss',
})
export class MammaVisitatieOverzichtPageComponent {
  private readonly dialog = inject(Dialog)
  private readonly visitatieService = inject(MammaVisitatieService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly documentenService = inject(DocumentService)
  private readonly toastService = inject(ToastService)

  protected inzienConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK],
    actie: Actie.INZIEN,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.MAMMA],
    level: ToegangLevel.LANDELIJK,
    organisatieTypeScopes: [OrganisatieType.RIVM, OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE],
    required: Required.ANY,
  }

  protected toevoegenConstraint: SecurityConstraint = {
    ...this.inzienConstraint,
    actie: Actie.TOEVOEGEN,
  }

  protected verwijderenConstraint: SecurityConstraint = {
    ...this.inzienConstraint,
    actie: Actie.VERWIJDEREN,
  }
  protected visitaties = signal<MammaVisitatieDto[]>([])
  protected readonly NL_DATE_TIME_FORMAT = NL_DATE_TIME_FORMAT
  protected paginering = signal<PagineringDto>({ paginaNummer: 1, paginaGrootte: 20, totaal: 0 })
  protected laatstePagina = computed(() => (this.paginering().totaal > 0 ? Math.ceil(this.paginering().totaal / this.paginering().paginaGrootte) : 1))
  private filter: MammaVisitatieWerklijstFilter | undefined
  protected sortering = signal<SorteerParameterDto>({ veld: 'omschrijving', richting: SorteerRichting.ASC })

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

  openVisitatieToevoegenDialog(visitatieType: string): void {
    this.dialog
      .open(MammaVisitatieBewerkenDialogComponent, { data: { visitatie: {} as MammaVisitatieDto, visitatieType } })
      .closed.pipe(
        take(1),
        filter((res) => !!res),
      )
      .subscribe((res) => this.visitaties.update((visitaties) => [...visitaties, res as MammaVisitatieDto]))
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
            this.visitaties.update((visitaties) =>
              visitaties.map((v) => {
                if (v.id === (res as MammaVisitatieDto).id) {
                  return res as MammaVisitatieDto
                }
                return v
              }),
            )
          })
      } else {
        this.toastService.warning('Visitatie is al uitgevoerd.')
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
      .subscribe(() => this.visitaties.update((visitaties) => visitaties.filter((v) => v.id !== visitatie.id)))
  }

  navigeerNaarVisitatie(visitatieId: number) {
    WicketUtils.toVisitatie(visitatieId)
  }

  filterValidaties(filter: MammaVisitatieWerklijstFilter) {
    this.filter = filter
    this.getVisitaties()
  }

  private getVisitaties() {
    if (!this.filter) {
      return
    }

    this.visitatieService
      .getVisitaties(this.filter, this.sortering(), this.paginering())
      .pipe(take(1))
      .subscribe((response: PagedResponse<MammaVisitatieDto[]>) => {
        this.paginering.set(response.paginering)
        this.visitaties.set(response.data)
      })
  }

  refreshData(state: ClrDatagridStateInterface<MammaVisitatieDto>) {
    this.paginering.update((paginering) => ({ ...paginering, paginaNummer: state.page?.current ?? 1 }))
    this.sortering.set({ veld: (state.sort?.by as string) ?? 'omschrijving', richting: state.sort?.reverse ? SorteerRichting.DESC : SorteerRichting.ASC })
    this.getVisitaties()
  }

  downloadDocument(documentId: number, bestandsnaam: string) {
    this.documentenService.download(documentId, bestandsnaam)
  }

  protected readonly OrganisatieType = OrganisatieType
}
