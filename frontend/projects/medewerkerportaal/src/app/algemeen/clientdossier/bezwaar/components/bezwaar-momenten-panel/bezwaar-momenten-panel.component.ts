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
import { ChangeDetectorRef, Component, computed, inject, signal, viewChild } from '@angular/core'
import {
  DsButtonComponent,
  DsButtonMenuDirective,
  DsCardComponent,
  DsCell,
  DsCellDef,
  DsColumnDef,
  DsHeaderCell,
  DsHeaderCellDef,
  DsHeaderRowComponent,
  DsHeaderRowDef,
  DsIconComponent,
  DsMenuItem,
  DsRowComponent,
  DsRowDef,
  DsTableComponent,
} from '@topicus-rgp-ds/web'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { DossierGebeurtenisDto } from '@/shared/types/algemeen/dto/dossier-gebeurtenis.dto'
import { BriefDto } from '@/shared/types/algemeen/dto/brief.dto'
import { BezwaarMomentDialogComponent } from '../bezwaar-brieven-dialog/bezwaar-moment-dialog.component'
import { Dialog } from '@angular/cdk/dialog'
import { ClientService } from '@/algemeen/services/client/client.service'
import { DocumentService } from '@/algemeen/services/document/document.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { BriefInzienDialogComponent } from '@/algemeen/components/brief-inzien-dialog/brief-inzien-dialog.component'
import { BriefActie, briefActieLabels } from '@/shared/types/algemeen/enum/brief-actie'
import { filter, fromEvent, take } from 'rxjs'
import { BriefService } from '@/algemeen/services/brief/brief.service'
import { gebeurtenisBronLabels } from '@/shared/types/algemeen/enum/gebeurtenis-bron'
import { faEllipsisVertical, faEye } from '@fortawesome/pro-regular-svg-icons'
import { faAngleRight } from '@fortawesome/pro-light-svg-icons'
import { BezwaarService } from '@algemeen/services/bezwaar/bezwaar.service'

@Component({
  selector: 'app-bezwaar-momenten-panel',
  imports: [
    DsCell,
    DsCellDef,
    DsColumnDef,
    DsHeaderCell,
    DsHeaderRowComponent,
    DsHeaderRowDef,
    DsRowComponent,
    DsRowDef,
    DsTableComponent,
    DsHeaderCellDef,
    EnumLabelPipe,
    DatumTijdPipe,
    DsIconComponent,
    DsButtonMenuDirective,
    DsButtonComponent,
    DsCardComponent,
  ],
  templateUrl: './bezwaar-momenten-panel.component.html',
  styles: `
    .ds-column-acties button {
      margin-right: var(--spacer-1);
    }
  `,
})
export class BezwaarMomentenPanelComponent {
  private readonly buttonMenuDirective = viewChild(DsButtonMenuDirective)
  private readonly dialogService = inject(Dialog)
  private readonly clientService = inject(ClientService)
  private readonly briefService = inject(BriefService)
  private readonly bezwaarService = inject(BezwaarService)
  private readonly notificatieService = inject(NotificationService)
  private readonly client = this.clientService.select('client')
  private readonly documentService = inject(DocumentService)
  private readonly cdr = inject(ChangeDetectorRef)

  protected readonly faEye = faEye
  protected readonly faEllipsisVertical = faEllipsisVertical
  protected readonly navigeerIcoon = faAngleRight
  protected readonly gebeurtenissen = signal<DossierGebeurtenisDto[]>([])
  protected readonly displayedColumns = ['bron', 'status', 'brief', 'datumTijd', 'acties', 'navigeren']
  protected readonly gebeurtenisBronLabels = gebeurtenisBronLabels
  protected readonly actiesHeaderLabel = computed(() => {
    const maxAantalActies = Math.max(0, ...this.gebeurtenissen().map((gebeurtenis) => this.aantalActies(gebeurtenis)))
    return maxAantalActies === 1 ? 'Actie' : 'Acties'
  })

  constructor() {
    this.haalGebeurtenissenOp()
  }

  protected openBrievenDialog(gebeurtenis: DossierGebeurtenisDto, toonFormulierVervangenDirect = false): void {
    const dialogRef = this.dialogService.open(BezwaarMomentDialogComponent, {
      data: {
        gebeurtenis,
        bezwaarMoment: this.client().bezwaarMomenten.find((bm) => bm.id === gebeurtenis.bezwaarMomentId),
        isMeestRecenteBezwaarMoment: this.isMeestRecenteBezwaarMoment(gebeurtenis),
        toonFormulierVervangenDirect,
      },
    })

    dialogRef?.closed.pipe(take(1)).subscribe((resultaat) => {
      const actie = resultaat as BriefActie | undefined
      if (!actie) {
        return
      }

      if (actie === BriefActie.VERVANGEN || actie === BriefActie.TEGENHOUDEN || actie === BriefActie.ACTIVEREN || actie === BriefActie.NOGMAALS_VERSTUREN) {
        this.verversHistorieEnClient()
        return
      }

      this.handleItemClick({ label: briefActieLabels[actie] }, gebeurtenis)
    })
  }

  protected inzienDocument(gebeurtenis: DossierGebeurtenisDto): void {
    if (!this.isMeestRecenteBezwaarMoment(gebeurtenis) || gebeurtenis.documentId === null) {
      return
    }

    this.documentService
      .getDocumentUrlById(gebeurtenis.documentId)
      .pipe(take(1))
      .subscribe((briefContent: string) => {
        this.dialogService.open(BriefInzienDialogComponent, {
          data: { brief: briefContent, titel: 'Formulier inzien' },
          panelClass: 'pdf-inzien-dialog',
        })
      })
  }

  getBriefActies(gebeurtenis: DossierGebeurtenisDto): DsMenuItem[][] {
    if (!this.isMeestRecenteBezwaarMoment(gebeurtenis)) {
      return [[]]
    }

    return [
      this.client()
        .bezwaarMomenten.find((bm) => bm.id === gebeurtenis.bezwaarMomentId)
        ?.briefActies.filter((briefActie) => briefActie !== BriefActie.TEMPLATE_INZIEN)
        .map((briefActie) => ({ label: briefActieLabels[briefActie] })) ?? [],
    ]
  }

  private aantalActies(gebeurtenis: DossierGebeurtenisDto): number {
    if (!this.isMeestRecenteBezwaarMoment(gebeurtenis)) {
      return 0
    }

    const aantalMenuActies = this.getBriefActies(gebeurtenis)[0].length
    const aantalInzienActies = gebeurtenis.documentId !== null ? 1 : 0
    return aantalMenuActies + aantalInzienActies
  }

  protected heeftBriefActies(gebeurtenis: DossierGebeurtenisDto): boolean {
    return this.getBriefActies(gebeurtenis)[0].length > 0
  }

  protected isMeestRecenteBezwaarMoment(gebeurtenis: DossierGebeurtenisDto): boolean {
    const bezwaarMoment = this.client().bezwaarMomenten.find((moment) => moment.id === gebeurtenis.bezwaarMomentId)
    if (!bezwaarMoment) {
      return false
    }

    return !this.client().bezwaarMomenten.some((moment) => new Date(moment.bezwaarDatum).getTime() > new Date(bezwaarMoment.bezwaarDatum).getTime())
  }

  protected handleItemClick(event: DsMenuItem, gebeurtenis: DossierGebeurtenisDto): void {
    if (!this.isMeestRecenteBezwaarMoment(gebeurtenis)) {
      return
    }

    if (event.label === briefActieLabels[BriefActie.VERVANGEN]) {
      this.openBrievenDialog(gebeurtenis, true)
      return
    }

    if (event.label === briefActieLabels[BriefActie.NOGMAALS_VERSTUREN]) {
      const bezwaarMoment = this.client().bezwaarMomenten.find((bm) => bm.bezwaarBriefId === gebeurtenis.documentId)
      if (!bezwaarMoment) {
        return
      }

      this.bezwaarService
        .verstuurBevestigingsbrievenBezwaarMomentNogmaals(bezwaarMoment.id)
        .pipe(take(1))
        .subscribe(() => {
          this.verversHistorieEnClient()
          this.notificatieService.success('Bevestigingsbrief succesvol nogmaals verstuurd')
        })
      return
    }

    const nieuwsteBrief = this.nieuwsteBrief(gebeurtenis.brieven)
    if (!nieuwsteBrief) {
      return
    }

    if (event.label === briefActieLabels[BriefActie.TEGENHOUDEN]) {
      this.briefService
        .houdBriefTegen(nieuwsteBrief.id, nieuwsteBrief.briefType)
        .pipe(take(1))
        .subscribe(() => {
          this.verversHistorieEnClient()
          this.notificatieService.success('Brief succesvol tegengehouden')
        })
    } else if (event.label === briefActieLabels[BriefActie.ACTIVEREN]) {
      this.bezwaarService
        .activeerBrief(nieuwsteBrief.id, nieuwsteBrief.briefType)
        .pipe(take(1))
        .subscribe(() => {
          this.verversHistorieEnClient()
          this.notificatieService.success('Tegenhouden brief succesvol hersteld')
        })
    }
  }

  private nieuwsteBrief(brieven: BriefDto[]): BriefDto | null {
    return brieven.reduce<BriefDto | null>((huidig, brief) => (!huidig || new Date(brief.creatieDatum).getTime() > new Date(huidig.creatieDatum).getTime() ? brief : huidig), null)
  }

  private verversHistorieEnClient(): void {
    this.haalGebeurtenissenOp()
    this.clientService.getClient(this.clientService.clientId()).pipe(take(1)).subscribe()
  }

  private haalGebeurtenissenOp(): void {
    this.clientService
      .getDossierGebeurtenissen(this.clientService.clientId(), 'bezwaar')
      .pipe(take(1))
      .subscribe((gebeurtenissen) => this.gebeurtenissen.set(gebeurtenissen))
  }

  protected setCloseOnClick(): void {
    fromEvent<MouseEvent>(document, 'mousedown')
      .pipe(
        filter((event: MouseEvent) => {
          const clickTarget = event.target as HTMLElement
          return !clickTarget.closest('.ds-context-menu') && !clickTarget.closest(`#${this.buttonMenuDirective()!.elementClass}`)
        }),
        take(1),
      )
      .subscribe(() => this.cdr.markForCheck())
  }
}
