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
import { Component, inject, signal } from '@angular/core'
import { BaseDialogComponent } from '@/shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { DossierGebeurtenisDto } from '@/shared/types/algemeen/dto/dossier-gebeurtenis.dto'
import { BezwaarLijstComponent } from '../bezwaar-lijst/bezwaar-lijst.component'
import { BezwaarMomentDto } from '@/shared/types/algemeen/dto/bezwaar-moment.dto'
import { BriefDto } from '@/shared/types/algemeen/dto/brief.dto'
import { BriefActie, briefActieLabels } from '@/shared/types/algemeen/enum/brief-actie'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { BrievenLijstComponent } from '@algemeen/clientdossier/components/brieven-lijst/brieven-lijst.component'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { FormControl, ReactiveFormsModule } from '@angular/forms'
import { BezwaarService } from '@algemeen/services/bezwaar/bezwaar.service'
import { BriefService } from '@algemeen/services/brief/brief.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { filter, switchMap, take } from 'rxjs'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'

@Component({
  imports: [BaseDialogComponent, BezwaarLijstComponent, DsButtonComponent, EnumLabelPipe, BrievenLijstComponent, SingleFileSelectorComponent, ReactiveFormsModule],
  templateUrl: './bezwaar-moment-dialog.component.html',
  styles: `
    app-bezwaar-lijst {
      border-right: 3px solid var(--stale-background-color);
    }
  `,
})
export class BezwaarMomentDialogComponent {
  protected readonly data: {
    gebeurtenis: DossierGebeurtenisDto
    bezwaarMoment?: BezwaarMomentDto
    toonFormulierVervangenDirect?: boolean
  } = inject(DIALOG_DATA)
  private readonly dialogRef = inject(DialogRef)
  private readonly bezwaarService = inject(BezwaarService)
  private readonly briefService = inject(BriefService)
  private readonly notificatieService = inject(NotificationService)
  private readonly dialogService = inject(Dialog)

  protected readonly briefActies = signal<BriefActie[]>((this.data.bezwaarMoment?.briefActies ?? []).filter((actie) => actie !== BriefActie.TEMPLATE_INZIEN))
  protected readonly brieven = signal<BriefDto[]>(this.sorteerOpCreatieDatum(this.data.gebeurtenis.brieven))
  protected readonly briefActieLabels = briefActieLabels
  protected readonly laatsteActie = signal<BriefActie | undefined>(undefined)

  protected readonly toonFormulierVervangen = signal(this.data.toonFormulierVervangenDirect ?? false)
  readonly uploadFormulierCtrl = new FormControl<File | null>(null)

  constructor() {
    this.uploadFormulierCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter((file: File | null) => file != null && this.uploadFormulierCtrl.valid),
      )
      .subscribe((file: File | null) => {
        this.vervangDocument(file!)
      })
  }

  sluiten() {
    this.dialogRef.close(this.laatsteActie())
  }

  handelActieAf(actie: BriefActie) {
    if (actie === BriefActie.TEGENHOUDEN) {
      this.tegenhoudenBrief()
      return
    }
    if (actie === BriefActie.ACTIVEREN) {
      this.activeerBrief()
      return
    }
    if (actie === BriefActie.NOGMAALS_VERSTUREN) {
      this.nogmaalsVersturenBrief()
      return
    }
    if (actie === BriefActie.VERVANGEN) {
      this.toggleFormulierVervangen()
      return
    }
    this.dialogRef.close(actie)
  }

  private tegenhoudenBrief() {
    const brief = this.brieven()[0]
    if (!brief) {
      return
    }
    this.briefService
      .houdBriefTegen(brief.id, brief.briefType)
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('Brief succesvol tegengehouden')
        this.briefActies.update((acties) => acties.filter((actie) => actie !== BriefActie.TEGENHOUDEN).concat(BriefActie.ACTIVEREN))
        this.zetTegengehouden(brief.id, true)
        this.laatsteActie.set(BriefActie.TEGENHOUDEN)
      })
  }

  private activeerBrief() {
    const brief = this.brieven()[0]
    if (!brief) {
      return
    }
    this.bezwaarService
      .activeerBrief(brief.id, brief.briefType)
      .pipe(take(1))
      .subscribe(() => {
        this.notificatieService.success('Tegenhouden brief succesvol hersteld')
        this.briefActies.update((acties) => acties.filter((actie) => actie !== BriefActie.ACTIVEREN).concat(BriefActie.TEGENHOUDEN))
        this.zetTegengehouden(brief.id, false)
        this.laatsteActie.set(BriefActie.ACTIVEREN)
      })
  }

  private zetTegengehouden(briefId: number, tegengehouden: boolean) {
    this.brieven.update((brieven) => brieven.map((brief) => (brief.id === briefId ? { ...brief, tegengehouden } : brief)))
  }

  private nogmaalsVersturenBrief() {
    const bezwaarMomentId = this.data.bezwaarMoment?.id
    if (bezwaarMomentId == null) {
      return
    }
    this.bezwaarService
      .verstuurBevestigingsbrievenBezwaarMomentNogmaals(bezwaarMomentId)
      .pipe(take(1))
      .subscribe((brieven) => {
        this.notificatieService.success('Bevestigingsbrief succesvol nogmaals verstuurd')
        this.brieven.update((oorspronkelijkeBrieven) =>
          oorspronkelijkeBrieven.map((brief) => {
            if (brieven.some((nieuweBrief) => nieuweBrief.id === brief.id)) {
              return brieven.find((nieuweBrief) => nieuweBrief.id === brief.id)!
            }
            return brief
          }),
        )
        this.laatsteActie.set(BriefActie.NOGMAALS_VERSTUREN)
      })
  }

  private sorteerOpCreatieDatum(brieven: BriefDto[]): BriefDto[] {
    return [...brieven].sort((a, b) => new Date(b.creatieDatum).getTime() - new Date(a.creatieDatum).getTime())
  }

  private toggleFormulierVervangen(): void {
    this.toonFormulierVervangen.update((vervangen) => !vervangen)
  }

  vervangDocument(file: File) {
    const bezwaarBriefId = this.data.bezwaarMoment?.bezwaarBriefId
    if (file == null || this.uploadFormulierCtrl.invalid || bezwaarBriefId == null) {
      return
    }

    this.dialogService
      .open(ConfirmationDialogComponent, { data: { title: 'Bevestiging', body: 'Weet u zeker dat u het document wilt vervangen?' } })
      .closed.pipe(
        take(1),
        filter((bevestiging: unknown) => bevestiging === true),
        switchMap(() => this.bezwaarService.vervangBezwaarDocument(bezwaarBriefId, file)),
      )
      .subscribe({
        next: () => {
          this.notificatieService.success('Formulier succesvol vervangen')
          this.dialogRef.close(BriefActie.VERVANGEN)
        },
        error: () => this.notificatieService.error('Er is een fout opgetreden bij het vervangen van het formulier'),
      })
  }
}
