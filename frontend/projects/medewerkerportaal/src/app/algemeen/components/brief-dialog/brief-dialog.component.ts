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
import { Component, computed, inject, OnInit, signal } from '@angular/core'
import { HttpErrorResponse } from '@angular/common/http'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { ScreeningRondeGebeurtenisDto } from '@shared/types/algemeen/dto/screening-ronde-gebeurtenis.dto'
import { typeGebeurtenisLabels } from '@shared/types/algemeen/enum/type-gebeurtenis'
import { EnumLabelPipe } from '@shared/pipes/enum-label/enum-label.pipe'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { BriefService } from '@algemeen/services/brief/brief.service'
import { BriefActie } from '@shared/types/algemeen/enum/brief-actie'
import { take } from 'rxjs'
import { NotificationService } from '@shared/services/notification/notification.service'
import { BriefInzienDialogComponent } from '@algemeen/components/brief-inzien-dialog/brief-inzien-dialog.component'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { saveAs } from 'file-saver'

@Component({
  selector: 'app-brief-dialog',
  imports: [BaseDialogComponent, EnumLabelPipe, DsButtonComponent, DatumTijdPipe],
  templateUrl: './brief-dialog.component.html',
})
export class BriefDialogComponent implements OnInit {
  protected gebeurtenis: ScreeningRondeGebeurtenisDto = inject(DIALOG_DATA)
  protected typeGebeurtenisLabels = typeGebeurtenisLabels
  private readonly briefService = inject(BriefService)
  private readonly dialogRef = inject(DialogRef)
  private readonly notificatieService = inject(NotificationService)
  private readonly dialogService = inject(Dialog)

  private readonly briefActies = signal<BriefActie[]>([])
  protected briefOpnieuwAangemaakt = false
  protected readonly magActiveren = computed(() => this.briefActies().includes(BriefActie.ACTIVEREN))
  protected readonly magTegenhouden = computed(() => this.briefActies().includes(BriefActie.TEGENHOUDEN))
  protected readonly magOpnieuwAanmaken = computed(() => this.briefActies().includes(BriefActie.OPNIEUW_AANMAKEN))
  protected readonly magTemplateInzien = computed(() => this.briefActies().includes(BriefActie.TEMPLATE_INZIEN))
  protected readonly magVerstuurdeBriefInzien = computed(() => this.briefActies().includes(BriefActie.VERSTUURDE_BRIEF_INZIEN))

  ngOnInit() {
    this.laadBriefActies()
  }

  annuleren() {
    this.dialogRef.close(this.briefOpnieuwAangemaakt)
  }

  activeren() {
    this.briefService
      .activeerBrief(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe(() => {
        this.laadBriefActies()
        this.notificatieService.success('Brief wordt niet meer tegengehouden')
      })
  }

  tegenhouden() {
    this.briefService
      .houdBriefTegen(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe(() => {
        this.laadBriefActies()
        this.notificatieService.success('Brief wordt tegengehouden')
      })
  }

  opnieuwAanmaken() {
    this.briefService
      .maakBriefOpnieuwAan(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe(() => {
        this.briefActies.update((briefActies) => briefActies.filter((briefActie) => briefActie !== BriefActie.OPNIEUW_AANMAKEN))
        this.briefOpnieuwAangemaakt = true
        this.notificatieService.success('Brief is opnieuw aangemaakt')
      })
  }

  templateInzien() {
    this.dialogRef.close(this.briefOpnieuwAangemaakt)
    this.briefService
      .getBriefTemplate(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe((briefContent: string) => {
        this.dialogService.open(BriefInzienDialogComponent, {
          data: { brief: briefContent, titel: 'Template inzien' },
          panelClass: 'pdf-inzien-dialog',
        })
      })
  }

  verstuurdeBriefInzien() {
    this.briefService
      .getVerstuurdeBrief(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe({
        next: (brief: Blob) => saveAs(brief, 'brief.pdf'),
        error: (fout: HttpErrorResponse) =>
          this.notificatieService.error(fout.status === 401 ? 'De bewaartermijn van de verstuurde brief is verlopen.' : 'Er is een fout bij het ophalen van de brief.'),
      })
  }

  private laadBriefActies() {
    this.briefService
      .getBriefActies(this.gebeurtenis.briefId, this.gebeurtenis.briefType)
      .pipe(take(1))
      .subscribe((briefActies) => this.briefActies.set(briefActies))
  }
}
