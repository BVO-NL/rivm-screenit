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
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { OnderzoeksresultatenActieDto } from '@shared/types/algemeen/dto/onderzoeksresultaten-actie.dto'
import { DatumTijdPipe } from '@shared/pipes/datum-tijd/datum-tijd.pipe'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { filter, switchMap, take } from 'rxjs'
import { BriefInzienDialogComponent } from '@algemeen/components/brief-inzien-dialog/brief-inzien-dialog.component'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { DocumentService } from '@algemeen/services/document/document.service'
import { BezwaarService } from '@algemeen/services/bezwaar/bezwaar.service'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms'
import { NotificationService } from '@shared/services/notification/notification.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'

@Component({
  selector: 'app-verwijderde-onderzoeksresultaten-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DatumTijdPipe, AutorisatieDirective, SingleFileSelectorComponent, FormsModule, ReactiveFormsModule],
  templateUrl: './verwijderde-onderzoeksresultaten-dialog.component.html',
})
export class VerwijderdeOnderzoeksresultatenDialogComponent {
  protected readonly onderzoeksresultaat: OnderzoeksresultatenActieDto = inject(DIALOG_DATA)
  private readonly dialogRef = inject(DialogRef)
  private readonly documentService = inject(DocumentService)
  private readonly dialogService = inject(Dialog)
  private readonly bezwaarService = inject(BezwaarService)
  private readonly notificatieService = inject(NotificationService)

  protected readonly heeftBrief = this.onderzoeksresultaat.getekendeBriefId != null
  protected readonly documentVervangenConstraint: SecurityConstraint = {
    recht: [Recht.VERVANGEN_DOCUMENTEN],
    actie: Actie.AANPASSEN,
    required: Required.ANY,
    level: ToegangLevel.REGIO,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
  }
  readonly uploadDocumentCtrl = new FormControl<File | null>(null)
  protected readonly toonDocumentVervangen = signal(false)

  constructor() {
    this.uploadDocumentCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter((file: File | null) => file != null && this.uploadDocumentCtrl.valid),
      )
      .subscribe((file: File | null) => {
        this.documentVervangen(file!)
      })
  }

  inzien() {
    this.sluitDialog()
    this.documentService
      .getDocumentUrlById(this.onderzoeksresultaat.getekendeBriefId)
      .pipe(take(1))
      .subscribe((briefContent: string) => {
        this.dialogService.open(BriefInzienDialogComponent, {
          data: { brief: briefContent, titel: 'Formulier inzien' },
          panelClass: 'pdf-inzien-dialog',
        })
      })
  }

  protected nogmaalsVersturen() {
    this.bezwaarService
      .nogmaalsVersturen(this.onderzoeksresultaat.id)
      .pipe(take(1))
      .subscribe({
        next: () => {
          this.notificatieService.success('De brief is opnieuw verstuurd')
          this.sluitDialog()
        },
        error: () => this.notificatieService.error('Er is een fout opgetreden bij het opnieuw versturen van de brief'),
      })
  }

  protected toggleDocumentVervangen() {
    this.toonDocumentVervangen.update((vervangen) => !vervangen)
  }

  protected documentVervangen(file: File) {
    this.dialogService
      .open(ConfirmationDialogComponent, { data: { title: 'Bevestiging', body: 'Weet u zeker dat u het document wilt vervangen?' } })
      .closed.pipe(
        take(1),
        filter((bevestiging: unknown) => bevestiging === true),
        switchMap(() => this.bezwaarService.vervangDocument(this.onderzoeksresultaat.id, file)),
      )
      .subscribe({
        next: ({ getekendeBriefId }) => {
          this.onderzoeksresultaat.getekendeBriefId = getekendeBriefId
          this.notificatieService.success('Document is succesvol vervangen')
          this.sluitDialog()
        },
        error: () => this.notificatieService.error('Er is een fout opgetreden bij het vervangen van het document'),
      })
  }

  sluitDialog() {
    this.dialogRef.close()
  }
}
