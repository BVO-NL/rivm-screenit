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
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { Component, inject, signal } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { filter, switchMap, take } from 'rxjs'
import type { CheckboxValueModel } from '@topicus-rgp-ds/web'
import { DsButtonComponent, DsCheckboxArrayComponent, DsDropdownComponent, DsIconComponent } from '@topicus-rgp-ds/web'
import { faPaperPlane } from '@fortawesome/pro-solid-svg-icons'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { DocumentService } from '@algemeen/services/document/document.service'
import { BriefInzienDialogComponent } from '@algemeen/components/brief-inzien-dialog/brief-inzien-dialog.component'
import { NotificationService } from '@shared/services/notification/notification.service'
import { StringsService } from '@shared/strings'
import { BriefType, briefTypeLabels } from '@shared/types/algemeen/enum/brief-type'
import { OverdrachtType } from '@shared/types/algemeen/enum/overdracht-type'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { alleBevolkingsonderzoeken } from '@shared/types/bevolkingsonderzoek'
import { OverdrachtPersoonsgegevensKeuzesDto, OverdrachtPersoonsgegevensVerwerkenDto } from '@shared/types/algemeen/dto/overdracht-persoonsgegevens.dto'

export interface OverdrachtPersoonsgegevensKeuzesVerwerkenDialogData {
  clientId: number
  gegevens: OverdrachtPersoonsgegevensVerwerkenDto
}

type BvoKeuze = 'bkGegevens' | 'bkBeelden' | 'bmhkGegevens' | 'dkGegevens'

enum OverdrachtVia {
  CLIENTPORTAAL = 'CLIENTPORTAAL',
  MEDEWERKER = 'MEDEWERKER',
}

@Component({
  selector: 'app-overdracht-persoonsgegevens-keuzes-verwerken-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsCheckboxArrayComponent, DsDropdownComponent, ReactiveFormsModule, SingleFileSelectorComponent, DsIconComponent],
  templateUrl: './overdracht-persoonsgegevens-keuzes-verwerken-dialog.component.html',
  styles: `
    .verstuurde-brieven {
      list-style: disc;
      margin-left: var(--spacer-3);
      margin-bottom: var(--spacer-3);
    }
  `,
})
export class OverdrachtPersoonsgegevensKeuzesVerwerkenDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly documentService = inject(DocumentService)
  private readonly dialog = inject(Dialog)
  private readonly notificationService = inject(NotificationService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly data = inject<OverdrachtPersoonsgegevensKeuzesVerwerkenDialogData>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly gegevens = this.data.gegevens
  protected readonly alleenLezen = this.gegevens.type === OverdrachtType.CLIENTPORTAAL || this.gegevens.status === AanvraagBriefStatus.VERWERKT
  protected readonly briefTypeLabels = briefTypeLabels
  protected readonly medewerkerToegestaan = this.gegevens.status === AanvraagBriefStatus.BRIEF || this.gegevens.status === AanvraagBriefStatus.BRIEF_ONTVANGEN
  protected readonly inzageStrings = this.strings.algemeen.clientDossier.acties.overdrachtPersoonsgegevens.keuzesVerwerkenDialog
  protected readonly titel = this.alleenLezen ? this.inzageStrings.titelAfgerond : this.inzageStrings.titel
  protected readonly ontvangen = this.gegevens.status === AanvraagBriefStatus.BRIEF_ONTVANGEN
  protected readonly aanvraagVerwerkt = this.gegevens.status === AanvraagBriefStatus.VERWERKT && this.gegevens.ontvangenFormulierId !== null
  protected readonly formulierInzienBeschikbaar = (this.aanvraagVerwerkt || this.ontvangen) && this.gegevens.ontvangenFormulierId !== null
  protected readonly faPaperPlane = faPaperPlane
  protected readonly magGeenHandtekeningBriefAanvragen =
    this.gegevens.status === AanvraagBriefStatus.BRIEF &&
    this.gegevens.verstuurdeBrieven.find((brief) => brief.briefType === BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_HANDTEKENING) === undefined

  protected readonly magDownloadenEnAfronden = this.autorisatieService.isToegestaan({
    recht: [Recht.MEDEWERKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS],
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: alleBevolkingsonderzoeken,
    actie: Actie.INZIEN,
    required: Required.ALL,
  })

  protected readonly keuzesOpties: CheckboxValueModel<BvoKeuze>[] = [
    { label: this.inzageStrings.bkGegevens, value: 'bkGegevens', disabled: !this.gegevens.heeftMammaDossier, selected: this.gegevens.keuzes.bkGegevens },
    { label: this.inzageStrings.bkBeelden, value: 'bkBeelden', disabled: !this.gegevens.heeftMammaDossier, selected: this.gegevens.keuzes.bkBeelden },
    { label: this.inzageStrings.bmhkGegevens, value: 'bmhkGegevens', disabled: !this.gegevens.heeftCervixDossier, selected: this.gegevens.keuzes.bmhkGegevens },
    { label: this.inzageStrings.dkGegevens, value: 'dkGegevens', disabled: !this.gegevens.heeftColonDossier, selected: this.gegevens.keuzes.dkGegevens },
  ]

  private readonly medewerkerOptieBeschikbaar = this.medewerkerToegestaan || this.gegevens.type === OverdrachtType.AANVRAAGFORMULIER
  protected readonly overdrachtViaOpties: { label: string; value: OverdrachtVia }[] = [
    { label: this.inzageStrings.clientportaal, value: OverdrachtVia.CLIENTPORTAAL },
    ...(this.medewerkerOptieBeschikbaar ? [{ label: this.inzageStrings.medewerker, value: OverdrachtVia.MEDEWERKER }] : []),
  ]
  private readonly startOverdrachtVia =
    this.gegevens.type !== OverdrachtType.CLIENTPORTAAL && this.medewerkerOptieBeschikbaar ? OverdrachtVia.MEDEWERKER : OverdrachtVia.CLIENTPORTAAL

  protected readonly form = this.formBuilder.group({
    overdrachtVia: this.formBuilder.nonNullable.control<OverdrachtVia>(this.startOverdrachtVia, Validators.required),
    keuzes: this.formBuilder.nonNullable.control<CheckboxValueModel<BvoKeuze>[]>(this.keuzesOpties, Validators.required),
    bestand: this.formBuilder.control<File | null>(null),
  })
  protected readonly vervangBestand = this.formBuilder.control<File | null>(null)
  protected readonly kanFormulierVervangen =
    this.ontvangen &&
    this.gegevens.ontvangenFormulierId !== null &&
    this.autorisatieService.isToegestaan({
      recht: [Recht.VERVANGEN_DOCUMENTEN],
      actie: Actie.AANPASSEN,
      required: Required.ALL,
      bevolkingsonderzoekScopes: alleBevolkingsonderzoeken,
    })
  protected readonly vervangenZichtbaar = signal(false)

  constructor() {
    this.vervangBestand.valueChanges
      .pipe(
        filter((bestand) => bestand !== null && this.vervangBestand.valid),
        takeUntilDestroyed(),
      )
      .subscribe(() => this.formulierVervangen())
    if (this.gegevens.status !== null) {
      this.form.get('overdrachtVia')?.disable()
    }
    if (this.alleenLezen) {
      this.form.disable()
    }
  }

  protected isMedewerker(): boolean {
    return this.form.getRawValue().overdrachtVia === OverdrachtVia.MEDEWERKER
  }

  protected kanVerwerken(): boolean {
    const heeftKeuzes = this.geselecteerdeKeuzes().length > 0
    const isMedewerker = this.isMedewerker()
    const heeftBestand = this.form.get('bestand')?.value !== null
    const bestandIsValide = this.form.get('bestand')?.valid ?? false
    return heeftKeuzes && (!isMedewerker || (heeftBestand && bestandIsValide))
  }

  private geselecteerdeKeuzes(): BvoKeuze[] {
    return this.form
      .getRawValue()
      .keuzes.filter((keuze) => keuze.selected)
      .map((keuze) => keuze.value)
  }

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected verwerk() {
    if (!this.kanVerwerken()) {
      return
    }
    const { overdrachtVia, bestand } = this.form.getRawValue()
    const gekozen = this.geselecteerdeKeuzes()
    const keuzes: OverdrachtPersoonsgegevensKeuzesDto = {
      bkGegevens: gekozen.includes('bkGegevens'),
      bkBeelden: gekozen.includes('bkBeelden'),
      bmhkGegevens: gekozen.includes('bmhkGegevens'),
      dkGegevens: gekozen.includes('dkGegevens'),
    }
    const verwerking$ =
      overdrachtVia === OverdrachtVia.MEDEWERKER
        ? this.contactActiesService.verwerkInzageKeuzesViaFormulier(this.data.clientId, keuzes, bestand)
        : this.contactActiesService.verwerkInzageKeuzesViaClientportaal(this.data.clientId, keuzes)
    verwerking$.pipe(take(1)).subscribe(() => {
      this.notificationService.success(this.ontvangen ? this.inzageStrings.notifications.opgeslagen : this.inzageStrings.notifications.succes)
      this.dialogRef.close(true)
    })
  }

  protected formulierInzien() {
    const ontvangenFormulierId = this.gegevens.ontvangenFormulierId
    if (ontvangenFormulierId === null) {
      return
    }
    this.dialogRef.close()
    this.documentService
      .getDocumentUrlById(ontvangenFormulierId)
      .pipe(take(1))
      .subscribe((formulierUrl: string) => {
        this.dialog.open(BriefInzienDialogComponent, {
          data: { brief: formulierUrl, titel: this.strings.algemeen.buttons.formulierInzien },
          panelClass: 'pdf-inzien-dialog',
        })
      })
  }

  protected gegevensOphalen() {
    this.contactActiesService.downloadInzageGegevens(this.data.clientId)
  }

  protected toonFormulierVervangen() {
    this.vervangenZichtbaar.set(true)
  }

  protected formulierVervangen() {
    const bestand = this.vervangBestand.value
    if (bestand === null) {
      return
    }
    this.contactActiesService
      .vervangInzageFormulier(this.data.clientId, bestand)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.inzageStrings.notifications.vervangenSucces)
        this.dialogRef.close(true)
      })
  }

  protected rondAf() {
    this.dialog
      .open<boolean>(ConfirmationDialogComponent, {
        data: {
          title: this.inzageStrings.afrondenDialog.titel,
          body: this.inzageStrings.afrondenDialog.vraag,
          jaButtonTekst: this.inzageStrings.afrondenDialog.jaButtonTekst,
        },
        hasBackdrop: true,
      })
      .closed.pipe(
        take(1),
        filter((bevestigd: unknown) => bevestigd === true),
        switchMap(() => this.contactActiesService.rondInzageAf(this.data.clientId)),
      )
      .subscribe(() => {
        this.notificationService.success(this.inzageStrings.notifications.afgerondSucces)
        this.dialogRef.close(true)
      })
  }

  protected geenHandtekening() {
    this.contactActiesService
      .vraagInzageGeenHandtekeningBriefAan(this.data.clientId)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.inzageStrings.notifications.geenHandtekeningSucces)
        this.dialogRef.close(true)
      })
  }
}
