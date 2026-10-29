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
import { Component, computed, inject } from '@angular/core'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { filter, map, Observable, of, switchMap, take } from 'rxjs'
import { DsButtonComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { BezwaarLijstComponent } from '@algemeen/clientdossier/bezwaar/components/bezwaar-lijst/bezwaar-lijst.component'
import { ClientService } from '@algemeen/services/client/client.service'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { BezwaarService } from '@algemeen/services/bezwaar/bezwaar.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { StringsService } from '@shared/strings'
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { algemeneBezwaarTypes, BezwaarType } from '@shared/types/algemeen/enum/bezwaar-type'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { BezwaarKeuze, BezwaarVerwerkenDto } from '@shared/types/algemeen/dto/bezwaar-verwerken.dto'
import { extensieValidator } from '@shared/validators/file/file.validator'
import { bevatBezwaar, zijnBezwarenGelijk } from '@shared/utils/bezwaar-utils'

@Component({
  imports: [BaseDialogComponent, BezwaarLijstComponent, DsButtonComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule, SingleFileSelectorComponent],
  selector: 'app-bezwaar-verwerken-dialog',
  styleUrl: './bezwaar-verwerken-dialog.component.scss',
  templateUrl: './bezwaar-verwerken-dialog.component.html',
})
export class BezwaarVerwerkenDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientService = inject(ClientService)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly bezwaarService = inject(BezwaarService)
  private readonly notificationService = inject(NotificationService)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly dialog = inject(Dialog)
  protected readonly strings = inject(StringsService).getStrings()
  private readonly bezwaarStrings = this.strings.algemeen.clientDossier.acties.bezwaar

  protected readonly gegevensVerwijderen = inject<boolean | null>(DIALOG_DATA, { optional: true }) === true
  protected readonly titel = this.gegevensVerwijderen ? this.bezwaarStrings.gegevensVerwijderenDialog.titel : this.bezwaarStrings.verwerkenDialog.titel
  protected readonly verwerkKnopTekst = this.gegevensVerwijderen ? this.strings.algemeen.buttons.verwijderen : this.strings.algemeen.buttons.verwerken
  private readonly bezwaarTypes = this.gegevensVerwijderen ? this.getToegestaneVerwijderBezwaarTypes() : algemeneBezwaarTypes

  private readonly clientId = this.clientService.clientId()
  private readonly bezwaarMomenten = computed(() => this.clientService.select('client')()?.bezwaarMomenten ?? [])
  protected readonly actiefBezwaarMoment = computed(() => this.bezwaarMomenten().find((bm) => bm.status === AanvraagBriefStatus.VERWERKT))
  private readonly initieleBezwaren = (this.actiefBezwaarMoment()?.bezwaren ?? []).filter((bezwaar) => bezwaar.type !== BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER)
  protected readonly bezwaarOpties = toSignal(
    this.clientService.getDoelgroepBvos(this.clientId).pipe(map((doelgroepBvos) => this.bezwaarService.getBewerkbareBezwaren(doelgroepBvos, this.bezwaarTypes))),
    {
      initialValue: [],
    },
  )

  protected readonly bezwaarForm = this.formBuilder.group({
    bezwaren: this.formBuilder.nonNullable.control<BezwaarKeuze[]>(this.initieleBezwaren),
    bestand: this.formBuilder.control<File | null>(null, [Validators.required, extensieValidator(['pdf'])]),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected verwerken() {
    const { bezwaren, bestand, notitie } = this.bezwaarForm.getRawValue()
    if (this.bezwaarForm.invalid || !bestand) {
      return
    }
    if (!this.heeftWijziging(bezwaren)) {
      this.notificationService.error(this.bezwaarStrings.verwerkenDialog.notifications.nietGewijzigd)
      return
    }
    this.bevestigVerwijderen()
      .pipe(
        filter((bevestigd) => bevestigd),
        switchMap(() => this.verstuur({ bezwaren: this.alleenBewerkbareBezwaren(bezwaren), notitie }, bestand)),
        switchMap(() => this.clientService.getClient(this.clientId)),
      )
      .subscribe(() => {
        this.notificationService.success(
          this.gegevensVerwijderen ? this.bezwaarStrings.verwerkenDialog.notifications.gegevensVerwijderd : this.bezwaarStrings.verwerkenDialog.notifications.succes,
        )
        this.dialogRef.close(true)
      })
  }

  private bevestigVerwijderen(): Observable<boolean> {
    if (!this.gegevensVerwijderen) {
      return of(true)
    }
    return this.dialog
      .open<boolean>(ConfirmationDialogComponent, {
        data: { title: this.titel, body: this.bezwaarStrings.gegevensVerwijderenDialog.waarschuwing },
        hasBackdrop: true,
      })
      .closed.pipe(
        take(1),
        map((bevestigd) => bevestigd === true),
      )
  }

  private verstuur(gegevens: BezwaarVerwerkenDto, bestand: File) {
    return this.gegevensVerwijderen
      ? this.contactActiesService.verwijderGegevens(this.clientId, gegevens, bestand)
      : this.contactActiesService.verwerkBezwaar(this.clientId, gegevens, bestand)
  }

  private alleenBewerkbareBezwaren(bezwaren: BezwaarKeuze[]) {
    return bezwaren.filter((bezwaar) => bevatBezwaar(this.bezwaarOpties(), bezwaar))
  }

  private heeftWijziging(bezwaren: BezwaarKeuze[]) {
    const gekozenBezwaren = this.alleenBewerkbareBezwaren(bezwaren)
    if (this.gegevensVerwijderen) {
      return gekozenBezwaren.some(
        (bezwaar) =>
          bezwaar.type === BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER || (bezwaar.type === BezwaarType.GEEN_OPNAME_UIT_BPR && !bevatBezwaar(this.initieleBezwaren, bezwaar)),
      )
    }
    return !zijnBezwarenGelijk(gekozenBezwaren, this.alleenBewerkbareBezwaren(this.initieleBezwaren))
  }

  private getToegestaneVerwijderBezwaarTypes() {
    const verwijderRechten: [Recht, BezwaarType][] = [
      [Recht.CLIENT_DOSSIER_VERWIJDEREN, BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER],
      [Recht.MEDEWERKER_BEZWAAR_BRP, BezwaarType.GEEN_OPNAME_UIT_BPR],
    ]
    return verwijderRechten
      .filter(([recht]) =>
        this.autorisatieService.isToegestaan({
          recht: [recht],
          actie: Actie.VERWIJDEREN,
          required: Required.ANY,
          bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
        }),
      )
      .map(([, bezwaarType]) => bezwaarType)
  }
}
