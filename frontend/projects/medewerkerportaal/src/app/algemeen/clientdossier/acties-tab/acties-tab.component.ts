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
import { Dialog } from '@angular/cdk/dialog'
import { filter, switchMap, take } from 'rxjs'
import { DsActionCardComponent } from '@topicus-rgp-ds/web'
import { ClientPaspoortComponent } from '@algemeen/components/client-paspoort/client-paspoort.component'
import { AanhefWijzigenDialogComponent } from '@algemeen/clientdossier/acties-tab/components/aanhef-wijzigen-dialog/aanhef-wijzigen-dialog.component'
import { DeelnamewensenDialogComponent, DeelnamewensenDialogData } from '@algemeen/clientdossier/acties-tab/components/deelnamewensen-dialog/deelnamewensen-dialog.component'
import { KeuzeGebruikGegevensBriefDialogComponent } from '@algemeen/clientdossier/acties-tab/components/keuze-gebruik-gegevens-brief-dialog/keuze-gebruik-gegevens-brief-dialog.component'
import {
  AdresgegevensBewerkenModalComponent,
  AdresgegevensBewerkenModalData,
} from '@algemeen/clientdossier/components/adresgegevens-panel/adresgegevens-bewerken-modal/adresgegevens-bewerken-modal.component'
import { OpnieuwAanvragenClientgegevensDialogComponent } from '@algemeen/clientdossier/acties-tab/components/opnieuw-aanvragen-clientgegevens-dialog/opnieuw-aanvragen-clientgegevens-dialog.component'
import { HeraanmeldenBaarmoederhalskankerDialogComponent } from '@algemeen/clientdossier/acties-tab/components/heraanmelden-baarmoederhalskanker-dialog/heraanmelden-baarmoederhalskanker-dialog.component'
import { ClientService } from '@algemeen/services/client/client.service'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { ContactActieOptie } from '@shared/types/algemeen/enum/contact-actie-optie'
import { NotificationService } from '@shared/services/notification/notification.service'
import { BezwaarVerwerkenDialogComponent } from '@algemeen/clientdossier/acties-tab/components/bezwaar-verwerken-dialog/bezwaar-verwerken-dialog.component'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { StringsService } from '@shared/strings'
import { Required } from '@shared/types/autorisatie/required'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'

@Component({
  selector: 'app-acties-tab',
  imports: [ClientPaspoortComponent, DsActionCardComponent],
  templateUrl: './acties-tab.component.html',
  styleUrl: './acties-tab.component.scss',
})
export class ActiesTabComponent {
  private readonly clientService = inject(ClientService)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly dialog = inject(Dialog)
  private readonly autorisatieService = inject(AutorisatieService)
  private readonly strings = inject(StringsService).getStrings()
  private readonly notificationService = inject(NotificationService)
  private readonly clientId = this.clientService.clientId()

  private readonly contactActiesState = this.contactActiesService.select('contactActies')
  private readonly contactActies = computed(() => this.contactActiesState() ?? [])
  private readonly onActieGekozen = (actieOptie: ContactActieOptie) => this.openActie(actieOptie)

  protected readonly algemeneActies = computed(() => this.contactActiesService.maakAlgemeneActies(this.contactActies(), this.onActieGekozen))
  protected readonly mammaActies = computed(() => this.contactActiesService.maakMammaActies(this.contactActies(), this.onActieGekozen))
  protected readonly cervixActies = computed(() => this.contactActiesService.maakCervixActies(this.contactActies(), this.onActieGekozen))
  protected readonly colonActies = computed(() => this.contactActiesService.maakColonActies(this.contactActies(), this.onActieGekozen))

  constructor() {
    this.contactActiesService.set('contactActies', [])
    this.contactActiesService.getContactActies(this.clientId)
  }

  private openActie(actieType: ContactActieOptie) {
    switch (actieType) {
      case ContactActieOptie.AANPASSEN_AANHEF:
        this.openAanhefWijzigen()
        break
      case ContactActieOptie.DEELNAMEWENSEN:
        this.openDeelnamewensen()
        break
      case ContactActieOptie.CERVIX_DEELNAME_BUITEN_BVO_BMHK:
        this.openDeelnameBuitenBvoBmhk()
        break
      case ContactActieOptie.CERVIX_HERAANMELDEN:
        this.openCervixHeraanmelden()
        break
      case ContactActieOptie.TIJDELIJK_ADRES:
        this.openTijdelijkAdresWijzigen()
        break
      case ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN:
        this.dialog.open(KeuzeGebruikGegevensBriefDialogComponent, { data: this.clientId })
        break
      case ContactActieOptie.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS:
        this.openOpnieuwAanvragenClientgegevens()
        break
      case ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_VERWERKEN:
        this.openBezwaarVerwerken()
        break
      default:
        this.notificationService.warning(this.strings.algemeen.clientDossier.acties.nietOndersteund)
    }
  }

  private openBezwaarVerwerken() {
    const magGegevensVerwijderen = this.autorisatieService.isToegestaan({
      recht: [Recht.CLIENT_DOSSIER_VERWIJDEREN, Recht.MEDEWERKER_BEZWAAR_BRP],
      actie: Actie.VERWIJDEREN,
      required: Required.ANY,
      bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA, Bevolkingsonderzoek.CERVIX],
    })
    if (!magGegevensVerwijderen) {
      this.openBezwaarVerwerkenDialog(true)
      return
    }
    this.dialog
      .open<boolean>(ConfirmationDialogComponent, {
        data: {
          title: this.strings.algemeen.clientDossier.acties.bezwaar.keuzeDialog.titel,
          body: this.strings.algemeen.clientDossier.acties.bezwaar.keuzeDialog.vraag,
          jaButtonTekst: this.strings.algemeen.clientDossier.acties.bezwaar.keuzeDialog.buttons.keuzesVerwerken,
          neeButtonTekst: this.strings.algemeen.clientDossier.acties.bezwaar.keuzeDialog.buttons.gegevensVerwijderen,
          neutraal: true,
        },
        hasBackdrop: true,
      })
      .closed.pipe(take(1))
      .subscribe((gegevensVerwerken) => {
        if (gegevensVerwerken !== undefined) {
          this.openBezwaarVerwerkenDialog(gegevensVerwerken)
        }
      })
  }

  private openBezwaarVerwerkenDialog(gegevensVerwerken: boolean) {
    this.dialog.open(BezwaarVerwerkenDialogComponent, { data: !gegevensVerwerken })
  }

  private openAanhefWijzigen() {
    this.clientService
      .getClientContactgegevens(this.clientId)
      .pipe(take(1))
      .subscribe((contactgegevens) => this.dialog.open(AanhefWijzigenDialogComponent, { data: contactgegevens }))
  }

  private openDeelnameBuitenBvoBmhk() {
    const actiesStrings = this.strings.algemeen.clientDossier.acties
    this.dialog
      .open<boolean>(ConfirmationDialogComponent, {
        data: {
          title: actiesStrings.deelnameBuitenBvoBmhk.dialogTitle,
          body: actiesStrings.deelnameBuitenBvoBmhk.dialogBody,
          neeButtonTekst: this.strings.algemeen.dialogs.bevestiging.neeButtonTekst,
          jaButtonTekst: actiesStrings.deelnameBuitenBvoBmhk.registrerenButton,
        },
        hasBackdrop: true,
      })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.contactActiesService.registreerDeelnameBuitenBvoBmhk(this.clientId)),
      )
      .subscribe(() => this.notificationService.success(actiesStrings.deelnameBuitenBvoBmhk.notifications.succes))
  }

  private openDeelnamewensen() {
    this.contactActiesService
      .getDeelnamewensen(this.clientId)
      .pipe(
        take(1),
        switchMap((deelnamewensen) => {
          const data: DeelnamewensenDialogData = { clientId: this.clientId, deelnamewensen }
          return this.dialog.open<boolean>(DeelnamewensenDialogComponent, { data }).closed
        }),
        filter((opgeslagen) => opgeslagen === true),
        switchMap(() => this.contactActiesService.getContactActies(this.clientId)),
      )
      .subscribe()
  }

  private openOpnieuwAanvragenClientgegevens() {
    this.dialog.open(OpnieuwAanvragenClientgegevensDialogComponent, { data: this.clientId })
  }

  private openTijdelijkAdresWijzigen() {
    this.clientService
      .getTijdelijkAdres(this.clientId)
      .pipe(
        take(1),
        switchMap((tijdelijkAdres) => {
          const data: AdresgegevensBewerkenModalData = { ...tijdelijkAdres, clientId: String(this.clientId), alsContactActie: true }
          return this.dialog.open<boolean>(AdresgegevensBewerkenModalComponent, { data }).closed
        }),
        filter((opgeslagen) => opgeslagen === true),
        switchMap(() => this.clientService.getClient(this.clientId)),
      )
      .subscribe()
  }

  private openCervixHeraanmelden(): void {
    this.dialog.open(HeraanmeldenBaarmoederhalskankerDialogComponent, { data: this.clientId })
  }
}
