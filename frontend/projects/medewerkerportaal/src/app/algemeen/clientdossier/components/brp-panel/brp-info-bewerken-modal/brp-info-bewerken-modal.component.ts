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
import { Component, inject, OnInit, signal } from '@angular/core'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { DsButtonComponent, DsInputComponent, DsValidators } from '@topicus-rgp-ds/web'
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms'
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { TijdelijkAdresDto } from '@shared/types/algemeen/dto/tijdelijk-adres.dto'
import { ClientService } from '@/algemeen/services/client/client.service'
import { ClientBrpGegevensDto } from '@shared/types/algemeen/dto/clientbrpgegevens.dto'
import { filter, switchMap, take } from 'rxjs'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { huisnummerValidator } from '@shared/validators/common-validators'
import { AutorisatieDirective } from '@/autorisatie/directive/autorisatie.directive'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'
import { Recht } from '@shared/types/autorisatie/recht'
import { Actie } from '@shared/types/autorisatie/actie'
import { Required } from '@shared/types/autorisatie/required'
import { ToegangLevel } from '@shared/types/autorisatie/toegang-level'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'

@Component({
  selector: 'app-brp-info-bewerken-modal',
  imports: [BaseDialogComponent, DsButtonComponent, DsInputComponent, ReactiveFormsModule, AutorisatieDirective],
  templateUrl: './brp-info-bewerken-modal.component.html',
  styleUrl: './brp-info-bewerken-modal.component.scss',
})
export class BrpInfoBewerkenModalComponent implements OnInit {
  private readonly formBuilder = inject(FormBuilder)
  private readonly dialogRef = inject(DialogRef)
  private readonly clientService = inject(ClientService)
  private readonly dialog = inject(Dialog)

  protected readonly tijdelijkAdres = signal<TijdelijkAdresDto | undefined>(undefined)
  protected readonly brpInfo = inject(DIALOG_DATA) as ClientBrpGegevensDto | undefined

  protected readonly verwijderConstraint: SecurityConstraint = {
    recht: [Recht.MEDEWERKER_GBA_AANVRAGEN],
    actie: Actie.VERWIJDEREN,
    required: Required.ANY,
    level: ToegangLevel.LANDELIJK,
    bevolkingsonderzoekScopes: [Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA],
  }

  brpGegevensForm: FormGroup = this.formBuilder.group({
    straatnaam: [this.tijdelijkAdres()?.straatnaam ?? '', [Validators.required, Validators.maxLength(56)]],
    huisnummer: [this.tijdelijkAdres()?.huisnummer ?? '', huisnummerValidator],
    huisletter: [this.tijdelijkAdres()?.huisletter ?? ''],
    huisnummerToevoeging: [this.tijdelijkAdres()?.huisnummerToevoeging ?? ''],
    aanduidingBijHuisnummer: [this.tijdelijkAdres()?.aanduidingBijHuisnummer ?? ''],
    postcode: [this.tijdelijkAdres()?.postcode ?? '', DsValidators.postcode],
    plaats: [this.tijdelijkAdres()?.plaats ?? ''],
  })

  ngOnInit() {
    this.getBrpTijdelijkAdres()
  }

  protected cancel() {
    this.dialogRef.close()
  }

  protected save() {
    if (this.brpInfo && this.brpInfo.id && this.brpGegevensForm.valid) {
      const tijdelijkAdres: TijdelijkAdresDto = {
        clientId: String(this.brpInfo.id),
        straatnaam: this.brpGegevensForm.value.straatnaam ?? '',
        huisnummer: this.brpGegevensForm.value.huisnummer ?? 0,
        huisletter: this.brpGegevensForm.value.huisletter ?? '',
        huisnummerToevoeging: this.brpGegevensForm.value.huisnummerToevoeging ?? '',
        aanduidingBijHuisnummer: this.brpGegevensForm.value.aanduidingBijHuisnummer ?? '',
        postcode: this.brpGegevensForm.value.postcode ?? '',
        plaats: this.brpGegevensForm.value.plaats ?? '',
      }

      this.clientService
        .saveClientBrpTijdelijkAdres(this.brpInfo.id, tijdelijkAdres)
        .pipe(take(1))
        .subscribe(() => this.dialogRef.close('opgeslagen'))
    }
  }

  protected deleteBrpTijdelijkAdres() {
    if (!this.brpInfo || !this.brpInfo.id) {
      return
    }
    const clientId = this.brpInfo.id
    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Tijdelijk BRP-adres verwijderen',
          body: 'Wilt u het tijdelijke BRP-adres echt verwijderen?',
        },
      })
      .closed.pipe(
        take(1),
        filter((bevestigd) => bevestigd === true),
        switchMap(() => this.clientService.deleteClientBrpTijdelijkAdres(clientId)),
      )
      .subscribe(() => this.dialogRef.close('verwijderd'))
  }

  getBrpTijdelijkAdres() {
    if (!this.brpInfo || !this.brpInfo.id) {
      return
    }
    this.clientService
      .getClientBrpTijdelijkAdres(this.brpInfo.id)
      .pipe(take(1))
      .subscribe((tijdelijkAdres) => {
        this.tijdelijkAdres.set(tijdelijkAdres ?? undefined)
        this.brpGegevensForm.patchValue({
          straatnaam: tijdelijkAdres?.straatnaam ?? '',
          huisnummer: tijdelijkAdres?.huisnummer ?? '',
          huisletter: tijdelijkAdres?.huisletter ?? '',
          huisnummerToevoeging: tijdelijkAdres?.huisnummerToevoeging ?? '',
          aanduidingBijHuisnummer: tijdelijkAdres?.aanduidingBijHuisnummer ?? '',
          postcode: tijdelijkAdres?.postcode ?? '',
          plaats: tijdelijkAdres?.plaats ?? '',
        })
      })
  }
}
