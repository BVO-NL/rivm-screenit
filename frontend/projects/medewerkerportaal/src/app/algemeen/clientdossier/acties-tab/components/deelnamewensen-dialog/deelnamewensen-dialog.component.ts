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
import { Component, inject } from '@angular/core'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { FormBuilder, ReactiveFormsModule } from '@angular/forms'
import { take } from 'rxjs'
import { CheckboxValueModel, DsButtonComponent, DsCheckboxArrayComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { DeelnamewensenDto } from '@shared/types/algemeen/dto/deelnamewensen.dto'
import { StringsService } from '@shared/strings'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'
import { createNieuweSelectieValidator } from '@shared/validators/nieuwe-selectie/nieuwe-selectie.validator'

export interface DeelnamewensenDialogData {
  clientId: number
  deelnamewensen: DeelnamewensenDto
}

@Component({
  selector: 'app-deelnamewensen-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsCheckboxArrayComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule],
  templateUrl: './deelnamewensen-dialog.component.html',
})
export class DeelnamewensenDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly data = inject<DeelnamewensenDialogData>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()

  protected readonly deelnamewensenForm = this.formBuilder.group({
    deelnamewensen: this.formBuilder.nonNullable.control<CheckboxValueModel<Bevolkingsonderzoek>[]>(
      [
        this.maakDeelnamewensCheckbox(Bevolkingsonderzoek.CERVIX, this.strings.algemeen.clientDossier.acties.categorieen.cervix, this.data.deelnamewensen.deelnamewensBmhk),
        this.maakDeelnamewensCheckbox(Bevolkingsonderzoek.MAMMA, this.strings.algemeen.clientDossier.acties.categorieen.mamma, this.data.deelnamewensen.deelnamewensBk),
      ],
      { validators: createNieuweSelectieValidator(this.bepaalVastgelegdeDeelnamewensen()) },
    ),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  private bepaalVastgelegdeDeelnamewensen(): Bevolkingsonderzoek[] {
    const vastgelegd: Bevolkingsonderzoek[] = []
    if (this.data.deelnamewensen.deelnamewensBmhk) {
      vastgelegd.push(Bevolkingsonderzoek.CERVIX)
    }
    if (this.data.deelnamewensen.deelnamewensBk) {
      vastgelegd.push(Bevolkingsonderzoek.MAMMA)
    }
    return vastgelegd
  }

  private maakDeelnamewensCheckbox(bevolkingsonderzoek: Bevolkingsonderzoek, label: string, vastgelegd: boolean): CheckboxValueModel<Bevolkingsonderzoek> {
    return { value: bevolkingsonderzoek, label, selected: vastgelegd, disabled: vastgelegd }
  }

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected opslaan() {
    if (this.deelnamewensenForm.invalid) {
      return
    }
    const { deelnamewensen, notitie } = this.deelnamewensenForm.getRawValue()
    const isGeselecteerd = (bevolkingsonderzoek: Bevolkingsonderzoek) =>
      deelnamewensen.some((checkbox) => checkbox.value === bevolkingsonderzoek && checkbox.selected === true)
    this.contactActiesService
      .registreerDeelnamewensen(this.data.clientId, {
        deelnamewensen: { deelnamewensBmhk: isGeselecteerd(Bevolkingsonderzoek.CERVIX), deelnamewensBk: isGeselecteerd(Bevolkingsonderzoek.MAMMA) },
        notitie,
      })
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.strings.algemeen.clientDossier.acties.deelnamewensen.notifications.succes)
        this.dialogRef.close(true)
      })
  }
}
