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
import { Component, effect, inject, signal } from '@angular/core'
import { DIALOG_DATA, DialogRef } from '@angular/cdk/dialog'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { map, take } from 'rxjs'
import { DsButtonComponent, DsDropdownComponent } from '@topicus-rgp-ds/web'
import { BaseDialogComponent } from '@shared/components/base-dialog/base-dialog.component'
import { NotitieAanmakenVeldComponent } from '@algemeen/clientdossier/components/notitie-aanmaken-veld/notitie-aanmaken-veld.component'
import { ContactActiesService } from '@algemeen/services/contact-acties/contact-acties.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { StringsService } from '@shared/strings'
import { AfmeldingType, afmeldingTypeLabels } from '@shared/types/algemeen/enum/afmelding-type'
import { AfmeldManier, afmeldManierLabels } from '@shared/types/algemeen/enum/afmeld-manier'
import { CervixAfmeldingReden, cervixAfmeldingRedenLabels } from '@shared/types/cervix/cervix-afmelding-reden'
import { enumNaarOpties } from '@shared/types/enum-optie'
import { SingleFileSelectorComponent } from '@shared/components/single-file-selector/single-file-selector.component'

@Component({
  selector: 'app-cervix-afmelden-dialog',
  imports: [BaseDialogComponent, DsButtonComponent, DsDropdownComponent, NotitieAanmakenVeldComponent, ReactiveFormsModule, SingleFileSelectorComponent],
  templateUrl: './cervix-afmelden-dialog.component.html',
})
export class CervixAfmeldenDialogComponent {
  private readonly dialogRef = inject<DialogRef<boolean>>(DialogRef)
  private readonly contactActiesService = inject(ContactActiesService)
  private readonly notificationService = inject(NotificationService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly clientId = inject<number>(DIALOG_DATA)
  protected readonly strings = inject(StringsService).getStrings()
  protected readonly afmeldenStrings = this.strings.algemeen.clientDossier.acties.cervixAfmelden

  protected readonly typeOpties = toSignal(
    this.contactActiesService.getCervixAfmeldOpties(this.clientId).pipe(map((types) => types.map((waarde) => ({ waarde, naam: afmeldingTypeLabels[waarde] })))),
    { initialValue: [] },
  )
  protected readonly manierOpties = enumNaarOpties(afmeldManierLabels)
  protected readonly redenOpties = enumNaarOpties(cervixAfmeldingRedenLabels)

  protected readonly afmeldForm = this.formBuilder.group({
    type: this.formBuilder.control<AfmeldingType | null>(null, Validators.required),
    manier: this.formBuilder.control<AfmeldManier | null>({ value: null, disabled: true }, Validators.required),
    bestand: this.formBuilder.control<File | null>({ value: null, disabled: true }, Validators.required),
    reden: this.formBuilder.control<CervixAfmeldingReden | null>({ value: null, disabled: true }, Validators.required),
    notitie: this.formBuilder.nonNullable.control<string>(''),
  })

  protected readonly isDefinitief = signal(false)
  protected readonly isDirect = signal(false)

  constructor() {
    effect(() => {
      const typeOpties = this.typeOpties()
      if (typeOpties.length === 1) {
        this.afmeldForm.get('type')?.setValue(typeOpties[0].waarde)
      }
    })
    this.afmeldForm
      .get('type')!
      .valueChanges.pipe(takeUntilDestroyed())
      .subscribe((type) => {
        const definitief = type === AfmeldingType.DEFINITIEF
        this.isDefinitief.set(definitief)
        this.zetVeldActief('manier', definitief)
      })
    this.afmeldForm
      .get('manier')!
      .valueChanges.pipe(takeUntilDestroyed())
      .subscribe((manier) => {
        const direct = manier === AfmeldManier.DIRECT
        this.isDirect.set(direct)
        this.zetVeldActief('bestand', direct)
        this.zetVeldActief('reden', direct)
      })
  }

  protected annuleren() {
    this.dialogRef.close(false)
  }

  protected bevestigen() {
    const { type, manier, reden, bestand, notitie } = this.afmeldForm.getRawValue()
    if (this.afmeldForm.invalid || !type) {
      return
    }
    const isDefinitief = type === AfmeldingType.DEFINITIEF
    const isDirect = isDefinitief && manier === AfmeldManier.DIRECT
    this.contactActiesService
      .cervixAfmelden(this.clientId, { type, manier: isDefinitief ? manier : null, reden: isDirect ? reden : null, notitie }, isDirect ? bestand : null)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success(this.afmeldenStrings.notifications.succes)
        this.dialogRef.close(true)
      })
  }

  private zetVeldActief(veld: 'manier' | 'bestand' | 'reden', actief: boolean) {
    const control = this.afmeldForm.get(veld)
    control?.reset()
    if (actief) {
      control?.enable()
    } else {
      control?.disable()
    }
  }
}
