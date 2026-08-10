/*-
 * ========================LICENSE_START=================================
 * huisartsportaal
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
import { Component, computed, effect, inject, input, output, signal } from '@angular/core'
import { DsButtonComponent, DsCheckboxComponent, DsDropdownComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { LocatieTabelComponent } from '../locatie-tabel/locatie-tabel.component'
import { FormBuilder, FormControl, ReactiveFormsModule, Validators } from '@angular/forms'
import { HttpClient } from '@angular/common/http'
import { take } from 'rxjs'
import { Dialog } from '@angular/cdk/dialog'
import { UpdateWachtwoordDialogComponent } from '../update-wachtwoord-dialog/update-wachtwoord-dialog.component'
import { WachtwoordFormInputComponent } from '../wachtwoord-form-input/wachtwoord-form-input.component'
import { NotificatieService } from '../../services/notificatie/notificatie.service'
import { HuisartsService } from '../../services/huisarts/huisarts.service'
import { CustomValidators } from '../../utils/custom-validators'
import { WoonplaatsSelectorComponent } from '../woonplaats-selector/woonplaats-selector.component'
import { HuisartsDto } from '../../models/HuisartsDto'
import { StateService } from '../../services/state/state.service'
import { WoonplaatsDto } from '../../models/WoonplaatsDto'
import { saveAs } from 'file-saver'
import { AuthService } from '../../services/auth/auth.service'

@Component({
  selector: 'app-gegevens-wijzigen',
  imports: [
    DsInputComponent,
    LocatieTabelComponent,
    ReactiveFormsModule,
    DsCheckboxComponent,
    DsButtonComponent,
    WachtwoordFormInputComponent,
    DsDropdownComponent,
    WoonplaatsSelectorComponent,
  ],
  templateUrl: './gegevens-wijzigen.component.html',
  styleUrl: './gegevens-wijzigen.component.scss',
})
export class GegevensWijzigenComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly http = inject(HttpClient)
  private readonly dialogController = inject(Dialog)
  private readonly notificationService = inject(NotificatieService)
  private readonly huisartsService = inject(HuisartsService)
  private readonly stateService = inject(StateService)
  private readonly authService = inject(AuthService)

  registreren = input<boolean>(false)
  controleren = signal<boolean>(false)
  opslaanTekst = computed(() => {
    if (this.controleren()) {
      return 'Registratie voltooien'
    } else if (this.registreren()) {
      return 'Registratie controleren'
    }
    return 'Gegevens opslaan'
  })
  opgeslagen = output<void>()

  get postAdresCtrl(): FormControl {
    return this.gegevensForm.get('postadres') as FormControl
  }

  wachtwoordValidators = computed(() => (this.registreren() ? [Validators.required, CustomValidators.wachtwoordSterkteValidator] : []))
  wachtwoordControleValidators = computed(() => (this.registreren() ? [Validators.required, CustomValidators.maakControleValidator(this.gegevensForm.get('wachtwoord')!)] : []))

  gegevensForm = this.formBuilder.group({
    agbcode: ['', Validators.required],
    email: ['', [Validators.required, Validators.email]],
    extraEmails: [''],
    wachtwoord: [''],
    wachtwoordControle: [''],
    username: ['', [Validators.required, Validators.minLength(6)]],
    aanhef: '',
    achternaam: ['', Validators.required],
    tussenvoegsel: '',
    overeenkomst: false,
    voorletters: '',
    telefoonnummer: ['', CustomValidators.telefoonnummerValidator],
    postadres: this.formBuilder.group({
      straat: ['', Validators.required],
      huisnummer: [null as unknown as number, Validators.required],
      huisnummertoevoeging: '',
      postcode: ['', [Validators.required, CustomValidators.postcodeValidator]],
      woonplaats: [null as unknown as WoonplaatsDto, Validators.required],
    }),
  })
  voorwaardenCheckboxCtrl = this.formBuilder.control<boolean>(!this.registreren())
  aanhefOpties = [
    { value: 'Dhr.', label: 'Dhr.' },
    { value: 'Mevr.', label: 'Mevr.' },
  ]

  constructor() {
    effect(() => {
      if (this.controleren()) {
        this.gegevensForm.disable()
        this.voorwaardenCheckboxCtrl.disable()
      } else {
        this.gegevensForm.enable()
        this.voorwaardenCheckboxCtrl.enable()
      }
    })

    const huisarts = this.stateService.huisarts()
    if (huisarts) {
      this.gegevensForm.patchValue(huisarts)
    }
  }

  downloadVoorwaarden() {
    this.http
      .get('/api/v1/overeenkomst', { responseType: 'blob' })
      .pipe(take(1))
      .subscribe((blob) => saveAs(blob, 'Zakelijke voorwaarden huisartsen.pdf'))
  }

  openWachtwoordDialog() {
    this.dialogController.open(UpdateWachtwoordDialogComponent)
  }

  opslaan() {
    this.gegevensForm.markAllAsTouched()
    this.gegevensForm.updateValueAndValidity()

    if (this.gegevensForm.invalid) {
      return
    }

    if (this.voorwaardenCheckboxCtrl.value === false) {
      this.notificationService.warning('U dient akkoord te gaan met de voorwaarden om u te kunnen registreren')
      return
    }

    const huisartsData = this.gegevensForm.value as unknown as HuisartsDto
    if ((this.controleren() && this.registreren()) || !this.registreren()) {
      this.slaGegevensOp(huisartsData)
    } else {
      this.controleerGegevens(huisartsData)
    }
  }

  private controleerGegevens(huisartsData: HuisartsDto) {
    this.huisartsService
      .controleerHuisarts(huisartsData)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.info('U kunt nu uw registratie controleren')
        this.controleren.set(true)
      })
  }

  private slaGegevensOp(huisartsData: HuisartsDto) {
    this.authService
      .registratieVoltooien(huisartsData)
      .pipe(take(1))
      .subscribe(() => {
        this.notificationService.success('De gegevens zijn succesvol opgeslagen')
        this.opgeslagen.emit()
      })
  }
}
