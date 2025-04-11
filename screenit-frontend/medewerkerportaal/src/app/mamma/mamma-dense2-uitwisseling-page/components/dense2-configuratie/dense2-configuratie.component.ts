/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import { Component, effect, inject, input, InputSignal } from '@angular/core'
import { ProjectType } from '@shared/types/algemeen/project-type'
import { CardComponent } from '@shared/components/card/card.component'
import { ClrCheckboxModule, ClrCommonFormsModule, ClrInputModule } from '@clr/angular'
import { FormControl, FormGroup, FormsModule, NonNullableFormBuilder, ReactiveFormsModule, Validators } from '@angular/forms'
import { ProjectSelectorComponent } from '@/algemeen/components/project-selector/project-selector.component'
import { filter, take } from 'rxjs'
import { MammaDense2Configuratie } from '@shared/types/mamma/mamma-dense2-configuratie'
import { ConfirmationDialogComponent } from '@shared/components/confirmation-dialog/confirmation-dialog.component'
import { ToastService } from '@/toast/service/toast.service'
import { Dialog } from '@angular/cdk/dialog'
import { Dense2Service } from '@/mamma/mamma-dense2-uitwisseling-page/services/dense2/dense2.service'

@Component({
  selector: 'app-dense2-configuratie',
  imports: [CardComponent, ClrCheckboxModule, ClrCommonFormsModule, ClrInputModule, FormsModule, ProjectSelectorComponent, ReactiveFormsModule],
  templateUrl: './dense2-configuratie.component.html',
  styleUrl: './dense2-configuratie.component.scss',
})
export class Dense2ConfiguratieComponent {
  private formBuilder: NonNullableFormBuilder = inject(NonNullableFormBuilder)
  private toastService: ToastService = inject(ToastService)
  private dialog: Dialog = inject(Dialog)
  private dense2Service: Dense2Service = inject(Dense2Service)
  configuratie: InputSignal<MammaDense2Configuratie | undefined> = input()

  get metingOpslaanCtrl(): FormControl {
    return this.configuratieForm.get('metingOpslaan') as FormControl
  }

  configuratieForm: FormGroup = this.formBuilder.group({
    cem: ['', Validators.required],
    mri: ['', Validators.required],
    controleGroep: ['', Validators.required],
    herinneringCem: ['', Validators.required],
    herinneringMri: ['', Validators.required],
    metingOpslaan: true,
    excludeProjecten: [],
  })

  constructor() {
    effect(() => {
      if (this.configuratie()) {
        this.configuratieForm.patchValue(this.configuratie()!)
      }
    })
  }

  configuratieOpslaan() {
    if (this.configuratieForm.invalid) {
      return
    }

    this.dialog
      .open(ConfirmationDialogComponent, {
        data: {
          title: 'Bevestiging',
          body: 'Weet u zeker dat u de configuratie wilt opslaan?',
        },
      })
      .closed.pipe(
        take(1),
        filter((res: unknown) => res === true),
      )
      .subscribe(() => this.opslaan())
  }

  private opslaan() {
    this.dense2Service
      .updateConfiguratie(this.configuratieForm.value)
      .pipe(take(1))
      .subscribe(() => {
        this.toastService.success('Configuratie is opgeslagen')
      })
  }

  protected readonly ProjectType = ProjectType
}
