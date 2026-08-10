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
import { Component, inject, output } from '@angular/core'
import { DsButtonComponent, DsCardComponent, DsDatepickerComponent, DsDropdownComponent, DsDurationDatepickerComponent, DsInputComponent } from '@topicus-rgp-ds/web'
import { MedewerkerService } from '@/algemeen/services/medewerker/medewerker.service'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { MammaFotobesprekingOnderzoekFilterDto } from '@shared/types/mamma/dto/fotobespreking/mamma-fotobespreking-onderzoek-filter.dto'
import { FormBuilder, FormControl, ReactiveFormsModule } from '@angular/forms'
import { bsnValidator } from '@shared/validators/bsn/bsn.validator'
import { fotobesprekingFilterValidator } from '@shared/validators/fotobespreking/fotobespreking-filter.validator'
import { subYears } from 'date-fns'
import { formatDate } from '@shared/utils/date-utils'
import { Recht } from '@shared/types/autorisatie/recht'
import { FotobesprekingService } from '@/mamma/mamma-fotobespreking-onderzoeken-page/services/fotobespreking.service'
import { NotificationService } from '@shared/services/notification/notification.service'
import { OrganisatieDto } from '@shared/types/algemeen/dto/organisatie.dto'
import { iif, Observable, startWith, switchMap } from 'rxjs'
import { OrganisatieService } from '@/algemeen/services/organisatie/organisatie.service'

@Component({
  selector: 'app-mamma-fotobespreking-filter',
  imports: [DsCardComponent, DsDurationDatepickerComponent, DsDropdownComponent, DsInputComponent, DsDatepickerComponent, ReactiveFormsModule, DsButtonComponent],
  templateUrl: './mamma-fotobespreking-filter.component.html',
  styleUrl: './mamma-fotobespreking-filter.component.scss',
})
export class MammaFotobesprekingFilterComponent {
  private readonly medewerkerService = inject(MedewerkerService)
  private readonly formBuilder = inject(FormBuilder)
  private readonly fotobesprekingService = inject(FotobesprekingService)
  private readonly notificationsService = inject(NotificationService)
  private readonly organisatieService = inject(OrganisatieService)
  filterForm = this.formBuilder.group(
    {
      client: this.formBuilder.control<string | null>(null),
      geboortedatum: this.formBuilder.control<Date | null>(null),
      onderzoeksdatum: this.formBuilder.control<{ startDate: Date | null; endDate: Date | null } | null>({ startDate: null, endDate: null }),
      bsn: this.formBuilder.control<string | null>(null, bsnValidator),
      medewerkerId: this.formBuilder.control<number | null>(null),
      redenFotobesprekingDoorMbber: this.formBuilder.control<string[]>([]),
      redenFotobesprekingMetMbber: this.formBuilder.control<string[]>([]),
      redenFotobesprekingDoorRadioloog: this.formBuilder.control<string[]>([]),
      redenDoorverwijzing: this.formBuilder.control<string[]>([]),
      discrepantie: this.formBuilder.control<string[]>([]),
      followUp: this.formBuilder.control<string[]>([]),
      beoordelingseenheidIds: this.formBuilder.control<number[]>([]),
      screeningseenheidIds: this.formBuilder.control<number[]>([]),
    },
    { validators: fotobesprekingFilterValidator(this.notificationsService) },
  )

  onderzoekMinDatum = formatDate(subYears(new Date(), 2))
  filter = output<MammaFotobesprekingOnderzoekFilterDto>()
  medewerkers = toSignal(this.medewerkerService.getMedewerkers(Recht.MEDEWERKER_SCREENING_MAMMA_SE_ONDERZOEK), { initialValue: [] })
  filterOpties = toSignal(this.fotobesprekingService.getFilterOpties(), {
    initialValue: {
      redenFotobesprekingDoorMbber: [],
      redenFotobesprekingMetMbber: [],
      redenFotobesprekingDoorRadioloog: [],
      redenDoorverwijzing: [],
      followUp: [],
    },
  })

  beoordelingseenheden = toSignal(this.organisatieService.getBeoordelingseenheden(), { initialValue: [] })
  screeningseenheden = toSignal(
    this.beoordelingseenheidCtrl.valueChanges.pipe(
      startWith([]),
      takeUntilDestroyed(),
      switchMap(
        (beoordelingseenheidIds: number[]): Observable<OrganisatieDto[]> =>
          iif(
            (): boolean => beoordelingseenheidIds?.length > 0,
            this.organisatieService.zoekScreeningseenheden(beoordelingseenheidIds),
            this.organisatieService.zoekScreeningseenheden(this.beoordelingseenheden().map((b) => b.id)),
          ),
      ),
    ),
    { initialValue: [] },
  )

  get beoordelingseenheidCtrl(): FormControl {
    return this.filterForm.get('beoordelingseenheidIds') as FormControl
  }

  zoeken() {
    this.filterForm.updateValueAndValidity()

    if (this.filterForm.valid) {
      this.filter.emit(this.filterForm.value)
    }
  }
}
