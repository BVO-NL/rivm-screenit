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
import { Component, inject, output, signal } from '@angular/core'
import { FormBuilder, FormControl, ReactiveFormsModule } from '@angular/forms'
import { MammaVisitatieWerklijstFilter } from '@shared/types/mamma/mamma-visitatie-werklijst-filter'
import { CardComponent } from '@shared/components/card/card.component'
import { OrganisatieService } from '@/algemeen/services/organisatie/organisatie.service'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ClrComboboxModule } from '@clr/angular'
import { OrganisatieDto } from '@/shared/types/algemeen/dto/organisatie.dto'
import { iif, startWith, switchMap, take } from 'rxjs'
import { MammaVisitatieStatus, mammaVisitatieStatusLijst } from '@shared/types/mamma/mamma-visitatie-status'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { OrganisatieType } from '@shared/types/algemeen/organisatie-type'
import { EnumWeergave } from '@shared/types/enum-weergave'
import { subMonths } from 'date-fns'

@Component({
  selector: 'app-mamma-visitatie-filter',
  imports: [CardComponent, ReactiveFormsModule, ClrComboboxModule],
  templateUrl: './mamma-visitatie-filter.component.html',
})
export class MammaVisitatieFilterComponent {
  private readonly formBuilder = inject(FormBuilder)
  private readonly organisatieService = inject(OrganisatieService)
  private readonly autorisatieService = inject(AutorisatieService)
  private firstRun = true

  visitatieStatusLijst = mammaVisitatieStatusLijst.filter((status) => !(this.isKwaliteitsplatform && status.enum === MammaVisitatieStatus.INGEPLAND))
  filterForm = this.formBuilder.group({
    beoordelingseenheidIds: [],
    centraleEenheidIds: [],
    screeningseenheidIds: [],
    statussen: [],
    vanaf: this.isKwaliteitsplatform ? subMonths(new Date(), 3) : null,
  })

  filter = output<MammaVisitatieWerklijstFilter>()
  centraleEenheden = signal<OrganisatieDto[]>([])
  beoordelingseenheden = signal<OrganisatieDto[]>([])
  screeningseenheden = signal<OrganisatieDto[]>([])

  get centraleEenheidCtrl(): FormControl {
    return this.filterForm.get('centraleEenheidIds') as FormControl
  }

  get beoordelingseenheidCtrl(): FormControl {
    return this.filterForm.get('beoordelingseenheidIds') as FormControl
  }

  get screeningseenhedenCtrl(): FormControl {
    return this.filterForm.get('screeningseenheidIds') as FormControl
  }

  get statussenCtrl(): FormControl {
    return this.filterForm.get('statussen') as FormControl
  }

  get isKwaliteitsplatform(): boolean {
    return this.autorisatieService.heeftOrganisatieType(OrganisatieType.KWALITEITSPLATFORM)
  }

  constructor() {
    this.statussenCtrl.setValue(
      this.isKwaliteitsplatform ? [MammaVisitatieStatus.VRIJGEGEVEN, MammaVisitatieStatus.UITGEVOERD] : [MammaVisitatieStatus.VRIJGEGEVEN, MammaVisitatieStatus.INGEPLAND],
    )

    this.organisatieService
      .getCentraleEenheden()
      .pipe(take(1))
      .subscribe((list: OrganisatieDto[]) => {
        this.centraleEenheden.set(list)
      })

    this.centraleEenheidCtrl.valueChanges
      .pipe(
        startWith([]),
        takeUntilDestroyed(),
        switchMap((centraleEenheidIds: number[]) =>
          iif(() => centraleEenheidIds?.length > 0, this.organisatieService.zoekBeoordelingseenheden(centraleEenheidIds), this.organisatieService.getBeoordelingseenheden()),
        ),
      )
      .subscribe((beoordelingseenheden: OrganisatieDto[]) => {
        this.beoordelingseenheden.set(beoordelingseenheden)
        this.beoordelingseenheidCtrl.setValue(beoordelingseenheden.map((be) => be.id))
      })

    this.beoordelingseenheidCtrl.valueChanges
      .pipe(
        startWith([]),
        takeUntilDestroyed(),
        switchMap((beoordelingseenheidIds: number[]) =>
          iif(
            () => beoordelingseenheidIds?.length > 0,
            this.organisatieService.zoekScreeningseenheden(beoordelingseenheidIds),
            this.organisatieService.zoekScreeningseenheden(this.beoordelingseenheden().map((b) => b.id)),
          ),
        ),
      )
      .subscribe((screeningseenheden: OrganisatieDto[]) => {
        this.screeningseenheden.set(screeningseenheden)
        this.screeningseenhedenCtrl.setValue(screeningseenheden.map((se) => se.id))

        if (this.firstRun) {
          this.firstRun = false
          this.filteren()
        }
      })
  }

  getSelectedBeoordelingseenheid(id: number): OrganisatieDto | undefined {
    return this.beoordelingseenheden().find((be) => be.id === id)
  }

  getSelectedScreeningseenheid(id: number): OrganisatieDto | undefined {
    return this.screeningseenheden().find((se) => se.id === id)
  }

  getSelectedCentraleEenheid(id: number): OrganisatieDto | undefined {
    return this.centraleEenheden().find((ce) => ce.id === id)
  }

  getSelectedStatus(waarde: MammaVisitatieStatus): EnumWeergave<MammaVisitatieStatus> | undefined {
    return this.visitatieStatusLijst.find((status) => status.enum === waarde)
  }

  filteren() {
    this.filter.emit({
      centraleEenheidIds: this.centraleEenheidCtrl.value || [],
      beoordelingseenheidIds: this.beoordelingseenheidCtrl.value || [],
      screeningseenheidIds: this.screeningseenhedenCtrl.value || [],
      statussen: this.statussenCtrl.value || [],
    })
  }
}
