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
import { Component, effect, forwardRef, inject, input, InputSignal } from '@angular/core'
import { ClrComboboxModule, ClrCommonFormsModule } from '@clr/angular'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule, Validators } from '@angular/forms'
import { ProjectService } from '@/algemeen/services/project/project.service'
import { Project } from '@shared/types/algemeen/project'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter, take } from 'rxjs'
import { NgIf } from '@angular/common'
import { ProjectType } from '@shared/types/algemeen/project-type'

@Component({
  selector: 'app-project-selector',
  imports: [ClrComboboxModule, ClrCommonFormsModule, ReactiveFormsModule, NgIf],
  styles: [
    `
      :host {
        display: block;
      }
    `,
  ],
  template: ` <clr-combobox-container>
    <label [class.clr-required-mark]="required()" for="project">{{ label() }}</label>
    <clr-combobox [clrMulti]="true" [formControl]="projectCtrl" required id="project" data-testid="cb_project_selector">
      <ng-container *clrOptionSelected="let selected">
        {{ selected?.naam }}
      </ng-container>
      <clr-options>
        <clr-option [clrValue]="project" *clrOptionItems="let project of projecten; field: 'naam'">{{ project.naam }}</clr-option>
      </clr-options>
    </clr-combobox>
    <clr-control-error *ngIf="required() && projectCtrl.invalid">De kamer is verplicht</clr-control-error>
  </clr-combobox-container>`,
  providers: [
    {
      provide: NG_VALUE_ACCESSOR,
      useExisting: forwardRef(() => ProjectSelectorComponent),
      multi: true,
    },
  ],
})
export class ProjectSelectorComponent implements ControlValueAccessor {
  private projectService: ProjectService = inject(ProjectService)
  projectCtrl = new FormControl<Project[] | null>(null)
  projecten: Project[] = []

  required = input(false)
  label = input('Projecten')
  projectType: InputSignal<ProjectType | undefined> = input()

  private onChange: ((value: number[]) => void) | undefined
  private onTouched: (() => void) | undefined
  private value: number[] | null = null

  constructor() {
    effect(() => {
      if (this.required()) {
        this.projectCtrl.setValidators([Validators.required])
      } else {
        this.projectCtrl.clearValidators()
      }
    })

    effect(() => {
      this.getProjecten(this.projectType())
    })

    this.projectCtrl.valueChanges
      .pipe(
        takeUntilDestroyed(),
        filter(() => this.onChange != undefined),
      )
      .subscribe((value: Project[] | null) => {
        const projecten: Project[] = value ?? []
        const projectIds = projecten.map((project: Project) => project.id)
        this.onChange!(projectIds)
      })
  }

  private getProjecten(projectType?: ProjectType) {
    this.projectService
      .getProjecten(projectType)
      .pipe(take(1))
      .subscribe((res) => {
        this.projecten = res
        this.setProject()
      })
  }

  private setProject() {
    if (this.value) {
      const projecten: Project[] = this.projecten.filter((project) => this.value?.includes(project.id))
      this.projectCtrl.setValue(projecten, { emitEvent: false })
    }
  }

  writeValue(value: number[] | null): void {
    this.value = value
    this.setProject()
  }

  registerOnChange(fn: (value: number[]) => void): void {
    this.onChange = fn
  }

  registerOnTouched(fn: () => void): void {
    this.onTouched = fn
  }

  setDisabledState(isDisabled: boolean): void {
    if (isDisabled) {
      this.projectCtrl.disable()
    } else {
      this.projectCtrl.enable()
    }
  }
}
