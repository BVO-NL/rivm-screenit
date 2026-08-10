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
import { Component, effect, forwardRef, inject, input, InputSignal, signal } from '@angular/core'
import { ControlValueAccessor, FormControl, NG_VALUE_ACCESSOR, ReactiveFormsModule, Validators } from '@angular/forms'
import { ProjectService } from '@/algemeen/services/project/project.service'
import { Project } from '@shared/types/algemeen/project'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { filter, take } from 'rxjs'

import { ProjectType } from '@shared/types/algemeen/project-type'
import { DsDropdownComponent } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-project-selector',
  imports: [ReactiveFormsModule, DsDropdownComponent],
  styles: [
    `
      :host {
        display: block;
      }
    `,
  ],
  template: `<ds-dropdown
    [formControl]="projectCtrl"
    [label]="label()"
    [required]="required()"
    [clearable]="!required()"
    data-testid="dd_project_selector"
    [items]="projecten()"
    [multiple]="true"
    bindLabel="naam"
    bindValue="id"
  />`,
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
  projectCtrl = new FormControl<number[] | null>([])
  projecten = signal<Project[]>([])

  required = input(false)
  label = input('Projecten')
  projectType: InputSignal<ProjectType | undefined> = input()

  private onChange: ((value: number[] | null) => void) | undefined
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
      .subscribe((value: number[] | null) => {
        this.onChange!(value)
      })
  }

  private getProjecten(projectType?: ProjectType) {
    this.projectService
      .getProjecten(projectType)
      .pipe(take(1))
      .subscribe((res) => {
        this.projecten.set(res)
        this.setProject()
      })
  }

  private setProject() {
    if (this.value) {
      this.projectCtrl.setValue(this.value, { emitEvent: false })
    }
  }

  writeValue(value: number[] | null): void {
    this.value = value
    this.setProject()
  }

  registerOnChange(fn: (value: number[] | null) => void): void {
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
