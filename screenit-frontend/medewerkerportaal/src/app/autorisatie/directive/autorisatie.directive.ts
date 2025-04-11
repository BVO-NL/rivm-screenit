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
import { Directive, inject, Input, OnInit, TemplateRef, ViewContainerRef } from '@angular/core'
import { AutorisatieService } from '@/autorisatie/service/autorisatie.service'
import { SecurityConstraint } from '@shared/types/autorisatie/security-constraint'

@Directive({
  selector: '[appAutorisatie]',
  standalone: true,
})
export class AutorisatieDirective implements OnInit {
  private viewContainer = inject(ViewContainerRef)
  private templateRef = inject(TemplateRef)
  private autorisatieService = inject(AutorisatieService)

  @Input({ required: true }) appAutorisatie: SecurityConstraint | undefined

  ngOnInit() {
    if (this.appAutorisatie && this.autorisatieService.isToegestaan(this.appAutorisatie)) {
      if (this.viewContainer.length === 0) {
        this.viewContainer.createEmbeddedView(this.templateRef)
      }
    } else {
      this.viewContainer.clear()
    }
  }
}
