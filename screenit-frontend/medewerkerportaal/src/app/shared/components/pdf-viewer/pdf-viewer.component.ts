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
import { Component, effect, ElementRef, inject, input, output, Renderer2, viewChild } from '@angular/core'

@Component({
  selector: 'app-pdf-viewer',
  imports: [],
  template: ` <iframe #iframeElement (load)="refreshStyle()" width="100%" height="100%">
    De pdf kon niet worden geladen, klik <a href="#" (click)="opslaan.emit()">hier</a> om de PDF te downloaden
  </iframe>`,
  styles: `
    :host {
      position: relative;
      display: block;
    }

    iframe {
      width: 100%;
      height: calc(70vh - 150px);
      border: 0;
    }
  `,
})
export class PdfViewerComponent {
  private readonly renderer = inject(Renderer2)
  url = input<string | undefined>(undefined)
  iframeElement = viewChild<ElementRef>('iframeElement')
  opslaan = output<void>()

  constructor() {
    effect(() => {
      if (this.url() && this.iframeElement()) {
        this.renderer.setAttribute(this.iframeElement()?.nativeElement, 'src', this.url()!)
      }
    })
  }

  refreshStyle() {
    const embed = this.iframeElement()!.nativeElement.contentDocument.querySelector('embed')
    if (embed) {
      this.renderer.setStyle(embed, 'position', 'absolute')
      this.renderer.setStyle(embed, 'top', '0')
      this.renderer.setStyle(embed, 'left', '0')
    }
  }
}
