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
import { Component, input, OnInit } from '@angular/core'
import { DsFooterActionsLeftDirective, DsFooterActionsRightDirective, DsModalComponent, DsModalConfig } from '@topicus-rgp-ds/web'

@Component({
  selector: 'app-base-dialog',
  imports: [DsModalComponent, DsFooterActionsRightDirective, DsFooterActionsLeftDirective],
  template: `
    <ds-modal [config]="modalConfig" [header]="titel()" data-testid="modal-titel">
      <ng-content select="[body]"></ng-content>
      <ng-template ds-footer-actions-left>
        <ng-content select="[verwijder-button]"></ng-content>
      </ng-template>
      <ng-template ds-footer-actions-right>
        <ng-content select="[buttons]"></ng-content>
      </ng-template>
    </ds-modal>
  `,
})
export class BaseDialogComponent implements OnInit {
  modalConfig: DsModalConfig = new DsModalConfig()
  titel = input.required<string>()
  size = input<string>('md')
  contentPadding = input<string>('1rem')

  private static readonly MODAL_BREEDTES: Record<string, string> = {
    md: '30rem',
    lg: '40rem',
    xl: '60rem',
    xxl: '80vw',
  }

  public ngOnInit(): void {
    this.modalConfig.enableClose = false
    this.modalConfig.width = BaseDialogComponent.MODAL_BREEDTES[this.size()] ?? BaseDialogComponent.MODAL_BREEDTES['md']
    this.modalConfig.maxWidth = BaseDialogComponent.MODAL_BREEDTES['xxl']
    this.modalConfig.contentPadding = this.contentPadding()
  }
}
