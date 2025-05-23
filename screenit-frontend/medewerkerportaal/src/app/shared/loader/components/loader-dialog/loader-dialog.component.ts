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
import { Component } from '@angular/core'
import { ClrModalModule } from '@clr/angular'
import { LoaderComponent } from '@shared/loader/components/loader/loader.component'

@Component({
  selector: 'app-loader-dialog',
  imports: [ClrModalModule, LoaderComponent],
  template: ` <clr-modal [clrModalClosable]="false" [clrModalOpen]="true" [clrModalStaticBackdrop]="true" clrModalSize="sm">
    <div class="modal-body clr-display-flex clr-align-content-center">
      <app-loader />
    </div>
  </clr-modal>`,
})
export class LoaderDialogComponent {}
