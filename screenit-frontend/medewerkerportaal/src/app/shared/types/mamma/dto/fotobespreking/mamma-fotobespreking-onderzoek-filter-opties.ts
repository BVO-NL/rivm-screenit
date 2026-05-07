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
import { EnumOptie } from '@shared/types/enum-optie'
import { MammaOnderzoekRedenFotobespreking } from '@shared/types/mamma/enum/mamma-onderzoek-reden-fotobespreking'
import { MammaLezingRedenenFotobesprekingMbber } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-mbber'
import { MammaLezingRedenenFotobesprekingRadioloog } from '@shared/types/mamma/enum/mamma-lezing-redenen-fotobespreking-radioloog'
import { MammaLaesieType } from '@shared/types/mamma/enum/mamma-laesie-type'
import { MammaFollowUpConclusieStatus } from '@shared/types/mamma/enum/mamma-follow-up-conclusie-status'

export interface MammaFotobesprekingOnderzoekFilterOptiesDto {
  redenFotobesprekingDoorMbber: MammaOnderzoekRedenFotobespreking[]
  redenFotobesprekingMetMbber: MammaLezingRedenenFotobesprekingMbber[]
  redenFotobesprekingDoorRadioloog: MammaLezingRedenenFotobesprekingRadioloog[]
  redenDoorverwijzing: MammaLaesieType[]
  followUp: MammaFollowUpConclusieStatus[]
}

export interface MammaFotobesprekingOnderzoekFilterOpties {
  redenFotobesprekingDoorMbber: EnumOptie<MammaOnderzoekRedenFotobespreking>[]
  redenFotobesprekingMetMbber: EnumOptie<MammaLezingRedenenFotobesprekingMbber>[]
  redenFotobesprekingDoorRadioloog: EnumOptie<MammaLezingRedenenFotobesprekingRadioloog>[]
  redenDoorverwijzing: EnumOptie<MammaLaesieType>[]
  followUp: EnumOptie<MammaFollowUpConclusieStatus>[]
}
