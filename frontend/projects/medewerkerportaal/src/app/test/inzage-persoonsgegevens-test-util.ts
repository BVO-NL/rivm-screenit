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
import { OverdrachtPersoonsgegevensVerwerkenDto } from '@shared/types/algemeen/dto/overdracht-persoonsgegevens.dto'
import { AanvraagBriefStatus } from '@shared/types/algemeen/enum/aanvraag-brief-status'
import { BriefType } from '@shared/types/algemeen/enum/brief-type'
import { maakBrief } from '@test/brief-test-utils'
import { OverdrachtType } from '@shared/types/algemeen/enum/overdracht-type'

export function maakInzagePersoonsgegevens(overrides: Partial<OverdrachtPersoonsgegevensVerwerkenDto> = {}): OverdrachtPersoonsgegevensVerwerkenDto {
  return {
    status: AanvraagBriefStatus.BRIEF,
    verstuurdeBrieven: [
      maakBrief({
        id: 1,
        briefType: BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_AANVRAAG,
        documentNaam: 'brief',
      }),
    ],
    keuzes: { bkGegevens: false, bkBeelden: false, bmhkGegevens: false, dkGegevens: false },
    type: OverdrachtType.AANVRAAGFORMULIER,
    heeftMammaDossier: true,
    heeftCervixDossier: true,
    heeftColonDossier: true,
    ontvangenFormulierId: null,
    ...overrides,
  }
}
