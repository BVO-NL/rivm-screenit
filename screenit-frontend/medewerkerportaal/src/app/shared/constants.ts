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
import { DsWeekday } from '@topicus-rgp-ds/web'
import { WeekdagOptie } from '@shared/types/weekdag-optie'

export const NL_DATE_FORMAT = 'dd-MM-yyyy'
export const NL_DATE_TIME_FORMAT = 'dd-MM-yyyy HH:mm'
export const DATE_FORMAT = 'yyyy-MM-dd'
export const COMPACT_DATE_FORMAT = 'yyyyMMdd'
export const ISO_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss"
export const LOCAL_TIME_FORMAT = 'HH:mm:ss'
export const TIME_FORMAT = 'HH:mm'
export const WEEK_DAGEN = [
  {
    label: 'Maandag',
    value: 1,
    afkorting: 'ma',
  },
  {
    label: 'Dinsdag',
    value: 2,
    afkorting: 'di',
  },
  {
    label: 'Woensdag',
    value: 3,
    afkorting: 'wo',
  },
  {
    label: 'Donderdag',
    value: 4,
    afkorting: 'do',
  },
  {
    label: 'Vrijdag',
    value: 5,
    afkorting: 'vr',
  },
  {
    label: 'Zaterdag',
    value: 6,
    afkorting: 'za',
  },
  {
    label: 'Zondag',
    value: 7,
    afkorting: 'zo',
  },
]
export const TDS_DAGEN: WeekdagOptie[] = [
  {
    day: DsWeekday.Maandag,
    selected: false,
    value: 1,
  },
  {
    day: DsWeekday.Dinsdag,
    selected: false,
    value: 2,
  },
  {
    day: DsWeekday.Woensdag,
    selected: false,
    value: 3,
  },
  {
    day: DsWeekday.Donderdag,
    selected: false,
    value: 4,
  },
  {
    day: DsWeekday.Vrijdag,
    selected: false,
    value: 5,
  },
  {
    day: DsWeekday.Zaterdag,
    selected: false,
    value: 6,
  },
  {
    day: DsWeekday.Zondag,
    selected: false,
    value: 7,
  },
]
export const COLON_ROOSTER_MAX_HERHALING_IN_MAANDEN = 18
export const AANTAL_RIJEN_PER_PAGINA = 20
