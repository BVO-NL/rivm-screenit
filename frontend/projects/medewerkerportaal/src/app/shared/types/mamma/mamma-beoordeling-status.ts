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
export enum MammaBeoordelingStatus {
  EERSTE_LEZING = '1ste lezing',
  EERSTE_LEZING_OPGESLAGEN = '1ste lezing opgeslagen',
  TWEEDE_LEZING = '2de lezing',
  TWEEDE_LEZING_OPGESLAGEN = '2de lezing opgeslagen',
  DISCREPANTIE = 'Discrepantie',
  ARBITRAGE = 'Arbitrage',
  OPGESCHORT = 'Beoordeling opgeschort',
  OPGESCHORT_MET_AFSPRAAK = 'Beoordeling opgeschort met afspraak',
  ONBEOORDEELBAAR_TE_VERSTUREN = 'Onbeoordeelbaar te versturen',
  ONBEOORDEELBAAR = 'Onbeoordeelbaar',
  VERSLAG_MAKEN = 'Verslag maken',
  VERSLAG_GEREED = 'Verslag gereed',
  VERSLAG_AFGEKEURD = 'Verslag afgekeurd',
  VERSLAG_GOEDKEURING_OPGESCHORT = 'Verslag goedkeuring opgeschort',
  UITSLAG_ONGUNSTIG = 'Uitslag ongunstig',
  GUNSTIG_MET_NEVENBEVINDING = 'Uitslag gunstig met nevenbevinding',
  UITSLAG_GUNSTIG = 'Uitslag gunstig',
  GEANNULEERD = 'Geannuleerde beoordeling',
}
