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
export enum RedenGbaVraag {
  ONJUIST_ADRES = 'ONJUIST_ADRES',
  ONJUISTE_PERSOONSGEGEVENS = 'ONJUISTE_PERSOONSGEGEVENS',
  MUTATIEBERICHT_ONBEKENDE_CLIENT = 'MUTATIEBERICHT_ONBEKENDE_CLIENT',
  BEZWAAR = 'BEZWAAR',
  BEZWAAR_INGETROKKEN = 'BEZWAAR_INGETROKKEN',
  AFGEMELD = 'AFGEMELD',
  AANGEMELD = 'AANGEMELD',
  BOVENGRENS_LEEFTIJD = 'BOVENGRENS_LEEFTIJD',
  SELECTIEBLOKKADE = 'SELECTIEBLOKKADE',
  BRIEF_VERSTUREN = 'BRIEF_VERSTUREN',
  ONVERWACHT_INDICATIE_VERWIJDERD = 'ONVERWACHT_INDICATIE_VERWIJDERD',
}

export const redenGbaVraagLabels: Record<RedenGbaVraag, string> = {
  [RedenGbaVraag.ONJUIST_ADRES]: 'Onjuist adres',
  [RedenGbaVraag.ONJUISTE_PERSOONSGEGEVENS]: 'Onjuiste persoonsgegevens',
  [RedenGbaVraag.MUTATIEBERICHT_ONBEKENDE_CLIENT]: 'Mutatiebericht ontvangen voor onbekende client',
  [RedenGbaVraag.BEZWAAR]: 'Bezwaar uitwisseling BRP gemaakt',
  [RedenGbaVraag.BEZWAAR_INGETROKKEN]: 'Bezwaar uitwisseling BRP ingetrokken',
  [RedenGbaVraag.AFGEMELD]: 'Afgemeld voor alle bevolkingsonderzoeken binnen leeftijddoelgroep',
  [RedenGbaVraag.AANGEMELD]: 'Heraanmelding met door BVO ingetrokken indicatie',
  [RedenGbaVraag.BOVENGRENS_LEEFTIJD]: 'Bovengrens leeftijd BRP indicatie bereikt',
  [RedenGbaVraag.SELECTIEBLOKKADE]: 'Geen reactie na selectieblokkade',
  [RedenGbaVraag.BRIEF_VERSTUREN]: 'Brief versturen bij door BVO ingetrokken indicatie',
  [RedenGbaVraag.ONVERWACHT_INDICATIE_VERWIJDERD]: 'Onverwacht indicatie-verwijderd-bericht ontvangen',
}
