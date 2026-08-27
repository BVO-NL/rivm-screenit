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
export enum BezwaarType {
  GEEN_WETENSCHAPPELIJK_ONDERZOEK = 'GEEN_WETENSCHAPPELIJK_ONDERZOEK',
  GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK = 'GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK',
  GEEN_KWALITEITSWAARBORGING = 'GEEN_KWALITEITSWAARBORGING',
  GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS = 'GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS',
  GEEN_SIGNALERING_VERWIJSADVIES = 'GEEN_SIGNALERING_VERWIJSADVIES',
  GEEN_REGISTRATIE_GEBOORTELAND = 'GEEN_REGISTRATIE_GEBOORTELAND',
  GEEN_OPNAME_UIT_BPR = 'GEEN_OPNAME_UIT_BPR',
  GEEN_UITWISSELING_MET_DE_HUISARTS = 'GEEN_UITWISSELING_MET_DE_HUISARTS',
  VERZOEK_TOT_VERWIJDERING_DOSSIER = 'VERZOEK_TOT_VERWIJDERING_DOSSIER',
}

export const bezwaarTypeLabels: Record<BezwaarType, string> = {
  [BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK]: 'Ik wil dat jullie mijn gegevens niet gebruiken voor wetenschappelijk onderzoek',
  [BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK]: 'Ik wil dat jullie mijn lichaamsmateriaal niet gebruiken voor wetenschappelijk onderzoek',
  [BezwaarType.GEEN_KWALITEITSWAARBORGING]: 'Ik wil dat jullie mijn gegevens niet gebruiken om de bevolkingsonderzoeken te verbeteren',
  [BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS]: 'Ik wil dat jullie mijn uitslagen en foto’s van het bevolkingsonderzoek borstkanker niet delen met het ziekenhuis',
  [BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES]:
    'Ik wil dat het laboratorium niet controleert of ik na een doorverwijzing van het bevolkingsonderzoek baarmoederhalskanker naar de gynaecoloog ben gegaan',
  [BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND]: 'Geen registratie geboorteland',
  [BezwaarType.GEEN_OPNAME_UIT_BPR]: 'Ik wil dat jullie al mijn contactgegevens en onderzoeksresultaten verwijderen',
  [BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS]: 'Geen uitwisseling met de huisarts',
  [BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER]: 'Ik wil dat jullie de onderzoeksresultaten van mijn deelname verwijderen',
}

export const bezwaarTypeSubtitels: Record<BezwaarType, string> = {
  [BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK]:
    'Als u deze keuze maakt, dan gebruiken we uw gegevens niet voor wetenschappelijk onderzoek. Dit geldt voor alle drie bevolkingsonderzoeken naar kanker, ook als u daar (nog) niet aan mee doet.',
  [BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK]:
    'Als u deze keuze maakt, wordt uw uitstrijkje of zelfafnameset niet gebruikt voor wetenschappelijk onderzoek en wordt het materiaal vernietigd.',
  [BezwaarType.GEEN_KWALITEITSWAARBORGING]:
    'Als u deze keuze maakt, dan gebruiken we uw gegevens niet voor de verbetering van onze onderzoeken. Dit geldt voor alle drie bevolkingsonderzoeken naar kanker, ook als u daar (nog) niet aan mee doet.',
  [BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS]:
    "Als u deze keuze maakt, dan krijgt het ziekenhuis uw foto's van het bevolkingsonderzoek niet. Als er verder onderzoek nodig is, maakt het ziekenhuis nieuwe foto's. De artsen in het ziekenhuis kunnen die foto's dan niet vergelijken met onze foto's. En ze horen niet wat de artsen tijdens het bevolkingsonderzoek hebben gezien.",
  [BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES]:
    'Als u deze keuze maakt dan controleert het laboratorium niet of u een afspraak heeft gemaakt met een gynaecoloog (als het nodig was om u verder te onderzoeken).',
  [BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND]: '',
  [BezwaarType.GEEN_OPNAME_UIT_BPR]: '',
  [BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS]: '',
  [BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER]: '',
}
