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
export enum ClientContactActieType {
  GEEN = 'GEEN',
  OPNIEUW_AANVRAGEN_CLIENTGEGEVENS = 'OPNIEUW_AANVRAGEN_CLIENTGEGEVENS',
  TIJDELIJK_ADRES = 'TIJDELIJK_ADRES',
  DEELNAMEWENSEN = 'DEELNAMEWENSEN',
  AANPASSEN_AANHEF = 'AANPASSEN_AANHEF',
  CERVIX_DEELNAME_BUITEN_BVO_BMHK = 'CERVIX_DEELNAME_BUITEN_BVO_BMHK',
  COLON_AANVRAGEN_NIEUWE_FIT = 'COLON_AANVRAGEN_NIEUWE_FIT',
  COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN = 'COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN',
  COLON_NIEUWE_AFSPRAAK_AANMAKEN = 'COLON_NIEUWE_AFSPRAAK_AANMAKEN',
  COLON_AFMELDEN = 'COLON_AFMELDEN',
  COLON_HERAANMELDEN = 'COLON_HERAANMELDEN',
  COLON_HUISARTS_WIJZIGEN = 'COLON_HUISARTS_WIJZIGEN',
  COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN = 'COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN',
  COLON_OPEN_UITNODIGING = 'COLON_OPEN_UITNODIGING',
  BEZWAAR = 'BEZWAAR',
  INZAGE_PERSOONSGEGEVENS = 'INZAGE_PERSOONSGEGEVENS',
  CERVIX_AFMELDEN = 'CERVIX_AFMELDEN',
  CERVIX_HERAANMELDEN = 'CERVIX_HERAANMELDEN',
  CERVIX_UITSTEL = 'CERVIX_UITSTEL',
  CERVIX_ZAS_AANVRAGEN = 'CERVIX_ZAS_AANVRAGEN',
  CERVIX_HERDRUK = 'CERVIX_HERDRUK',
  CERVIX_FRISSE_START = 'CERVIX_FRISSE_START',
  CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN = 'CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN',
  MAMMA_RONDE_FORCEREN = 'MAMMA_RONDE_FORCEREN',
  MAMMA_AFSPRAAK_MAKEN = 'MAMMA_AFSPRAAK_MAKEN',
  MAMMA_AFSPRAAK_MAKEN_FORCEREN = 'MAMMA_AFSPRAAK_MAKEN_FORCEREN',
  MAMMA_AFSPRAAK_WIJZIGEN = 'MAMMA_AFSPRAAK_WIJZIGEN',
  MAMMA_UITSTELLEN = 'MAMMA_UITSTELLEN',
  MAMMA_AFMELDEN = 'MAMMA_AFMELDEN',
  MAMMA_HERAANMELDEN = 'MAMMA_HERAANMELDEN',
  MAMMA_HERBEOORDELEN = 'MAMMA_HERBEOORDELEN',
  MAMMA_DOELGROEP_WIJZIGEN = 'MAMMA_DOELGROEP_WIJZIGEN',
  MAMMA_HUISARTS_WIJZIGEN = 'MAMMA_HUISARTS_WIJZIGEN',
  MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS = 'MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS',
  MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS = 'MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS',
  MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK = 'MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK',
  MAMMA_VERZOEK_CLIENT_CONTACT = 'MAMMA_VERZOEK_CLIENT_CONTACT',
  MAMMA_INFOBRIEF_PROTHESEN = 'MAMMA_INFOBRIEF_PROTHESEN',
}

export const clientContactActieTypeLabels: Record<ClientContactActieType, string> = {
  [ClientContactActieType.GEEN]: 'Geen vervolgstap',
  [ClientContactActieType.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS]: 'Opnieuw aanvragen cliëntgegevens',
  [ClientContactActieType.TIJDELIJK_ADRES]: 'Tijdelijk adres',
  [ClientContactActieType.DEELNAMEWENSEN]: 'Deelnamewensen',
  [ClientContactActieType.AANPASSEN_AANHEF]: 'Aanhef aanpassen',
  [ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK]: 'Deelname buiten BVO BMHK',
  [ClientContactActieType.COLON_AANVRAGEN_NIEUWE_FIT]: 'Aanvragen nieuwe FIT darmkanker',
  [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN]: 'Afspraak wijzigen / afzeggen darmkanker',
  [ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN]: 'Afspraak aanmaken darmkanker',
  [ClientContactActieType.COLON_AFMELDEN]: 'Afmelden darmkanker',
  [ClientContactActieType.COLON_HERAANMELDEN]: 'Heraanmelden darmkanker',
  [ClientContactActieType.COLON_HUISARTS_WIJZIGEN]: 'Huisarts darmkanker vastleggen',
  [ClientContactActieType.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: 'Aanvraag verwijderen uitslag',
  [ClientContactActieType.COLON_OPEN_UITNODIGING]: 'Open uitnodiging darmkanker',
  [ClientContactActieType.BEZWAAR]: 'Keuze gebruik gegevens',
  [ClientContactActieType.INZAGE_PERSOONSGEGEVENS]: 'Inzage/overdracht persoonsgegevens',
  [ClientContactActieType.CERVIX_AFMELDEN]: 'Afmelden baarmoederhalskanker',
  [ClientContactActieType.CERVIX_HERAANMELDEN]: 'Heraanmelden baarmoederhalskanker',
  [ClientContactActieType.CERVIX_UITSTEL]: 'Uitstel baarmoederhalskanker',
  [ClientContactActieType.CERVIX_ZAS_AANVRAGEN]: 'Aanvraag ZAS',
  [ClientContactActieType.CERVIX_HERDRUK]: 'Aanmaken nieuwe uitnodiging baarmoederhalskanker',
  [ClientContactActieType.CERVIX_FRISSE_START]: 'Frisse start baarmoederhalskanker',
  [ClientContactActieType.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: 'Aanvraag verwijderen resultaten monster',
  [ClientContactActieType.MAMMA_RONDE_FORCEREN]: 'Ronde forceren',
  [ClientContactActieType.MAMMA_AFSPRAAK_MAKEN]: 'Afspraak maken borstkanker',
  [ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN]: 'Afspraak forceren',
  [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN]: 'Afspraak wijzigen borstkanker',
  [ClientContactActieType.MAMMA_UITSTELLEN]: 'Uitstel borstkanker',
  [ClientContactActieType.MAMMA_AFMELDEN]: 'Afmelden borstkanker',
  [ClientContactActieType.MAMMA_HERAANMELDEN]: 'Heraanmelden borstkanker',
  [ClientContactActieType.MAMMA_HERBEOORDELEN]: 'Herbeoordelen',
  [ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN]: 'Doelgroep wijzigen',
  [ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN]: 'Huisarts borstkanker wijzigen',
  [ClientContactActieType.MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS]: 'Mindervalidenonderzoek in ziekenhuis',
  [ClientContactActieType.MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS]: 'Mindervalidenonderzoek in ziekenhuis terugdraaien',
  [ClientContactActieType.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK]: 'Geen vervolg onderbroken onderzoek borstkanker',
  [ClientContactActieType.MAMMA_VERZOEK_CLIENT_CONTACT]: 'Maak oproepbrief na onderbroken onderzoek borstkanker',
  [ClientContactActieType.MAMMA_INFOBRIEF_PROTHESEN]: 'Infobrief prothesen aanvragen',
}
