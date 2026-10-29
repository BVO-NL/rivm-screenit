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
import { ClientContactActieType } from '@shared/types/algemeen/enum/client-contact-actie-type'

export enum ContactActieOptie {
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
  KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN = 'KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN',
  KEUZE_GEBRUIK_GEGEVENS_VERWERKEN = 'KEUZE_GEBRUIK_GEGEVENS_VERWERKEN',
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

export const contactActieOptieLabels: Record<ContactActieOptie, string> = {
  [ContactActieOptie.GEEN]: 'Geen vervolgstap',
  [ContactActieOptie.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS]: 'Opnieuw aanvragen cliëntgegevens',
  [ContactActieOptie.TIJDELIJK_ADRES]: 'Tijdelijk adres',
  [ContactActieOptie.DEELNAMEWENSEN]: 'Deelnamewensen',
  [ContactActieOptie.AANPASSEN_AANHEF]: 'Aanhef aanpassen',
  [ContactActieOptie.CERVIX_DEELNAME_BUITEN_BVO_BMHK]: 'Deelname buiten BVO BMHK',
  [ContactActieOptie.COLON_AANVRAGEN_NIEUWE_FIT]: 'Aanvragen nieuwe FIT darmkanker',
  [ContactActieOptie.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN]: 'Afspraak wijzigen / afzeggen darmkanker',
  [ContactActieOptie.COLON_NIEUWE_AFSPRAAK_AANMAKEN]: 'Afspraak aanmaken darmkanker',
  [ContactActieOptie.COLON_AFMELDEN]: 'Afmelden darmkanker',
  [ContactActieOptie.COLON_HERAANMELDEN]: 'Heraanmelden darmkanker',
  [ContactActieOptie.COLON_HUISARTS_WIJZIGEN]: 'Huisarts darmkanker vastleggen',
  [ContactActieOptie.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: 'Aanvraag verwijderen uitslag',
  [ContactActieOptie.COLON_OPEN_UITNODIGING]: 'Open uitnodiging darmkanker',
  [ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN]: 'Brief keuze gebruik gegevens versturen',
  [ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_VERWERKEN]: 'Keuze gebruik gegevens verwerken',
  [ContactActieOptie.INZAGE_PERSOONSGEGEVENS]: 'Inzage/overdracht persoonsgegevens',
  [ContactActieOptie.CERVIX_AFMELDEN]: 'Afmelden baarmoederhalskanker',
  [ContactActieOptie.CERVIX_HERAANMELDEN]: 'Heraanmelden baarmoederhalskanker',
  [ContactActieOptie.CERVIX_UITSTEL]: 'Uitstel baarmoederhalskanker',
  [ContactActieOptie.CERVIX_ZAS_AANVRAGEN]: 'Aanvraag ZAS',
  [ContactActieOptie.CERVIX_HERDRUK]: 'Aanmaken nieuwe uitnodiging baarmoederhalskanker',
  [ContactActieOptie.CERVIX_FRISSE_START]: 'Frisse start baarmoederhalskanker',
  [ContactActieOptie.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: 'Aanvraag verwijderen resultaten monster',
  [ContactActieOptie.MAMMA_RONDE_FORCEREN]: 'Ronde forceren',
  [ContactActieOptie.MAMMA_AFSPRAAK_MAKEN]: 'Afspraak maken borstkanker',
  [ContactActieOptie.MAMMA_AFSPRAAK_MAKEN_FORCEREN]: 'Afspraak forceren',
  [ContactActieOptie.MAMMA_AFSPRAAK_WIJZIGEN]: 'Afspraak wijzigen borstkanker',
  [ContactActieOptie.MAMMA_UITSTELLEN]: 'Uitstel borstkanker',
  [ContactActieOptie.MAMMA_AFMELDEN]: 'Afmelden borstkanker',
  [ContactActieOptie.MAMMA_HERAANMELDEN]: 'Heraanmelden borstkanker',
  [ContactActieOptie.MAMMA_HERBEOORDELEN]: 'Herbeoordelen',
  [ContactActieOptie.MAMMA_DOELGROEP_WIJZIGEN]: 'Doelgroep wijzigen',
  [ContactActieOptie.MAMMA_HUISARTS_WIJZIGEN]: 'Huisarts borstkanker wijzigen',
  [ContactActieOptie.MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS]: 'Mindervalidenonderzoek in ziekenhuis',
  [ContactActieOptie.MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS]: 'Mindervalidenonderzoek in ziekenhuis terugdraaien',
  [ContactActieOptie.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK]: 'Geen vervolg onderbroken onderzoek borstkanker',
  [ContactActieOptie.MAMMA_VERZOEK_CLIENT_CONTACT]: 'Maak oproepbrief na onderbroken onderzoek borstkanker',
  [ContactActieOptie.MAMMA_INFOBRIEF_PROTHESEN]: 'Infobrief prothesen aanvragen',
}

const contactActieOptiesPerActieType: Record<ClientContactActieType, ContactActieOptie[]> = {
  [ClientContactActieType.GEEN]: [ContactActieOptie.GEEN],
  [ClientContactActieType.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS]: [ContactActieOptie.OPNIEUW_AANVRAGEN_CLIENTGEGEVENS],
  [ClientContactActieType.TIJDELIJK_ADRES]: [ContactActieOptie.TIJDELIJK_ADRES],
  [ClientContactActieType.DEELNAMEWENSEN]: [ContactActieOptie.DEELNAMEWENSEN],
  [ClientContactActieType.AANPASSEN_AANHEF]: [ContactActieOptie.AANPASSEN_AANHEF],
  [ClientContactActieType.CERVIX_DEELNAME_BUITEN_BVO_BMHK]: [ContactActieOptie.CERVIX_DEELNAME_BUITEN_BVO_BMHK],
  [ClientContactActieType.COLON_AANVRAGEN_NIEUWE_FIT]: [ContactActieOptie.COLON_AANVRAGEN_NIEUWE_FIT],
  [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN]: [ContactActieOptie.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN],
  [ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN]: [ContactActieOptie.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
  [ClientContactActieType.COLON_AFMELDEN]: [ContactActieOptie.COLON_AFMELDEN],
  [ClientContactActieType.COLON_HERAANMELDEN]: [ContactActieOptie.COLON_HERAANMELDEN],
  [ClientContactActieType.COLON_HUISARTS_WIJZIGEN]: [ContactActieOptie.COLON_HUISARTS_WIJZIGEN],
  [ClientContactActieType.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: [ContactActieOptie.COLON_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN],
  [ClientContactActieType.COLON_OPEN_UITNODIGING]: [ContactActieOptie.COLON_OPEN_UITNODIGING],
  [ClientContactActieType.BEZWAAR]: [ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_BRIEF_VERSTUREN, ContactActieOptie.KEUZE_GEBRUIK_GEGEVENS_VERWERKEN],
  [ClientContactActieType.INZAGE_PERSOONSGEGEVENS]: [ContactActieOptie.INZAGE_PERSOONSGEGEVENS],
  [ClientContactActieType.CERVIX_AFMELDEN]: [ContactActieOptie.CERVIX_AFMELDEN],
  [ClientContactActieType.CERVIX_HERAANMELDEN]: [ContactActieOptie.CERVIX_HERAANMELDEN],
  [ClientContactActieType.CERVIX_UITSTEL]: [ContactActieOptie.CERVIX_UITSTEL],
  [ClientContactActieType.CERVIX_ZAS_AANVRAGEN]: [ContactActieOptie.CERVIX_ZAS_AANVRAGEN],
  [ClientContactActieType.CERVIX_HERDRUK]: [ContactActieOptie.CERVIX_HERDRUK],
  [ClientContactActieType.CERVIX_FRISSE_START]: [ContactActieOptie.CERVIX_FRISSE_START],
  [ClientContactActieType.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN]: [ContactActieOptie.CERVIX_VERWIJDEREN_UITSLAG_BRIEF_AANVRAGEN],
  [ClientContactActieType.MAMMA_RONDE_FORCEREN]: [ContactActieOptie.MAMMA_RONDE_FORCEREN],
  [ClientContactActieType.MAMMA_AFSPRAAK_MAKEN]: [ContactActieOptie.MAMMA_AFSPRAAK_MAKEN],
  [ClientContactActieType.MAMMA_AFSPRAAK_MAKEN_FORCEREN]: [ContactActieOptie.MAMMA_AFSPRAAK_MAKEN_FORCEREN],
  [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN]: [ContactActieOptie.MAMMA_AFSPRAAK_WIJZIGEN],
  [ClientContactActieType.MAMMA_UITSTELLEN]: [ContactActieOptie.MAMMA_UITSTELLEN],
  [ClientContactActieType.MAMMA_AFMELDEN]: [ContactActieOptie.MAMMA_AFMELDEN],
  [ClientContactActieType.MAMMA_HERAANMELDEN]: [ContactActieOptie.MAMMA_HERAANMELDEN],
  [ClientContactActieType.MAMMA_HERBEOORDELEN]: [ContactActieOptie.MAMMA_HERBEOORDELEN],
  [ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN]: [ContactActieOptie.MAMMA_DOELGROEP_WIJZIGEN],
  [ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN]: [ContactActieOptie.MAMMA_HUISARTS_WIJZIGEN],
  [ClientContactActieType.MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS]: [ContactActieOptie.MAMMA_MINDERVALIDE_ONDERZOEK_ZIEKENHUIS],
  [ClientContactActieType.MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS]: [ContactActieOptie.MAMMA_MINDERVALIDE_NIET_MEER_ZIEKENHUIS],
  [ClientContactActieType.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK]: [ContactActieOptie.MAMMA_CLIENT_WIL_GEEN_VERVOLG_ONDERZOEK],
  [ClientContactActieType.MAMMA_VERZOEK_CLIENT_CONTACT]: [ContactActieOptie.MAMMA_VERZOEK_CLIENT_CONTACT],
  [ClientContactActieType.MAMMA_INFOBRIEF_PROTHESEN]: [ContactActieOptie.MAMMA_INFOBRIEF_PROTHESEN],
}

export const bepaalContactActieOpties = (actieType: ClientContactActieType): ContactActieOptie[] => contactActieOptiesPerActieType[actieType]
