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
export const MAMMA_STRINGS = {
  labels: {
    onderzoeksdatum: 'Onderzoeksdatum',
    beoordelingseenheden: 'Beoordelingseenheden',
    screeningseenheden: 'Screeningseenheden',
    centraleEenheden: 'Centrale eenheden',
    be: 'BE',
    beoordelingseenheid: 'Beoordelingseenheid',
    rapportage: 'Rapportage',
    vragenlijst: 'Vragenlijst',
    insteltechniek: 'Insteltechniek',
    fotorichting: 'Fotorichting',
    intervalcarcinomen: 'Intervalcarcinomen',
    verwijzingen: 'Verwijzingen',
    t2ScreenDetected: 'T2+ screen detected',
    protheses: 'Protheses',
    discrepantie: 'Discrepantie',
    followUp: 'Follow-up',
  },
  visitatie: {
    pageTitle: 'Visitatie overzicht',
    buttons: {
      visitatie: 'Visitatie',
      generate: 'Visitatielijst genereren',
    },
    tooltips: {
      rapportageDownloaden: 'Rapportage downloaden',
      vragenlijstDownloaden: 'Vragenlijst downloaden',
      naarVisitatie: 'Naar visitatie gaan',
      visitatieVerwijderen: 'Visitatie verwijderen',
    },
    bewerken: {
      dialogTitle: 'Visitatie',
    },
    verwijderen: {
      dialogTitle: 'Visitatie verwijderen',
      body: 'Onderstaande visitatie definitief verwijderen?',
      labels: {},
    },
    notifications: {
      alUitgevoerd: 'Visitatie is al uitgevoerd.',
    },
  },
  visitatielijstGenereren: {
    dialogTitle: 'Visitatielijsten insteltechniek genereren',
    generateButton: 'Rapport genereren',
  },
  visitatielijstRapport: {
    dialogTitle: 'Visitatielijsten insteltechniek rapport',
    columns: {
      aantalMedewerkers: 'Aantal medewerkers',
      voldoendeIn2Maanden: 'Voldoende beelden in 2 maanden',
      voldoendeIn4Maanden: 'Voldoende beelden in 4 maanden',
      onvoldoendeBeelden: 'Onvoldoende beelden',
    },
    medewerkersTeWeinig: {
      heading: 'Overzicht van medewerkers met te weinig beelden:',
      columns: {
        aantalBeelden: 'Aantal beelden',
      },
    },
    resultatenPerMedewerker: {
      heading: 'Overzicht resultaten per medewerker:',
      columns: {
        aantalIn2Maanden: 'Aantal beelden in 2 maanden',
        aantalIn4Maanden: 'Aantal beelden in 4 maanden',
      },
    },
  },
  fotobespreking: {
    pageTitle: 'Fotobespreking bewerken',
    pageTitleMetOmschrijving: (omschrijving: string) => `Fotobespreking bewerken - ${omschrijving}`,
    labels: {
      periodeMedewerkerTitle: 'Periode en medewerker',
      specificatiesTitle: 'Specificaties onderzoek',
      redenMbber: "Reden fotobespreking door MBB'er",
      redenRadioloog: 'Reden fotobespreking door radioloog',
      redenMetMbber: "Reden fotobespreking met MBB'er",
      redenDoorverwijzing: 'Reden doorverwijzing',
    },
  },
  dense2: {
    pageTitle: 'DENSE-2 uitwisseling',
    configuratie: {
      cardTitle: 'Configuratie',
      subheader: 'Vul hier de configuratie in voor de DENSE-2 studie. Links ziet u de projecten waarvoor de configuratie geldt en rechts de projectnaam.',
      labels: {
        cem: 'DENSE-2 CEM',
        mri: 'DENSE-2 MRI',
        controlegroep: 'DENSE-2 Controlegroep',
        herinneringCem: 'DENSE-2 Herinnering CEM',
        herinneringMri: 'DENSE-2 Herinnering MRI',
        excluderen: 'Deelnemers uit deze projecten excluderen',
        dichtheidMeting: 'Densiteit meting DENSE-2 opslaan',
      },
      bevestiging: 'Weet u zeker dat u de configuratie wilt opslaan?',
      notifications: {
        saveSuccess: 'Configuratie is opgeslagen.',
      },
    },
    import: {
      cardTitle: 'Import',
      notifications: {
        typeError: 'Type bestand kan niet verwerkt worden.',
      },
    },
    export: {
      cardTitle: 'Export',
      notifications: {
        downloadSuccess: 'Bestand is gedownload.',
      },
    },
  },
}
