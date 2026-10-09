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
export const COLON_STRINGS = {
  feestdagen: {
    pageTitle: 'Roosterbeperkingen voor feestdagen',
    addButton: 'Feestdag',
    columns: {
      beperking: 'Beperking',
    },
    bewerken: {
      titleNieuw: 'Feestdag toevoegen',
      titleBestaand: 'Feestdag bewerken',
      errors: {
        naamVerplicht: 'De naam is verplicht.',
        datumVerplicht: 'De datum is verplicht.',
        invalidDate: 'De ingevoerde datum is niet valide.',
      },
    },
    foutmeldingPopup: {
      columns: {
        intakelocatie: 'Intakelocatie',
        afspraakslots: 'Afspraakslots',
      },
    },
    deleteConfirm: 'Weet u zeker dat u deze feestdag wilt verwijderen?',
  },
  rooster: {
    buttons: {
      blokkade: 'Blokkade',
      afspraakslot: 'Afspraakslot',
      viewWeek: 'Week',
      viewWerkweek: 'Werkweek',
      viewDag: 'Dag',
      deleteAfspraakslots: 'Afspraakslots',
      deleteBlokkades: 'Blokkades',
    },
    legenda: {
      vrijTooltip: 'Vrij te verplaatsen',
      vrij: 'Vrij',
      capaciteitTooltip: 'Gebruikt voor capaciteitsberekening',
      capaciteit: 'Capaciteit',
      intakeTooltip: 'Intake ingepland',
      intake: 'Intake',
      digitaleIntakeTooltip: 'Digitale intake',
      digitaleIntake: 'Digitale intake',
      geblokkerdTooltip: 'Geblokkeerd door een blokkade',
      geblokkerd: 'Geblokkeerd',
      beperktTooltip: 'Weekend, tijd en feestdag beperkt planbaar',
      beperkt: 'Beperkt planbaar',
    },
    geprognosticeerd: 'Geprognosticeerde aantal afspraakslots:',
    huidig: '/ Huidige aantal gemaakte afspraakslots dit jaar:',
    warnings: {
      slotsNietIngevoerd: (tekst: string, deadline: string) =>
        `De afspraakslots voor de periode ${tekst} zijn nog niet ingevoerd. Om cliënten uit te kunnen nodigen is het noodzakelijk dat deze slots voor ${deadline} in ScreenIT worden ingevoerd.`,
    },
    labels: {
      kamer: 'Kamer',
      alleKamers: 'Alle kamers',
      heleDag: 'Hele dag',
      herhalen: 'Herhalen',
      alleenOpWerkdagen: 'Alleen op werkdagen',
      nietHerhalen: 'Niet herhalen',
      dagelijks: 'Dagelijks',
      wekelijks: 'Wekelijks',
      tweewekelijks: '2-wekelijks',
    },
    afspraakslot: {
      titleNieuw: 'Aanmaken afspraakslot(s)',
      titleBestaand: 'Bewerken afspraakslot',
      titleReadOnly: 'Afspraakslot',
      confirm: {
        deleteBody: 'Weet u zeker dat u dit afspraakslot wilt verwijderen?',
      },
      notifications: {
        saveError: 'Er is een fout opgetreden bij het opslaan van het afspraakslot. Probeer het opnieuw.',
      },
    },
    blokkade: {
      titleNieuw: 'Aanmaken blokkade(s)',
      titleBestaand: 'Bewerken blokkade',
      titleReadOnly: 'Blokkade',
      confirm: {
        deleteBody: 'Weet u zeker dat u deze blokkade wilt verwijderen?',
      },
      notifications: {
        saveError: 'Er is een fout opgetreden bij het opslaan van de blokkade. Probeer het opnieuw.',
      },
    },
    bulkAanmaken: {
      confirm: {
        kunnenNietWorden: (type: string) => `Onderstaande ${type} kunnen niet worden aangemaakt:`,
        afwijkendeDagTijd: (type: string) => `Onderstaande ${type} worden aangemaakt op een afwijkende dag/tijd:`,
      },
    },
    tijdslots: {
      verwijderen: {
        dialogTitle: (type: string) => `${type} verwijderen`,
        leeg: (type: string) => `Er zijn geen ${type} gevonden`,
      },
      bevestiging: {
        wordtVerwijderd: 'Wordt verwijderd',
        wordtNietVerwijderd: 'Wordt niet verwijderd',
        gereserveerd: 'Gereserveerd',
        metAfspraak: 'Met afspraak',
        letOp: 'Let op:',
        maxVerwijderd: (max: number, type: string) => `Er worden maximaal ${max} ${type} tegelijk verwijderd`,
      },
      notifications: {
        deleteError: 'Er is een fout opgetreden bij het verwijderen van de tijdslots. Probeer het opnieuw.',
      },
    },
    beperking: {
      zacht: 'Zacht',
      hard: 'Hard',
    },
  },
  weekendWerkdag: {
    pageTitle: 'Roosterbeperkingen voor tijd en weekend',
    tijdsbeperkingTitle: 'Tijdsbeperking (elke dag)',
    weekendbeperkingTitle: 'Weekendbeperking',
    labels: {
      beperking: 'Beperking',
      ochtend: 'Ochtend',
      avond: 'Avond',
      zaterdag: 'Zaterdag',
      zondag: 'Zondag',
    },
    notifications: {
      saveSuccess: 'De beperkingen zijn opgeslagen.',
    },
  },
}
