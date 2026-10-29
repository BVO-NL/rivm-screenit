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
export const ALGEMEEN_STRINGS = {
  buttons: {
    annuleren: 'Annuleren',
    opslaan: 'Opslaan',
    toevoegen: 'Toevoegen',
    verwijderen: 'Verwijderen',
    bewerken: 'Bewerken',
    sluiten: 'Sluiten',
    zoeken: 'Zoeken',
    filteren: 'Filteren',
    bevestigen: 'Bevestigen',
    exporteren: 'Exporteren',
    genereren: 'Genereren',
    ok: 'Ok',
    allesUitklappen: 'Alles uitklappen',
    wijzigingenOpslaan: 'Wijzigingen opslaan',
    overzicht: 'Overzicht',
    bekijken: 'Bekijken',
    formulierInzien: 'Formulier inzien',
    formulierVervangen: 'Formulier vervangen',
    nogmaalsVersturen: 'Nogmaals versturen',
    inzien: 'Inzien',
    selecteren: 'Selecteren',
    volgende: 'Volgende',
    vorige: 'Vorige',
    bladeren: 'Bladeren',
    verwerken: 'Verwerken',
    downloaden: 'Downloaden',
    afzeggen: 'Afzeggen',
    begrepen: 'Begrepen',
    aanvragen: 'Aanvragen',
    heraanmelden: 'Heraanmelden',
  },
  dialogs: {
    bevestiging: {
      titel: 'Bevestiging',
      wiltUDoorgaan: 'Wilt u doorgaan?',
      neeButtonTekst: 'Nee, annuleren',
      jaButtonTekst: 'Ja, bevestigen',
    },
  },
  labels: {
    naam: 'Naam',
    datum: 'Datum',
    datumVan: 'Datum van',
    datumTotEnMet: 'Datum tot en met',
    tijd: 'Tijd',
    status: 'Status',
    omschrijving: 'Omschrijving',
    geboortedatum: 'Geboortedatum',
    geboortenaam: 'Geboortenaam',
    bsn: 'BSN',
    postcode: 'Postcode',
    postcodeHuisnummer: 'Postcode + huisnummer',
    straatnaam: 'Straatnaam',
    huisnummer: 'Huisnummer',
    straatnaamHuisnummer: 'Straatnaam en huisnummer',
    huisletter: 'Huisletter',
    toevoeging: 'Toevoeging',
    aanduiding: 'Aanduiding',
    plaats: 'Plaats',
    afstand: 'Afstand',
    afstandInKm: 'Afstand (km)',
    starttijd: 'Starttijd',
    eindtijd: 'Eindtijd',
    begindatum: 'Begindatum',
    einddatum: 'Einddatum',
    medewerker: 'Medewerker',
    bestandsnaam: 'Bestandsnaam',
    bestand: 'Bestand',
    bestandUploaden: 'Bestand uploaden',
    reden: 'Reden',
    datumTijd: 'Datum/tijd',
    notitie: 'Notitie',
    actie: 'Actie',
    uitgevoerdeActie: 'Uitgevoerde actie',
    bevolkingsonderzoek: 'Bevolkingsonderzoek',
    briefType: 'Brieftype',
    bron: 'Bron',
    briefVerstuurd: 'Brief verstuurd',
    ontvangenOp: 'Ontvangen op',
    tijdelijkAdres: 'Tijdelijk adres',
    postAdres: 'Postadres',
    aNummer: 'A-nummer',
    doelgroep: 'Doelgroep',
    dubbeletijdReden: 'Dubbele tijd reden',
    overlijdensdatum: 'Overlijdensdatum',
    aanspreekvorm: 'Aanspreekvorm',
    genderidentiteit: 'Genderidentiteit',
    mobielNummer: 'Mobiel nummer',
    extraNummer: 'Extra nummer',
    emailadres: 'E-mailadres',
    briefkenmerk: 'Briefkenmerk',
    locatie: 'Locatie',
    locatiebeschrijving: 'Locatiebeschrijving',
    intakelocatie: 'Intakelocatie',
    uur: 'uur',
    typeAfspraak: 'Type afspraak',
    brief: 'Brief',
    kiesEenBrief: 'Kies een brief',
    betreft: 'Betreft',
    nee: 'Nee',
    ja: 'Ja',
    onbekend: 'Onbekend',
    dagen: 'Dagen',
    vanaf: 'Vanaf',
    totEnMet: 'Tot en met',
    gestart: 'Gestart',
    afgerond: 'Afgerond',
    aangemaakt: 'Aangemaakt',
    door: 'Door',
    aangemaaktDoor: 'Aangemaakt door',
    medewerkercode: 'Medewerkercode',
    aan: 'Aan',
    uit: 'Uit',
    client: 'Cliënt',
    bevolkingsonderzoeken: 'Bevolkingsonderzoek(en)',
    projectnaam: 'Projectnaam',
    gewijzigdDoor: 'Gewijzigd door',
    laatstBijgewerktOp: 'Laatst bijgewerkt op',
    acties: 'Acties',
    documentnaam: 'Documentnaam',
  },
  placeholders: {
    geboortedatum: 'DD-MM-JJJJ',
    datum: 'DD-MM-JJJJ',
    intakelocatieNaam: 'Naam intakelocatie',
    plaats: 'Plaats',
    afstand: 'Maximale afstand',
    briefkenmerk: 'KXXXX',
    postcode: '1234AB',
    huisnummer: '123',
    huisletter: 'A',
    mobiel: '06-12345678',
    kiesReden: 'Kies een reden',
  },
  errors: {
    verplicht: (field: string) => `${field} is verplicht.`,
    invalidDate: 'De ingevoerde datum is niet valide.',
    maxKarakters: (field: string, max: number) => `${field} mag maximaal ${max} karakters bevatten.`,
    minKarakters: (field: string, min: number) => `${field} moet minimaal ${min} karakters bevatten.`,
    invalidPostcode: 'Voer een geldige postcode in (bijv. 1234AB).',
    invalidEmail: 'Het e-mailadres is incorrect.',
    invalidPhone: 'Het telefoonnummer is incorrect.',
    invalidMobile: 'Het mobiele nummer is incorrect.',
    invalidBsn: 'Ingevoerde BSN is niet valide.',
    invalidTime: 'De ingevoerde tijd is incorrect.',
    invalidDateTime: 'De ingevoerde datum en tijd zijn niet valide.',
    datumTijdInVerleden: 'De ingevoerde datum en tijd mogen niet in het verleden liggen.',
    datumInVerleden: 'De datum ligt in het verleden. Dit is niet toegestaan.',
    datumEnOfTijdInVerleden: 'Datum en/of tijd liggen in het verleden. Dit is niet toegestaan.',
    minimumDatum: (datum: string) => `De datum mag niet voor ${datum} liggen`,
    maximumDatum: (datum: string) => `De datum mag niet na ${datum} liggen`,
    briefkenmerkPatroon: 'Briefkenmerk moet beginnen met een K.',
    geboortedatumVerplicht: 'De geboortedatum is verplicht.',
    geboortedatumToekomst: 'De geboortedatum mag niet in de toekomst liggen.',
    huisnummerAlleenCijfers: 'Huisnummer mag alleen cijfers bevatten.',
    huisletterPatroon: 'Huisletter moet een letter zijn (a-z).',
    veldNaVeld: (field1: string, field2: string) => `${field1} moet na ${field2} liggen.`,
    veldVoorVeld: (field1: string, field2: string) => `${field1} moet voor ${field2} liggen.`,
    ingevuldeDatumNa: (datum: string) => `De ingevulde datum moet na ${datum} liggen.`,
    ingevuldeDatumVoor: (datum: string) => `De ingevulde datum moet voor ${datum} liggen.`,
    maxToekomst: (field: string, tijdseenheid: string) => `De ${field} mag maximaal ${tijdseenheid} in de toekomst liggen.`,
    aantalBestanden: (maxAantal: number) => `Er mogen maximaal ${maxAantal} bestanden worden geüpload.`,
    extensie: (bestandsnaam: string, toegestaneExtensies: string[]) =>
      `'${bestandsnaam}' heeft niet de juiste extensie. U moet een bestand met extensie '${toegestaneExtensies}' uploaden.`,
    exceptie: 'Er ging iets mis. Neem contact op met de beheerder.',
  },
  session: {
    title: 'Uw sessie verloopt',
    body: (timer: string) => `Uw sessie verloopt over: ${timer}`,
    instruction: 'Klik op "Ok" om ingelogd te blijven.',
  },
  timepicker: {
    ariaOpenButton: 'Open timepicker',
    ariaTijdkiezer: 'Tijdkiezer',
    ariaUren: 'Uren',
    ariaMinuten: 'Minuten',
  },
  clientZoeken: {
    pageTitle: 'Cliënten zoeken',
    zoekPlaceholder: 'Zoek een cliënt',
    instructieGeavanceerd: 'Vul minstens 1 veld in om een cliënt te zoeken',
    instructieStandaard: 'Vul geboortedatum en minstens 1 extra veld in om een cliënt te zoeken',
    geavanceerdZoeken: 'Geavanceerd zoeken',
    labels: {
      bkUitnodigingsnummer: 'BK uitnodigingsnummer',
      bmhkMonsterId: 'BMHK monsterID',
      bmhkUitnodigingsId: 'BMHK uitnodigingsID',
      dkBarcode: 'DK barcode',
      placeholderBarcode: 'Barcode',
      dkUitnodigingsId: 'DK uitnodigingsID',
    },
    tabel: {
      leeg: 'Geen cliënten gevonden',
      badgeOverleden: 'Overleden',
    },
  },
  clientDossier: {
    pageTitle: 'Cliëntdossier',
    clientZoekenButton: 'Cliënt zoeken',
    tabClientgegevens: 'Cliëntgegevens',
    tabAfspraken: 'Afspraken',
    tabActies: 'Acties',
    tabMeldingen: 'Meldingen',
    tabCisHistorie: 'CIS Historie',
    acties: {
      nietOndersteund: 'Deze actie wordt (nog) niet ondersteund',
      opnieuwAanvragenClientgegevens: {
        dialogTitle: 'Opnieuw aanvragen cliëntgegevens',
        notifications: {
          succes: 'Cliëntgegevens succesvol aangevraagd',
        },
      },
      categorieen: {
        algemeen: 'Algemeen',
        colon: 'Darmkanker',
        cervix: 'Baarmoederhalskanker',
        mamma: 'Borstkanker',
      },
      aanhefWijzigen: {
        dialogTitle: 'Aanhef wijzigen',
        aanspreekvormPlaceholder: 'Kies een aanspreekvorm',
        wijzigenButton: 'Wijzigen',
        notifications: {
          succes: 'Contactgegevens succesvol gewijzigd',
        },
      },
      keuzeGebruikGegevensBrief: {
        dialogTitle: 'Brief "Keuze gebruik gegevens" versturen',
        briefkeuze: 'Briefkeuze',
        handtekeningVergeten: 'Handtekening vergeten',
        notifications: {
          succes: 'Brief succesvol aangevraagd',
        },
      },
      bezwaar: {
        verwerkenDialog: {
          titel: 'Keuzes verwerken',
          notifications: {
            succes: 'Keuzes succesvol verwerkt.',
            nietGewijzigd: 'Keuzes gebruik gegevens zijn niet gewijzigd.',
            gegevensVerwijderd: 'Gegevens succesvol verwijderd.',
          },
        },
        keuzeDialog: {
          titel: 'Keuze gebruik gegevens verwerken',
          vraag: 'Selecteer welk proces u wilt starten voor de cliënt',
          buttons: {
            gegevensVerwijderen: 'Gegevens verwijderen',
            keuzesVerwerken: 'Keuzes verwerken',
          },
        },
        gegevensVerwijderenDialog: {
          titel: 'Gegevens verwijderen',
          waarschuwing: 'Let op: dit kan niet ongedaan gemaakt worden. Weet u zeker dat u dit wilt uitvoeren?',
        },
      },
      heraanmeldenBmhk: {
        dialogTitle: 'Heraanmelden baarmoederhalskanker',
        uitnodigingMelding: 'Er wordt automatisch een uitnodiging gestuurd.',
        notifications: {
          succes: 'Cliënt succesvol heraangemeld.',
        },
      },
      deelnameBuitenBvoBmhk: {
        dialogTitle: 'Deelname buiten BVO BMHK',
        dialogBody: 'Weet u zeker dat u de cliënt wilt registreren als deelnemer buiten BVO baarmoederhalskanker?',
        registrerenButton: 'Ja, registreren',
        notifications: {
          succes: 'Cliënt succesvol geregistreerd',
        },
      },
      deelnamewensen: {
        dialogTitle: 'Deelnamewensen',
        deelnamewensenPerBvo: 'Deelnamewensen per BVO',
        errors: {
          geenNieuweDeelnamewens: 'Selecteer minimaal één nieuwe deelnamewens.',
        },
        notifications: {
          succes: 'Deelnamewensen succesvol opgeslagen',
        },
      },
    },
  },
  clientInfoBewerken: {
    dialogTitle: 'Contactgegevens aanpassen',
    notifications: {
      saveSuccess: 'Contactgegevens succesvol gewijzigd.',
      saveError: 'Er is een fout opgetreden bij het opslaan van de contactgegevens. Probeer het later opnieuw.',
    },
    doelgroepWijzigenMelding: {
      titel: 'Doelgroep gewijzigd',
      body: (datum: string) =>
        `De cliënt heeft een afspraak staan op ${datum}. Het wordt geadviseerd om de afspraak te verzetten of uit te stellen als de doelgroep wordt gewijzigd.`,
    },
  },
  brpBewerken: {
    dialogTitle: 'BRP Status',
    tijdelijkAdresHeading: 'Tijdelijk BRP-adres',
    adresVerwijderenButton: 'Adres verwijderen',
    deleteConfirm: 'Wilt u het tijdelijke BRP-adres echt verwijderen?',
  },
  adresgegevensBewerken: {
    dialogTitle: 'Adresgegevens aanpassen',
    tijdelijkAdresHeading: 'Tijdelijk adres',
    notifications: {
      saveSuccess: 'Adresgegevens succesvol opgeslagen.',
      saveSuccessMetVerlopen: 'Adresgegevens succesvol opgeslagen. Dit tijdelijke adres heeft een einddatum in het verleden en is daarom niet zichtbaar in het overzicht.',
      saveError: 'Er is een fout opgetreden bij het opslaan van de adresgegevens. Probeer het later opnieuw.',
    },
  },
  bvoStatus: {
    cardTitle: 'Status bevolkingsonderzoek',
    columns: {
      vervolgIndicatie: 'Vervolg indicatie',
    },
    volgendeUitnodiging: (datum: string) => `Volgende uitnodiging op ${datum}`,
  },
  projecten: {
    cardTitle: 'Projecten',
    leeg: 'Er zijn geen projecten gevonden',
  },
  screeningsorganisatie: {
    cardTitle: 'Verantwoordelijke screeningsorganisatie',
  },
  handleidingen: {
    pageTitle: 'Handleidingen',
    addButton: 'Handleiding toevoegen',
    aanmaken: {
      dialogTitle: 'Handleiding aanmaken',
      uploaderLabel: 'Handleiding uploaden',
      notifications: {
        uploadSuccess: 'Alle handleidingen zijn succesvol geüpload.',
      },
    },
    bewerken: {
      dialogTitle: 'Handleiding bewerken',
      fileSelectorLabel: 'Handleiding',
      notifications: {
        updateSuccess: (naam: string) => `${naam} is succesvol bijgewerkt.`,
      },
    },
    confirm: {
      deleteTitle: 'Bevestiging',
      deleteBody: (naam: string) => `Weet u zeker dat u '${naam}' wilt verwijderen?`,
    },
    notifications: {
      downloadSuccess: (naam: string) => `${naam} is gedownload`,
      deleteSuccess: (naam: string) => `'${naam}' is succesvol verwijderd.`,
      deleteError: 'Er is een fout opgetreden bij het verwijderen van de handleiding.',
    },
  },
  ebo: {
    clientZoekenPageTitle: 'Cliënt zoeken',
    keuzeHerstellenPageTitle: 'Keuze gebruik gegevens herstellen',
    leeg: 'Er zijn geen cliënten gevonden',
    notifications: {
      keuzeHersteld: 'Keuze gebruik gegevens BRP hersteld.',
    },
  },
  navigation: {
    algemeen: 'Algemeen',
    overzicht: 'Overzicht',
    bevolkingsonderzoek: 'Bevolkingsonderzoek',
  },
  briefInzien: {
    dialogTitle: 'Brief openen',
  },
  notities: {
    cardTitle: 'Notities',
    buttons: {
      notitieToevoegen: 'Notitie toevoegen',
      bewerkNotitie: 'Bewerk notitie',
      bekijkNotitie: 'Bekijk notitie',
      verwijderNotitie: 'Notitie verwijderen',
    },
    geenNotitie: {
      titel: 'Geen notities',
      subtitel: 'Voeg een notitie toe',
    },
    placeholder: 'Voeg hier je notitie toe',
  },
  brieven: {
    herdrukVan: 'Herdruk van',
    activerenSuccessMessage: 'Brief wordt niet meer tegengehouden',
    tegenhoudenSuccessMessage: 'Brief wordt tegengehouden',
    opnieuwAanmakenSuccessMessage: 'Brief wordt opnieuw aangemaakt',
    dialogTitle: 'Template inzien',
    bewaarTermijnVerlopenMessage: 'De bewaartermijn van de verstuurde brief is verlopen.',
    briefInzienFoutmessage: 'Er is een fout bij het ophalen van de brief.',
    buttons: {
      verstuurdeBriefInzien: 'Verstuurde brief inzien',
      templateInzien: 'Template inzien',
      herstelBriefAanmaken: 'Herstel brief aanmaken',
      tegenhouden: 'Tegenhouden',
      opnieuwAanmaken: 'Opnieuw aanmaken',
    },
  },
  bezwaar: {
    bezwaarPanel: {
      titel: 'Actuele keuzes gebruik gegevens',
      titelKort: 'Actueel',
      subtitel: 'Bekijk keuzehistorie onder overzicht',
    },
    bezwaarMomentDialog: {
      titel: 'Keuzehistorie',
    },
    bezwaarLijst: {
      geenBezwaren: {
        titel: 'Geen actuele keuzes doorgegeven',
      },
      groep: {
        algemeen: 'Algemeen',
      },
    },
  },
  brp: {
    labels: {
      indicatieStatus: 'Indicatie status',
      datumLaatsteBrpMutatie: 'Datum laatste BRP-mutatie',
      laatstAangevraagdOp: 'Laatst aangevraagd op',
      tijdelijkBrpAdres: 'Tijdelijk BRP-adres',
      vertrokkenUitNl: 'Vertrokken uit Nederland',
    },
  },
  afspraken: {
    digitaal: 'Digitaal',
    intakeafspraak: 'Intakeafspraak',
    digitaalNaarFysiekDialog: {
      body: 'Wilt u de afspraak omzetten naar een fysieke afspraak?',
      jaButtonTekst: 'Ja, omzetten',
    },
    buttons: {
      tijdstipWijzigen: 'Tijdstip wijzigen',
      locatieWijzigen: 'Locatie wijzigen',
      afspraakMaken: 'Maak afspraak',
      afspraakAfzeggen: 'Afspraak afzeggen',
    },
    geenAfspraken: {
      titel: 'Geen afspraak gevonden',
      subtitel: 'De cliënt kan een afspraak maken voor het onderzoek.',
    },
    verplaatsenDialog: {
      titelTijdstipWijzigen: 'Tijdstip wijzigen',
      titelLocatieWijzigen: 'Locatie wijzigen',
      titelAanmaken: 'Maak afspraak',
      afspraakZoekenStep: {
        titel: 'Kies een afspraak',
        subtitel: 'Wanneer wil je een afspraak maken?',
        vanafDatum: 'De Datum vanaf',
        totEnMetDatum: 'de Datum tot en met',
      },
      bevestigingStep: {
        titel: 'Afspraakbevestiging',
        briefKeuze: 'Kies een afspraakbevestiging voor de cliënt:',
        geenBevestigingsbrief: 'Geen bevestigingsbrief versturen',
        medischeVerwijzing: {
          titel: 'Medische verwijzing',
          vraag: 'Gaat dit om een medische verwijzing?',
          veldnaam: 'Deze vraag',
        },
      },
      samenvattingStep: {
        titel: 'Samenvatting',
        afspraakDetails: 'Afspraakdetails',
      },
      succesMessageTijdstipGewijzigd: 'Tijdstip is gewijzigd',
      succesMessageLocatieGewijzigd: 'Locatie is gewijzigd',
      succesMessageAangemaakt: 'Afspraak is ingepland',
      afspraakMessage: (datum: string, tijd: string, locatie: string) => `Huidige afspraak staat gepland op ${datum} om ${tijd} uur op locatie ${locatie}.`,
      afspraakMessageDigitaal: (datum: string, locatie: string) => `Huidige afspraak staat gepland in ${datum} op locatie ${locatie}.`,
    },
    afzeggen: {
      dialogTitle: 'Afspraak afzeggen',
      briefTegenhouden: 'De afspraak wordt afgezegd en er wordt <b>geen</b> bevestigingsbrief verstuurd naar de cliënt.',
      briefVersturen: 'De afspraak wordt afgezegd en er wordt een bevestigingsbrief verstuurd naar de cliënt.',
      succesMessage: 'De afspraak is succesvol afgezegd.',
    },
    geenBriefSturen: 'Geen bevestigingsbrief sturen',
  },
}
