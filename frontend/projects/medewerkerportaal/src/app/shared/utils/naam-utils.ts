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
import { ClientDto } from '../types/algemeen/dto/client.dto'
import { NaamGebruik } from '../types/algemeen/enum/naam-gebruik'
import { ClientPaspoortDto } from '../types/algemeen/dto/client-paspoort.dto'
import { isStringNullOfLeeg } from '@shared/utils/string-utils'
import { PersoonDto } from '@shared/types/algemeen/dto/persoon.dto'

type NaamData = (PersoonDto & Partial<Pick<ClientDto, 'titel' | 'naamGebruik' | 'partnerAchternaam' | 'partnerTussenvoegsel'>>) | ClientPaspoortDto

export class NaamUtils {
  static getNaamMedewerker(medewerker: PersoonDto): string | null {
    if (medewerker == null) {
      return null
    }

    let medewerkerNaam = ''
    if (!isStringNullOfLeeg(medewerker.voorletters)) {
      medewerkerNaam += `${this.standaardiseerVoorletters(medewerker.voorletters)} `
    }
    medewerkerNaam += this.getTussenvoegselEnAchternaam(medewerker)
    return medewerkerNaam
  }

  private static getTussenvoegselEnAchternaam(medewerker: PersoonDto): string | null {
    if (medewerker == null) {
      return null
    }

    let medewerkerNaam = ''
    if (!isStringNullOfLeeg(medewerker.tussenvoegsel)) {
      medewerkerNaam += `${medewerker.tussenvoegsel} `
    }
    medewerkerNaam += medewerker.achternaam
    return medewerkerNaam
  }

  private static standaardiseerVoorletters(ruweVoorletters?: string): string {
    let voorletters = ''
    if (!isStringNullOfLeeg(ruweVoorletters)) {
      for (const voorletter of ruweVoorletters!.toUpperCase()) {
        if (/[A-Z]/i.test(voorletter)) {
          voorletters += `${voorletter}.`
        }
      }
    }
    return voorletters
  }

  static getNaamVolledig(medewerker: PersoonDto): string | null {
    if (medewerker == null) {
      return null
    }

    let naamVolledig = medewerker.achternaam

    if (!isStringNullOfLeeg(medewerker.voorletters) || !isStringNullOfLeeg(medewerker.voornaam)) {
      naamVolledig += ', '
    }

    if (!isStringNullOfLeeg(medewerker.voorletters)) {
      naamVolledig += `${medewerker.voorletters} `
    } else if (!isStringNullOfLeeg(medewerker.voornaam)) {
      naamVolledig += `${medewerker.voornaam} `
    }

    if (!isStringNullOfLeeg(medewerker.tussenvoegsel)) {
      naamVolledig += `${medewerker.tussenvoegsel} `
    }

    return naamVolledig
  }

  static titelVoorlettersTussenvoegselEnAanspreekAchternaam(client: NaamData): string {
    if (client == null) {
      return ''
    }

    let naam = ''
    if (!isStringNullOfLeeg(client.titel)) {
      naam += `${client.titel} `
    }

    naam += this.voorlettersTussenvoegselEnAanspreekAchternaam(client)

    return naam
  }

  static voorlettersTussenvoegselEnAanspreekAchternaam(client: NaamData): string {
    if (client == null) {
      return ''
    }

    let naam = ''
    const voorletters = this.getVoorlettersClient(client)
    naam += voorletters
    if (voorletters) {
      naam += ' '
    }

    naam += this.getAanspreekTussenvoegselEnAchternaam(client)

    return naam
  }

  static getVoorlettersClient(client: NaamData): string {
    if (client == null) {
      return ''
    }

    let voorletters = ''
    if (client.voornaam) {
      const voornamen = client.voornaam.split(' ')
      for (const voornaam of voornamen) {
        if (voornaam) {
          voorletters += voornaam.toUpperCase().charAt(0)

          if (voornaam.toUpperCase().startsWith('IJ')) {
            voorletters += voornaam.toUpperCase().charAt(1)
          }

          voorletters += '.'
        }
      }
    }
    return voorletters
  }

  static getAanspreekTussenvoegselEnAchternaam(client: NaamData): string {
    if (client == null) {
      return ''
    }
    let volledigeNaam = this.getTussenvoegsel(client)
    if (!isStringNullOfLeeg(volledigeNaam)) {
      volledigeNaam += ' '
    }
    volledigeNaam += this.getAanspreekNaamZonderTussenvoegsel(client)
    return volledigeNaam
  }

  private static getAanspreekNaamZonderTussenvoegsel(client: NaamData): string {
    let volledigeNaam = ''
    const naamGebruik = client.naamGebruik
    if (naamGebruik == NaamGebruik.EIGEN || naamGebruik == NaamGebruik.EIGEN_PARTNER) {
      if (!isStringNullOfLeeg(client.achternaam)) {
        volledigeNaam += client.achternaam
      }
      if (naamGebruik == NaamGebruik.EIGEN_PARTNER) {
        if (volledigeNaam && client.partnerAchternaam) {
          volledigeNaam += ' - '
        }
        volledigeNaam += this.getPartnernaam(client)
      }
    } else if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER || naamGebruik == NaamGebruik.PARTNER_EIGEN) {
      if (client.partnerAchternaam) {
        volledigeNaam += client.partnerAchternaam
      }
      if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER_EIGEN) {
        if (volledigeNaam && client.achternaam) {
          volledigeNaam += ' - '
        }
        if (volledigeNaam && client.tussenvoegsel) {
          volledigeNaam += client.tussenvoegsel + ' '
        }
        if (client.achternaam) {
          volledigeNaam += client.achternaam
        }
      }
    }
    return volledigeNaam
  }

  private static getTussenvoegsel(client: NaamData): string {
    let tussenvoegels = ''
    const naamGebruik = client.naamGebruik
    if (
      (NaamGebruik.EIGEN == naamGebruik ||
        NaamGebruik.EIGEN_PARTNER == naamGebruik ||
        (NaamGebruik.PARTNER_EIGEN == naamGebruik && !client.partnerTussenvoegsel && !client.partnerAchternaam)) &&
      client.tussenvoegsel
    ) {
      tussenvoegels += ' ' + client.tussenvoegsel
    } else if ((NaamGebruik.PARTNER == naamGebruik || NaamGebruik.PARTNER_EIGEN == naamGebruik) && client.partnerTussenvoegsel) {
      tussenvoegels += ' ' + client.partnerTussenvoegsel
    } else if (naamGebruik == null) {
      if (client.partnerTussenvoegsel && client.partnerAchternaam) {
        tussenvoegels += ' ' + client.partnerTussenvoegsel
      } else if (client.tussenvoegsel) {
        tussenvoegels += ' ' + client.tussenvoegsel
      }
    }
    return tussenvoegels
  }

  private static getPartnernaam(client: NaamData): string {
    let partnernaam = ''
    if (client.partnerTussenvoegsel) {
      partnernaam += client.partnerTussenvoegsel + ' '
    }
    if (client.partnerAchternaam) {
      partnernaam += client.partnerAchternaam
    }
    return partnernaam
  }

  static getTitel(titel: string) {
    const titelAanwezig = titel !== null
    return titelAanwezig ? titel + ' ' : ''
  }
}
