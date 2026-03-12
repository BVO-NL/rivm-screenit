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
import { MammaVisitatieLijstRapportData } from '@shared/types/mamma/dto/mamma-visitatielijst-dialog-data'

export class MammaVisitatielijstRapport {
  private minimumBeeldenPerMedewerker = 240
  private aantalOnderzoekenPerMedewerker: MammaVisitatieLijstRapportData
  aantalMedewerkersVerwerkt: number
  aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden: number
  aantalMedewerkersMetOnvoldoendeBeelden: number
  aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden: number
  uitgeslotenMedewerkerIds: { medewerkercode: string; aantal: number }[]
  aantalOnderzoekenLijst: { medewerkercode: string; aantalBeeldenBinnen2Maanden: number; aantalBeeldenBinnen4Maanden: number }[]

  constructor(rapportData: MammaVisitatieLijstRapportData) {
    this.aantalOnderzoekenPerMedewerker = rapportData
    this.aantalMedewerkersVerwerkt = this.getAantalMedewerkersVerwerkt()
    this.aantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden = this.getAantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden()
    this.aantalMedewerkersMetOnvoldoendeBeelden = this.getAantalMedewerkersMetOnvoldoendeBeelden()
    this.aantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden = this.getAantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden()
    this.uitgeslotenMedewerkerIds = this.getUitgeslotenMedewerkerIds()
    this.aantalOnderzoekenLijst = this.getAantalOnderzoekenLijst()
  }

  getAantalMedewerkersVerwerkt(): number {
    return Object.keys(this.aantalOnderzoekenPerMedewerker).length
  }

  getAantalMedewerkersMetVoldoendeBeeldenBinnen2Maanden(): number {
    return Object.values(this.aantalOnderzoekenPerMedewerker).filter((aantal) => aantal.aantalBeeldenBinnen2Maanden >= this.minimumBeeldenPerMedewerker).length
  }

  getAantalMedewerkersMetOnvoldoendeBeelden(): number {
    return Object.values(this.aantalOnderzoekenPerMedewerker).filter((aantal) => aantal.aantalBeeldenBinnen4Maanden < this.minimumBeeldenPerMedewerker).length
  }

  getAantalMedewerkersMetVoldoendeBeeldenBinnen4Maanden(): number {
    return Object.values(this.aantalOnderzoekenPerMedewerker).filter((aantal) => aantal.aantalBeeldenBinnen4Maanden >= this.minimumBeeldenPerMedewerker).length
  }

  getUitgeslotenMedewerkerIds(): { medewerkercode: string; aantal: number }[] {
    return Object.entries(this.aantalOnderzoekenPerMedewerker)
      .filter(([_, aantal]) => aantal.aantalBeeldenBinnen4Maanden < this.minimumBeeldenPerMedewerker)
      .map(([medewerkercode, aantal]) => ({ medewerkercode, aantal: aantal.aantalBeeldenBinnen4Maanden }))
  }

  getAantalOnderzoekenLijst(): { medewerkercode: string; aantalBeeldenBinnen2Maanden: number; aantalBeeldenBinnen4Maanden: number }[] {
    return Object.entries(this.aantalOnderzoekenPerMedewerker).map(([medewerkercode, aantal]) => ({
      medewerkercode,
      ...aantal,
    }))
  }
}
