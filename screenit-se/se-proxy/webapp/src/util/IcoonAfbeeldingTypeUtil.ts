/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import type {AnnotatieIcoonType, LegacyIcoonType} from "../datatypes/AnnotatieIcoon"
import type {IcoonAfbeelding} from "../datatypes/IcoonAfbeelding"

export const getAfbeeldingByType = (type: AnnotatieIcoonType | LegacyIcoonType, isNietVisueleInspectie?: boolean): IcoonAfbeelding => {
	if (isNietVisueleInspectie && type === "AMPUTATIE") {
		return amputatieVorigeOnderzoekProps
	}

	switch (type) {
		case "AMPUTATIE":
			return amputatieProps

		case "BORSTVERGROTING":
			return borstvergrotingProps

		case "EENZIJDIGE_BORSTVERKLEINING":
			return eenzijdigeBorstverkleiningProps

		case "DUBBELZIJDIGE_BORSTVERKLEINING":
			return dubbelzijdigeBorstverkleiningProps

		case "UITWENDIGE_AFWIJKING":
			return uitwendigeAfwijkingProps

		case "LITTEKEN_LBRO":
			return littekenLbRoProps

		case "LITTEKEN_RBLO":
			return littekenRbLoProps

		case "LITTEKEN_VERTICAAL":
			return littekenVerticaalProps

		case "LITTEKEN_HORIZONTAAL":
			return littekenHorizontaalProps

		case "INGETROKKEN_TEPEL":
			return ingetrokkenTepelProps

		case "GROTER_DAN":
			return groterDanProps

		case "KLEINER_DAN":
			return kleinerDanProps

		case "WRAT":
			return massaProps

		case "SIGNALERING_ASYMMETRIE":
			return asymmetrieProps

		case "SIGNALERING_MASSA":
			return signaleringMassaProps

		case "SIGNALERING_ARCHITECTUURVERSTORING":
			return architectuurVerstoringProps

		case "SIGNALERING_CALCIFICATIES":
			return calcificatiesProps

		case "LEGACY_PLUS":
			return plusProps

		case "LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES":
			return architectuurVerstoringCalcificatiesProps

		case "LEGACY_MASSA_MET_ARCHITECTUURVERSTORING":
			return architectuurVerstoringMassaProps

		case "LEGACY_CONFORM":
			return conformProps

		case "LEGACY_GEEN_BIJZONDERHEDEN":
			return geenBijzonderhedenProps

		case "LEGACY_MASSA_MET_SPICULAE":
			return massaMetUitlopersProps

		case "LEGACY_PROJECTIE_NAAR_LINKS":
			return projectiesLinksProps

		case "LEGACY_PROJECTIE_NAAR_RECHTS":
			return projectiesRechtsProps

		case "LEGACY_MASSA_MET_CALCIFICATIES":
			return verdichtingMicrokalkProps

		case "LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES":
			return verdichtingUitlopendMicrokalkProps

		case "LEGACY_MARKERING":
			return markeringProps

		case "LEGACY_BENIGNE_KALK":
			return begineProps

		default:
			throw Error("Supplied value isn't valid")
	}
}
const iconenInspectieFolder = "./images/iconen_inspectie"
const amputatieProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/amputatie.svg`,
	width: 20,
	height: 21,
}
const amputatieVorigeOnderzoekProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/amputatieVorigeOnderzoek.svg`,
	width: 20,
	height: 21,
}
const borstvergrotingProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/borstvergroting.svg`,
	width: 25,
	height: 25,
}
const dubbelzijdigeBorstverkleiningProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/dubbelzijdige-borstverkleining.svg`,
	width: 38,
	height: 26,
}
const eenzijdigeBorstverkleiningProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/eenzijdige-borstverkleining.svg`,
	width: 18,
	height: 26,
}
const groterDanProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/groter-dan.svg`,
	width: 25,
	height: 25,
}
const ingetrokkenTepelProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/ingetrokken-tepel.svg`,
	width: 25,
	height: 25,
}
const kleinerDanProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/kleiner-dan.svg`,
	width: 25,
	height: 25,
}
const littekenHorizontaalProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/litteken-horizontaal.svg`,
	width: 37,
	height: 9,
}
const littekenLbRoProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/litteken-lo-rb.svg`,
	width: 25,
	height: 25,
}
const littekenRbLoProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/litteken-rb-lo.svg`,
	width: 25,
	height: 25,
}
const littekenVerticaalProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/litteken-verticaal.svg`,
	width: 9,
	height: 33,
}
const massaProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/massa.svg`,
	width: 25,
	height: 25,
}
const uitwendigeAfwijkingProps: IcoonAfbeelding = {
	afbeelding: `${iconenInspectieFolder}/uitwendige-afwijking.svg`,
	width: 25,
	height: 25,
	isRightUpperCornerOrigin: true,
}

const iconenBeoordelingFolder = "./images/iconen_beoordeling"
const plusProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_PLUS.svg`,
	width: 25,
	height: 25,
}
const architectuurVerstoringProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/ARCHITECTUURVERSTORING.svg`,
	width: 26,
	height: 26,
}
const asymmetrieProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/ASYMMETRIE.svg`,
	width: 26,
	height: 26,
}
const calcificatiesProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/CALCIFICATIES.svg`,
	width: 30,
	height: 33,
}
const signaleringMassaProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/MASSA.svg`,
	width: 26,
	height: 26,
}
const architectuurVerstoringCalcificatiesProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES.svg`,
	width: 26,
	height: 26,
}
const architectuurVerstoringMassaProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_MASSA_MET_ARCHITECTUURVERSTORING.svg`,
	width: 26,
	height: 26,
}
const conformProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_CONFORM.svg`,
	width: 26,
	height: 26,
}
const geenBijzonderhedenProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_GEEN_BIJZONDERHEDEN.svg`,
	width: 26,
	height: 26,
}
const massaMetUitlopersProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_MASSA_MET_SPICULAE.svg`,
	width: 26,
	height: 26,
}
const projectiesLinksProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_PROJECTIE_NAAR_LINKS.svg`,
	width: 26,
	height: 26,
}
const projectiesRechtsProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_PROJECTIE_NAAR_RECHTS.svg`,
	width: 26,
	height: 26,
}
const verdichtingMicrokalkProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_MASSA_MET_CALCIFICATIES.svg`,
	width: 26,
	height: 26,
}
const verdichtingUitlopendMicrokalkProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES.svg`,
	width: 26,
	height: 26,
}
const markeringProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_MARKERING.svg`,
	width: 26,
	height: 26,
}
const begineProps: IcoonAfbeelding = {
	afbeelding: `${iconenBeoordelingFolder}/LEGACY_BENIGNE_KALK.svg`,
	width: 26,
	height: 26,
}
