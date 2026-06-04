/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import {RouteProps} from "react-router"
import * as React from "react"
import {ClientContactActieType} from "../datatypes/ClientContactActieType"
import LoginPage from "../pages/authentication/LoginPage"
import DigiDInloggenAfgebrokenPage from "../pages/authentication/DigiDInloggenAfgebrokenPage"
import LogoutPage from "../pages/authentication/LogoutPage"
import NietInBevolkingsonderzoekPage from "../pages/authentication/NietInBevolkingsonderzoekPage"
import LandingPage from "../pages/landing/LandingPage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../datatypes/Bevolkingsonderzoek"
import BvoLandingPageController from "../pages/bvo/landing/BvoLandingPageController"
import CervixUitstelAanvragenPage from "../pages/bvo/cervix/CervixUitstelAanvragenPage"
import ZasAanvragenPage from "../pages/bvo/cervix/ZasAanvragenPage"
import CervixHerdrukAanvragenPage from "../pages/bvo/cervix/CervixHerdrukAanvragenPage"
import ColonFitAanvragenPage from "../pages/bvo/colon/ColonFitAanvragenPage"
import ColonAfspraakAfzeggenPage from "../pages/bvo/colon/ColonAfspraakAfzeggenPage"
import ColonAfspraakMakenPage from "../pages/bvo/colon/afspraak/ColonAfspraakMakenPage"
import AfmeldenPage from "../pages/bvo/gedeeld/AfmeldenPage"
import HeraanmeldenPage from "../pages/bvo/gedeeld/HeraanmeldenPage"
import ProfielPage from "../pages/profiel/ProfielPage"
import TelefoonnummerWijzigenPage from "../pages/profiel/TelefoonnummerWijzigenPage"
import TijdelijkAdresWijzigenPage from "../pages/profiel/TijdelijkAdresWijzigenPage"
import MammaAfspraakMakenPage from "../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage"
import HuisartsPage from "../pages/bvo/gedeeld/huisarts/HuisartsPage"
import MammaAfspraakUitstellenPage from "../pages/bvo/mamma/afspraak/MammaAfspraakUitstellenPage"
import AutoLoginPage from "../pages/authentication/AutoLoginPage"
import {capitalize} from "@mui/material"
import AanhefWijzigenPage from "../pages/profiel/AanhefWijzigenPage"
import EmailWijzigenPage from "../pages/profiel/email/EmailWijzigenPage"
import BezwaarWijzigenPage from "../pages/profiel/BezwaarWijzigenPage"
import MammaAfspraakBevestigenPage from "../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakBevestigenPage"
import MammaAfspraakBevestigingSelectiePage from "../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakBevestigingSelectiePage"
import MammaAfspraakHerinneringPage from "../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakHerinneringPage"
import MammaAfspraakOverzichtPage from "../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakOverzichtPage"
import MammaAfspraakHuisartsPage from "../pages/bvo/mamma/afspraak/bevestigingswizard/MammaAfspraakHuisartsPage"
import ColonAfspraakOverzichtPage from "../pages/bvo/colon/afspraak/ColonAfspraakOverzichtPage"
import {isDigitaleIntakeBeschikbaar} from "../utils/FeatureFlagsUtil"
import OpenstaandeOnderzoekenPage from "../pages/bvo/gedeeld/openstaande-onderzoeken/OpenstaandeOnderzoekenPage"
import ColonAfspraakIntakePage from "../pages/bvo/colon/afspraak/ColonAfspraakIntakePage"
import ColonAfspraakGezondheidPage from "../pages/bvo/colon/afspraak/ColonAfspraakGezondheidPage"
import ColonAfspraakMakenWizardPage from "../pages/bvo/colon/afspraak/ColonAfspraakMakenWizardPage"
import ColonAfspraakContactPage from "../pages/bvo/colon/afspraak/ColonAfspraakContactPage"

export type RoutePath =
	"/"
	| "/login/"
	| "/autologin/"
	| "/logout/"
	| "/inloggen-geannuleerd/"
	| "/niet-in-bevolkingsonderzoek/"
	| "/openstaande-onderzoeken/"

	| "/cervix/"
	| "/cervix/afmelden/"
	| "/cervix/heraanmelden/"
	| "/cervix/herdrukken/"
	| "/cervix/uitstellen/"
	| "/cervix/zas/"

	| "/colon/"
	| "/colon/afmelden/"
	| "/colon/afspraak-maken-heraanmelding/"
	| "/colon/afspraak-maken/"
	| "/colon/afspraak-wijzigen/"
	| "/colon/afspraak/intake/"
	| "/colon/afspraak/contact/"
	| "/colon/afspraak/uw-gezondheid/"
	| "/colon/afspraak/kiezen/"
	| "/colon/afspraak/overzicht/"
	| "/colon/afzeggen/"
	| "/colon/fit/"
	| "/colon/heraanmelden/"
	| "/colon/huisarts/"

	| "/mamma/"
	| "/mamma/afspraak/"
	| "/mamma/afspraak/bevestigen/"
	| "/mamma/afspraak/bevestiging-selectie/"
	| "/mamma/afspraak/herinnering/"
	| "/mamma/afspraak/overzicht/"
	| "/mamma/afspraak/uw-huisarts/"
	| "/mamma/afmelden/"
	| "/mamma/heraanmelden/"
	| "/mamma/huisarts/"
	| "/mamma/huisarts/zoeken/"
	| "/mamma/uitstellen/"

	| "/profiel/"
	| "/profiel/adres/"
	| "/profiel/telefoonnummer/"
	| "/profiel/aanspreekvorm/"
	| "/profiel/email/"
	| "/profiel/gebruikgegevens/"

export type RouteDef = RouteProps & {
	name: string
	path: RoutePath
	private: boolean
	component: React.ComponentType<any>
	requiredContactActions?: ClientContactActieType[]
	bvo?: Bevolkingsonderzoek
	redirectPage?: RoutePath
	activateGuard?: () => boolean
}

const routes: RouteDef[] = [
	{
		private: false,
		path: "/login/",
		name: "Inloggen",
		component: LoginPage,
	},
	{
		private: false,
		path: "/autologin/",
		name: "Inloggen",
		component: AutoLoginPage,
	},
	{
		private: false,
		path: "/logout/",
		name: "Uitloggen",
		component: LogoutPage,
	},
	{
		private: false,
		path: "/inloggen-geannuleerd/",
		name: "Inloggen geannuleerd",
		component: DigiDInloggenAfgebrokenPage,
	},
	{
		private: false,
		path: "/niet-in-bevolkingsonderzoek/",
		name: "Niet bekend in Mijn Bevolkingsonderzoek",
		component: NietInBevolkingsonderzoekPage,
	},
	{
		private: true,
		path: "/",
		name: "Hoofdmenu",
		component: LandingPage,
	},
	{
		private: true,
		path: "/mamma/",
		name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.MAMMA]),
		component: BvoLandingPageController,
		bvo: Bevolkingsonderzoek.MAMMA,
		redirectPage: "/",
	},
	{
		private: true,
		path: "/mamma/huisarts/",
		name: "Huisarts",
		component: HuisartsPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN],
		redirectPage: "/mamma/",
	},
	{
		private: true,
		path: "/mamma/huisarts/zoeken/",
		name: "",
		component: HuisartsPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN],
		redirectPage: "/mamma/",
	},
	{
		private: true,
		path: "/cervix/",
		name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.CERVIX]),
		component: BvoLandingPageController,
		bvo: Bevolkingsonderzoek.CERVIX,
		redirectPage: "/",
	},
	{
		private: true,
		path: "/cervix/uitstellen/",
		name: "Uitstellen deelname",
		component: CervixUitstelAanvragenPage,
		bvo: Bevolkingsonderzoek.CERVIX,
		requiredContactActions: [ClientContactActieType.CERVIX_UITSTEL],
		redirectPage: "/cervix/",
	},
	{
		private: true,
		path: "/cervix/zas/",
		name: "Zelfafnameset aanvragen",
		component: ZasAanvragenPage,
		bvo: Bevolkingsonderzoek.CERVIX,
		requiredContactActions: [ClientContactActieType.CERVIX_ZAS_AANVRAGEN],
		redirectPage: "/cervix/",
	},
	{
		private: true,
		path: "/cervix/herdrukken/",
		name: "Nieuwe uitnodiging aanvragen",
		component: CervixHerdrukAanvragenPage,
		bvo: Bevolkingsonderzoek.CERVIX,
		requiredContactActions: [ClientContactActieType.CERVIX_HERDRUK],
		redirectPage: "/cervix/",
	},
	{
		private: true,
		path: "/colon/",
		name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.COLON]),
		component: BvoLandingPageController,
		bvo: Bevolkingsonderzoek.COLON,
		redirectPage: "/",
	},
	{
		private: true,
		path: "/colon/huisarts/",
		name: "Huisarts",
		component: HuisartsPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_HUISARTS_WIJZIGEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/colon/fit/",
		name: "Nieuwe ontlastingstest aanvragen",
		component: ColonFitAanvragenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AANVRAGEN_NIEUWE_FIT],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/colon/afzeggen/",
		name: "Afzeggen afspraak",
		component: ColonAfspraakAfzeggenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/mamma/afmelden/",
		name: "Afmelden",
		component: AfmeldenPage,
		requiredContactActions: [ClientContactActieType.MAMMA_AFMELDEN],
		redirectPage: "/mamma/",
	},
	{
		private: true,
		path: "/cervix/afmelden/",
		name: "Afmelden",
		component: AfmeldenPage,
		bvo: Bevolkingsonderzoek.CERVIX,
		requiredContactActions: [ClientContactActieType.CERVIX_AFMELDEN],
		redirectPage: "/cervix/",
	},
	{
		private: true,
		path: "/colon/afmelden/",
		name: "Afmelden",
		component: AfmeldenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFMELDEN],
		redirectPage: "/",
	},
	{
		private: true,
		path: "/mamma/heraanmelden/",
		name: "Opnieuw aanmelden",
		component: HeraanmeldenPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_HERAANMELDEN],
		redirectPage: "/mamma/",
	},
	{
		private: true,
		path: "/cervix/heraanmelden/",
		name: "Opnieuw aanmelden",
		component: HeraanmeldenPage,
		bvo: Bevolkingsonderzoek.CERVIX,
		requiredContactActions: [ClientContactActieType.CERVIX_HERAANMELDEN],
		redirectPage: "/cervix/",
	},
	{
		private: true,
		path: "/colon/heraanmelden/",
		name: "Opnieuw aanmelden",
		component: HeraanmeldenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_HERAANMELDEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/profiel/",
		name: "Profiel",
		component: ProfielPage,
	},
	{
		private: true,
		path: "/profiel/telefoonnummer/",
		name: "Telefoonnummer",
		component: TelefoonnummerWijzigenPage,
		requiredContactActions: [ClientContactActieType.INZAGE_PERSOONSGEGEVENS],
		redirectPage: "/profiel/",
	},
	{
		private: true,
		path: "/profiel/email/",
		name: "E-mailadres",
		component: EmailWijzigenPage,
		requiredContactActions: [ClientContactActieType.INZAGE_PERSOONSGEGEVENS],
		redirectPage: "/profiel/",
	},
	{
		private: true,
		path: "/profiel/adres/",
		name: "Tijdelijk adres",
		component: TijdelijkAdresWijzigenPage,
		requiredContactActions: [ClientContactActieType.TIJDELIJK_ADRES],
		redirectPage: "/profiel/",
	},
	{
		private: true,
		path: "/profiel/aanspreekvorm/",
		name: "Aanspreekvorm",
		component: AanhefWijzigenPage,
		requiredContactActions: [ClientContactActieType.INZAGE_PERSOONSGEGEVENS],
		redirectPage: "/profiel/",
	},
	{
		private: true,
		path: "/profiel/gebruikgegevens/",
		name: "Gebruik gegevens",
		component: BezwaarWijzigenPage,
		requiredContactActions: [ClientContactActieType.BEZWAAR],
		redirectPage: "/profiel/",
	},
	{
		private: true,
		path: "/mamma/afspraak/",
		name: "Afspraak",
		component: MammaAfspraakMakenPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/afspraak/bevestigen/",
		name: "Uw afspraak",
		component: MammaAfspraakBevestigenPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/afspraak/bevestiging-selectie/",
		name: "Bevestigen",
		component: MammaAfspraakBevestigingSelectiePage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/afspraak/herinnering/",
		name: "Herinneren",
		component: MammaAfspraakHerinneringPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/afspraak/overzicht/",
		name: "Overzicht",
		component: MammaAfspraakOverzichtPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/afspraak/uw-huisarts/",
		name: "Uw huisarts",
		component: MammaAfspraakHuisartsPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
	},
	{
		private: true,
		path: "/mamma/uitstellen/",
		name: "Onderzoek uitstellen",
		component: MammaAfspraakUitstellenPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		requiredContactActions: [ClientContactActieType.MAMMA_UITSTELLEN],
	},
	{
		private: true,
		path: "/colon/afspraak-wijzigen/",
		name: "Afspraak veranderen",
		component: ColonAfspraakMakenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/colon/afspraak-maken-heraanmelding/",
		name: "Afspraak maken",
		component: ColonAfspraakMakenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/colon/afspraak-maken/",
		name: "Afspraak maken",
		component: ColonAfspraakMakenPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
	},
	{
		private: true,
		path: "/openstaande-onderzoeken/",
		name: "Onderzoeken",
		component: OpenstaandeOnderzoekenPage,
		bvo: Bevolkingsonderzoek.MAMMA,
		redirectPage: "/",
	},
	{
		private: true,
		path: "/colon/afspraak/intake/",
		name: "Intake",
		component: ColonAfspraakIntakePage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
		activateGuard: isDigitaleIntakeBeschikbaar,
	},
	{
		private: true,
		path: "/colon/afspraak/uw-gezondheid/",
		name: "Uw gezondheid",
		component: ColonAfspraakGezondheidPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
		activateGuard: isDigitaleIntakeBeschikbaar,
	},
	{
		private: true,
		path: "/colon/afspraak/kiezen/",
		name: "Afspraak kiezen",
		component: ColonAfspraakMakenWizardPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
		activateGuard: isDigitaleIntakeBeschikbaar,
	}, {
		private: true,
		path: "/colon/afspraak/contact/",
		name: "Contactgegevens",
		component: ColonAfspraakContactPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
		activateGuard: isDigitaleIntakeBeschikbaar,
	},
	{
		private: true,
		path: "/colon/afspraak/overzicht/",
		name: "Overzicht",
		component: ColonAfspraakOverzichtPage,
		bvo: Bevolkingsonderzoek.COLON,
		requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
		redirectPage: "/colon/",
		activateGuard: isDigitaleIntakeBeschikbaar,
	},
]

export default routes
