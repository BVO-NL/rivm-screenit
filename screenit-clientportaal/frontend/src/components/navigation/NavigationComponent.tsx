/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import {useEffect, useState} from "react"
import {Nav, Navbar, NavItem} from "react-bootstrap"
import styles from "./NavigationComponent.module.scss"
import classNames from "classnames"
import {Link, useNavigate} from "react-router"
import NavigationLinkComponent from "./NavigationLinkComponent"
import {getLogoutLink, getPersoonNaam} from "../header/HeaderComponent"
import {useSelector} from "react-redux"
import MijnBevolkingsOnderzoekLogo from "../../scss/media/MijnBevolkingsOnderzoekLogo"
import {State} from "../../datatypes/State"
import {getContactUrl} from "../../utils/UrlUtil"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"
import {BevolkingsonderzoekNaam} from "../../datatypes/Bevolkingsonderzoek"

const NavigationComponent = () => {
	const [sticky, setSticky] = useState(true)
	const [expanded, setExpanded] = useState(false)

	const persoon = useSelector((state: State) => state.client.persoon)
	const landingOverzicht = useSelector((state: State) => state.landingOverzicht)
	const nietTonenHamburger = persoon.id !== undefined

	const navigate = useNavigate()

	useEffect(() => {
		fixNavBarPositieBijScrollen()
	}, [])

	const hideNavbar = () => setTimeout(() => setExpanded(false), 200)

	const hideNavbarEnStuurDatadogEvent = (itemName: string) => {
		datadogService.stuurEvent("menuItemGeklikt", AnalyticsCategorie.MENUBALK, {naam: itemName})
		hideNavbar()
	}

	return (
		<Navbar expand={"lg"} expanded={expanded}
				className={classNames(styles.navBar, "justify-content-between", sticky ? "sticky-top" : "fixed-top", !sticky && styles.navBarFixed)}>
			<Navbar.Brand>
				<Link to={"/"} onClick={hideNavbar}>
					<MijnBevolkingsOnderzoekLogo/>
				</Link>
			</Navbar.Brand>
			{nietTonenHamburger && <Navbar.Toggle className={styles.hamburger} aria-controls={"navbar-collapse"}
												  onClick={() => setExpanded(!expanded)}/>}
			{nietTonenHamburger && <Navbar.Collapse id={"navbar-collapse"} className={styles.nav}>
				<Nav className={"ml-auto"}>
					{landingOverzicht.behoortTotMammaDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/mamma"}
													 text={"Borstkanker"}
													 bold={true}
													 onClick={() => hideNavbarEnStuurDatadogEvent(BevolkingsonderzoekNaam.MAMMA)}/>
						</NavItem>
					}
					{landingOverzicht.behoortTotCervixDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/cervix"}
													 text={"Baarmoederhalskanker"}
													 bold={true}
													 onClick={() => hideNavbarEnStuurDatadogEvent(BevolkingsonderzoekNaam.CERVIX)}/>
						</NavItem>
					}
					{landingOverzicht.behoortTotColonDoelgroep &&
						<NavItem>
							<NavigationLinkComponent url={"/colon"}
													 text={"Darmkanker"}
													 bold={true}
													 onClick={() => hideNavbarEnStuurDatadogEvent(BevolkingsonderzoekNaam.COLON)}/>
						</NavItem>
					}
					<NavItem>
						<NavigationLinkComponent url={"/profiel"}
												 text={"Mijn profiel"}
												 bold={false}
												 onClick={() => hideNavbarEnStuurDatadogEvent("mijn_profiel")}/>
					</NavItem>
					<NavItem>
						<NavigationLinkComponent url={getContactUrl()}
												 text={"Contact"}
												 bold={false}
												 onClick={() => hideNavbarEnStuurDatadogEvent("contact")}/>
					</NavItem>

					{<div className={styles.clientLogout}>
						<NavItem>
							<span>{getPersoonNaam(persoon)}</span>
						</NavItem>
						<NavItem>
							{getLogoutLink(() => navigate("/logout"))}
						</NavItem>
					</div>}
				</Nav>
			</Navbar.Collapse>}
		</Navbar>
	)

	function fixNavBarPositieBijScrollen() {
		window.addEventListener("scroll", () => {
			setSticky(document.body.scrollTop < 100 && document.documentElement.scrollTop < 100)
		})
	}
}

export default NavigationComponent
