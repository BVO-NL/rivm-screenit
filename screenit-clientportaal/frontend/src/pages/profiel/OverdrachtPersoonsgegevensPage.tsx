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
import {FC} from "react"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./OverdrachtPersoonsgegevensPage.json"
import styles from "./OverdrachtPersoonsgegevensPage.module.scss"
import ActieBasePage from "../ActieBasePage"
import {useSelector} from "react-redux"
import {selectOverdrachtPersoonsgegevens} from "../../selectors/ClientSelectors"
import ScreenitBackend from "../../utils/Backend"
import {downloadFile} from "../../utils/FileUtil"
import {Col, Row} from "react-bootstrap"
import {Alert, Button} from "@mui/material"
import VerticalDividerComponent from "../../components/vectors/VerticalDividerComponent"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import {useSelectedBvo} from "../../utils/Hooks"
import classNames from "classnames"
import {formatDateText, plusDagen} from "../../utils/DateUtil"

const OverdrachtPersoonsgegevensPage: FC = () => {
	const overdrachtPersoonsgegevens = useSelector(selectOverdrachtPersoonsgegevens)
	const selectedBvo = useSelectedBvo()
	const zipNaam = "Clientgegevens.zip"

	async function downloadOverdrachtPersoonsgegevens(): Promise<void> {
		const response = await ScreenitBackend.get("overdracht-persoonsgegevens/download")
		downloadFile(await response.blob(), zipNaam)
	}

	return (
		<ActieBasePage
			bvoName={""}
			title={getString(properties.page.title)}
			description={getString(properties.page.description, [formatDateText(plusDagen(overdrachtPersoonsgegevens.dataDumpGegenereerdOp, 14))])}>

			{overdrachtPersoonsgegevens.mammaBeeldenOphalenMislukt ?
				<Alert severity="error" data-testid="alert_mamma_beelden_ophalen_mislukt">
					{getString(properties.beelden_ophalen_mislukt)}
				</Alert>
				:
				<Row className={classNames(styles.downloadCard, BevolkingsonderzoekStyle[selectedBvo!])}>
					<VerticalDividerComponent className={styles.verticalRectangle}/>
					<Col md={8} className={styles.tekst}>
						{zipNaam}
					</Col>
					<Col md={4}>
						<Button variant="outlined" color="primary" size="small" onClick={downloadOverdrachtPersoonsgegevens}>
							{getString(properties.download_button)}
						</Button>
					</Col>
				</Row>
			}
		</ActieBasePage>
	)
}

export default OverdrachtPersoonsgegevensPage
