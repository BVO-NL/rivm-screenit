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
import type {Lezing, LezingDto} from "./Lezing"
import {mapLezingDtoToLezing} from "./Lezing"
import type {Onderzoek, OnderzoekDto} from "./Onderzoek"
import type {OnderbrokenOnderzoekOption} from "./visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek"
import type {OpschortenReden} from "./OpschortenReden"
import type {Signalering, SignaleringDto} from "./Signalering"
import {mapSignaleringFromDto} from "./Signalering"
import type {AnnotatieAfbeelding, AnnotatieAfbeeldingDto} from "./AnnotatieAfbeelding"
import {mapAfbeeldingDtoToAfbeelding} from "./AnnotatieAfbeelding"

export type KeyValue = {
	key: string;
	value: string;
};
export type VorigOnderzoekDto = {
	eersteBeeindigdeAfspraakOp: string;
	onderzoekDatum: string;
	uitnodigingsNr: number;
	uitslagGunstig?: boolean;
	onbeoordeelbaar: boolean;
	uitvoerendMbber: string;
	extraMedewerker: string;
	meerdereOnderzoekenInRondeOnderbrokenRedenen?: Array<OnderbrokenOnderzoekOption>;
	meerdereOnderzoekenInRondeOpschortRedenen?: Array<OpschortenReden>;
	onderzoek: OnderzoekDto;
	lezingen: Array<LezingDto>;
	verslagLezing: LezingDto;
	visueleInspectieAfbeelding: AnnotatieAfbeeldingDto;
	signaleren: SignaleringDto;
	teksten: Array<KeyValue>;
	nevenbevindingen: string;
	nevenbevindingenOpmerkingen: Array<string>;
	beeldenBeschikbaar?: boolean;
};
export type VorigOnderzoek = {
	eersteBeeindigdeAfspraakOp: string;
	onderzoekDatum: string;
	uitnodigingsNr: number;
	uitslagGunstig?: boolean;
	onbeoordeelbaar: boolean;
	uitvoerendMbber: string;
	extraMedewerker: string;
	meerdereOnderzoekenInRondeOnderbrokenRedenen?: Array<OnderbrokenOnderzoekOption>;
	meerdereOnderzoekenInRondeOpschortRedenen?: Array<OpschortenReden>;
	onderzoek: Onderzoek;
	lezingen: Array<Lezing>;
	verslagLezing?: Lezing;
	visueleInspectieAfbeelding: AnnotatieAfbeelding;
	signaleren?: Signalering;
	teksten: Array<KeyValue>;
	nevenbevindingen: string;
	nevenbevindingenOpmerkingen: Array<string>;
	beeldenBeschikbaar?: boolean;
};

export function vorigOnderzoekDtoToVorigOnderzoek(vorigOnderzoekDto: VorigOnderzoekDto): VorigOnderzoek | undefined {
	try {
		const result: VorigOnderzoek = {
			eersteBeeindigdeAfspraakOp: vorigOnderzoekDto.eersteBeeindigdeAfspraakOp,
			onderzoekDatum: vorigOnderzoekDto.onderzoekDatum,
			uitnodigingsNr: vorigOnderzoekDto.uitnodigingsNr,
			uitslagGunstig: vorigOnderzoekDto.uitslagGunstig,
			onbeoordeelbaar: vorigOnderzoekDto.onbeoordeelbaar,
			uitvoerendMbber: vorigOnderzoekDto.uitvoerendMbber,
			extraMedewerker: vorigOnderzoekDto.extraMedewerker,
			meerdereOnderzoekenInRondeOnderbrokenRedenen: vorigOnderzoekDto.meerdereOnderzoekenInRondeOnderbrokenRedenen,
			meerdereOnderzoekenInRondeOpschortRedenen: vorigOnderzoekDto.meerdereOnderzoekenInRondeOpschortRedenen,
			onderzoek: vorigOnderzoekDto.onderzoek,
			lezingen: [],
			verslagLezing: undefined,
			visueleInspectieAfbeelding: mapAfbeeldingDtoToAfbeelding(0, vorigOnderzoekDto.visueleInspectieAfbeelding),
			signaleren: mapSignaleringFromDto(vorigOnderzoekDto.signaleren, 0),
			teksten: vorigOnderzoekDto.teksten,
			nevenbevindingen: vorigOnderzoekDto.nevenbevindingen,
			nevenbevindingenOpmerkingen: vorigOnderzoekDto.nevenbevindingenOpmerkingen,
			beeldenBeschikbaar: vorigOnderzoekDto.beeldenBeschikbaar,
		}

		if (vorigOnderzoekDto.lezingen) {
			result.lezingen = vorigOnderzoekDto.lezingen.map(lezing => mapLezingDtoToLezing(lezing, result))
		}

		if (vorigOnderzoekDto.verslagLezing) {
			result.verslagLezing = mapLezingDtoToLezing(vorigOnderzoekDto.verslagLezing, result)
		}

		return result
	} catch (exception: any) {
		console.warn(`Fout tijdens vorig onderzoek mappen: ${exception.message}`)
		return undefined
	}
}
