/*-
 * ========================LICENSE_START=================================
 * se-proxy
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
export type Dagsynchronisatie = {
	gemaakt: number;
	verwerkt: number;
};

export type Dagafsluiting = {
	aantalDoorgevoerd: number;
};

export type MedewerkerDagproductie = {
	ingeschrevenCount?: number;
	onderzochtCount?: number;
	afgerondCount?: number;
	onderbrokenCount?: number;
	onvolledigCount?: number;
	afwijkingenCount?: number;
};

export type Dagproductie = {
	[medewerker: string]: MedewerkerDagproductie
};

export type DagPlanningSamenvatting = {
	dagCapaciteit: number;
	beschikbaarheid: number;
	starttijd?: string;
	eindtijd?: string;
	aantalVerwacht: number;
	aantalAfgerond: number;
	aantalOnderbroken: number;
	aantalOnvolledig: number;
}

export type Dagverslag = {
	dagSynchronisatie: Dagsynchronisatie;
	dagafsluiting: Dagafsluiting;
	dagproductie: Dagproductie;
	nietAfgeslotenVanaf?: string;
	dagPlanningSamenvatting: DagPlanningSamenvatting;
}
