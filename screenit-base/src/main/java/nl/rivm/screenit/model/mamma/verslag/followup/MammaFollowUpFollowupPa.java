package nl.rivm.screenit.model.mamma.verslag.followup;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "mamma")
@Getter
@Setter
public class MammaFollowUpFollowupPa
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MammaFollowUpVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "followupPa", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "320632", displayName = "Monster/materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:specimen"
	})
	private MammaFollowUpMonstermateriaal monstermateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_punctie_conclusie_bk", values = {
		@DSValueSetValue(code = "77289001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30389008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103635003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44085002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "68453008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300015", displayName = "C-classificatie punctie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.596']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue cclassificatiePunctie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_ER_bk", values = {
		@DSValueSetValue(code = "OHT-2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "416053008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "441117001", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300020", displayName = "Oestrogeen receptor status", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.593']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue oestrogeenReceptorStatus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_PR_bk", values = {
		@DSValueSetValue(code = "OHT-4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "416561008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "441118006", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300030", displayName = "Progesteron receptor status", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.594']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue progesteronReceptorStatus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_status_HER2_bk", values = {
		@DSValueSetValue(code = "OHT-6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "431396003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "427685000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OHT-8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1")
	})
	@VraagElement(conceptId = "300040", displayName = "HER2 status", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.595']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue her2Status;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_typetumor_bk", values = {
		@DSValueSetValue(code = "77289001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30389008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "110396000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "50673007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44085002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "399919001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "68453008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "49611000146109", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "86049000", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300050", displayName = "B-classificatie op mammabiopt", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.596']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue bclassificatieOpMammabiopt;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BloomRichardson_bk", values = {
		@DSValueSetValue(code = "369790002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369791003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "369792005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "384668003", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300060", displayName = "Maligniteitsgraad", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.597']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue maligniteitsgraad;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "followupPa", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "300070", displayName = "pTNM en gradering", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.598']]]/hl7:observation"
	})
	private MammaFollowUpPtnmEnGradering ptnmEnGradering;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "TypeInvasieveTumorWHO_bk", values = {
		@DSValueSetValue(code = "22694002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "82711006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "127575007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "79143006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30156004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "89740008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "4631006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "72495009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128705006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "45410002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "11671000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "139771000146103", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "4079000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128702009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "703578005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "55937004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "156691000146102", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "703595002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "703594003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "734075007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "14799000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128928004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "320637", displayName = "Type invasieve tumor (WHO) + overige", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.77.0.2.10.5']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue typeInvasieveTumorwhoOverige;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "GraderingDCIS_bk", values = {
		@DSValueSetValue(code = "190041000146109", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "190051000146107", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "190061000146105", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "384741006", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "320636", displayName = "Gradering DCIS", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.77.0.2.10.4']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue graderingDcis;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "mamma", name = "mamma_follow_up_followup_pa_type_niet_eenduidig_benigne_laesies")
	@DSValueSet(name = "TypeNietEenduidigBenigneLaesies_bk", values = {
		@DSValueSetValue(code = "5244003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "58811002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "31390008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "6660000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "67011000146101", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "133855003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "16566002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "1156873009*16566002*282292002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "320635", displayName = "Type niet eenduidig benigne laesie(s)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.77.0.2.10.3']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> typeNietEenduidigBenigneLaesies = new ArrayList<>();

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "mamma", name = "mamma_follow_up_followup_pa_type_eenduidig_benigne_laesies")
	@DSValueSet(name = "TypeEenduidigBenigneLaesies_bk", values = {
		@DSValueSetValue(code = "1156873009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "67617000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "1156654007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "58811002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "367643001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "8360001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "367647000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "447956004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "705153001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "51398009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "81274009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "57597008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "112674009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "320634", displayName = "Type eenduidig benigne laesie(s)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.77.0.2.10.2']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> typeEenduidigBenigneLaesies = new ArrayList<>();

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "mamma", name = "mamma_follow_up_followup_pa_type_cis")
	@DSValueSet(name = "TypeCIS_bk", values = {
		@DSValueSetValue(code = "1162814007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "77284006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30566004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "444591006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "190021000146103", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "703545003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "703546002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "320633", displayName = "Type CIS", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.77.0.2.10.1']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> typeCis = new ArrayList<>();

}
