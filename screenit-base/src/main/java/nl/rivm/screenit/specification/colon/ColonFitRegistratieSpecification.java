package nl.rivm.screenit.specification.colon;

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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import jakarta.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonBrief_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitRegistratie_;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging_;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.DateSpecification.truncate;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFitRegistratieSpecification
{
	public static Specification<ColonFitRegistratie> heeftDossier(ColonDossier dossier)
	{
		return (r, q, cb) -> cb.equal(join(r, ColonFitRegistratie_.screeningRonde).get(ColonScreeningRonde_.dossier), dossier);
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftStatusDatumVoorOfOp(LocalDateTime peilmoment)
	{
		return (r, q, cb) ->
			cb.lessThanOrEqualTo(r.get(ColonFitRegistratie_.statusDatum), DateUtil.toUtilDate(peilmoment));
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftFitType(ColonFitType type)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonFitRegistratie_.type), type);
	}

	public static Specification<ColonFitRegistratie> isDatumLaatstGecontroleerdNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, ColonFitRegistratie_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return cb.and(
				cb.greaterThan(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), DateUtil.toUtilDate(signalerenVanaf)),
				cb.greaterThan(truncate("day", r.get(ColonFitRegistratie_.analyseDatum), cb),
					truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb)));
		};
	}

	public static Specification<ColonFitRegistratie> isFitAnalyseDatumNa(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
			cb.greaterThan(r.get(ColonFitRegistratie_.analyseDatum), DateUtil.toUtilDate(signalerenVanaf));
	}

	public static Specification<ColonFitRegistratie> isGecontroleerdNaSignalering(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, ColonFitRegistratie_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			return cb.or(
				cb.lessThanOrEqualTo(truncate("day", dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering), cb),
					DateUtil.toUtilDate(signalerenVanaf)),
				cb.isNull(dossierJoin.get(Dossier_.datumLaatstGecontroleerdeSignalering))
			);
		};
	}

	public static Specification<ColonFitRegistratie> heeftGeenUitslagBrief()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonBrief.class);
			var briefRoot = subquery.from(ColonBrief.class);
			var projectBriefJoin = join(briefRoot, ClientBrief_.projectBrief, JoinType.LEFT);

			subquery.select(briefRoot);
			subquery.where(
				cb.equal(briefRoot.get(ColonBrief_.fitRegistratie), r),
				cb.or(
					cb.and(
						cb.isFalse(briefRoot.get(ClientBrief_.vervangendeProjectBrief)),
						briefRoot.get(Brief_.briefType).in(BriefType.COLON_UITSLAG_BRIEVEN),
						cb.isTrue(briefRoot.get(Brief_.gegenereerd))
					),
					cb.and(
						cb.isTrue(briefRoot.get(ClientBrief_.vervangendeProjectBrief)),
						projectBriefJoin.get(Brief_.briefType).in(BriefType.COLON_UITSLAG_BRIEVEN),
						cb.isTrue(projectBriefJoin.get(Brief_.gegenereerd))
					)
				)
			);
			return cb.not(cb.exists(subquery));
		};

	}

	public static Specification<ColonFitRegistratie> heeftGeenNieuweUitnodiging()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonUitnodiging.class);
			var uitnodigingRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = join(r, ColonFitRegistratie_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery
				.select(uitnodigingRoot)
				.where(cb.equal(join(uitnodigingRoot, ColonUitnodiging_.screeningRonde).get(ColonScreeningRonde_.dossier),
						dossierJoin),
					cb.equal(truncate("day", uitnodigingRoot.get(Uitnodiging_.creatieDatum), cb),
						truncate("day", r.get(ColonFitRegistratie_.statusDatum), cb)));
			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<ColonFitRegistratie> heeftNieuweUitnodigingZonderGekoppeldeFit()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonUitnodiging.class);
			var uitnodigingZonderFitRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = join(r, ColonFitRegistratie_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery
				.select(uitnodigingZonderFitRoot)
				.where(
					cb.equal(uitnodigingZonderFitRoot.get(ColonUitnodiging_.screeningRonde).get(ColonScreeningRonde_.dossier), dossierJoin),
					cb.equal(truncate("day", uitnodigingZonderFitRoot.get(Uitnodiging_.creatieDatum), cb),
						truncate("day", r.get(ColonFitRegistratie_.statusDatum), cb)),
					cb.isNull(uitnodigingZonderFitRoot.get(ColonUitnodiging_.gekoppeldeFitRegistratie)));
			return cb.exists(subquery);
		};
	}

	public static Specification<ColonFitRegistratie> valideerFitUitslagStatus(LocalDate signalerenVanaf)
	{
		return (r, q, cb) ->
			cb.and(
				cb.or(
					cb.and(
						isGecontroleerdNaSignalering(signalerenVanaf).toPredicate(r, q, cb),
						isFitAnalyseDatumNa(signalerenVanaf).toPredicate(r, q, cb)
					),
					cb.and(
						isDatumLaatstGecontroleerdNa(signalerenVanaf).toPredicate(r, q, cb)
					)
				),
				cb.or(
					cb.and(
						heeftStatusIn(List.of(ColonFitRegistratieStatus.UITGEVOERD)).toPredicate(r, q, cb),
						heeftGeenUitslagBrief().toPredicate(r, q, cb)
					),
					cb.and(
						heeftStatusIn(List.of(ColonFitRegistratieStatus.VERVALDATUMVERLOPEN, ColonFitRegistratieStatus.NIETTEBEOORDELEN)).toPredicate(r, q, cb),
						cb.or(
							cb.and(
								heeftGeenNieuweUitnodiging().toPredicate(r, q, cb),
								heeftGeenUitslagBrief().toPredicate(r, q, cb)
							),
							heeftNieuweUitnodigingZonderGekoppeldeFit().toPredicate(r, q, cb)
						)
					)
				)
			);
	}

	public static Specification<ColonFitRegistratie> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClient().with(r ->
		{
			var ronde = join(r, ColonFitRegistratie_.screeningRonde);
			var dossier = join(ronde, ColonScreeningRonde_.dossier);
			return join(dossier, ColonDossier_.client);
		});
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftStatusIn(List<ColonFitRegistratieStatus> statussen)
	{
		return (r, q, cb) -> r.get(ColonFitRegistratie_.status).in(statussen);
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftStatus(ColonFitRegistratieStatus fitStatus)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonFitRegistratie_.status), fitStatus);
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftGeenStatus()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonFitRegistratie_.status));
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftGunstigeUitslag()
	{
		return (r, q, cb) -> cb.lessThan(r.get(ColonFitRegistratie_.uitslag), r.get(ColonFitRegistratie_.normWaarde));
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftOngunstigeReguliereOfStudieUitslag()
	{
		return (r, q, cb) -> cb.or(cb.greaterThanOrEqualTo(r.get(ColonFitRegistratie_.uitslag), r.get(ColonFitRegistratie_.normWaarde)),
			cb.equal(r.get(ColonFitRegistratie_.geinterpreteerdeUitslag), ColonGeinterpreteerdeUitslag.ONGUNSTIG));
	}

	public static ExtendedSpecification<ColonFitRegistratie> heeftHerinnering(Boolean value)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonFitRegistratie_.herinnering), value);
	}

	public static ExtendedSpecification<ColonFitRegistratie> fitIsHoudbaar(LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(ColonHoudbaarheidFitReeks.class);
			subquery
				.select(cb.literal(1L))
				.where(cb.and(
					cb.greaterThan(subRoot.get(ColonHoudbaarheidFitReeks_.vervalDatum), DateUtil.toUtilDate(peildatum)),
					cb.lessThanOrEqualTo(subRoot.get(ColonHoudbaarheidFitReeks_.barcodeStart), r.get(ColonFitRegistratie_.barcode)),
					cb.greaterThanOrEqualTo(subRoot.get(ColonHoudbaarheidFitReeks_.barcodeEnd), r.get(ColonFitRegistratie_.barcode)),
					cb.equal(subRoot.get(ColonHoudbaarheidFitReeks_.lengthBarcode), cb.length(r.get(ColonFitRegistratie_.barcode)))));
			return cb.exists(subquery);
		};
	}
}
