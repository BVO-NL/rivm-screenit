package nl.rivm.screenit.main.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;
import java.util.stream.Collectors;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.SingularAttribute;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBaseWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeWerklijstZoekObject;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaLezing_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import static jakarta.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.ARBITRAGE;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.DISCREPANTIE;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.EERSTE_LEZING;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.TWEEDE_LEZING;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.VERSLAG_AFGEKEURD;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.VERSLAG_GEREED;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.VERSLAG_MAKEN;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenFalse;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefTypeIn;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isGegenereerd;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterBsn;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterGeboortedatum;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterHuisnummer;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterPostcode;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.filterBeoordelingsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.filterStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatusDatumVanaf;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatusDatumVoor;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.isNietToegewezenAanSpecifiekeRadioloog;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.isToegewezenAan;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.isAfwezigOfGedaanDoor;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.isGedaanDoor;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.isNietGedaanDoor;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.isVerwezen;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.isVerwezenDoor;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterOnderzoekType;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.screeningRondeJoin;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBeoordelingWerklijstSpecification
{

	public static final String VERWIJZEND_SORT_PROPERTY = "verwijzend";

	public static ExtendedSpecification<MammaOnderzoek> heeftOnbevestigdeLezing()
	{
		return filterStatusIn(List.of(EERSTE_LEZING_OPGESLAGEN, TWEEDE_LEZING_OPGESLAGEN)).with(beoordelingAttribute());
	}

	public static Specification<MammaOnderzoek> beWerklijstSpecification(MammaBeWerklijstZoekObject zoekObject)
	{
		return basisFilterBeoordelingWerklijst(zoekObject)
			.and(filterStatusIn(zoekObject.getBeoordelingStatussen()).with(beoordelingAttribute()))
			.and(filterBeWerklijst(zoekObject));
	}

	public static ExtendedSpecification<MammaOnderzoek> ceWerklijstSpecification(MammaCeWerklijstZoekObject zoekObject, LocalDate peildatumOngunstigeUitslagen)
	{
		return filterBeEnCe(zoekObject)
			.and(filterBeoordelingStatusVoorCeWerklijst(zoekObject.getBeoordelingStatussen(), peildatumOngunstigeUitslagen))
			.and(basisFilterBeoordelingWerklijst(zoekObject));
	}

	public static Specification<MammaOnderzoek> ceProcesMonitoringSpecification(MammaCeWerklijstZoekObject zoekObject, LocalDate peildatumProcesMonitoring,
		LocalDate peildatumOngunstigeUitslagen)
	{
		return ceWerklijstSpecification(zoekObject, peildatumOngunstigeUitslagen)
			.and(heeftStatusDatumVoor(peildatumProcesMonitoring).with(beoordelingAttribute()));
	}

	private static ExtendedSpecification<MammaOnderzoek> basisFilterBeoordelingWerklijst(MammaBaseWerklijstZoekObject zoekObject)
	{
		return persoonSpecification(zoekObject)
			.and(filterScreeningsEenheid(zoekObject.getScreeningsEenheden()))
			.and(filterOnderzoekType(zoekObject.getOnderzoekType()));
	}

	private static ExtendedSpecification<MammaOnderzoek> persoonSpecification(MammaBaseWerklijstZoekObject zoekObject)
	{
		var filterNodig = zoekObject.getGeboortedatum() != null || StringUtils.isNotBlank(zoekObject.getBsn())
			|| StringUtils.isNotBlank(zoekObject.getPostcode()) || zoekObject.getHuisnummer() != null;
		return skipWhenFalse(filterNodig,
			filterGeboortedatum(zoekObject.getGeboortedatum())
				.and(filterBsn(zoekObject.getBsn()))
				.and(filterPostcode(zoekObject.getPostcode()))
				.and(filterHuisnummer(zoekObject.getHuisnummer()))
				.with(r -> persoonJoin(r)));
	}

	private static Specification<MammaOnderzoek> filterBeWerklijst(MammaBeWerklijstZoekObject zoekObject)
	{
		return filterBeoordelingsEenheid(zoekObject.getBeoordelingsEenheid()).with(beoordelingAttribute())
			.and(specifiekeWerklijstSpecification(zoekObject));
	}

	private static Specification<MammaOnderzoek> specifiekeWerklijstSpecification(MammaBeWerklijstZoekObject zoekObject)
	{
		if (zoekObject.getBeoordelingStatussen().contains(VERSLAG_MAKEN)
			|| zoekObject.getBeoordelingStatussen().contains(VERSLAG_GEREED)
			|| zoekObject.getBeoordelingStatussen().contains(VERSLAG_AFGEKEURD))
		{
			return verslagWerklijstSpecification(zoekObject.getOrganisatieMedewerker()).with(beoordelingAttribute());
		}
		else if (zoekObject.getBeoordelingStatussen().contains(ARBITRAGE))
		{
			return arbitrageWerklijstSpecification(zoekObject.getOrganisatieMedewerker()).with(beoordelingAttribute());
		}
		else if (zoekObject.getBeoordelingStatussen().contains(DISCREPANTIE))
		{
			return discrepantieWerklijstSpecification(zoekObject.getOrganisatieMedewerker()).with(beoordelingAttribute());
		}
		return beoordelenWerklijstSpecification(zoekObject.getOrganisatieMedewerker());
	}

	private static Specification<MammaOnderzoek> beoordelenWerklijstSpecification(OrganisatieMedewerker radioloog)
	{
		return MammaOnderzoekSpecification.heeftBeeldenBeschikbaar()
			.and(MammaOnderzoekSpecification.isDoorgevoerd(true))
			.and(beoordelenEersteEnTweedeLezerSpecification(radioloog).with(beoordelingAttribute()));
	}

	private static ExtendedSpecification<MammaBeoordeling> beoordelenEersteEnTweedeLezerSpecification(OrganisatieMedewerker radioloog)
	{
		var beschikbaarVoorEersteLezing = filterStatusIn(List.of(EERSTE_LEZING, EERSTE_LEZING_OPGESLAGEN))
			.and(isAfwezigOfGedaanDoor(radioloog).with(MammaBeoordeling_.eersteLezing, LEFT));

		var beschikbaarVoorTweedeLezing = filterStatusIn(List.of(TWEEDE_LEZING, TWEEDE_LEZING_OPGESLAGEN))
			.and(isNietGedaanDoor(radioloog).with(MammaBeoordeling_.eersteLezing, LEFT))
			.and(isAfwezigOfGedaanDoor(radioloog).with(MammaBeoordeling_.tweedeLezing, LEFT));

		return beschikbaarVoorEersteLezing.or(beschikbaarVoorTweedeLezing);
	}

	private static ExtendedSpecification<MammaBeoordeling> discrepantieWerklijstSpecification(OrganisatieMedewerker radioloog)
	{
		return isGedaanDoor(radioloog).with(MammaBeoordeling_.eersteLezing)
			.or(isGedaanDoor(radioloog).with(MammaBeoordeling_.tweedeLezing));
	}

	private static ExtendedSpecification<MammaBeoordeling> arbitrageWerklijstSpecification(OrganisatieMedewerker radioloog)
	{
		return isNietGedaanDoor(radioloog).with(MammaBeoordeling_.eersteLezing)
			.and(isNietGedaanDoor(radioloog).with(MammaBeoordeling_.tweedeLezing));
	}

	private static ExtendedSpecification<MammaBeoordeling> verslagWerklijstSpecification(OrganisatieMedewerker radioloog)
	{
		return verslagGereedSpecification(radioloog)
			.or(verslagToegewezenSpecification(radioloog))
			.or(verslagAfgekeurdSpecification(radioloog))
			.or(verslagMakenSpecification(radioloog));
	}

	private static ExtendedSpecification<MammaBeoordeling> verslagGereedSpecification(OrganisatieMedewerker radioloog)
	{
		return heeftStatus(VERSLAG_GEREED)
			.and(isNietToegewezenAanSpecifiekeRadioloog())
			.and(isGedaanDoor(radioloog).with(MammaBeoordeling_.verslagLezing, LEFT));
	}

	private static ExtendedSpecification<MammaBeoordeling> verslagToegewezenSpecification(OrganisatieMedewerker radioloog)
	{
		return filterStatusIn(List.of(VERSLAG_GEREED, VERSLAG_AFGEKEURD, VERSLAG_MAKEN))
			.and(isToegewezenAan(radioloog));
	}

	private static ExtendedSpecification<MammaBeoordeling> verslagMakenSpecification(OrganisatieMedewerker radioloog)
	{
		return heeftStatus(VERSLAG_MAKEN)
			.and(isNietToegewezenAanSpecifiekeRadioloog())
			.and(magVerslagMaken(radioloog));
	}

	private static ExtendedSpecification<MammaBeoordeling> magVerslagMaken(OrganisatieMedewerker radioloog)
	{
		var metArbitrage = MammaBeoordelingSpecification.heeftArbitrageLezing()
			.and(isVerwezenDoor(radioloog).with(MammaBeoordeling_.eersteLezing)
				.or(isVerwezenDoor(radioloog).with(MammaBeoordeling_.tweedeLezing))
				.or(isGedaanDoor(radioloog).with(MammaBeoordeling_.arbitrageLezing, LEFT)));

		var zonderArbitrage = MammaBeoordelingSpecification.heeftGeenArbitrageLezing()
			.and(isGedaanDoor(radioloog).with(MammaBeoordeling_.eersteLezing)
				.or(isGedaanDoor(radioloog).with(MammaBeoordeling_.tweedeLezing)));

		return metArbitrage.or(zonderArbitrage);
	}

	private static ExtendedSpecification<MammaBeoordeling> verslagAfgekeurdSpecification(OrganisatieMedewerker radioloog)
	{
		return (r, q, cb) -> cb.and(
			cb.equal(r.get(MammaBeoordeling_.status), VERSLAG_AFGEKEURD),
			cb.equal(join(r, MammaBeoordeling_.verslagLezing, LEFT).get(MammaLezing_.beoordelaar), radioloog),
			r.get(MammaBeoordeling_.toegewezenOrganisatieMedewerker).isNull());
	}

	private static Join<Client, Persoon> persoonJoin(From<?, ? extends MammaOnderzoek> onderzoekRoot)
	{
		var screeningRondeJoin = screeningRondeJoin(onderzoekRoot);
		var dossierJoin = join(screeningRondeJoin, MammaScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, MammaDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

	private static SingularAttribute<MammaOnderzoek, MammaBeoordeling> beoordelingAttribute()
	{
		return MammaOnderzoek_.laatsteBeoordeling;
	}

	private static ExtendedSpecification<MammaOnderzoek> filterBeEnCe(MammaCeWerklijstZoekObject zoekObject)
	{
		return filterBeoordelingsEenheid(zoekObject.getBeoordelingsEenheden())
			.and(BeoordelingsEenheidSpecification.filterCentraleEenheid(zoekObject.getCentraleEenheden()).with(MammaBeoordeling_.beoordelingsEenheid))
			.with(beoordelingAttribute());
	}

	private static ExtendedSpecification<MammaOnderzoek> filterBeoordelingStatusVoorCeWerklijst(List<MammaBeoordelingStatus> statussen, LocalDate peildatumOngunstigeUitslagen)
	{
		if (!CollectionUtils.isEmpty(statussen) && statussen.contains(UITSLAG_ONGUNSTIG))
		{
			return filterOnGunstigeUitslagAlleenMetNietAfgedrukteBrieven(statussen, peildatumOngunstigeUitslagen);
		}

		return filterStatusIn(statussen).with(beoordelingAttribute());
	}

	private static ExtendedSpecification<MammaOnderzoek> filterOnGunstigeUitslagAlleenMetNietAfgedrukteBrieven(List<MammaBeoordelingStatus> statussen,
		LocalDate peildatumOngunstigeUitslagen)
	{
		var overigeStatussen = statussen.stream().filter(s -> s != UITSLAG_ONGUNSTIG).collect(Collectors.toList());
		return filterStatusIn(overigeStatussen).with(beoordelingAttribute())
			.or(heeftOnGunstigeUitslagMetNietGegenereerdeBrief(peildatumOngunstigeUitslagen));
	}

	private static ExtendedSpecification<MammaOnderzoek> heeftOnGunstigeUitslagMetNietGegenereerdeBrief(LocalDate peildatumOngunstigeUitslagen)
	{
		return heeftStatus(UITSLAG_ONGUNSTIG)
			.and(heeftStatusDatumVanaf(peildatumOngunstigeUitslagen)).with(beoordelingAttribute())
			.and(heeftNietGenereerdeUitslagBrief());
	}

	private static ExtendedSpecification<MammaOnderzoek> heeftNietGenereerdeUitslagBrief()
	{
		return heeftBriefTypeIn(BriefType.getMammaOngunstigeUitslagBriefTypen())
			.and(isGegenereerd(false))
			.with(r -> brievenJoin(r));
	}

	private static Join<MammaScreeningRonde, MammaBrief> brievenJoin(From<?, ? extends MammaOnderzoek> root)
	{
		var screeningRondeJoin = screeningRondeJoin(root);
		return join(screeningRondeJoin, MammaScreeningRonde_.brieven);
	}

	public static Order verwijzendSortering(Sort.Order order, Root<MammaOnderzoek> root, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.equals(VERWIJZEND_SORT_PROPERTY))
		{
			var beoordeling = join(root, MammaOnderzoek_.laatsteBeoordeling);

			var eersteLezingPredicate = heeftStatus(EERSTE_LEZING_OPGESLAGEN).and(isVerwezen().with(MammaBeoordeling_.eersteLezing, LEFT))
				.toPredicate(beoordeling, null, cb);

			var tweedeLezingPredicate = heeftStatus(TWEEDE_LEZING_OPGESLAGEN).and(isVerwezen().with(MammaBeoordeling_.tweedeLezing, LEFT))
				.toPredicate(beoordeling, null, cb);

			return cb.desc(cb.selectCase()
				.when(eersteLezingPredicate, 1)
				.when(tweedeLezingPredicate, 1)
				.otherwise(0));
		}

		return null;
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftGeenAfspraakNaScreeningRondeVanBeoordeling()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaAfspraak.class);
			var subUitnodigingJoin = join(subRoot, MammaAfspraak_.uitnodiging);
			var subScreeningRondeJoin = join(subUitnodigingJoin, MammaUitnodiging_.screeningRonde);

			subquery.select(cb.literal(1L))
				.where(cb.and(
					cb.equal(subScreeningRondeJoin.get(MammaScreeningRonde_.dossier), r.get(MammaScreeningRonde_.dossier)),
					cb.greaterThan(subScreeningRondeJoin.get(ScreeningRonde_.creatieDatum), r.get(ScreeningRonde_.creatieDatum))
				));

			return cb.not(cb.exists(subquery));
		};
	}

	public static ExtendedSpecification<MammaScreeningRonde> zijnBeeldenNietGedownload()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(MammaScreeningRonde_.FOLLOW_UP_RADIOLOGIE_VERSLAGEN));
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftGeenPathologieVerslag()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.correlate((Root<MammaScreeningRonde>) r);
			var subRootJoin = join(subRoot, MammaScreeningRonde_.followUpVerslagen);
			subquery.select(cb.literal(1L))
				.where(cb.equal(subRootJoin.get(MammaVerslag_.type), VerslagType.MAMMA_PA_FOLLOW_UP));

			return cb.not(cb.exists(subquery));
		};
	}

	public static Join<MammaAfspraak, MammaOnderzoek> laatsteOnderzoekVanLaatsteAfspraakJoin(From<?, ? extends MammaScreeningRonde> root)
	{
		return join(join(join(root, MammaScreeningRonde_.laatsteUitnodiging), MammaUitnodiging_.laatsteAfspraak), MammaAfspraak_.onderzoek);
	}
}
