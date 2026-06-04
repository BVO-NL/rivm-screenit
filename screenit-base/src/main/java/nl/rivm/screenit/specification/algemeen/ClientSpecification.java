package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.InpakbareUitnodiging_;
import nl.rivm.screenit.model.Persoon_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitRegistratie_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.heeftDeelnamemodus;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGbaAdresMetPostcode;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverleden;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverledenEnWoontInNederland;
import static nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification.heeftStatusNullOfActief;
import static org.springframework.data.jpa.domain.Specification.not;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientSpecification
{
	public static ExtendedSpecification<Client> heeftIndicatie()
	{
		return heeftGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
	}

	public static ExtendedSpecification<Client> heeftNietIngetrokkenIndicatie()
	{
		return heeftGbaStatus(GbaStatus.INDICATIE_AANWEZIG).and(heeftRedenIntrekkenGbaIndicatie(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN));
	}

	public static ExtendedSpecification<Client> isNietAfgevoerd()
	{
		return (r, q, cb) -> cb.notEqual(r.get(Client_.gbaStatus), GbaStatus.AFGEVOERD);
	}

	public static ExtendedSpecification<Client> heeftActieveClient()
	{
		return isNietOverledenEnWoontInNederland().with(Client_.persoon)
			.and(heeftIndicatie());
	}

	public static ExtendedSpecification<Client> isNietOverledenOfAfgevoerd()
	{
		return isNietOverleden().with(Client_.persoon).and(isNietAfgevoerd());
	}

	public static Specification<Client> heeftGeboorteJaarVoorLeeftijdBereik(int minimaleLeeftijd, int maximaleLeeftijd, LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var persoon = join(r, Client_.persoon);
			var geboorteDatum = persoon.get(Persoon_.geboortedatum);
			var maxGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - minimaleLeeftijd, 12, 31));
			var minGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - maximaleLeeftijd, 1, 1));
			return cb.and(cb.greaterThanOrEqualTo(geboorteDatum, minGeboortedatum), cb.lessThanOrEqualTo(geboorteDatum, maxGeboortedatum));
		};
	}

	public static Specification<Client> heeftANummer(String anummer)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			if (StringUtils.isNotEmpty(anummer))
			{
				return cb.equal(persoonJoin.get(Persoon_.anummer), anummer);
			}
			return null;
		};
	}

	public static Specification<Client> heeftTitelCode(String titelCode)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return cb.equal(persoonJoin.get(Persoon_.titelCode), titelCode);
		};
	}

	public static Specification<Client> heeftGbaMutaties()
	{
		return (r, q, cb) -> cb.notEqual(cb.size(r.get(Client_.gbaMutaties)), 0);
	}

	public static Specification<Client> heeftBsnDieEindigtMet(String bsn)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return cb.equal(cb.function("right", String.class, persoonJoin.get(Persoon_.bsn), cb.literal(9)), bsn);
		};
	}

	public static ExtendedSpecification<Client> heeftGbaStatus(GbaStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(Client_.gbaStatus), status);
	}

	public static Specification<Client> heeftNietGbaStatus(GbaStatus status)
	{
		return ((r, q, cb) -> cb.notEqual(r.get(Client_.gbaStatus), status));
	}

	public static Specification<Client> heeftNietGbaStatussen(List<GbaStatus> statussen)
	{
		return (r, q, cb) -> cb.not(r.get(Client_.gbaStatus).in(statussen));
	}

	public static ExtendedSpecification<Client> heeftGbaStatusIn(List<GbaStatus> statussen)
	{
		return (r, q, cb) -> r.get(Client_.gbaStatus).in(statussen);
	}

	public static ExtendedSpecification<Client> heeftMammaDossier()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Client_.mammaDossier));
	}

	public static ExtendedSpecification<Client> heeftColonDossier()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Client_.colonDossier));
	}

	public static ExtendedSpecification<Client> heeftCervixDossier()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Client_.cervixDossier));
	}

	public static Specification<Client> heeftMinimaalEenDossier()
	{
		return heeftCervixDossier().or(heeftColonDossier()).or(heeftMammaDossier());
	}

	public static Specification<Client> voldoetAanMammaClientSelectieRestricties()
	{
		return not(heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).with(Client_.mammaDossier))
			.and(heeftStatusNullOfActief().with(Client_.mammaDossier))
			.and(heeftGbaAdresMetPostcode().with(Client_.persoon))
			.and(heeftActieveClient());
	}

	public static ExtendedSpecification<Client> heeftRedenIntrekkenGbaIndicatie(RedenIntrekkenGbaIndicatie redenIntrekkenGbaIndicatie)
	{
		return (r, q, cb) -> cb.equal(r.get(Client_.redenIntrekkenGbaIndicatieDoorBvo), redenIntrekkenGbaIndicatie);
	}

	public static ExtendedSpecification<Client> heeftGeenActieveProjectClienten(List<Long> exclusieGroepIds)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(ProjectClient.class);
			var clientJoin = join(subRoot, ProjectClient_.client);

			subquery.select(cb.literal(1L))
				.where(cb.and(
					cb.isTrue(subRoot.get(ProjectClient_.actief)),
					subRoot.get(ProjectClient_.groep).get(AbstractHibernateObject_.id).in(exclusieGroepIds),
					cb.equal(clientJoin, r)
				));

			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<Client> heeftTePrintenBrieven()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Client.class);
			var subRoot = subquery.from(ClientBrief.class);
			subquery.select(subRoot.get(ClientBrief_.client)).where(
				cb.and(cb.equal(subRoot.get(ClientBrief_.client), r),
					cb.and(cb.isFalse(subRoot.get(ClientBrief_.gegenereerd)),
						cb.isFalse(subRoot.get(ClientBrief_.vervangendeProjectBrief)),
						cb.isFalse(subRoot.get(ClientBrief_.vervangen)),
						cb.isFalse(subRoot.get(ClientBrief_.tegenhouden)))
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> metBeoordelingId(Long beoordelingId)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = join(r, Client_.mammaDossier);
			var screeningRondeJoin = join(dossierJoin, MammaDossier_.screeningRondes);
			var uitnodigingJoin = join(screeningRondeJoin, MammaScreeningRonde_.uitnodigingen);
			var afspraakJoin = join(uitnodigingJoin, MammaUitnodiging_.afspraken);
			var onderzoekJoin = join(afspraakJoin, MammaAfspraak_.onderzoek);
			var beoordelingJoin = join(onderzoekJoin, MammaOnderzoek_.beoordelingen);
			return cb.equal(beoordelingJoin.get(AbstractHibernateObject_.id), beoordelingId);
		};
	}

	public static Specification<Client> heeftNogTeVersturenBmhkUitnodigingen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(CervixUitnodiging.class);
			var uitnodigingRoot = subquery.from(CervixUitnodiging.class);
			var screeningRondeJoin = join(uitnodigingRoot, CervixUitnodiging_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, CervixScreeningRonde_.dossier);
			subquery.select(uitnodigingRoot).where(
				cb.and(
					cb.equal(r.get(Client_.cervixDossier), dossierJoin),
					cb.or(
						cb.isFalse(uitnodigingRoot.get(InpakbareUitnodiging_.verstuurd)),
						cb.isNull(uitnodigingRoot.get(InpakbareUitnodiging_.verstuurdDatum))
					),
					cb.equal(uitnodigingRoot.get(CervixUitnodiging_.monsterType), CervixMonsterType.ZAS)
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftNogTeVersturenDkUitnodigingen()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(ColonUitnodiging.class);
			var uitnodigingRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = SpecificationUtil.join(uitnodigingRoot, ColonUitnodiging_.screeningRonde);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery.select(uitnodigingRoot).where(
				cb.and(
					cb.equal(r.get(Client_.colonDossier), dossierJoin),
					cb.or(
						cb.isFalse(uitnodigingRoot.get(InpakbareUitnodiging_.verstuurd)),
						cb.isNull(uitnodigingRoot.get(InpakbareUitnodiging_.verstuurdDatum))
					)
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftBkUitnodigingsnummer(long uitnodigingsnummer)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var rondeRoot = subquery.from(MammaScreeningRonde.class);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(rondeRoot.get(MammaScreeningRonde_.dossier), r.get(Client_.mammaDossier)),
					cb.equal(rondeRoot.get(MammaScreeningRonde_.uitnodigingsNr), uitnodigingsnummer)
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftBmhkMonsterId(String monsterId)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var monsterRoot = subquery.from(CervixMonster.class);
			var uitnodigingJoin = join(monsterRoot, CervixMonster_.uitnodiging);
			var screeningRondeJoin = join(uitnodigingJoin, CervixUitnodiging_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, CervixScreeningRonde_.dossier);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(dossierJoin, r.get(Client_.cervixDossier)),
					cb.equal(monsterRoot.get(CervixMonster_.monsterId), StringUtils.trimToEmpty(monsterId))
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftDkBarcode(String barcode)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var fitRoot = subquery.from(ColonFitRegistratie.class);
			var screeningRondeJoin = join(fitRoot, ColonFitRegistratie_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(dossierJoin, r.get(Client_.colonDossier)),
					cb.equal(fitRoot.get(ColonFitRegistratie_.barcode), barcode)
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftEmailadres(String emailadres)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return SpecificationUtil.exactCaseInsensitive(cb, persoonJoin.get(Persoon_.emailadres), emailadres);
		};
	}

	public static Specification<Client> heeftBmhkUitnodigingsId(long uitnodigingsId)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var uitnodigingRoot = subquery.from(CervixUitnodiging.class);
			var screeningRondeJoin = join(uitnodigingRoot, CervixUitnodiging_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, CervixScreeningRonde_.dossier);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(dossierJoin, r.get(Client_.cervixDossier)),
					cb.equal(uitnodigingRoot.get(InpakbareUitnodiging_.uitnodigingsId), uitnodigingsId)
				));
			return cb.exists(subquery);
		};
	}

	public static Specification<Client> heeftDkUitnodigingsId(long uitnodigingsId)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var uitnodigingRoot = subquery.from(ColonUitnodiging.class);
			var screeningRondeJoin = join(uitnodigingRoot, ColonUitnodiging_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, ColonScreeningRonde_.dossier);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(dossierJoin, r.get(Client_.colonDossier)),
					cb.equal(uitnodigingRoot.get(InpakbareUitnodiging_.uitnodigingsId), uitnodigingsId)
				));
			return cb.exists(subquery);
		};
	}
}
