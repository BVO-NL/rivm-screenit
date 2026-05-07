package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingOnderzoekFilterDto;
import nl.rivm.screenit.main.service.mamma.MammaOnderzoekService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.RangeUtil;

import org.apache.shiro.util.CollectionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.filterOpLaesies;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.filterOpRedenenFotobesprekingMbber;
import static nl.rivm.screenit.specification.mamma.MammaLezingSpecification.filterOpRedenenFotobesprekingRadioloog;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterBeoordelingseenheidIds;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterOpAfgerondDoor;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterRedenFotobespreking;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterScreeningseenheidIds;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.valtInPeriode;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.filterFollowUpConclusieStatus;

@Component
@RequiredArgsConstructor
public class MammaOnderzoekServiceImpl implements MammaOnderzoekService
{
	private final MammaOnderzoekRepository onderzoekRepository;

	@Override
	public Page<MammaOnderzoek> zoekOnderzoeken(MammaFotobesprekingOnderzoekFilterDto filter, PageRequest pageRequest)
	{
		return onderzoekRepository.findAll(zoekOnderzoekZoekSpecification(filter), pageRequest);
	}

	@Override
	public long countOnderzoeken(MammaFotobesprekingOnderzoekFilterDto filter)
	{
		return onderzoekRepository.count(zoekOnderzoekZoekSpecification(filter));
	}

	private Specification<MammaOnderzoek> zoekOnderzoekZoekSpecification(MammaFotobesprekingOnderzoekFilterDto filter)
	{
		return valtInPeriode(RangeUtil.closedOpen(filter.getOnderzoeksdatum().startDate(), filter.getOnderzoeksdatum().endDate()))
			.and(filterOpAfgerondDoor(filter.getMedewerkerId()))
			.and(MammaBeoordelingSpecification.heeftStatusIn(MammaBeoordelingStatus.uitslagStatussen()).with(MammaOnderzoek_.laatsteBeoordeling))
			.and(filterBeoordelingseenheidIds(filter.getBeoordelingseenheidIds()))
			.and(filterScreeningseenheidIds(filter.getScreeningseenheidIds()))
			.and(filterOpLezingMetRedenenRadioloog(filter.getRedenFotobesprekingDoorRadioloog()))
			.and(filterOpLezingMetRedenenMetMbber(filter.getRedenFotobesprekingMetMbber()))
			.and(filterOpLezingMetRedenDoorverwijzing(filter.getRedenDoorverwijzing()))
			.and(filterRedenFotobespreking(filter.getRedenFotobesprekingDoorMbber()))
			.and(filterFollowUpConclusieStatus(filter.getFollowUp()).with(r -> getScreeningRondeJoin(r)))
			.and(PersoonSpecification.filterBsn(filter.getBsn()).and(PersoonSpecification.filterGeboortedatum(DateUtil.toUtilDate(filter.getGeboortedatum())))
				.with(r -> getPersoonJoin(r)));
	}

	private Specification<MammaOnderzoek> filterOpLezingMetRedenenRadioloog(List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobespreking)
	{
		return CollectionUtils.isEmpty(redenenFotobespreking) ? null : (r, q, cb) ->
		{
			var beoordelingJoin = join(r, MammaOnderzoek_.laatsteBeoordeling);
			var eersteLezing = filterOpRedenenFotobesprekingRadioloog(redenenFotobespreking).toPredicate(join(beoordelingJoin, MammaBeoordeling_.eersteLezing, JoinType.LEFT), q,
				cb);
			var tweedeLezing = filterOpRedenenFotobesprekingRadioloog(redenenFotobespreking).toPredicate(join(beoordelingJoin, MammaBeoordeling_.tweedeLezing, JoinType.LEFT), q,
				cb);
			var discrepantieLezing = filterOpRedenenFotobesprekingRadioloog(redenenFotobespreking).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.discrepantieLezing, JoinType.LEFT), q, cb);
			var arbitrageLezing = filterOpRedenenFotobesprekingRadioloog(redenenFotobespreking).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.arbitrageLezing, JoinType.LEFT), q, cb);
			return cb.or(eersteLezing, tweedeLezing, discrepantieLezing, arbitrageLezing);
		};
	}

	private Specification<MammaOnderzoek> filterOpLezingMetRedenenMetMbber(List<MammaLezingRedenenFotobesprekingMbber> redenenFotobespreking)
	{
		return CollectionUtils.isEmpty(redenenFotobespreking) ? null : (r, q, cb) ->
		{
			var beoordelingJoin = join(r, MammaOnderzoek_.laatsteBeoordeling, JoinType.LEFT);
			var eersteLezing = filterOpRedenenFotobesprekingMbber(redenenFotobespreking).toPredicate(join(beoordelingJoin, MammaBeoordeling_.eersteLezing, JoinType.LEFT), q, cb);
			var tweedeLezing = filterOpRedenenFotobesprekingMbber(redenenFotobespreking).toPredicate(join(beoordelingJoin, MammaBeoordeling_.tweedeLezing, JoinType.LEFT), q, cb);
			var discrepantieLezing = filterOpRedenenFotobesprekingMbber(redenenFotobespreking).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.discrepantieLezing, JoinType.LEFT), q, cb);
			var arbitrageLezing = filterOpRedenenFotobesprekingMbber(redenenFotobespreking).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.arbitrageLezing, JoinType.LEFT), q, cb);
			return cb.or(eersteLezing, tweedeLezing, discrepantieLezing, arbitrageLezing);
		};
	}

	private Specification<MammaOnderzoek> filterOpLezingMetRedenDoorverwijzing(List<MammaLaesieType> redenenDoorverwijzing)
	{
		return CollectionUtils.isEmpty(redenenDoorverwijzing) ? null : (r, q, cb) ->
		{
			var beoordelingJoin = join(r, MammaOnderzoek_.laatsteBeoordeling, JoinType.LEFT);
			var eersteLezing = filterOpLaesies(redenenDoorverwijzing).toPredicate(join(beoordelingJoin, MammaBeoordeling_.eersteLezing, JoinType.LEFT), q, cb);
			var tweedeLezing = filterOpLaesies(redenenDoorverwijzing).toPredicate(join(beoordelingJoin, MammaBeoordeling_.tweedeLezing, JoinType.LEFT), q, cb);
			var discrepantieLezing = filterOpLaesies(redenenDoorverwijzing).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.discrepantieLezing, JoinType.LEFT), q, cb);
			var arbitrageLezing = filterOpLaesies(redenenDoorverwijzing).toPredicate(
				join(beoordelingJoin, MammaBeoordeling_.arbitrageLezing, JoinType.LEFT), q, cb);
			return cb.or(eersteLezing, tweedeLezing, discrepantieLezing, arbitrageLezing);
		};
	}

	private Join<Client, Persoon> getPersoonJoin(From<?, ? extends MammaOnderzoek> root)
	{
		var uitnodigingJoin = getUitnodigingJoin(root);
		var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		var dossierJoin = join(screeningRondeJoin, MammaScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, MammaDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

	private Join<MammaUitnodiging, MammaScreeningRonde> getScreeningRondeJoin(From<?, ? extends MammaOnderzoek> root)
	{
		var uitnodigingJoin = getUitnodigingJoin(root);
		return join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
	}

	private Join<MammaAfspraak, MammaUitnodiging> getUitnodigingJoin(From<?, ? extends MammaOnderzoek> root)
	{
		var afspraakJoin = join(root, MammaOnderzoek_.afspraak, JoinType.LEFT);
		return join(afspraakJoin, MammaAfspraak_.uitnodiging);
	}
}
