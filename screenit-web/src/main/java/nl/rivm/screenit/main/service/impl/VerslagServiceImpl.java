package nl.rivm.screenit.main.service.impl;

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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.VerslagService;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.repository.algemeen.OntvangenCdaBerichtRepository;
import nl.rivm.screenit.repository.cervix.CervixCytologieVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonMdlVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonPaVerslagRepository;
import nl.rivm.screenit.repository.colon.ColonVerslagRepository;
import nl.rivm.screenit.repository.mamma.MammaFollowUpVerslagRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.specification.cervix.CervixVerslagSpecification;
import nl.rivm.screenit.specification.colon.ColonVerslagSpecification;
import nl.rivm.screenit.specification.mamma.MammaFollowUpVerslagSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static java.util.Arrays.stream;
import static nl.rivm.screenit.specification.algemeen.OntvangenCdaBerichtSpecification.maakZoekSpecification;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftClientIdInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftClientIdInPaVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTypeInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftTypeInPaVerslag;

@Service
public class VerslagServiceImpl implements VerslagService
{

	@Autowired
	private BerichtToBatchService cdaBerichtToBatchService;

	@Autowired
	private BaseVerslagService baseVerslagService;

	@Autowired
	private ColonMdlVerslagRepository mdlVerslagRepository;

	@Autowired
	private OntvangenCdaBerichtRepository ontvangenCdaBerichtRepository;

	@Autowired
	private ColonPaVerslagRepository paVerslagRepository;

	@Autowired
	private CervixCytologieVerslagRepository cervixCytologieVerslagRepository;

	@Autowired
	private MammaFollowUpVerslagRepository mammaVerslagRepository;

	@Autowired
	private ColonVerslagRepository colonVerslagRepository;

	@Override
	public List<OntvangenCdaBericht> zoekBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending)
	{
		var sort = Sort.by(ascending ? Sort.Direction.ASC : Sort.Direction.DESC, property);
		return ontvangenCdaBerichtRepository.findWith(maakZoekSpecification(filter), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countBerichten(BerichtZoekFilter filter)
	{
		return ontvangenCdaBerichtRepository.count(maakZoekSpecification(filter));
	}

	@Override
	@Transactional
	public void herverwerkAlleBerichten(BerichtZoekFilter filter)
	{
		var idsEnBerichtTypen = ontvangenCdaBerichtRepository.findWith(maakZoekSpecification(filter), Object[].class,
			q -> q.projections((cb, r) -> List.of(r.get(AbstractHibernateObject_.id), r.get(OntvangenCdaBericht_.berichtType)))).all();
		stream(BerichtType.values()).forEach(berichtType -> berichtenOpnieuwVerwerken(
			idsEnBerichtTypen.stream()
				.filter(o -> (((Object[]) o)[1]) == berichtType)
				.map(o -> ((Long) ((Object[]) o)[0]))
				.collect(Collectors.toList()),
			berichtType.getBevolkingsonderzoek()));
	}

	@Override
	public <V extends Verslag<?, ?>> List<V> zoekVerslagen(V zoekCriteria, int first, int aantal, String property, boolean ascending)
	{
		var sort = Sort.by(ascending ? Sort.Direction.ASC : Sort.Direction.DESC, property);

		if (zoekCriteria instanceof MdlVerslag)
		{
			var specification = getZoekMdlVerslagenSpecification((MdlVerslag) zoekCriteria);
			return (List<V>) mdlVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof PaVerslag)
		{
			var specification = getZoekPaVerslagenSpecification((PaVerslag) zoekCriteria);
			return (List<V>) paVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof CervixCytologieVerslag)
		{
			var specification = getZoekCervixVerslagenSpecification((CervixCytologieVerslag) zoekCriteria);
			return (List<V>) cervixCytologieVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof MammaFollowUpVerslag)
		{
			var specification = getZoekFollowUpVerslagenSpecification((MammaFollowUpVerslag) zoekCriteria);
			return (List<V>) mammaVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
		}

		if (zoekCriteria instanceof ColonVerslag)
		{
			var specification = getZoekColonVerslagenSpecification((ColonVerslag) zoekCriteria);
			var colonVerslagen = colonVerslagRepository.findWith(specification, q -> q.sortBy(sort)).all(first, aantal);
			return colonVerslagen.stream().map(v -> (V) v).collect(Collectors.toList());
		}

		throw new IllegalArgumentException("Onbekend verslag type: " + zoekCriteria.getClass().getName());
	}

	private Specification<MdlVerslag> getZoekMdlVerslagenSpecification(MdlVerslag zoekObject)
	{
		var specification = heeftClientIdInMdlVerslag(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(heeftTypeInMdlVerslag(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<PaVerslag> getZoekPaVerslagenSpecification(PaVerslag zoekObject)
	{
		var specification = heeftClientIdInPaVerslag(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(heeftTypeInPaVerslag(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<CervixCytologieVerslag> getZoekCervixVerslagenSpecification(CervixCytologieVerslag zoekObject)
	{
		var specification = CervixVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(CervixVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<MammaFollowUpVerslag> getZoekFollowUpVerslagenSpecification(MammaFollowUpVerslag zoekObject)
	{
		var specification = MammaFollowUpVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(MammaFollowUpVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	private Specification<ColonVerslag> getZoekColonVerslagenSpecification(ColonVerslag zoekObject)
	{
		var specification = ColonVerslagSpecification.heeftClientId(zoekObject.getScreeningRonde().getDossier().getClient().getId());
		if (zoekObject.getType() != null)
		{
			specification = specification.and(ColonVerslagSpecification.heeftType(zoekObject.getType()));
		}
		return specification;
	}

	@Override
	public <V extends Verslag<?, ?>> long countVerslagen(V zoekObject)
	{
		if (zoekObject instanceof MdlVerslag)
		{
			var specification = getZoekMdlVerslagenSpecification((MdlVerslag) zoekObject);
			return mdlVerslagRepository.count(specification);
		}

		if (zoekObject instanceof PaVerslag)
		{
			var specification = getZoekPaVerslagenSpecification((PaVerslag) zoekObject);
			return paVerslagRepository.count(specification);
		}

		if (zoekObject instanceof CervixCytologieVerslag)
		{
			var specification = getZoekCervixVerslagenSpecification((CervixCytologieVerslag) zoekObject);
			return cervixCytologieVerslagRepository.count(specification);
		}

		if (zoekObject instanceof MammaFollowUpVerslag)
		{
			var specification = getZoekFollowUpVerslagenSpecification((MammaFollowUpVerslag) zoekObject);
			return mammaVerslagRepository.count(specification);
		}

		if (zoekObject instanceof ColonVerslag)
		{
			var specification = getZoekColonVerslagenSpecification((ColonVerslag) zoekObject);
			return colonVerslagRepository.count(specification);
		}

		throw new IllegalArgumentException("Onbekend verslag type: " + zoekObject.getClass().getName());
	}

	@Override
	public void berichtenOpnieuwVerwerken(List<Long> ids, Bevolkingsonderzoek bvo)
	{
		if (!ids.isEmpty())
		{
			baseVerslagService.setBerichtenOpnieuwVerwerken(ids);
			cdaBerichtToBatchService.queueCDABericht(bvo);
		}
	}

	@Override
	public void berichtOpnieuwVerwerken(OntvangenCdaBericht ontvangenCdaBericht)
	{
		berichtenOpnieuwVerwerken(List.of(ontvangenCdaBericht.getId()), ontvangenCdaBericht.getBerichtType().getBevolkingsonderzoek());
	}
}
