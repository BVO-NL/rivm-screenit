package nl.rivm.screenit.batch.jobs.colon.controleuitslag.controlestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Expression;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitRegistratie_;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftFitType;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.heeftStatusDatumVoorOfOp;
import static nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification.valideerFitUitslagStatus;

@Component
@AllArgsConstructor
public class ColonControleMissendeUitslagenReader extends BaseSpecificationScrollableResultReader<ColonFitRegistratie>
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	protected Specification<ColonFitRegistratie> createSpecification()
	{
		var signaleringstermijn = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.COLON_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);
		var vandaag = currentDateSupplier.getLocalDate();
		var signalerenVanaf = vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		var minimaleSignaleringsdatum = vandaag.minusDays(signaleringstermijn);
		return valideerFitUitslagStatus(signalerenVanaf)
			.and(heeftStatusDatumVoorOfOp(minimaleSignaleringsdatum.atStartOfDay()))
			.and(heeftFitType(ColonFitType.GOLD))
			.and(heeftActieveClient());
	}

	@Override
	protected Expression<Long> createProjection(Root<ColonFitRegistratie> r, CriteriaBuilder cb)
	{
		return dossierAttribuut(r);
	}

	@Override
	protected Order getOrder(Root<ColonFitRegistratie> r, CriteriaBuilder cb)
	{
		return cb.asc(createProjection(r, cb));
	}

	private static Path<Long> dossierAttribuut(Root<ColonFitRegistratie> r)
	{
		return join(join(r, ColonFitRegistratie_.screeningRonde), ColonScreeningRonde_.dossier).get(AbstractHibernateObject_.id);
	}
}
