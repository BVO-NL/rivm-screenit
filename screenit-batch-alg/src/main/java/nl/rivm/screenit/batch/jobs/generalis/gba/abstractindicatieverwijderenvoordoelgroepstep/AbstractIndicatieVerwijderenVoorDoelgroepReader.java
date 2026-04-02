package nl.rivm.screenit.batch.jobs.generalis.gba.abstractindicatieverwijderenvoordoelgroepstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import java.util.function.Function;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Root;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGbaStatusIn;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftRedenIntrekkenGbaIndicatie;

public abstract class AbstractIndicatieVerwijderenVoorDoelgroepReader extends BaseSpecificationScrollableResultReader<Client>
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	protected Specification<Client> createSpecification()
	{
		var clientSpecification = heeftGbaStatusIn(List.of(GbaStatus.INDICATIE_AANWEZIG, GbaStatus.PUNT_ADRES))
			.and(heeftRedenIntrekkenGbaIndicatie(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN));
		var persoonSpecification = PersoonSpecification.isNietOverledenEnWoontInNederland();

		return clientSpecification.and(persoonSpecification.withRoot(getPersoonJoin())).and(heeftGeenTePrintenBrieven());

	}

	@Override
	protected Class<Client> getEntityClass()
	{
		return Client.class;
	}

	private static Specification<Client> heeftGeenTePrintenBrieven()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Client.class);
			var subRoot = subquery.from(ClientBrief.class);
			subquery.select(subRoot.get(ClientBrief_.client)).where(
				cb.and(cb.isFalse(subRoot.get(ClientBrief_.gegenereerd)),
					cb.isFalse(subRoot.get(ClientBrief_.vervangendeProjectBrief)),
					cb.isFalse(subRoot.get(ClientBrief_.vervangen)),
					cb.isFalse(subRoot.get(ClientBrief_.tegenhouden)))
			);
			return cb.not(r.in(subquery));
		};
	}

	protected Function<Root<Client>, From<?, ? extends Persoon>> getPersoonJoin()
	{
		return (r) -> SpecificationUtil.join(r, Client_.persoon);
	}

	@Override
	protected int getMaxResults()
	{
		return preferenceService.getInteger(PreferenceKey.BRP_MAXIMAAL_AANTAL_INDICATIES_INTREK_AANVRAGEN.name(), 0);
	}

}
