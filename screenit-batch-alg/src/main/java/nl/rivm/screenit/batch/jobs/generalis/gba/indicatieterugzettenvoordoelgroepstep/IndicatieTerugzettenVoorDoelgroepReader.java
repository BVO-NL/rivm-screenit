package nl.rivm.screenit.batch.jobs.generalis.gba.indicatieterugzettenvoordoelgroepstep;

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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGbaStatusIn;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftNogTeVersturenBmhkUitnodigingen;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftNogTeVersturenDkUitnodigingen;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftRedenIntrekkenGbaIndicatie;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftTePrintenBrieven;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverledenEnWoontInNederland;

@Component
@RequiredArgsConstructor
public class IndicatieTerugzettenVoorDoelgroepReader extends BaseSpecificationScrollableResultReader<Client>
{

	@Override
	protected Specification<Client> createSpecification()
	{
		var persoonSpecification = isNietOverledenEnWoontInNederland();
		var clientSpecification = heeftGbaStatusIn(List.of(GbaStatus.INDICATIE_AANWEZIG, GbaStatus.PUNT_ADRES))
			.and(Specification.not(heeftRedenIntrekkenGbaIndicatie(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN)));

		return clientSpecification
			.and(persoonSpecification.withRoot(getPersoonJoin()))
			.and((heeftTePrintenBrieven())
				.or(heeftNogTeVersturenBmhkUitnodigingen())
				.or(heeftNogTeVersturenDkUitnodigingen()));
	}

	private Function<Root<Client>, From<?, ? extends Persoon>> getPersoonJoin()
	{
		return (r) -> join(r, Client_.persoon);
	}
}
