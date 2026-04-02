package nl.rivm.screenit.batch.jobs.generalis.gba.teoudeclientstep;

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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.generalis.gba.abstractindicatieverwijderenvoordoelgroepstep.AbstractIndicatieVerwijderenVoorDoelgroepReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TeOudeClientenReader extends AbstractIndicatieVerwijderenVoorDoelgroepReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final SimplePreferenceService preferenceService;

	@Override
	protected Specification<Client> createSpecification()
	{
		var geboorteDatumVoorTachtigPlus = currentDateSupplier.getLocalDate()
			.minusYears(preferenceService.getInteger(PreferenceKey.BOVENGRENS_LEEFTIJD_VOOR_VERWIJDEREN_BRP_INDICATIES.name(), 80));
		var specification = super.createSpecification();
		return specification.and(PersoonSpecification.isGeborenVoorOfOp(geboorteDatumVoorTachtigPlus).withRoot(getPersoonJoin()));
	}
}
