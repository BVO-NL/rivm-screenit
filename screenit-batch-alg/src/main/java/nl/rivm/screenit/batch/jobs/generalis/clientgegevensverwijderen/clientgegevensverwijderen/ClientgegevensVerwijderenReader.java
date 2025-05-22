package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.clientgegevensverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen.AbstractGegevensVerwijderenReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ClientgegevensVerwijderenReader extends AbstractGegevensVerwijderenReader
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		var clientGegevensVerwijderenLeeftijd = preferenceService.getInteger(PreferenceKey.LEEFTIJD_PERSOONSGEGEVENS_UIT_EXTRA_BEVEILIGDE_OMGEVING_VERWIJDEREN.name(), 77);
		var maxGeboortedatum = currentDateSupplier.getLocalDate().minusYears(clientGegevensVerwijderenLeeftijd);
		var specification = super.createSpecification();
		return specification.and(PersoonSpecification.isGeborenVoorOfOp(maxGeboortedatum).with(Client_.persoon));
	}
}
