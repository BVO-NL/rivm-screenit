package nl.rivm.screenit.batch.jobs.colon.fitherinnering.step;

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

import java.time.LocalDateTime;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.fitherinnering.ColonFitHerinneringJobConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonFitRegistratieSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.primitives.Ints;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class ColonFitHerinneringBriefReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ClientRepository clientRepository;

	@Override
	protected Specification<Client> createSpecification()
	{
		var minimumLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		var maximumLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		var interval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());

		var peildatum = currentDateSupplier.getLocalDate();

		var specification = ClientSpecification.heeftActieveClient()
			.and(ScreeningRondeSpecification.isLopend().with(r -> laatsteScreeningRondeJoin(r)))
			.and(ColonFitRegistratieSpecification.heeftStatus(ColonFitRegistratieStatus.ACTIEF).with(r -> fitJoin(r)))
			.and(ColonFitRegistratieSpecification.heeftStatusDatumVoorOfOp(getHerinneringsDatum()).with(r -> fitJoin(r)))
			.and(ColonFitRegistratieSpecification.heeftHerinnering(Boolean.FALSE).with(r -> fitJoin(r)))
			.and(ColonFitRegistratieSpecification.heeftFitType(ColonFitType.GOLD).with(r -> fitJoin(r)))
			.and(PersoonSpecification.valtBinnenLeeftijdGrensRestricties(minimumLeeftijd, maximumLeeftijd, interval, peildatum).with(r -> persoonJoin(r)))
			.and(ColonFitRegistratieSpecification.fitIsHoudbaar(peildatum).with(r -> fitJoin(r)))
			.and(ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(BriefType.COLON_UITSLAG_BRIEVEN).with(r -> laatsteScreeningRondeJoin(r)));

		var uniqueResults = clientRepository.countDistinct(specification);
		getExecutionContext().putInt(ColonFitHerinneringJobConstants.GESELECTEERD, Ints.checkedCast(uniqueResults));

		return specification;
	}

	private static Join<? extends Client, Persoon> persoonJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.persoon);
	}

	private From<?, ? extends ColonScreeningRonde> laatsteScreeningRondeJoin(From<?, ? extends Client> r)
	{
		return join(join(r, Client_.colonDossier), ColonDossier_.laatsteScreeningRonde);
	}

	private From<?, ? extends ColonFitRegistratie> fitJoin(From<?, ? extends Client> r)
	{
		return join(laatsteScreeningRondeJoin(r), ColonScreeningRonde_.laatsteFitRegistratie);
	}

	private LocalDateTime getHerinneringsDatum()
	{
		try
		{
			var herinneringsPeriode = preferenceService.getInteger(PreferenceKey.COLON_HERINNERINGS_PERIODE.name());
			return currentDateSupplier.getLocalDateTime().minusDays(herinneringsPeriode);
		}
		catch (Exception e)
		{
			crashMelding("De FIT herinnering-periode is niet gezet in de parameterisatie.", e);
			throw e;
		}
	}

}
