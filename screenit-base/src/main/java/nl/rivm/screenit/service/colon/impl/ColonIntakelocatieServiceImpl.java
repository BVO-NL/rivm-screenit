package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.specification.colon.ColonIntakelocatieSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

@Service
@AllArgsConstructor
public class ColonIntakelocatieServiceImpl implements ColonIntakelocatieService
{
	private final ColonIntakelocatieRepository intakelocatieRepository;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	public Range<LocalDate> getSignaleringstermijnBereik()
	{
		var signaleringstermijn = preferenceService.getInteger(PreferenceKey.COLON_SIGNALERINGSTERMIJN_GEEN_CAPACITEIT.name(), 8);
		var aaneengeslotenPeriode = preferenceService.getInteger(PreferenceKey.COLON_SIGNALERINGSTERMIJN_AANEENGESLOTEN_PERIODE.name(), 3);

		var start = dateSupplier.getLocalDate().plusWeeks(signaleringstermijn);
		if (!start.getDayOfWeek().equals(DayOfWeek.MONDAY))
		{
			start = start.with(TemporalAdjusters.previous(DayOfWeek.MONDAY));
		}
		var end = start.plusWeeks(aaneengeslotenPeriode).minusDays(1);

		return Range.closed(start, end);
	}

	@Override
	public LocalDate getSignaleringstermijnDeadline()
	{
		var start = dateSupplier.getLocalDate();
		if (!start.getDayOfWeek().equals(DayOfWeek.MONDAY))
		{
			start = start.with(TemporalAdjusters.next(DayOfWeek.MONDAY));
		}
		return start;
	}

	@Override
	public String getSignaleringstermijnTekst()
	{
		var signaleringstermijn = getSignaleringstermijnBereik();
		return signaleringstermijn.lowerEndpoint().format(DateUtil.LOCAL_DATE_FORMAT) + " - " + signaleringstermijn.lowerEndpoint().plusDays(6).format(DateUtil.LOCAL_DATE_FORMAT);
	}

	@Override
	public boolean intakelocatieHeeftGeenCapaciteit(ColonIntakelocatie intakelocatie)
	{
		var results = intakelocatieRepository.findFirst(
			ColonIntakelocatieSpecification.heeftGeenCapaciteitBinnenDatum(getSignaleringstermijnBereik())
				.and(ColonIntakelocatieSpecification.isActief())
				.and(ColonIntakelocatieSpecification.isIntakelocatie(intakelocatie)), Sort.unsorted());
		return results.isPresent();
	}

	@Override
	@Transactional
	public void saveIntakelocatieBeschrijving(ColonIntakelocatie intakelocatie, String locatieBeschrijving, InstellingGebruiker instellingGebruiker)
	{
		intakelocatieRepository.save(intakelocatie);
		var locatieBeschrijvingParameter = organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.COLON_INTAKELOCATIE_BESCHRIJVING,
			locatieBeschrijving, intakelocatie);
		organisatieParameterService.saveOrUpdateOrganisatieParameters(List.of(locatieBeschrijvingParameter),
			instellingGebruiker);
	}

	@Override
	public void saveIntakelocatieDigitaleIntake(ColonIntakelocatie intakelocatie, String digitaleIntakeTekst, InstellingGebruiker instellingGebruiker)
	{
		var digitaleIntakeParameter = organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.COLON_DIGITALE_INTAKE,
			digitaleIntakeTekst, intakelocatie);
		organisatieParameterService.saveOrUpdateOrganisatieParameters(List.of(digitaleIntakeParameter), instellingGebruiker);
	}
}
