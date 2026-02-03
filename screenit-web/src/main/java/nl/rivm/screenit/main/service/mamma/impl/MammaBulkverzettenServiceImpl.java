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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaBulkVerzettenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.main.service.mamma.MammaBulkverzettenService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDigitaalClientBericht;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaMindervalideUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static java.math.BigDecimal.ZERO;

@Service
@RequiredArgsConstructor
public class MammaBulkverzettenServiceImpl implements MammaBulkverzettenService
{
	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final LogService logService;

	private final SimplePreferenceService preferenceService;

	private final MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	private final BaseBriefService baseBriefService;

	private final BerichtToSeRestBkService berichtToSeRestBkService;

	private final MammaBaseKansberekeningService kansberekeningService;

	private final MammaDigitaalContactService digitaalContactService;

	@Override
	public BigDecimal getVrijeCapaciteitBulkverzetten(IMammaBulkVerzettenFilter filter)
	{
		var vanafMoment = DateUtil.toUtilDate(filter.getVanafLocalDate());
		var totEnMetMoment = DateUtil.toUtilDate(filter.getTotEnMetLocalDate().plusDays(1));
		var capaciteitsBlokken = baseCapaciteitsBlokService.getNietGeblokkeerdeScreeningCapaciteitBlokDtos(filter.getStandplaatsPeriode(), vanafMoment, totEnMetMoment, null);

		var vrijeCapaciteit = capaciteitsBlokken.stream().map(MammaPlanningUtil::getVrijeCapaciteitVanBlok).reduce(ZERO, BigDecimal::add);
		var gereserveerdeMindervalideCapaciteit = gereserveerdeMindervalideCapaciteit(capaciteitsBlokken, filter);
		return vrijeCapaciteit.subtract(gereserveerdeMindervalideCapaciteit);
	}

	private BigDecimal gereserveerdeMindervalideCapaciteit(Collection<MammaCapaciteitBlokDto> capaciteitsBlokken, IMammaBulkVerzettenFilter filter)
	{
		var minderValideReserveringIngeschakeld = preferenceService.getBoolean(PreferenceKey.MAMMA_MINDERVALIDE_RESERVERING_ACTIEF.name(), false);
		if (!minderValideReserveringIngeschakeld)
		{
			return BigDecimal.ZERO;
		}

		var vrijgevenMindervalideReserveringenTotEnMetDatum = getVrijgevenMindervalideReserveringenTotEnMetDatum();
		var factorMindervalide = MammaScreeningsEenheidUtil.getScreeningsOrganisatie(filter.getStandplaatsPeriode().getScreeningsEenheid()).getFactorMindervalideBk();

		var aantalOnbezetteMinderValideReserveringen = capaciteitsBlokken.stream()
			.filter(blok -> !MammaMindervalideUtil.zijnMindervalideReserveringenVrijgegeven(blok, vrijgevenMindervalideReserveringenTotEnMetDatum))
			.flatMap(this::onbezetteMindervalideReserveringen)
			.count();
		return factorMindervalide.multiply(BigDecimal.valueOf(aantalOnbezetteMinderValideReserveringen));
	}

	private Stream<LocalTime> onbezetteMindervalideReserveringen(MammaCapaciteitBlokDto capaciteitBlokDto)
	{
		return capaciteitBlokDto.getMindervalideReserveringen().stream()
			.filter(reserveringVanaf -> MammaMindervalideUtil.isMindervalideReserveringOnbezet(capaciteitBlokDto, reserveringVanaf));
	}

	private LocalDate getVrijgevenMindervalideReserveringenTotEnMetDatum()
	{
		var vrijgevenMindervalideReserveringenBinnenAantalDagen = preferenceService.getInteger(PreferenceKey.MAMMA_VRIJGEVEN_MINDERVALIDE_RESERVERINGEN_BINNEN_AANTAL_DAGEN.name());
		return currentDateSupplier.getLocalDate().plusDays(vrijgevenMindervalideReserveringenBinnenAantalDagen);
	}

	@Override
	@Transactional
	public void bulkVerzetten(IMammaBulkVerzettenFilter filter, List<MammaAfspraak> afspraken, Account account, LocalDate verzettenVanDatum)
	{
		var standplaatsPeriode = filter.getStandplaatsPeriode();
		var vanaf = filter.getVanafLocalDate();
		var totEnMet = filter.getTotEnMetLocalDate();

		var verzetClientenDto = new PlanningVerzetClientenDto();
		verzetClientenDto.verzetStandplaatsPeriodeId = standplaatsPeriode.getId();
		var capaciteitVolledigBenutTotEnMetAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN.toString());
		kansberekeningService.resetPreferences();

		Set<LocalDate> afspraakDatums = new HashSet<>();
		afspraakDatums.add(verzettenVanDatum);

		for (var bestaandeAfspraak : afspraken)
		{
			hibernateService.getHibernateSession().flush();

			var uitnodiging = bestaandeAfspraak.getUitnodiging();
			var dossier = uitnodiging.getScreeningRonde().getDossier();

			var voorlopigeOpkomstkans = kansberekeningService.getVoorlopigeOpkomstkans(uitnodiging, standplaatsPeriode, filter.getVerzettenReden());

			var afspraakOptie = baseAfspraakService.getAfspraakOptieBulkVerzetten(dossier, standplaatsPeriode, vanaf, totEnMet, voorlopigeOpkomstkans,
				capaciteitVolledigBenutTotEnMetAantalWerkdagen);

			var annuleerVorigeAfspraak = bestaandeAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) > 0;

			var capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, afspraakOptie.getCapaciteitBlokId());

			var digitaleBerichten = dossier.getLaatsteScreeningRonde().getBerichten();
			var emailAdres = dossier.getClient().getPersoon().getEmailadres();
			var kanEmailVersturen = !digitaleBerichten.isEmpty() && !bulkVerzettenAlleenViaBrief() && StringUtils.isNotBlank(emailAdres);

			var magMailVerzenden = false;

			if (kanEmailVersturen)
			{
				magMailVerzenden = bepaalOfErMailVerzondenMoetWorden(bestaandeAfspraak);
			}

			var nieuweAfspraak = baseAfspraakService.maakAfspraak(uitnodiging.getScreeningRonde(), capaciteitBlok,
				DateUtil.toUtilDate(afspraakOptie.getDatumTijd()), filter.getStandplaatsPeriode(), filter.getVerzettenReden(),
				annuleerVorigeAfspraak, false, true, true, true, account, false);

			var nieuweAfspraakDate = DateUtil.toLocalDate(nieuweAfspraak.getVanaf());
			afspraakDatums.add(nieuweAfspraakDate);

			verzetClientenDto.clientIdSet.add(dossier.getClient().getId());

			verstuurBriefOfMail(nieuweAfspraak, magMailVerzenden);
		}

		baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);

		berichtToSeRestBkService.notificeerScreeningsEenheidVerversenDaglijst(standplaatsPeriode.getScreeningsEenheid(), afspraakDatums);

		var formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");
		var melding = "Bulk verzetting: #" + afspraken.size() + " afspraken, reden: " + filter.getVerzettenReden() + ", vanaf: "
			+ vanaf.format(formatter) + ", tot/met: " + totEnMet.format(formatter);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_VERZET, account, melding, Bevolkingsonderzoek.MAMMA);
	}

	private boolean bulkVerzettenAlleenViaBrief()
	{
		return preferenceService.getBoolean(PreferenceKey.MAMMA_BULK_VERZETTEN_ALLEEN_BRIEF.name(), false);
	}

	private boolean bepaalOfErMailVerzondenMoetWorden(MammaAfspraak bestaandeAfspraak)
	{
		var digitaleContacten = bestaandeAfspraak.getUitnodiging().getScreeningRonde().getBerichten();

		var laatsteDigitaleMailContact = digitaleContacten.stream().filter(d -> d.getDigitaalBerichtTemplateType() == DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING)
			.max(Comparator.comparing(MammaDigitaalClientBericht::getCreatieMoment));

		var brieven = bestaandeAfspraak.getUitnodiging().getScreeningRonde().getBrieven();
		var laatsteAfspraakVerzetBrief = brieven.stream().filter(b -> b.getBriefType() == BriefType.MAMMA_AFSPRAAK_VERZET).max(Comparator.comparing(MammaBrief::getCreatieDatum));

		var creatieMomentDigitaalBericht = laatsteDigitaleMailContact.orElseGet(MammaDigitaalClientBericht::new).getCreatieMoment();

		if (creatieMomentDigitaalBericht == null)
		{
			return false;
		}

		if (laatsteAfspraakVerzetBrief.isPresent())
		{
			var briefCreatieMoment = laatsteAfspraakVerzetBrief.get().getCreatieDatum();

			if (creatieMomentDigitaalBericht.isBefore(DateUtil.toLocalDateTime(briefCreatieMoment)))
			{
				return false;
			}
		}
		var bestaandeAfspraakCreatieMoment = DateUtil.toLocalDateTime(bestaandeAfspraak.getCreatiedatum());
		return !creatieMomentDigitaalBericht.isBefore(bestaandeAfspraakCreatieMoment);
	}

	private void verstuurBriefOfMail(MammaAfspraak nieuweAfspraak, boolean mailVerzenden)
	{
		if (!mailVerzenden)
		{
			baseBriefService.maakBvoBrief(nieuweAfspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET);
		}
		else
		{
			digitaalContactService.sendBevestigAfspraakMail(nieuweAfspraak);
		}
	}
}
