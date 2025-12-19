package nl.rivm.screenit.clientportaal.services.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.mamma.MammaAfspraakZoekFilterMapper;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakOptieDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.clientportaal.services.mamma.MammaAfspraakService;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakOptieMetAfstandDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaHuidigeAfspraakDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsPeriodeService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaAfspraakServiceImpl implements MammaAfspraakService
{
	private final MammaAfspraakZoekFilterMapper afspraakZoekFilterMapper;

	private final HibernateService hibernateService;

	private final MammaBaseFactory baseFactory;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaBaseStandplaatsService standplaatsService;

	private final MammaBaseStandplaatsPeriodeService standplaatsPeriodeService;

	@Override
	public List<LocalDate> getAlleDatumsMetBeschikbareAfspraken(Client client, String plaats, String afstand)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var filterVoorOphalenAfspraakOpties = MammaAfspraakWijzigenFilterDto.filterVoorOphalenAfsprakenBinnenPeriode(client, plaats,
			afstand, vandaag,
			maximaalAfspraakDagVoorPlaats(client, plaats, afstand));
		var afspraakOpties = baseAfspraakService.getAfspraakOpties(client, filterVoorOphalenAfspraakOpties);

		return afspraakOpties.stream().map(MammaAfspraakOptieMetAfstandDto::getDatum).sorted().distinct().collect(Collectors.toList());
	}

	private LocalDate maximaalAfspraakDagVoorPlaats(Client client, String plaats, String afstand)
	{
		var laatstMogelijkeAfspraakDatum = baseAfspraakService.laatstMogelijkeAfspraakDatum(client.getMammaDossier());
		var maximumVrijgegevenTotEnMetDag = getMaximaleVrijgegevenTotEnMetDatumBijPlaats(plaats, afstand);

		if (laatstMogelijkeAfspraakDatum != null && laatstMogelijkeAfspraakDatum.isBefore(maximumVrijgegevenTotEnMetDag))
		{
			return laatstMogelijkeAfspraakDatum;
		}
		return maximumVrijgegevenTotEnMetDag;
	}

	private LocalDate getMaximaleVrijgegevenTotEnMetDatumBijPlaats(String plaats, String afstand)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var filterOphalenStandplaatsPerioden = MammaAfspraakWijzigenFilterDto.filterVoorOphalenStandplaatsenViaPlaatsOfAfstand(plaats, afstand, vandaag,
			vandaag.plusYears(2));
		var standplaatsPerioden = standplaatsPeriodeService.getStandplaatsPerioden(filterOphalenStandplaatsPerioden);

		return DateUtil.toLocalDate(standplaatsService.getMaximaleVrijgegevenTotEnMetDatum(standplaatsPerioden));
	}

	@Override
	public MammaAfspraakWijzigenFilterDto toAfspraakFilter(MammaAfspraakZoekFilterDto body, Client client, boolean buitenRegio)
	{
		var filter = afspraakZoekFilterMapper.afspraakZoekFilterToAfspraakWijzigenFilterDto(body, client);
		filter.setVerzettenReden(MammaVerzettenReden.CLIENTEN_PORTAAL);
		filter.setBuitenRegio(buitenRegio);
		return filter;
	}

	@Override
	public MammaAfspraak toAfspraak(MammaAfspraakOptieDto afspraakOptieDto, Client client)
	{
		var capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, afspraakOptieDto.getCapaciteitBlokId());
		var standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class,
			afspraakOptieDto.getStandplaatsPeriodeId());
		var uitnodiging = client.getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging();
		var vanaf = DateUtil.toUtilDate(afspraakOptieDto.getDatumTijd());

		return baseFactory.maakDummyAfspraak(uitnodiging, vanaf, capaciteitBlok, standplaatsPeriode, MammaVerzettenReden.CLIENTEN_PORTAAL);
	}

	@Override
	public MammaAfspraakOptieDto toAfspraakOptieDto(MammaAfspraakOptieMetAfstandDto afspraakOptieMetAfstandDto, Client client)
	{
		var afspraakOptieDto = new MammaAfspraakOptieDto(afspraakOptieMetAfstandDto);
		var standplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			afspraakOptieMetAfstandDto.getStandplaatsPeriodeId());
		var locatie = standplaatsService.getStandplaatsLocatie(standplaatsPeriode.getStandplaatsRonde().getStandplaats(),
			DateUtil.toUtilDate(afspraakOptieMetAfstandDto.getDatum()));

		afspraakOptieDto.setAdres(AdresUtil.getStraatMetHuisnummerVoorStandplaatsLocatie(locatie, false));
		afspraakOptieDto.setPostcode(locatie.getPostcode());
		afspraakOptieDto.setPlaats(locatie.getPlaats());

		var briefKanVerzondenWorden = baseAfspraakService.briefKanNogVerzondenWorden(DateUtil.toUtilDate(afspraakOptieMetAfstandDto.getDatum()));
		afspraakOptieDto.setToonBevestigingsBriefOptie(briefKanVerzondenWorden);

		var smsKanVerzondenWorden = baseAfspraakService.smsKanNogVerzondenWorden(afspraakOptieMetAfstandDto.getDatumTijd());
		afspraakOptieDto.setToonSmsHerinneringOptie(smsKanVerzondenWorden);

		afspraakOptieDto.setClientEmailAdres(client.getPersoon().getEmailadres());
		afspraakOptieDto.setClientMobielNummer(client.getPersoon().getTelefoonnummer1());

		return afspraakOptieDto;
	}

	@Override
	public MammaHuidigeAfspraakDto toHuidigeAfspraakDto(MammaAfspraak huidigeAfspraak)
	{
		var naamStandplaats = huidigeAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam();
		var adresStandplaats = AdresUtil.getAdresVoorStandplaatsLocatie(baseAfspraakService.getMammaStandplaatsLocatieAfspraak(huidigeAfspraak));

		var huidigeAfspraakDto = new MammaHuidigeAfspraakDto();
		huidigeAfspraakDto.setWeergaveAfspraakMoment(DateUtil.getWeergaveDatumClientportaal(DateUtil.toLocalDateTime(huidigeAfspraak.getVanaf())));
		huidigeAfspraakDto.setNaamStandplaats(naamStandplaats);
		huidigeAfspraakDto.setAdresStandplaats(adresStandplaats);

		return huidigeAfspraakDto;
	}
}
