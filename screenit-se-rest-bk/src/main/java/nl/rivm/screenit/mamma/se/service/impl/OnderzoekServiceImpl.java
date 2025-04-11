package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dto.ZorginstellingDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdDto;
import nl.rivm.screenit.mamma.se.dto.actions.MaakDubbeleTijdRedenDto;
import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.actions.SignalerenOpslaanDto;
import nl.rivm.screenit.mamma.se.repository.MammaAfspraakRepository;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.OnderzoekAfrondenService;
import nl.rivm.screenit.mamma.se.service.OnderzoekService;
import nl.rivm.screenit.mamma.se.service.dtomapper.ZorginstellingDtoMapper;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.repository.algemeen.InstellingRepository;
import nl.rivm.screenit.repository.mamma.MammaOnderzoekRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static com.google.common.collect.Range.closed;
import static nl.rivm.screenit.mamma.se.specification.OrganisatieSpecification.heeftSubInstellingVanType;
import static nl.rivm.screenit.mamma.se.specification.OrganisatieSpecification.isZorgInstelling;
import static nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus.BESCHIKBAAR;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftAfgerondeMammografie;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftIlmStatus;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftOnderzoekDoorgevoerd;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftScreeningsEenheid;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class OnderzoekServiceImpl implements OnderzoekService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private OnderzoekAfrondenService onderzoekAfrondenService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Autowired
	private MammaOnderzoekRepository onderzoekRepository;

	@Autowired
	private InstellingRepository instellingRepository;

	@Autowired
	private MammaAfspraakRepository afspraakRepository;

	@Override
	public void opslaan(OnderzoekOpslaanDto action, InstellingGebruiker gebruiker)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), gebruiker);
		var onderzoekSeDto = action.getOnderzoek();

		var onderzoek = afspraak.getOnderzoek();
		onderzoek.setEerderMammogramZorginstelling(
			onderzoekSeDto.getEerderMammogramZorginstellingId() == null ? null : hibernateService.get(ZorgInstelling.class, onderzoekSeDto.getEerderMammogramZorginstellingId()));
		onderzoek.setEerderMammogramJaartal(onderzoekSeDto.getEerderMammogramJaartal());
		onderzoek.setSuboptimaleInsteltechniek(onderzoekSeDto.getSuboptimaleInsteltechniek());
		onderzoek.setRedenFotobespreking(onderzoekSeDto.getRedenFotobespreking());
		onderzoek.setExtraMedewerker(
			onderzoekSeDto.getExtraMedewerkerId() == null ? null : hibernateService.get(InstellingGebruiker.class, onderzoekSeDto.getExtraMedewerkerId()));
		onderzoek.setOpmerkingMbber(onderzoekSeDto.getOpmerkingMbber());
		onderzoek.setOpmerkingVoorRadioloog(onderzoekSeDto.getOpmerkingVoorRadioloog());
		onderzoek.setOperatieRechts(onderzoekSeDto.isOperatieRechts());
		onderzoek.setOperatieLinks(onderzoekSeDto.isOperatieLinks());
		onderzoek.setAmputatie(onderzoekSeDto.getAmputatie());
		onderzoek.setAanvullendeInformatieOperatie(onderzoekSeDto.getAanvullendeInformatieOperatie());
		onderzoek.setHuidscheuring(onderzoekSeDto.isHuidscheuring());
		onderzoek.setStatus(onderzoekSeDto.getStatus());
		onderzoek.setOnderzoekType(onderzoekSeDto.getOnderzoekType() != null ? onderzoekSeDto.getOnderzoekType() : MammaOnderzoekType.MAMMOGRAFIE);

		onderzoek.setOnvolledigOnderzoek(onderzoekSeDto.getOnvolledigOnderzoek());
		onderzoek.setOnderbrokenOnderzoek(onderzoekSeDto.getOnderbrokenOnderzoek());
		onderzoek.setExtraFotosRedenen(onderzoekSeDto.getExtraFotosRedenen());

		onderzoek.setAdviesHuisarts(onderzoekSeDto.getAdviesHuisarts());
	}

	@Override
	public void signalerenOpslaan(SignalerenOpslaanDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		var signalering = action.getSignaleren();
		var onderzoek = afspraak.getOnderzoek();
		onderzoekAfrondenService.maakSignalering(instellingGebruiker, onderzoek, signalering, transactieDatumTijd);

	}

	@Override
	public void maakDubbeleTijd(MaakDubbeleTijdDto action, InstellingGebruiker account, LocalDateTime transactieDatumTijd)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), account);
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();

		if (moetDoelgroepUpdaten(action, dossier))
		{
			dossier.setDoelgroep(action.isDubbeleTijd() ? MammaDoelgroep.DUBBELE_TIJD : MammaDoelgroep.REGULIER);
			maakLoggebeurtenisDoelgroepGewijzigd(afspraak, account, transactieDatumTijd);
			baseKansberekeningService.dossierEventHerzien(dossier);
		}
	}

	@Override
	public void maakDubbeleTijdReden(MaakDubbeleTijdRedenDto action, InstellingGebruiker account)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), account);
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();

		dossier.setDubbeleTijdReden(action.getDubbeleTijdReden());
	}

	@Override
	public Map<Long, Integer> getOnderzochtByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekRepository.readOnderzochtVanSeOpWerkdagToMap(datum, DateUtil.eindDag(datum), seCode);
	}

	@Override
	public Map<Long, Integer> getAfgerondByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekRepository.readOnderzoekStatusCountVanSeOpWerkdagtoMap(datum, DateUtil.eindDag(datum), seCode, MammaOnderzoekStatus.AFGEROND);
	}

	@Override
	public Map<Long, Integer> getOnderbrokenByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekRepository.readOnderzoekStatusCountVanSeOpWerkdagtoMap(datum, DateUtil.eindDag(datum), seCode, MammaOnderzoekStatus.ONDERBROKEN);
	}

	@Override
	public Map<Long, Integer> getOnvolledigByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekRepository.readOnderzoekStatusCountVanSeOpWerkdagtoMap(datum, DateUtil.eindDag(datum), seCode, MammaOnderzoekStatus.ONVOLLEDIG);
	}

	@Override
	public Map<Long, Integer> getAfwijkingenByGebruikerOpDatumVoorSe(Date datum, String seCode)
	{
		return onderzoekRepository.readAfwijkingenVanSeOpWerkdagToMap(datum, DateUtil.eindDag(datum), seCode);
	}

	@Override
	public int getAantalOnderzoekenMetBeelden(LocalDate datum, String seCode)
	{
		var range = closed(datum, datum);
		return (int) afspraakRepository.count(
			heeftScreeningsEenheid(seCode).and((heeftAfgerondeMammografie().and(MammaAfspraakSpecification.valtInDatumPeriode(range)))));
	}

	@Override
	public int getAantalOnderzoekenMetBeeldenBeschikbaarInIms(LocalDate datum, String seCode)
	{
		var range = closed(datum, datum);
		return (int) afspraakRepository.count(heeftAfgerondeMammografie().and(
			heeftScreeningsEenheid(seCode)).and(MammaAfspraakSpecification.valtInDatumPeriode(range)).and(heeftIlmStatus(BESCHIKBAAR)));
	}

	@Override
	public int getAantalDoorgevoerdVanDag(LocalDate datum, String seCode)
	{
		var range = closed(datum, datum);
		return (int) afspraakRepository.count(
			heeftAfgerondeMammografie().and(heeftScreeningsEenheid(seCode)).and(MammaAfspraakSpecification.valtInDatumPeriode(range))
				.and(heeftOnderzoekDoorgevoerd()));
	}

	@Override
	public List<ZorginstellingDto> getBKZorginstellingen()
	{
		var mapper = new ZorginstellingDtoMapper();
		var lijstOrganisatieType = List.of(OrganisatieType.MAMMAPOLI, OrganisatieType.RADIOLOGIEAFDELING);
		return instellingRepository.findWith(isZorgInstelling()
				.and(heeftSubInstellingVanType(lijstOrganisatieType)), q -> q.distinct().all())
			.stream().map(mapper::createZorginstellingDto)
			.toList();
	}

	private void maakLoggebeurtenisDoelgroepGewijzigd(MammaAfspraak afspraak, InstellingGebruiker account, LocalDateTime transactieDatumTijd)
	{
		var contact = new ClientContact();
		var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		contact.setClient(client);
		contact.setInstellingGebruiker(account);
		contact.setDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime()));

		var actie = new ClientContactActie();
		actie.setType(ClientContactActieType.MAMMA_DOELGROEP_WIJZIGEN);
		actie.setContact(contact);
		hibernateService.saveOrUpdateAll(contact, actie);

		var diffFieldToLatestVersion = EntityAuditUtil.getDiffFieldsToLatestVersion(afspraak.getUitnodiging().getScreeningRonde().getDossier(),
			hibernateService.getHibernateSession(), "doelgroep");
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_DOELGROEP_GEWIJZIGD, afspraak.getStandplaatsPeriode().getScreeningsEenheid(), account, client, diffFieldToLatestVersion,
			transactieDatumTijd, Bevolkingsonderzoek.MAMMA);
	}

	private boolean moetDoelgroepUpdaten(MaakDubbeleTijdDto action, MammaDossier dossier)
	{
		return (action.isDubbeleTijd() && dossier.getDoelgroep().equals(MammaDoelgroep.REGULIER)) ||
			(!action.isDubbeleTijd() && dossier.getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD));
	}
}
