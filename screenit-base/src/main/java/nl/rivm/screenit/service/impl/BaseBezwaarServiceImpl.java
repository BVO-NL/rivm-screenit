package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OnderzoeksresultatenActieType;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.envers.RevisionKenmerkInThreadHolder;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.algemeen.BagAdresRepository;
import nl.rivm.screenit.repository.algemeen.BezwaarBriefRepository;
import nl.rivm.screenit.repository.algemeen.BezwaarMomentRepository;
import nl.rivm.screenit.repository.algemeen.BezwaarRepository;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.algemeen.GbaVraagRepository;
import nl.rivm.screenit.repository.algemeen.OnderzoeksresultatenActieRepository;
import nl.rivm.screenit.repository.algemeen.OverdrachtPersoonsgegevensRepository;
import nl.rivm.screenit.repository.algemeen.PersoonRepository;
import nl.rivm.screenit.repository.cervix.CervixBaseMonsterRepository;
import nl.rivm.screenit.repository.cervix.CervixDossierRepository;
import nl.rivm.screenit.repository.colon.ColonDossierRepository;
import nl.rivm.screenit.repository.mamma.MammaDossierRepository;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekenVerzoekRepository;
import nl.rivm.screenit.service.BaseBezwaarService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseClientContactService;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.cervix.CervixBaseDossierService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.specification.algemeen.BriefSpecification;
import nl.rivm.screenit.specification.algemeen.ClientBriefSpecification;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK;
import static nl.rivm.screenit.model.enums.BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES;
import static nl.topicuszorg.util.collections.CollectionUtils.isNotEmpty;

@Slf4j
@Service
public class BaseBezwaarServiceImpl implements BaseBezwaarService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired(required = false)
	private MammaBaseDossierService mammaBaseDossierService;

	@Autowired(required = false)
	private CervixBaseDossierService cervixBaseDossierService;

	@Autowired(required = false)
	private CervixMailService mailService;

	@Autowired(required = false)
	private CervixBaseMonsterRepository monsterRepository;

	@Autowired(required = false)
	private ColonDossierBaseService colonDossierBaseService;

	@Autowired
	private ApplicationEventPublisher applicationEventPublisher;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private BaseGbaVraagService baseGbaVraagService;

	@Autowired
	private BezwaarBriefRepository bezwaarBriefRepository;

	@Autowired
	private ClientRepository clientRepository;

	@Autowired
	private OnderzoeksresultatenActieRepository onderzoeksresultatenActieRepository;

	@Autowired
	private BezwaarMomentRepository bezwaarMomentRepository;

	@Autowired
	private BezwaarRepository bezwaarRepository;

	@Autowired
	private GbaVraagRepository gbaVraagRepository;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private BaseClientContactService clientContactService;

	@Autowired
	private OverdrachtPersoonsgegevensRepository overdrachtPersoonsgegevensRepository;

	@Autowired
	private MammaDownloadOnderzoekenVerzoekRepository mammaDownloadOnderzoekenVerzoekRepository;

	@Autowired
	private BagAdresRepository bagAdresRepository;

	@Autowired
	private PersoonRepository persoonRepository;

	@Autowired
	private ColonDossierRepository colonDossierRepository;

	@Autowired
	private CervixDossierRepository cervixDossierRepository;

	@Autowired
	private MammaDossierRepository mammaDossierRepository;

	@Autowired
	private BaseOverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	public BezwaarGroupViewWrapper getBezwaarGroupViewWrapperFromList(List<BezwaarGroupViewWrapper> lijstBezwaarGroupViewWrappers, Bevolkingsonderzoek onderzoek)
	{
		var wrapperName = "ALGEMEEN";
		if (onderzoek != null)
		{
			wrapperName = onderzoek.toString();
		}
		for (var groupWrapper : lijstBezwaarGroupViewWrappers)
		{
			if (wrapperName.equals(groupWrapper.getKey()))
			{
				return groupWrapper;
			}
		}

		return getGroupWrapper(onderzoek);
	}

	@Override
	public List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment)
	{
		return getEditBezwaarGroupViewWrappers(client, laatstVoltooideMoment, true, BezwaarType.ALGEMENE_BEZWAAR_TYPES);
	}

	@Override
	public List<BezwaarGroupViewWrapper> getEditBezwaarGroupViewWrappers(Client client, BezwaarMoment laatstVoltooideMoment, boolean checkDossierBezwaar,
		List<BezwaarType> bezwaarTypes)
	{
		var lijstBezwaarViewWrappers = new ArrayList<BezwaarGroupViewWrapper>();
		for (var bezwaar : getBewerkbareBezwaren(client, laatstVoltooideMoment, bezwaarTypes))
		{
			var onderzoek = bezwaar.getBevolkingsonderzoek();
			var groupWrapper = getBezwaarGroupViewWrapperFromList(lijstBezwaarViewWrappers, onderzoek);
			var wrapper = getBezwaarViewWrapper(bezwaar.getType(), BezwaarUtil.isBezwaarActiefVoor(laatstVoltooideMoment, bezwaar.getType(), onderzoek, checkDossierBezwaar),
				onderzoek);
			wrapper.setResourceKey("BezwaarType." + groupWrapper.getKey() + "." + wrapper.getType());
			groupWrapper.getBezwaren().add(wrapper);
			if (!lijstBezwaarViewWrappers.contains(groupWrapper))
			{
				lijstBezwaarViewWrappers.add(groupWrapper);
			}
		}
		return lijstBezwaarViewWrappers;
	}

	@Override
	public List<Bezwaar> getBewerkbareBezwaren(Client client, BezwaarMoment laatstVoltooideMoment, List<BezwaarType> bezwaarTypes)
	{
		return getBewerkbareBezwaren(doelgroepService.totWelkeBevolkingsonderzoekenHoortDezeClient(client), laatstVoltooideMoment, bezwaarTypes);
	}

	@Override
	public List<Bezwaar> getBewerkbareBezwaren(List<Bevolkingsonderzoek> bevolkingsonderzoeken, BezwaarMoment laatstVoltooideMoment, List<BezwaarType> bezwaarTypes)
	{
		var onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		onderzoeken.add(null);
		onderzoeken.addAll(bevolkingsonderzoeken);

		var bewerkbareBezwaren = new ArrayList<Bezwaar>();
		for (var onderzoek : onderzoeken)
		{
			bezwaarTypes.stream()
				.filter(type -> isBezwaarTypeBewerkbaar(type, onderzoek, laatstVoltooideMoment))
				.forEach(type -> bewerkbareBezwaren.add(maakBezwaar(type, onderzoek)));
		}
		return bewerkbareBezwaren;
	}

	private static boolean isBezwaarTypeBewerkbaar(BezwaarType type, Bevolkingsonderzoek onderzoek, BezwaarMoment laatstVoltooideMoment)
	{
		var onderzoekenVanType = Arrays.asList(type.getBevolkingsonderzoeken());
		var hoortBijOnderzoek = onderzoek == null ? onderzoekenVanType.isEmpty() : onderzoekenVanType.contains(onderzoek);

		return hoortBijOnderzoek && (BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS != type || BezwaarUtil.isBezwaarActiefVoor(laatstVoltooideMoment, type, onderzoek, true));
	}

	private static Bezwaar maakBezwaar(BezwaarType type, Bevolkingsonderzoek onderzoek)
	{
		var bezwaar = new Bezwaar();
		bezwaar.setType(type);
		bezwaar.setBevolkingsonderzoek(onderzoek);
		return bezwaar;
	}

	@Override
	public List<Bezwaar> bepaalNieuweBezwaren(BezwaarMoment laatstVoltooideMoment, List<Bezwaar> bewerkbareBezwaren, List<Bezwaar> gekozenBezwaren)
	{

		var behoudenBezwaren = laatstVoltooideMoment == null ? Stream.<Bezwaar> empty()
			: laatstVoltooideMoment.getBezwaren().stream().filter(bezwaar -> !BezwaarUtil.bevatBezwaar(bewerkbareBezwaren, bezwaar));

		var nieuweBezwaren = new ArrayList<Bezwaar>();
		Stream.concat(behoudenBezwaren, gekozenBezwaren.stream())
			.filter(bezwaar -> !BezwaarUtil.bevatBezwaar(nieuweBezwaren, bezwaar))
			.forEach(bezwaar -> nieuweBezwaren.add(maakBezwaar(bezwaar.getType(), bezwaar.getBevolkingsonderzoek())));
		return nieuweBezwaren;
	}

	@Override
	public boolean zijnBezwarenGewijzigd(BezwaarMoment laatstVoltooideMoment, List<Bezwaar> nieuweBezwaren)
	{
		var huidigeBezwaren = laatstVoltooideMoment == null ? List.<Bezwaar> of() : laatstVoltooideMoment.getBezwaren();
		return !naarSleutels(huidigeBezwaren).equals(naarSleutels(nieuweBezwaren));
	}

	private static Set<String> naarSleutels(List<Bezwaar> bezwaren)
	{
		return bezwaren.stream().map(bezwaar -> bezwaar.getBevolkingsonderzoek() + "_" + bezwaar.getType()).collect(Collectors.toSet());
	}

	@Override
	public List<BezwaarGroupViewWrapper> getBezwaarGroupViewWrappers(BezwaarMoment moment, boolean verzoekTotBezwaarTeZien)
	{
		List<BezwaarGroupViewWrapper> lijstBezwaarViewWrappers = new ArrayList<>();
		var bezwaren = moment.getBezwaren();
		for (var bezwaar : bezwaren)
		{
			if (!BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(bezwaar.getType()) || verzoekTotBezwaarTeZien)
			{
				var onderzoek = bezwaar.getBevolkingsonderzoek();
				var groupWrapper = getBezwaarGroupViewWrapperFromList(lijstBezwaarViewWrappers, onderzoek);
				var wrapper = getBezwaarViewWrapper(bezwaar.getType(), Boolean.TRUE, onderzoek);
				wrapper.setResourceKey("BezwaarType." + groupWrapper.getKey() + "." + wrapper.getType());
				groupWrapper.getBezwaren().add(wrapper);
				if (!lijstBezwaarViewWrappers.contains(groupWrapper))
				{
					lijstBezwaarViewWrappers.add(groupWrapper);
				}
			}
		}
		return lijstBezwaarViewWrappers;
	}

	@Override
	@Transactional
	public BezwaarBrief maakBezwaarAanvraag(Client client)
	{
		return maakBezwaarAanvraag(client, false, BriefType.CLIENT_BEZWAAR_AANVRAAG);
	}

	@Override
	@Transactional
	public BezwaarBrief maakBezwaarAanvraag(Client client, boolean zonderHandtekening, BriefType briefType)
	{
		return briefService.maakBezwaarBrief(client, briefType, currentDateSupplier.getDate(), zonderHandtekening);
	}

	@Override
	@Transactional
	public void bezwaarAfronden(BezwaarMoment moment, Account account, List<BezwaarGroupViewWrapper> groupWrappers) throws IllegalStateException
	{
		var bezwaren = bepaalNieuweBezwaren(moment.getClient().getLaatstVoltooideBezwaarMoment(), groupWrappers);
		bezwaren.forEach(bezwaar -> bezwaar.setBezwaarMoment(moment));
		moment.setBezwaren(bezwaren);
		bezwaarAfronden(moment, account);
	}

	@Override
	@Transactional
	public void bezwaarAfronden(BezwaarMoment moment, Account account)
	{
		var nu = currentDateSupplier.getDate();
		var client = moment.getClient();
		bezwarenOngedaanMaken(account, client.getLaatstVoltooideBezwaarMoment(), moment);

		moment.setStatus(AanvraagBriefStatus.VERWERKT);
		moment.setStatusDatum(nu);
		moment.setBezwaarDatum(nu);
		bezwaarMomentRepository.persist(moment);
		bezwaarRepository.persistAll(moment.getBezwaren());
		client.setLaatstVoltooideBezwaarMoment(moment);
		client.getBezwaarMomenten().add(moment);
		clientRepository.persist(client);

		var briefType = bepaalBevestigingBriefTypeVoorAfrondenBezwaar(moment);
		maakBevestigingsbrief(moment, briefType);
		bezwaarAangepastLogging(account, moment);
		bezwarenDoorvoeren(moment);
	}

	@Override
	@Transactional
	public void onderzoeksresultatenVerwijderen(OnderzoeksresultatenActie actie, Account account, List<BezwaarGroupViewWrapper> groupWrappers)
	{
		var bevolkingsonderzoeken = groupWrappers.stream()
			.flatMap(groupWrapper -> groupWrapper.getBezwaren().stream())
			.filter(wrapper -> Boolean.TRUE.equals(wrapper.getActief()) && wrapper.getType() == BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER)
			.map(BezwaarViewWrapper::getBevolkingsonderzoek)
			.toList();
		onderzoeksresultatenVerwijderen(actie.getClient(), actie.getGetekendeBrief(), bevolkingsonderzoeken, account);
	}

	@Override
	@Transactional
	public void onderzoeksresultatenVerwijderen(Client client, UploadDocument getekendeBrief, List<Bevolkingsonderzoek> bevolkingsonderzoeken, Account account)
	{
		for (var bevolkingsonderzoek : bevolkingsonderzoeken)
		{
			var onderzoeksresultatenActie = new OnderzoeksresultatenActie();
			onderzoeksresultatenActie.setMoment(currentDateSupplier.getLocalDateTime());
			onderzoeksresultatenActie.setType(OnderzoeksresultatenActieType.getTypeVoorBevolkingsonderzoek(bevolkingsonderzoek));
			onderzoeksresultatenActie.setGetekendeBrief(getekendeBrief);
			onderzoeksresultatenActie.setClient(client);
			client.getOnderzoeksresultatenActies().add(onderzoeksresultatenActie);

			var brief = briefService.maakBezwaarBrief(client, BriefType.CLIENT_BEZWAAR_BEVESTIGING_VERWIJDERING_DOSSIER, currentDateSupplier.getDate());
			onderzoeksresultatenActie.getBrieven().add(brief);
			brief.setOnderzoeksresultatenActie(onderzoeksresultatenActie);

			onderzoeksresultatenActieRepository.persist(onderzoeksresultatenActie);

			verwerkLeegDossier(client, bevolkingsonderzoek);

			logService.logGebeurtenis(LogGebeurtenis.CLIENT_ONDERZOEKRESULTATEN_ACTIE, account, client, onderzoeksresultatenActie.getType().getOmschrijving());
		}
	}

	@Override
	public boolean isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(BezwaarMoment nieuwBezwaarMoment, BezwaarType bezwaarType)
	{
		var client = nieuwBezwaarMoment.getClient();
		if (client.getBezwaarMomenten().size() == 1)
		{
			return true;
		}

		return client.getBezwaarMomenten()
			.stream()
			.filter(bezwaarMoment -> !bezwaarMoment.getId().equals(nieuwBezwaarMoment.getId())
				&& AanvraagBriefStatus.VERWERKT.equals(bezwaarMoment.getStatus()) && bezwaarMoment.getBezwaarDatum() != null)
			.sorted(Comparator.comparing(BezwaarMoment::getBezwaarDatum))
			.reduce((first, second) -> second)
			.map(bezwaarMoment -> bezwaarMoment.getBezwaren()
				.stream()
				.noneMatch(bezwaar -> bezwaar.getType().equals(bezwaarType)))
			.orElse(true);
	}

	@Override
	public BezwaarGroupViewWrapper getGroupWrapperForClientPortaal(BezwaarMoment laatstVoltooideMoment, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		var groupWrapper = getGroupWrapper(bevolkingsonderzoek);
		voegBezwaarToe(groupWrapper, laatstVoltooideMoment, true, true, BezwaarType.ALGEMENE_BEZWAAR_TYPES);
		return groupWrapper;
	}

	@Override
	@Transactional
	public void bezwarenDoorvoeren(BezwaarMoment moment)
	{
		var client = moment.getClient();
		for (var bezwaar : moment.getBezwaren())
		{
			switch (bezwaar.getType())
			{
			case GEEN_WETENSCHAPPELIJK_ONDERZOEK:
			case GEEN_KWALITEITSWAARBORGING:
				bezwaarWetenSchappelijkOnderzoekEnKwaliteitswaarborging(moment);
				break;
			case GEEN_OPNAME_UIT_BPR:
				bezwaarOpnameUitBrp(client);
				break;
			case GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK:
				if (isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(moment, GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK))
				{
					verwerkBezwaarLichaamsmateriaal(client);
				}
				break;
			case GEEN_SIGNALERING_VERWIJSADVIES:
				if (isBezwaarNieuwVergelekenMetVorigeBezwaarMoment(moment, GEEN_SIGNALERING_VERWIJSADVIES))
				{
					verwerkGeenControleVerwijsAdvies(client);
				}
				break;
			case GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS:
				break;
			default:
			}
		}

		client.setLaatstVoltooideBezwaarMoment(moment);
	}

	@Override
	@Transactional(propagation = Propagation.NEVER, readOnly = true)
	public boolean bezwarenGewijzigd(BezwaarMoment laatsteVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> wrappers, Bevolkingsonderzoek bvo)
	{
		if (bvo == null)
		{
			return bezwarenGewijzigd(laatsteVoltooideBezwaarMoment, wrappers);
		}
		var nieuweBezwaren = bepaalNieuweBezwaren(laatsteVoltooideBezwaarMoment, wrappers).stream()
			.filter(bezwaar -> hoortBijOnderzoekOfAlgemeen(bezwaar, bvo))
			.toList();
		var huidigeBezwaren = laatsteVoltooideBezwaarMoment == null ? List.<Bezwaar> of()
			: laatsteVoltooideBezwaarMoment.getBezwaren().stream().filter(bezwaar -> hoortBijOnderzoekOfAlgemeen(bezwaar, bvo)).toList();
		return !naarSleutels(huidigeBezwaren).equals(naarSleutels(nieuweBezwaren));
	}

	private static boolean hoortBijOnderzoekOfAlgemeen(Bezwaar bezwaar, Bevolkingsonderzoek bvo)
	{
		return bezwaar.getBevolkingsonderzoek() == null || bezwaar.getBevolkingsonderzoek() == bvo;
	}

	private List<Bezwaar> bepaalNieuweBezwaren(BezwaarMoment laatstVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> groupWrappers)
	{
		var heeftBezwaarBRP = isErEenBezwaarMetType(groupWrappers, BezwaarType.GEEN_OPNAME_UIT_BPR);
		var bewerkbareBezwaren = new ArrayList<Bezwaar>();
		var gekozenBezwaren = new ArrayList<Bezwaar>();
		for (var groupWrapper : groupWrappers)
		{

			for (var wrapper : groupWrapper.getBezwaren().stream().filter(wrapper -> !(heeftBezwaarBRP && BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER == wrapper.getType()))
				.toList())
			{
				var bezwaar = maakBezwaar(wrapper.getType(), groupWrapper.getBevolkingsonderzoek());
				bewerkbareBezwaren.add(bezwaar);
				if (Boolean.TRUE.equals(wrapper.getActief()))
				{
					gekozenBezwaren.add(bezwaar);
				}
			}
		}
		return bepaalNieuweBezwaren(laatstVoltooideBezwaarMoment, bewerkbareBezwaren, gekozenBezwaren);
	}

	@Override
	@Transactional(propagation = Propagation.NEVER, readOnly = true)
	public boolean bezwarenGewijzigd(BezwaarMoment laatsteVoltooideBezwaarMoment, List<BezwaarGroupViewWrapper> wrappers)
	{
		return zijnBezwarenGewijzigd(laatsteVoltooideBezwaarMoment, bepaalNieuweBezwaren(laatsteVoltooideBezwaarMoment, wrappers));
	}

	@Override
	public boolean checkBezwaarInLaatsteBezwaarMomentAanwezigIs(Client client, BezwaarType bezwaarType)
	{
		var laatstVoltooideBezwaarMoment = client.getLaatstVoltooideBezwaarMoment();
		if (laatstVoltooideBezwaarMoment == null)
		{
			return false;
		}
		return laatstVoltooideBezwaarMoment
			.getBezwaren().stream()
			.anyMatch(bezwaar -> bezwaar.getType().equals(bezwaarType));
	}

	@Override
	public boolean heeftBezwaarIngediendInAfgelopenAantalDagen(Client client, BezwaarType bezwaarType, Bevolkingsonderzoek bevolkingsonderzoek, int aantalDagen)
	{
		var bezwaarTermijn = currentDateSupplier.getLocalDate().minusDays(aantalDagen);
		for (var bezwaarMoment : client.getBezwaarMomenten())
		{
			if (DateUtil.toLocalDate(bezwaarMoment.getBezwaarDatum()).isAfter(bezwaarTermijn))
			{
				for (var bezwaar : bezwaarMoment.getBezwaren())
				{
					if (bezwaarType.equals(bezwaar.getType()) && bevolkingsonderzoek.equals(bezwaar.getBevolkingsonderzoek()))
					{
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	@Transactional
	public void bezwaarAfrondenVanuitClientPortaal(Client client, List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers)
	{
		var nogNietVerwerkteBezwaarBrief = getLaatsteBezwaarBriefVanTypeVoorClient(client, BriefType.CLIENT_BEZWAAR_AANVRAAG);
		nogNietVerwerkteBezwaarBrief.ifPresent(bezwaarBrief -> baseBriefService.briefTegenhouden(bezwaarBrief, client));

		bezwaarAfronden(haalBezwaarMomentOp(nogNietVerwerkteBezwaarBrief, client), client, bezwaarGroupViewWrappers);
	}

	@Override
	public Optional<BezwaarBrief> getLaatsteBezwaarBriefVanTypeVoorClient(Client client, BriefType briefType)
	{
		return bezwaarBriefRepository.findFirst(
			ClientBriefSpecification.<BezwaarBrief> heeftClient(client)
				.and(BriefSpecification.<BezwaarBrief> isNietVerstuurdVoorAfdrukken().and(BriefSpecification.heeftBriefType(briefType))),
			Sort.by(Sort.Order.desc(Brief_.CREATIE_DATUM)));
	}

	@Override
	public List<BezwaarBrief> getBezwaarBrievenVanClient(Client client)
	{
		return bezwaarBriefRepository.findByClient(client);
	}

	@Override
	public boolean isErEenBezwaarMetType(List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers, BezwaarType type)
	{
		return bezwaarGroupViewWrappers.stream()
			.flatMap(groupWrapper -> groupWrapper.getBezwaren().stream())
			.filter(wrapper -> Boolean.TRUE.equals(wrapper.getActief()))
			.anyMatch(wrapper -> type.equals(wrapper.getType()));
	}

	@Override
	@Transactional
	public void verwijderPersoonsgegevens(Client client)
	{
		verwijderGbaVragen(client);
		clientContactService.verwijderClientContacten(client, Bevolkingsonderzoek.values());
		verwijderOverdrachtPersoonsgegevensLijst(client);
		verwijderAlgemeneBrieven(client);
		verwijderBezwaarBrieven(client);
		verwijderAlleBezwarenBehalveBrp(client);
		verwijderDossiers(client);
		verwijderDocumenten(client);
		projectService.verwijderClientVanProjecten(client);
		logService.verwijderLogRegelsVanClient(client);
		verwijderAdres(client);
		leegNaamClient(client);

		clientRepository.persist(client);
	}

	@Override
	@Transactional
	public void verwijderClient(Client client)
	{
		verwijderBezwaarMomenten(client);
		verwijderPersoon(client);
		clientRepository.delete(client);
	}

	private void verwijderAlleBezwarenBehalveBrp(Client client)
	{
		var inactieveBezwaarMomenten = client.getBezwaarMomenten()
			.stream()
			.filter(bezwaarMoment -> !bezwaarMoment.equals(client.getLaatstVoltooideBezwaarMoment()))
			.toList();

		for (var bezwaarMoment : inactieveBezwaarMomenten)
		{
			verwijderBezwaarMoment(bezwaarMoment);
			client.getBezwaarMomenten().remove(bezwaarMoment);
		}

		var bezwaren = client.getLaatstVoltooideBezwaarMoment().getBezwaren();
		var geenBrpBezwaren = bezwaren.stream().filter(bezwaar -> bezwaar.getType() != BezwaarType.GEEN_OPNAME_UIT_BPR).collect(Collectors.toList());
		bezwaarRepository.deleteAll(geenBrpBezwaren);
		client.getLaatstVoltooideBezwaarMoment().getBezwaren().removeAll(geenBrpBezwaren);
	}

	@Override
	@Transactional
	public void verwijderBezwaarMomenten(Client client)
	{
		var bezwaarMomenten = client.getBezwaarMomenten();
		for (var bezwaarMoment : bezwaarMomenten)
		{
			verwijderBezwaarMoment(bezwaarMoment);
		}
		client.getBezwaarMomenten().clear();
		client.setLaatstVoltooideBezwaarMoment(null);
	}

	@Override
	@Transactional
	public void leegDossiers(Client client)
	{
		if (client.getColonDossier() != null)
		{
			verwerkLeegDossier(client, Bevolkingsonderzoek.COLON);
		}
		if (client.getMammaDossier() != null)
		{
			verwerkLeegDossier(client, Bevolkingsonderzoek.MAMMA);
		}
		if (client.getCervixDossier() != null)
		{
			verwerkLeegDossier(client, Bevolkingsonderzoek.CERVIX);
		}
	}

	private void leegNaamClient(Client client)
	{
		var persoon = client.getPersoon();
		persoon.setVoornaam(null);
		persoon.setAchternaam(null);
		persoon.setPartnerAchternaam(null);
		persoon.setNaamGebruik(null);
		persoon.setGeslacht(null);
		persoon.setAanhef(null);
		persoon.setTussenvoegsel(null);
		persoon.setPartnerTussenvoegsel(null);
		persoonRepository.persist(persoon);
	}

	private void verwijderBezwaarMoment(BezwaarMoment bezwaarMoment)
	{
		if (bezwaarMoment == null)
		{
			return;
		}

		var brief = bezwaarMoment.getBezwaarBrief();
		if (brief != null)
		{
			bezwaarMoment.setBezwaarBrief(null);
			bezwaarMomentRepository.persist(bezwaarMoment);
			uploadDocumentService.delete(brief);
		}

		for (var bezwaarBrief : bezwaarMoment.getBrieven())
		{
			briefService.verwijderBrief(bezwaarBrief);
		}
		bezwaarMoment.getBrieven().clear();
		bezwaarMomentRepository.delete(bezwaarMoment);
	}

	private void verwijderPersoon(Client client)
	{
		var persoon = client.getPersoon();
		persoonRepository.delete(persoon);
		client.setPersoon(null);
	}

	private void verwijderGbaVragen(Client client)
	{
		var gbaVragen = client.getGbaVragen();
		gbaVraagRepository.deleteAll(gbaVragen);
		client.getGbaVragen().clear();
	}

	private void verwijderOverdrachtPersoonsgegevensLijst(Client client)
	{
		var overdrachtGegevensLijst = client.getOverdrachtPersoonsgegevensLijst();
		overdrachtGegevensLijst.forEach(overdrachtPersoonsgegevensService::verwijderDataDump);

		var downloadVerzoeken = new ArrayList<MammaDownloadOnderzoekenVerzoek>();
		for (var overdracht : overdrachtGegevensLijst)
		{
			var downloadVerzoek = overdracht.getMammaDownloadOnderzoekenVerzoek();
			if (downloadVerzoek != null)
			{
				overdracht.setMammaDownloadOnderzoekenVerzoek(null);
				downloadVerzoeken.add(downloadVerzoek);
			}
		}

		overdrachtPersoonsgegevensRepository.deleteAll(overdrachtGegevensLijst);
		client.getOverdrachtPersoonsgegevensLijst().clear();

		for (var downloadVerzoek : downloadVerzoeken)
		{
			if (downloadVerzoek.getZipBestand() != null)
			{
				uploadDocumentService.delete(downloadVerzoek.getZipBestand());
			}
			mammaDownloadOnderzoekenVerzoekRepository.delete(downloadVerzoek);
		}
	}

	private void verwijderAlgemeneBrieven(Client client)
	{
		var algemeneBrieven = client.getAlgemeneBrieven();
		for (var brief : algemeneBrieven)
		{
			baseBriefService.verwijderBrief(brief);
		}
		client.getAlgemeneBrieven().clear();
	}

	private void verwijderBezwaarBrieven(Client client)
	{
		var brieven = bezwaarBriefRepository.findByClient(client);
		for (var brief : brieven)
		{
			baseBriefService.verwijderBrief(brief);
		}
	}

	private void verwijderDossiers(Client client)
	{
		var colonDossier = client.getColonDossier();
		if (colonDossier != null)
		{
			colonDossierRepository.delete(colonDossier);
			client.setColonDossier(null);
		}

		var cervixDossier = client.getCervixDossier();
		if (cervixDossier != null)
		{
			cervixDossierRepository.delete(cervixDossier);
			client.setCervixDossier(null);
		}

		var mammaDossier = client.getMammaDossier();
		if (mammaDossier != null)
		{
			mammaDossierRepository.delete(mammaDossier);
			client.setMammaDossier(null);
		}
	}

	private void verwijderDocumenten(Client client)
	{
		var documenten = client.getDocuments();
		if (documenten == null || documenten.isEmpty())
		{
			return;
		}

		for (var document : documenten)
		{
			uploadDocumentService.delete(document);
		}
		client.getDocuments().clear();
	}

	private void verwijderAdres(Client client)
	{
		var adres = client.getPersoon().getGbaAdres();
		bagAdresRepository.delete(adres);
		client.getPersoon().setGbaAdres(null);
	}

	private BezwaarMoment haalBezwaarMomentOp(Optional<BezwaarBrief> bezwaarBrief, Client client)
	{
		if (bezwaarBrief.isPresent() && bezwaarBrief.get().getBezwaarMoment() != null)
		{
			return bezwaarBrief.get().getBezwaarMoment();
		}
		else
		{
			var moment = new BezwaarMoment();
			moment.setClient(client);
			moment.setManier(ClientContactManier.DIRECT);
			return moment;
		}
	}

	private BezwaarGroupViewWrapper getGroupWrapper(Bevolkingsonderzoek onderzoek)
	{
		var groupWrapper = new BezwaarGroupViewWrapper();
		if (onderzoek == null)
		{
			groupWrapper.setKey("ALGEMEEN");
		}
		else
		{
			groupWrapper.setBevolkingsonderzoek(onderzoek);
			groupWrapper.setKey(onderzoek.name());
		}
		return groupWrapper;
	}

	private void voegBezwaarToe(BezwaarGroupViewWrapper groupWrapper, BezwaarMoment voltooideMoment, boolean clientPortaal, boolean checkDossierBezwaar,
		List<BezwaarType> bezwaarTypes)
	{
		var onderzoek = groupWrapper.getBevolkingsonderzoek();
		bezwaarTypes.forEach(type ->
		{
			if (isBezwaarTypeBewerkbaar(type, onderzoek, voltooideMoment) && (!clientPortaal || !type.getOnzichtbaarOpClientPortaal()))
			{
				var wrapper = getBezwaarViewWrapper(type, BezwaarUtil.isBezwaarActiefVoor(voltooideMoment, type, onderzoek, checkDossierBezwaar), onderzoek);
				wrapper.setResourceKey("BezwaarType." + groupWrapper.getKey() + "." + wrapper.getType());
				groupWrapper.getBezwaren().add(wrapper);
			}
		});
	}

	private BezwaarViewWrapper getBezwaarViewWrapper(BezwaarType type, Boolean actief, Bevolkingsonderzoek bvo)
	{
		var wrapper = new BezwaarViewWrapper();
		wrapper.setType(type);
		wrapper.setActief(actief);
		wrapper.setBevolkingsonderzoek(bvo);
		return wrapper;
	}

	private BriefType bepaalBevestigingBriefTypeVoorAfrondenBezwaar(BezwaarMoment moment)
	{
		if (moment.getBezwaren().stream().anyMatch(b -> b.getType().equals(BezwaarType.GEEN_OPNAME_UIT_BPR)))
		{
			return BriefType.CLIENT_BEZWAAR_BEVESTIGING_VERWIJDERING_ALLES;
		}

		if (moment.getBezwaren().stream().anyMatch(b -> b.getType().equals(BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER)))
		{
			return BriefType.CLIENT_BEZWAAR_BEVESTIGING_VERWIJDERING_DOSSIER;
		}

		return BriefType.CLIENT_BEZWAAR_BEVESTIGING_ALGEMEEN;
	}

	private void maakBevestigingsbrief(BezwaarMoment moment, BriefType briefType)
	{
		var brief = briefService.maakBezwaarBrief(moment.getClient(), briefType, currentDateSupplier.getDate());
		brief.setBezwaarMoment(moment);
		moment.getBrieven().add(brief);
		bezwaarMomentRepository.persist(moment);
	}

	private void bezwaarAangepastLogging(Account account, BezwaarMoment moment)
	{
		String loggingMelding = null;
		Map<BezwaarType, String> bezwarenMap = new HashMap<>();
		var bezwaren = moment.getBezwaren();
		if (isNotEmpty(bezwaren))
		{
			for (var bezwaar : bezwaren)
			{
				String melding = null;
				if (!bezwarenMap.containsKey(bezwaar.getType()))
				{
					melding = bezwaar.getType().getNaam();
					if (bezwaar.getBevolkingsonderzoek() != null)
					{
						melding += "(" + bezwaar.getBevolkingsonderzoek().getAfkorting() + ")";
					}
					bezwarenMap.put(bezwaar.getType(), melding);
				}
				else
				{
					melding = bezwarenMap.get(bezwaar.getType());
					melding = melding.substring(0, melding.length() - 1);
					melding += ", " + bezwaar.getBevolkingsonderzoek().getAfkorting() + ")";
					bezwarenMap.put(bezwaar.getType(), melding);
				}
			}

			loggingMelding = "Volgende bezwaren zijn geldig: ";
			loggingMelding += String.join(", ", bezwarenMap.values());
		}
		else
		{
			loggingMelding = "Er zijn voor deze client geen bezwaren meer actief";
		}

		var logEvent = new LogEvent();
		logEvent.setMelding(loggingMelding);
		logEvent.setLevel(Level.INFO);

		logService.logGebeurtenis(LogGebeurtenis.CLIENT_BEZWAAR_AANGEPAST, logEvent, account, moment.getClient());
	}

	private void bezwarenOngedaanMaken(Account account, BezwaarMoment huidigBezwaar, BezwaarMoment nieuwBezwaar)
	{
		if (nieuwBezwaar != null && huidigBezwaar != null)
		{
			for (var bezwaar : huidigBezwaar.getBezwaren())
			{
				var client = nieuwBezwaar.getClient();
				if (!BezwaarUtil.isBezwaarActiefVoor(nieuwBezwaar, bezwaar.getType(), bezwaar.getBevolkingsonderzoek(), true)
					&& bezwaar.getType() == BezwaarType.GEEN_OPNAME_UIT_BPR)
				{
					baseGbaVraagService.verzoekPlaatsIndicatieBijIntrekkenBezwaarBrp(client, account);
				}
			}
		}
	}

	private void bezwaarOpnameUitBrp(Client client)
	{
		wisPersoonsGegevensVoorMakenBezwaarBrp(client.getPersoon());

		leegDossiers(client);

		client.setGbaStatus(GbaStatus.BEZWAAR);
		clientRepository.persist(client);

		baseGbaVraagService.verzoekVerwijderIndicatieBijBezwaarBrp(client);
	}

	private void wisPersoonsGegevensVoorMakenBezwaarBrp(Persoon persoon)
	{

		persoon.setAkteNummerOverlijden(null);
		persoon.setDatumAangaanPartnerschap(null);
		persoon.setDatumAanvangAdreshouding(null);
		persoon.setDatumOntbindingPartnerschap(null);
		persoon.setDatumVertrokkenUitNederland(null);
		persoon.setDatumVestigingNederland(null);
		persoon.setEmailadres(null);
		persoon.setIndicatieGeheim(null);
		persoon.setMobielnummer(null);
		persoon.setOverlijdensdatum(null);
		persoon.setRegisterGemeenteAkteOverlijden(null);
		persoon.setTelefoonnummer1(null);
		persoon.setTelefoonnummer2(null);
		persoon.setTitel(null);
		persoon.setTitelCode(null);
		persoonRepository.persist(persoon);
	}

	private void bezwaarWetenSchappelijkOnderzoekEnKwaliteitswaarborging(BezwaarMoment moment)
	{
		var projectClient = ProjectUtil.getHuidigeProjectClient(moment.getClient(), currentDateSupplier.getDate(), false);
		if (projectClient != null && Boolean.TRUE.equals(projectClient.getProject().getExcludeerBezwaar()))
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.BEZWAAR, null);
		}
	}

	private void verwerkLeegDossier(Client client, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		RevisionKenmerkInThreadHolder.setKenmerk(RevisionKenmerk.DOSSIERVERWIJDERING_DOOR_BEZWAAR, applicationEventPublisher);
		if (Bevolkingsonderzoek.COLON.equals(bevolkingsonderzoek))
		{
			var dossier = client.getColonDossier();
			colonDossierBaseService.maakDossierLeeg(dossier, false);
		}

		if (Bevolkingsonderzoek.CERVIX.equals(bevolkingsonderzoek))
		{
			var dossier = client.getCervixDossier();
			cervixBaseDossierService.maakDossierLeeg(dossier, false);
		}

		if (Bevolkingsonderzoek.MAMMA.equals(bevolkingsonderzoek))
		{
			var dossier = client.getMammaDossier();
			mammaBaseDossierService.maakDossierLeeg(dossier, false);
		}
	}

	private void verwerkGeenControleVerwijsAdvies(Client client)
	{
		if (mailService != null && monsterRepository != null && client.getCervixDossier().getLaatsteScreeningRonde() != null)
		{
			var laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			monsterRepository.findAllByOntvangstScreeningRondeAndBriefNotNull(laatsteScreeningRonde)
				.forEach(cervixMonster -> mailService.sendBMHKBezwaarControlleVerwijsAdviesMail(cervixMonster));
		}
	}

	private void verwerkBezwaarLichaamsmateriaal(Client client)
	{
		if (mailService != null && client.getCervixDossier().getLaatsteScreeningRonde() != null)
		{
			var laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			monsterRepository.findAllByOntvangstScreeningRondeAndBriefNotNull(laatsteScreeningRonde)
				.forEach(cervixMonster -> mailService.sendBMHKBezwaarLichaamsmateriaalMailAsync(cervixMonster));
		}
	}
}
