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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVerslag_;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag_;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.repository.colon.ColonMdlVerslagRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.colon.ColonScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftColonDossier;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftGeenOntvangenBericht;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftScreeningRondeInMdlVerslag;
import static nl.rivm.screenit.specification.colon.ColonVerslagSpecification.heeftVerslagStatus;
import static org.springframework.data.domain.Sort.Direction.DESC;

@Service
@Slf4j
public class ColonVerwerkVerslagServiceImpl implements ColonVerwerkVerslagService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private BaseVerslagService verslagService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonMdlVerslagRepository mdlVerslagRepository;

	@Autowired
	private LogService logService;

	@Override
	@Transactional
	public void verwerkInDossier(MdlVerslag verslag)
	{
		verslag.setVervolgbeleid(dossierBaseService.getVervolgbeleid(verslag));
		mdlVerslagRepository.save(verslag);

		var screeningRonde = verslag.getScreeningRonde();
		var nu = currentDateSupplier.getDate();
		screeningRonde.setStatusDatum(nu);
		var dossier = screeningRonde.getDossier();

		var laatsteAfspraak = screeningRonde.getLaatsteAfspraak();
		if (laatsteAfspraak != null && laatsteAfspraak.getStatus() == ColonAfspraakStatus.GEPLAND)
		{
			laatsteAfspraak.setStatus(ColonAfspraakStatus.UITGEVOERD);
		}
		if (dossier.getLaatsteScreeningRonde().equals(screeningRonde))
		{
			if (rondeHeeftDefinitiefMdlVervolgbeleid(screeningRonde))
			{
				if (ScreeningRondeStatus.AFGEROND != screeningRonde.getStatus())
				{
					screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
					screeningRonde.setAfgerondReden("definitieve diagnose");
				}
			}
			else if (Boolean.FALSE.equals(dossier.getAangemeld()))
			{
				dossier.setStatus(DossierStatus.ACTIEF);
				dossier.setInactiveerReden(null);
				dossier.setInactiefVanaf(null);
			}
		}
	}

	@Override
	public boolean rondeHeeftDefinitiefMdlVervolgbeleid(ColonScreeningRonde screeningRonde)
	{
		var actueelsteVervolgbeleid = getActueelsteVervolgbeleid(screeningRonde);
		return MdlVervolgbeleid.isDefinitief(actueelsteVervolgbeleid);
	}

	private void bepaalEnSetUitnodigingsinterval(MdlVerslag verslag, ColonDossier dossier)
	{
		dossierBaseService.setVolgendeUitnodigingVoorVerslag(verslag);
		hibernateService.saveOrUpdate(dossier);
	}

	private MdlVervolgbeleid getActueelsteVervolgbeleid(ColonScreeningRonde screeningRonde)
	{
		var actueelsteVerslag = mdlVerslagRepository.findFirst(heeftScreeningRondeInMdlVerslag(screeningRonde).and(heeftVerslagStatus(VerslagStatus.AFGEROND)),
			Sort.by(Sort.Order.desc(MdlVerslag_.DATUM_ONDERZOEK)));
		return actueelsteVerslag.map(mdlVerslag -> dossierBaseService.getVervolgbeleid(mdlVerslag)).orElse(null);
	}

	@Override
	@Transactional
	public void onAfterVerwerkVerslagContent(MdlVerslag verslag)
	{

		var content = verslag.getVerslagContent();
		var coloscopieMedischeObservatie = content.getColoscopieMedischeObservatie();
		if (coloscopieMedischeObservatie != null)
		{
			fixAfbrekenColoscopie(coloscopieMedischeObservatie);
			fixPeriodeVervolgScopieSurveillance(coloscopieMedischeObservatie);
			copyEindconclusieNaarProfiel(coloscopieMedischeObservatie);
			bepaalEnSetUitnodigingsinterval(verslag, verslag.getScreeningRonde().getDossier());
		}
		verslag.setDatumOnderzoek(content.getVerrichting().getAanvangVerrichting());

		converteerVolledigheidWegnameMateriaal(verslag);
	}

	private void copyEindconclusieNaarProfiel(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		var eindconclusie = coloscopieMedischeObservatie.getEindconclusie();
		if (eindconclusie != null && List.of("HRP", "LRP").contains(eindconclusie.getCode()))
		{
			coloscopieMedischeObservatie.setProfiel(verslagService.getDsValue(eindconclusie.getCode(), eindconclusie.getCodeSystem(), "ProfielColoscopie"));
		}
	}

	private void fixPeriodeVervolgScopieSurveillance(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		var definitiefVervolgbeleidVoorBevolkingsonderzoekg = coloscopieMedischeObservatie.getDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		if (definitiefVervolgbeleidVoorBevolkingsonderzoekg != null)
		{
			var periodeVervolgScopie = definitiefVervolgbeleidVoorBevolkingsonderzoekg.getPeriodeVervolgScopie();
			var definitiefVervolgbeleidVoorBevolkingsonderzoek = definitiefVervolgbeleidVoorBevolkingsonderzoekg.getDefinitiefVervolgbeleidVoorBevolkingsonderzoek();
			if (definitiefVervolgbeleidVoorBevolkingsonderzoek != null && periodeVervolgScopie != null
				&& definitiefVervolgbeleidVoorBevolkingsonderzoek.getCode().equals("410410006") 
				&& List.of("6", "12", "13", "14", "15").contains(periodeVervolgScopie.getCode())) 
			{
				definitiefVervolgbeleidVoorBevolkingsonderzoekg.setPeriodeVervolgSurveillance(
					verslagService.getDsValue(periodeVervolgScopie.getCode(), periodeVervolgScopie.getCodeSystem(), "vs_periode_vervolg_surveillance"));
				definitiefVervolgbeleidVoorBevolkingsonderzoekg.setPeriodeVervolgScopie(null);
			}
		}
	}

	@Override
	@Transactional
	public void onAfterVerwerkVerslagContent(PaVerslag verslag)
	{

		var content = verslag.getVerslagContent();
		if (content.getVerrichting() != null)
		{
			verslag.setDatumOnderzoek(content.getVerrichting().getAanvangVerrichting());
			if (verslag.getDatumOnderzoek() == null)
			{
				verslag.setDatumOnderzoek(content.getVerrichting().getEindeVerrichting());
			}
		}
	}

	@Override
	public List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client)
	{
		var spec = heeftColonDossier(client.getColonDossier()).and(heeftVerslagStatus(VerslagStatus.AFGEROND));
		return mdlVerslagRepository.findAll(spec, Sort.by(DESC, ColonVerslag_.DATUM_ONDERZOEK));
	}

	@Override
	public Optional<MdlVerslag> getMdlVerslagUitRonde(ColonScreeningRonde ronde)
	{
		var spec = heeftScreeningRondeInMdlVerslag(ronde).and(heeftVerslagStatus(VerslagStatus.AFGEROND).and(heeftGeenOntvangenBericht()));
		return mdlVerslagRepository.findFirst(spec, Sort.by(Sort.Direction.DESC, MdlVerslag_.DATUM_VERWERKT));
	}

	@Override
	public MdlVerslag maakMdlVerslagVoorAfspraak(ColonIntakeAfspraak afspraak)
	{
		var ronde = afspraak.getScreeningRonde();

		var verslag = new MdlVerslag();
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verslag.setScreeningRonde(ronde);
		verslag.setStatus(VerslagStatus.AFGEROND);
		verslag.setType(VerslagType.MDL);

		var content = new MdlVerslagContent();
		verslag.setVerslagContent(content);
		content.setVerslag(verslag);

		var coloscopieMedischeObservatie = new MdlColoscopieMedischeObservatie();
		content.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);
		coloscopieMedischeObservatie.setVerslagContent(content);

		var definitiefVervolgbeleidVoorBevolkingsonderzoekg = new MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg();
		coloscopieMedischeObservatie.setDefinitiefVervolgbeleidVoorBevolkingsonderzoekg(definitiefVervolgbeleidVoorBevolkingsonderzoekg);
		definitiefVervolgbeleidVoorBevolkingsonderzoekg.setColoscopieMedischeObservatie(coloscopieMedischeObservatie);

		var verrichting = new MdlVerrichting();
		verrichting.setVerslagContent(content);
		content.setVerrichting(verrichting);

		ronde.getVerslagen().add(verslag);
		return verslag;
	}

	@Override
	@Transactional
	public void handmatigMdlVerslagOpslaan(MdlVerslag verslag, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		verslag.setInvoerder(ingelogdeOrganisatieMedewerker);
		verslag.getVerslagContent().setVersie(VerslagGeneratie.getHuidigeGeneratie(VerslagType.MDL));
		verslag.setDatumVerwerkt(currentDateSupplier.getDate());
		verwerkInDossier(verslag);
		onAfterVerwerkVerslagContent(verslag);
		var client = verslag.getScreeningRonde().getDossier().getClient();
		var organisatie = ingelogdeOrganisatieMedewerker.getOrganisatie();
		var melding = "Handmatige invoer eindconclusie en vervolgbeleid.";
		melding += " Datum onderzoek: " + DateUtil.formatShortDate(verslag.getVerslagContent().getVerrichting().getAanvangVerrichting());
		melding += " Coloscopie locatie: " + organisatie.getNaam();
		logService.logGebeurtenis(LogGebeurtenis.MDL_VERSLAG_VAST, ingelogdeOrganisatieMedewerker, client, melding,
			Bevolkingsonderzoek.COLON);
	}

	private void converteerVolledigheidWegnameMateriaal(MdlVerslag mdlVerslag)
	{
		if (mdlVerslag.getVerslagContent().getVersie().ordinal() < VerslagGeneratie.V4.ordinal())
		{
			var inTotoCompleet = verslagService.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");
			var piecemealCompleet = verslagService.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_verwijdering_compleet");
			var incompleet = verslagService.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_verwijdering_compleet");

			var inToto = verslagService.getDsValue("255619001", "2.16.840.1.113883.6.96", "vs_method_of_excision");
			var piecemeal = verslagService.getDsValue("2", "2.16.840.1.113883.2.4.3.36.77.5.35", "vs_method_of_excision");
			var radicaal = verslagService.getDsValue("255612005", "2.16.840.1.113883.6.96", "vs_extent");
			var irradicaal = verslagService.getDsValue("255599008", "2.16.840.1.113883.6.96", "vs_extent");
			for (var mdlLaesiecoloscopiecentrum : mdlVerslag.getVerslagContent().getLaesiecoloscopiecentrum())
			{
				var poliep = mdlLaesiecoloscopiecentrum.getPoliep();
				var volledigheidWegnameMateriaal = poliep.getVolledigheidWegnameMateriaal();
				if (volledigheidWegnameMateriaal != null)
				{
					if (volledigheidWegnameMateriaal.equals(inTotoCompleet))
					{
						poliep.setMethodeVanVerwijderen(inToto);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(piecemealCompleet))
					{
						poliep.setMethodeVanVerwijderen(piecemeal);
						poliep.setResultaatVerwijdering(radicaal);
					}
					else if (volledigheidWegnameMateriaal.equals(incompleet))
					{
						poliep.setResultaatVerwijdering(irradicaal);
					}
					poliep.setVolledigheidWegnameMateriaal(null);
				}
			}
		}
	}

	private void fixAfbrekenColoscopie(MdlColoscopieMedischeObservatie coloscopieMedischeObservatie)
	{
		if (coloscopieMedischeObservatie.getRedenAfbrekingColoscopie() != null)
		{
			List<DSValue> oudeWaarden = new ArrayList<>();
			for (var afbrekenColoscopie : coloscopieMedischeObservatie.getRedenAfbrekingColoscopie())
			{
				if (afbrekenColoscopie.getCode().equals("6") || afbrekenColoscopie.getCode().equals("7"))
				{
					oudeWaarden.add(afbrekenColoscopie);
				}
			}
			if (!oudeWaarden.isEmpty())
			{
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().removeAll(oudeWaarden);
				coloscopieMedischeObservatie.getRedenAfbrekingColoscopie().add(verslagService.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
			}

		}
		var redenCoecumNietBereikt = coloscopieMedischeObservatie.getRedenCoecumNietBereikt();
		if (redenCoecumNietBereikt != null && (redenCoecumNietBereikt.getCode().equals("6") || redenCoecumNietBereikt.getCode().equals("7")))
		{
			coloscopieMedischeObservatie.setRedenCoecumNietBereikt(verslagService.getDsValue("12", "2.16.840.1.113883.2.4.3.36.77.5.37", "vs_afbreken_coloscopie"));
		}
	}

	@Override
	public ColonScreeningRonde getValideScreeningsRonde(Client client, Verslag oudeVersieVerslag, Date onderzoeksdatum)
	{
		var dossier = client.getColonDossier();
		ColonScreeningRonde rondeVoorVerslag = null;
		if (oudeVersieVerslag instanceof ColonVerslag<?> verslag)
		{

			rondeVoorVerslag = verslag.getScreeningRonde();
		}
		if (rondeVoorVerslag == null)
		{
			var screeningRondes = new ArrayList<>(dossier.getScreeningRondes());
			Collections.sort(screeningRondes, new PropertyComparator<>("creatieDatum", false, false));

			var heeftOngunstigeUitslagOuderDanOnderzoeksdatum = onderzoeksdatum == null
				|| screeningRondes.stream().anyMatch(r -> isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(r, onderzoeksdatum));

			if (heeftOngunstigeUitslagOuderDanOnderzoeksdatum)
			{
				rondeVoorVerslag = screeningRondes.stream().filter(r -> ColonScreeningRondeUtil.getEersteOngunstigeFitRegistratie(r) != null || r.getOpenUitnodiging() != null)
					.findFirst()
					.orElse(null);
			}
		}
		return rondeVoorVerslag;
	}

	private boolean isOngunstigeUitslagVerwerktVoorOnderzoeksdatum(ColonScreeningRonde ronde, Date onderzoeksdatum)
	{
		var eersteOngunstigeTest = ColonScreeningRondeUtil.getEersteOngunstigeFitRegistratie(ronde);
		return eersteOngunstigeTest != null && eersteOngunstigeTest.getVerwerkingsDatum().compareTo(onderzoeksdatum) < 0;
	}
}
