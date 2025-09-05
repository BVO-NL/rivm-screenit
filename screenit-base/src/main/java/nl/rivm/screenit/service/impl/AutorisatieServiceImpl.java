package nl.rivm.screenit.service.impl;

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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.OrganisatieMedewerkerRolRepository;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AutorisatieServiceImpl implements AutorisatieService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ScopeService scopeService;

	@Autowired
	@Qualifier(value = "testModus")
	private Boolean testModus;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private OrganisatieMedewerkerRolRepository organisatieMedewerkerRolRepository;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean mustChangePassword(OrganisatieMedewerker organisatieMedewerker)
	{
		boolean mustChange = false;
		Medewerker medewerker = organisatieMedewerker.getMedewerker();
		if (!InlogMethode.UZIPAS.equals(medewerker.getInlogMethode()))
		{
			if (medewerker.getLaatsteKeerWachtwoordGewijzigd() == null)
			{

				medewerker.setLaatsteKeerWachtwoordGewijzigd(currentDateSupplier.getDate());
				medewerker.setWachtwoordVerlooptWaarschuwingVerzonden(false);
				hibernateService.saveOrUpdate(medewerker);
			}
			else
			{
				Integer aantalDagen = preferenceService.getInteger(PreferenceKey.DAGEN_WACHTWOORD_GELDIG.name());
				if (aantalDagen != null && aantalDagen > 0 && getVerschilInDagen(medewerker.getLaatsteKeerWachtwoordGewijzigd()) >= aantalDagen)
				{

					mustChange = true;
				}
			}
		}
		return mustChange;
	}

	private int getVerschilInDagen(Date laatsteKeerWachtwoordGewijzigd)
	{
		long verschil = 0;

		verschil = currentDateSupplier.getDate().getTime() - laatsteKeerWachtwoordGewijzigd.getTime();

		verschil = verschil / 1000 / 60 / 60 / 24;

		return Long.valueOf(verschil).intValue();
	}

	@Override
	public Actie getActieVoorMedewerker(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Medewerker currentSelectedMedewerker, Recht... rechten)
	{

		List<Permissie> permissies = getPermissies(ingelogdeOrganisatieMedewerker, null, rechten);

		List<Actie> acties = bepaalActies(ingelogdeOrganisatieMedewerker, currentSelectedMedewerker, permissies);

		return getHoogsteActie(acties);
	}

	@Override
	public Actie getActieVoorOrganisatie(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Organisatie currentSelectedOrganisatie, Recht... rechten)
	{

		List<Permissie> permissies = getPermissies(ingelogdeOrganisatieMedewerker, null, rechten);

		List<Actie> acties = bepaalActies(ingelogdeOrganisatieMedewerker, currentSelectedOrganisatie, permissies);

		return getHoogsteActie(acties);
	}

	private List<Permissie> getPermissies(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Actie minimumActie, Recht... rechten)
	{
		List<OrganisatieMedewerkerRol> rollen = ingelogdeOrganisatieMedewerker.getRollen();
		return rollen
			.stream()
			.filter(OrganisatieMedewerkerRol::isRolActief)
			.map(OrganisatieMedewerkerRol::getRol)
			.flatMap(rol -> rol.getPermissies().stream())
			.filter(permissie -> magPermissieToevoegen(permissie, minimumActie, rechten)).toList();
	}

	private boolean magPermissieToevoegen(Permissie permissie, Actie minimumActie, Recht... rechten)
	{
		var add = false;
		var permissieValid = !Boolean.FALSE.equals(permissie.getActief()) && (minimumActie == null || minimumActie.getNiveau() <= permissie.getActie().getNiveau());

		for (Recht recht : rechten)
		{
			if ((Boolean.TRUE.equals(testModus) || !Recht.TESTEN.equals(recht)) && permissie.getRecht().equals(recht))
			{
				add = true;
				break;
			}
		}
		return add && permissieValid;
	}

	private List<Actie> bepaalActies(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Medewerker currentSelectedMedewerker, List<Permissie> permissies)
	{
		List<Actie> acties = new ArrayList<Actie>();

		for (Permissie permissie : permissies)
		{
			fillRechtTypes(ingelogdeOrganisatieMedewerker, currentSelectedMedewerker, acties, permissie);
		}
		return acties;
	}

	private List<Actie> bepaalActies(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Organisatie currentSelectedOrganisatie, List<Permissie> permissies)
	{
		List<Actie> acties = new ArrayList<Actie>();

		for (Permissie permissie : permissies)
		{
			fillRechtTypes(ingelogdeOrganisatieMedewerker, currentSelectedOrganisatie, acties, permissie);
		}
		return acties;
	}

	private void fillRechtTypes(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Medewerker currentSelectedMedewerker, List<Actie> acties, Permissie permissie)
	{
		if (currentSelectedMedewerker == null)
		{
			acties.add(permissie.getActie());
		}
		else if (currentSelectedMedewerker.getId() == null)
		{
			acties.add(permissie.getActie());
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.LANDELIJK))
		{

			acties.add(permissie.getActie());
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.ORGANISATIE) || permissie.getToegangLevel().equals(ToegangLevel.REGIO))
		{
			bepaalActiesMedewerker(ingelogdeOrganisatieMedewerker, currentSelectedMedewerker, acties, permissie);
		}
		else if (permissie.getToegangLevel().equals(ToegangLevel.EIGEN) && currentSelectedMedewerker.getId().equals(ingelogdeOrganisatieMedewerker.getMedewerker().getId()))
		{
			acties.add(permissie.getActie());
		}
	}

	private void fillRechtTypes(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Organisatie currentSelectedOrganisatie, List<Actie> acties, Permissie permissie)
	{
		if (currentSelectedOrganisatie == null)
		{
			acties.add(permissie.getActie());
		}
		else if (currentSelectedOrganisatie.getId() == null)
		{
			acties.add(permissie.getActie());
		}
		else
		{
			ToegangLevel toegangLevel = permissie.getToegangLevel();
			switch (toegangLevel)
			{
			case REGIO:
				switch (currentSelectedOrganisatie.getOrganisatieType())
				{
				case SCREENINGSORGANISATIE:
					if (currentSelectedOrganisatie.getId().equals(ingelogdeOrganisatieMedewerker.getOrganisatie().getId()))
					{
						acties.add(permissie.getActie());
					}
					break;
				case PA_LABORATORIUM:
					for (ColoscopieLocatie locatie : ((PaLaboratorium) currentSelectedOrganisatie).getColoscopielocaties())
					{
						if (valtBinnenRegio(ingelogdeOrganisatieMedewerker, locatie))
						{
							acties.add(permissie.getActie());
							break;
						}
					}
					break;
				case CENTRALE_EENHEID:
					if (currentSelectedOrganisatie.getRegio() == null || 
						currentSelectedOrganisatie.getRegio().equals(ingelogdeOrganisatieMedewerker.getOrganisatie()))
					{
						acties.add(permissie.getActie());
					}
					break;
				case BEOORDELINGSEENHEID:
					if (currentSelectedOrganisatie.getParent() == null || 
						currentSelectedOrganisatie.getParent().getRegio() == null || 
						currentSelectedOrganisatie.getParent().getRegio().equals(ingelogdeOrganisatieMedewerker.getOrganisatie()))
					{
						acties.add(permissie.getActie());
					}
					break;
				default:
					if (valtBinnenRegio(ingelogdeOrganisatieMedewerker, currentSelectedOrganisatie))
					{
						acties.add(permissie.getActie());
					}
					break;
				}
				break;
			case ORGANISATIE:
				if (currentSelectedOrganisatie.getId().equals(ingelogdeOrganisatieMedewerker.getOrganisatie().getId()))
				{
					acties.add(permissie.getActie());
				}
				break;
			default:

				acties.add(permissie.getActie());
				break;
			}
		}
	}

	private boolean valtBinnenRegio(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Organisatie currentSelectedOrganisatie)
	{
		boolean valtBinnenRegio = false;

		Organisatie parentSelected = currentSelectedOrganisatie;
		Organisatie parentIngelogdeOrganisatie = ingelogdeOrganisatieMedewerker.getOrganisatie();

		if (parentSelected != null && parentSelected.getId().equals(parentIngelogdeOrganisatie.getId()))
		{
			valtBinnenRegio = true;
		}
		if (!valtBinnenRegio)
		{
			while (parentSelected != null && parentSelected.getOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
			{
				parentSelected = parentSelected.getParent();
			}
			while (parentIngelogdeOrganisatie != null && parentIngelogdeOrganisatie.getOrganisatieType() != OrganisatieType.SCREENINGSORGANISATIE)
			{
				parentIngelogdeOrganisatie = parentIngelogdeOrganisatie.getParent();
			}

			if (parentSelected != null && parentIngelogdeOrganisatie != null && parentSelected.getId().equals(parentIngelogdeOrganisatie.getId()))
			{
				valtBinnenRegio = true;
			}
		}
		return valtBinnenRegio;
	}

	private void bepaalActiesMedewerker(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, Medewerker currentSelectedMedewerker, List<Actie> acties, Permissie permissie)
	{

		if (CollectionUtils.isNotEmpty(currentSelectedMedewerker.getOrganisatieMedewerkers()))
		{
			for (OrganisatieMedewerker organisatieMedewerker : currentSelectedMedewerker.getOrganisatieMedewerkers())
			{
				Organisatie organisatie = organisatieMedewerker.getOrganisatie();
				if (organisatie.getId().equals(organisatieMedewerker.getOrganisatie().getId()))
				{
					acties.add(permissie.getActie());
				}
			}
		}
	}

	private Actie getHoogsteActie(List<Actie> acties)
	{
		Actie result = null;
		for (Actie actie : acties)
		{
			if (result == null)
			{
				result = actie;
			}
			else if (actie.getNiveau() > result.getNiveau())
			{
				result = actie;
			}
		}
		return result;
	}

	@Override
	public List<OrganisatieType> getOrganisatieTypes(OrganisatieMedewerker organisatieMedewerker, boolean checkBvo)
	{
		return getOrganisatieTypes(organisatieMedewerker, Actie.INZIEN, checkBvo);
	}

	@Override
	public List<OrganisatieType> getOrganisatieTypes(OrganisatieMedewerker organisatieMedewerker, Actie minimumActie, boolean checkBvo)
	{
		List<OrganisatieType> organisatieTypes = new ArrayList<>();

		for (OrganisatieType organisatieType : OrganisatieType.values())
		{

			ToegangLevel level = getToegangLevel(organisatieMedewerker, minimumActie, checkBvo, organisatieType.getRecht());
			if (level != null)
			{
				organisatieTypes.add(organisatieType);
			}
		}
		return organisatieTypes;
	}

	@Override
	public ToegangLevel getToegangLevel(OrganisatieMedewerker organisatieMedewerker, Actie minimumActie, boolean checkBvo, Recht... rechten)
	{
		Constraint constraint = new Constraint();
		constraint.setActie(minimumActie);
		constraint.setBevolkingsonderzoek(organisatieMedewerker.getBevolkingsonderzoeken());
		ToegangLevel hoogsteOveralToegangLevel = null;
		for (Recht recht : rechten)
		{
			constraint.setRecht(recht);

			ToegangLevel hoogsteToegangLevel = scopeService.getHoogsteToegangLevel(organisatieMedewerker, constraint, checkBvo);
			if (hoogsteOveralToegangLevel == null)
			{
				hoogsteOveralToegangLevel = hoogsteToegangLevel;
			}
			if (hoogsteToegangLevel != null)
			{
				if (hoogsteOveralToegangLevel != null && hoogsteToegangLevel.getNiveau() > hoogsteOveralToegangLevel.getNiveau())
				{
					hoogsteOveralToegangLevel = hoogsteToegangLevel;
				}
				if (hoogsteToegangLevel.equals(ToegangLevel.LANDELIJK))
				{

					break;
				}
			}
		}
		return hoogsteOveralToegangLevel;
	}

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken(OrganisatieMedewerker organisatieMedewerker)
	{
		List<Bevolkingsonderzoek> onderzoeken = new ArrayList<Bevolkingsonderzoek>();
		for (OrganisatieMedewerkerRol rol : organisatieMedewerker.getRollen())
		{
			if (!rol.isRolActief())
			{
				continue;
			}
			for (Bevolkingsonderzoek onderzoek : rol.getBevolkingsonderzoeken())
			{
				if (!onderzoeken.contains(onderzoek))
				{
					onderzoeken.add(onderzoek);
				}
			}
		}
		return onderzoeken;
	}

	@Override
	public List<Recht> getRechtWithBevolkingsonderzoek(List<Bevolkingsonderzoek> onderzoeken)
	{
		List<Recht> rechten = new ArrayList<Recht>();
		for (Recht recht : Recht.values())
		{
			for (Bevolkingsonderzoek bevolkingsonderzoek : recht.getBevolkingsonderzoeken())
			{
				if (onderzoeken != null && onderzoeken.contains(bevolkingsonderzoek))
				{
					rechten.add(recht);
					break;
				}
				else if (onderzoeken == null)
				{
					rechten.add(recht);
				}
			}
		}
		return rechten;
	}

	@Override
	@Transactional
	public void inactiveerRolKoppeling(OrganisatieMedewerkerRol rolKoppeling)
	{
		rolKoppeling.setActief(false);
		rolKoppeling.setEindDatum(currentDateSupplier.getDate());
		organisatieMedewerkerRolRepository.save(rolKoppeling);
	}
}
