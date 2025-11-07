package nl.rivm.screenit.util;

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

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Huisarts;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.weergave.persoon.NaamWeergaveHelper;

import org.apache.commons.lang3.StringUtils;

import com.google.common.base.Strings;

public abstract class NaamUtil
{
	public static String getNaamMedewerker(Medewerker medewerker)
	{
		if (medewerker == null)
		{
			return null;
		}

		StringBuilder medewerkerNaam = new StringBuilder();
		if (StringUtils.isNotBlank(medewerker.getVoorletters()))
		{
			medewerkerNaam.append(NaamWeergaveHelper.standaardiseerVoorletters(medewerker.getVoorletters()));
			medewerkerNaam.append(" ");
		}
		medewerkerNaam.append(getTussenvoegselEnAchternaam(medewerker));
		return medewerkerNaam.toString();
	}

	public static List<String> getNamenMedewerkers(List<Medewerker> medewerkers)
	{
		return medewerkers.stream().map(Medewerker::getNaamVolledig).collect(Collectors.toList());
	}

	public static String getTussenvoegselEnAchternaam(Medewerker medewerker)
	{
		if (medewerker == null)
		{
			return null;
		}

		StringBuilder medewerkerNaam = new StringBuilder();
		if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
		{
			medewerkerNaam.append(medewerker.getTussenvoegsel());
			medewerkerNaam.append(" ");

		}
		medewerkerNaam.append(medewerker.getAchternaam());
		return medewerkerNaam.toString();
	}

	public static String getTussenvoegselEnEigenAchternaam(Persoon persoon)
	{
		if (persoon == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();
		if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			naam.append(persoon.getTussenvoegsel());
			naam.append(" ");

		}
		naam.append(persoon.getAchternaam());
		return naam.toString();
	}

	public static String getNaamClientMetBsn(Client client, boolean withClosure)
	{
		String naamClient = titelVoorlettersTussenvoegselEnAanspreekAchternaam(client);
		if (Strings.isNullOrEmpty(naamClient))
		{
			return null;
		}
		StringBuilder naam = new StringBuilder();
		naam.append(naamClient);
		Persoon persoon = client.getPersoon();
		if (!Strings.isNullOrEmpty(persoon.getBsn()))
		{
			naam.append(" (Bsn: ");
			naam.append(ng01BsnSafeCheck(persoon.getBsn()));
			if (!withClosure)
			{
				naam.append(" ");
			}
		}
		if (!Strings.isNullOrEmpty(persoon.getBsn()) && withClosure)
		{
			naam.append(")");
		}
		return naam.toString();
	}

	public static String ng01BsnSafeCheck(String bsn)
	{
		if (bsn.length() == 12)
		{
			return bsn.substring(3, bsn.length());
		}
		return bsn;
	}

	public static String getNaamClientMetBsnMetGeboortedatum(Client client)
	{
		StringBuilder naam = new StringBuilder();
		String naamClient = getNaamClientMetBsn(client, false);
		if (Strings.isNullOrEmpty(naamClient))
		{
			return null;
		}
		Persoon persoon = client.getPersoon();
		naam.append(naamClient);
		if (persoon.getGeboortedatum() != null)
		{
			if (Strings.isNullOrEmpty(persoon.getBsn()))
			{
				naam.append(" (");
			}
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			naam.append("Geboortedatum: ");
			naam.append(format.format(persoon.getGeboortedatum()));
			naam.append(")");
		}
		else if (!Strings.isNullOrEmpty(persoon.getBsn()))
		{
			naam.append(")");
		}
		return naam.toString();
	}

	public static String titelVoorlettersTussenvoegselEnAanspreekAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();

		Persoon persoon = client.getPersoon();
		if (!Strings.isNullOrEmpty(persoon.getTitel()))
		{
			naam.append(persoon.getTitel());
			naam.append(" ");
		}

		naam.append(voorlettersTussenvoegselEnAanspreekAchternaam(client));
		return naam.toString();
	}

	public static String voorlettersTussenvoegselEnAanspreekAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}

		StringBuilder naam = new StringBuilder();
		String voorletters = getVoorlettersClient(client);
		naam.append(voorletters);
		if (!Strings.isNullOrEmpty(voorletters))
		{
			naam.append(" ");
		}

		naam.append(getAanspreekTussenvoegselEnAchternaam(client));

		return naam.toString();
	}

	public static String getAanspreekTussenvoegselEnAchternaam(Client client)
	{
		if (client == null)
		{
			return null;
		}
		var persoon = client.getPersoon();
		var volledigeNaam = StringUtils.trim(getTussenvoegsel(persoon));
		if (volledigeNaam.length() > 0)
		{
			volledigeNaam += " ";
		}
		volledigeNaam += getAanspreekNaamZonderTussenvoegsel(persoon);
		return volledigeNaam;
	}

	private static String getAanspreekNaamZonderTussenvoegsel(Persoon persoon)
	{
		var volledigeNaam = "";
		var naamGebruik = persoon.getNaamGebruik();
		if (naamGebruik == NaamGebruik.EIGEN || naamGebruik == NaamGebruik.EIGEN_PARTNER)
		{
			if (StringUtils.isNotBlank(persoon.getAchternaam()))
			{
				volledigeNaam += persoon.getAchternaam();
			}
			if (naamGebruik == NaamGebruik.EIGEN_PARTNER)
			{
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
				{
					volledigeNaam += " - ";
				}
				volledigeNaam += getPartnernaam(persoon);
			}
		}
		else if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER || naamGebruik == NaamGebruik.PARTNER_EIGEN)
		{
			if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				volledigeNaam += persoon.getPartnerAchternaam();
			}
			if (naamGebruik == null || naamGebruik == NaamGebruik.PARTNER_EIGEN)
			{
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getAchternaam()))
				{
					volledigeNaam += " - ";
				}
				if (StringUtils.isNotBlank(volledigeNaam) && StringUtils.isNotBlank(persoon.getTussenvoegsel()))
				{
					volledigeNaam += persoon.getTussenvoegsel() + " ";
				}
				if (StringUtils.isNotBlank(persoon.getAchternaam()))
				{
					volledigeNaam += persoon.getAchternaam();
				}
			}
		}
		return volledigeNaam;
	}

	private static String getEigennaam(Persoon persoon)
	{
		var eigennaam = "";
		if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			eigennaam += persoon.getTussenvoegsel() + " ";
		}
		if (StringUtils.isNotBlank(persoon.getAchternaam()))
		{
			eigennaam += persoon.getAchternaam();
		}
		return eigennaam;
	}

	private static String getPartnernaam(Persoon persoon)
	{
		var partnernaam = "";
		if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
		{
			partnernaam += persoon.getPartnerTussenvoegsel() + " ";
		}
		if (StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
		{
			partnernaam += persoon.getPartnerAchternaam();
		}
		return partnernaam;
	}

	public static String getVolledigeAchternaamVoorlettersTussenvoegsel(Client client)
	{
		if (client == null)
		{
			return null;
		}
		var persoon = client.getPersoon();
		var volledigeNaam = getAanspreekNaamZonderTussenvoegsel(persoon);
		volledigeNaam += ", " + getVoorlettersClient(client);
		volledigeNaam += getTussenvoegsel(persoon);

		return volledigeNaam;
	}

	private static String getTussenvoegsel(Persoon persoon)
	{
		var tussenvoegels = "";
		var naamGebruik = persoon.getNaamGebruik();
		if (((NaamGebruik.EIGEN.equals(naamGebruik) || NaamGebruik.EIGEN_PARTNER.equals(naamGebruik))
			|| (NaamGebruik.PARTNER_EIGEN.equals(naamGebruik) && StringUtils.isBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isBlank(persoon.getPartnerAchternaam())))
			&& StringUtils.isNotBlank(persoon.getTussenvoegsel()))
		{
			tussenvoegels += " " + persoon.getTussenvoegsel();
		}
		else if ((NaamGebruik.PARTNER.equals(naamGebruik) || NaamGebruik.PARTNER_EIGEN.equals(naamGebruik)) && StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()))
		{
			tussenvoegels += " " + persoon.getPartnerTussenvoegsel();
		}
		else if (naamGebruik == null)
		{
			if (StringUtils.isNotBlank(persoon.getPartnerTussenvoegsel()) && StringUtils.isNotBlank(persoon.getPartnerAchternaam()))
			{
				tussenvoegels += " " + persoon.getPartnerTussenvoegsel();
			}
			else if (StringUtils.isNotBlank(persoon.getTussenvoegsel()))
			{
				tussenvoegels += " " + persoon.getTussenvoegsel();
			}
		}
		return tussenvoegels;
	}

	public static String getVoorlettersClient(Client client)
	{
		if (client == null)
		{
			return null;
		}

		var voorletters = new StringBuilder();

		if (!Strings.isNullOrEmpty(client.getPersoon().getVoornaam()))
		{
			String[] voornamen = client.getPersoon().getVoornaam().split(" ");
			for (String voornaam : voornamen)
			{
				if (!Strings.isNullOrEmpty(voornaam))
				{
					voorletters.append(voornaam.toUpperCase().charAt(0));

					if (voornaam.toUpperCase().startsWith("IJ"))
					{
						voorletters.append(voornaam.toUpperCase().charAt(1));
					}

					voorletters.append(".");
				}
			}
		}
		return voorletters.toString();
	}

	public static String getNaamHuisarts(Huisarts huisarts)
	{
		var naam = new StringBuilder();
		if (huisarts == null)
		{
			return naam.toString();
		}

		if (StringUtils.isNotBlank(huisarts.getVoorletters()))
		{
			naam.append(NaamWeergaveHelper.standaardiseerVoorletters(huisarts.getVoorletters()));
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(huisarts.getTussenvoegels()))
		{
			naam.append(huisarts.getTussenvoegels());
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(huisarts.getAchternaam()))
		{
			naam.append(huisarts.getAchternaam());
		}

		return naam.toString();
	}

	public static String getNaamHuisarts(CervixHuisarts cervixHuisarts)
	{
		var naam = new StringBuilder();
		if (cervixHuisarts == null)
		{
			return naam.toString();
		}
		var medewerker = cervixHuisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		if (StringUtils.isNotBlank(medewerker.getVoorletters()))
		{
			naam.append(NaamWeergaveHelper.standaardiseerVoorletters(medewerker.getVoorletters()));
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
		{
			naam.append(medewerker.getTussenvoegsel());
			naam.append(" ");
		}
		if (StringUtils.isNotBlank(medewerker.getAchternaam()))
		{
			naam.append(medewerker.getAchternaam());
		}

		return naam.toString();
	}

	public static String getHuisartsWeergavenaamMetPlaats(Huisarts huisarts)
	{
		if (huisarts == null || StringUtils.isBlank(huisarts.getWeergavenaam()))
		{
			return "";
		}
		var weergavenaamMetPlaats = new StringBuilder();
		var adres = huisarts.getAdres();
		weergavenaamMetPlaats.append(huisarts.getWeergavenaam());
		if (adres != null && StringUtils.isNotBlank(adres.getPlaats()))
		{
			weergavenaamMetPlaats.append(" (").append(adres.getPlaats()).append(")");
		}
		return weergavenaamMetPlaats.toString();
	}

	public static String getGeboorteTussenvoegselEnAchternaam(Persoon persoon)
	{
		return getEigennaam(persoon);
	}

	public static String getGewensteAanspreekVorm(Client client)
	{
		if (client != null)
		{
			var persoon = client.getPersoon();
			var sb = new StringBuilder();
			var aanhef = Aanhef.bepaalJuisteAanhef(persoon);
			sb.append(aanhef.getNaam()).append(" ");
			if (aanhef == Aanhef.GEACHTE)
			{
				sb.append(voorlettersTussenvoegselEnAanspreekAchternaam(client));
			}
			else
			{
				sb.append(StringUtils.capitalize(getAanspreekTussenvoegselEnAchternaam(client)));
			}
			return sb.toString();
		}
		return null;
	}
}
