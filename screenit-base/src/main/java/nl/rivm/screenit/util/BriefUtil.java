package nl.rivm.screenit.util;

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

import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import jakarta.persistence.EntityManager;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.envers.ScreenitRevisionEntity;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.collections4.CollectionUtils;
import org.hibernate.Hibernate;

public class BriefUtil
{
	private BriefUtil()
	{

	}

	public static String getBriefTypeNaam(Brief brief)
	{
		return brief.getBriefType() != null ? brief.getBriefType().name() : brief.getClass().getSimpleName();
	}

	public static Bevolkingsonderzoek[] getOnderzoekenUitBriefType(Brief brief)
	{
		return brief.getBriefType() != null ? brief.getBriefType().getOnderzoeken() : new Bevolkingsonderzoek[0];
	}

	public static boolean isOngunstigeUitslagBrief(ColonBrief bestaandeBrief)
	{
		var briefType = bestaandeBrief.getBriefType();
		return briefType.equals(BriefType.COLON_UITNODIGING_INTAKE) || briefType.equals(BriefType.COLON_INTAKE_AFMELDING)
			|| briefType.equals(BriefType.COLON_INTAKE_GEWIJZIGD);
	}

	public static boolean isUitslagBrief(ColonBrief bestaandeBrief)
	{
		var briefType = bestaandeBrief.getBriefType();
		return isOngunstigeUitslagBrief(bestaandeBrief) || briefType.equals(BriefType.COLON_GUNSTIGE_UITSLAG) || briefType.equals(BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
	}

	public static Brief getOrigineleBrief(Brief brief)
	{
		if (brief != null)
		{
			brief = (Brief) Hibernate.unproxy(brief);
		}
		if (brief instanceof ProjectBrief projectBrief && projectBrief.getBrief() != null)
		{
			brief = (ClientBrief<?, ?, ?>) Hibernate.unproxy(projectBrief.getBrief());
		}
		return brief;
	}

	@Deprecated(forRemoval = true)
	public static MergedBrieven getMergedBrieven(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.getMergedBrieven();
		}
		return null;
	}

	public static boolean isNietGegenereerdEnNietVervangen(Brief brief)
	{
		return !isGegenereerd(brief) && !brief.isVervangen();
	}

	public static ClientBrief getHerdruk(ClientBrief brief)
	{
		brief = (ClientBrief) getOrigineleBrief(brief);
		if (brief != null)
		{
			return brief.getHerdruk();
		}
		return null;
	}

	public static boolean isHerdruk(ClientBrief brief)
	{
		brief = (ClientBrief) getOrigineleBrief(brief);
		if (brief != null)
		{
			return brief.getHerdruk() != null;
		}
		return getHerdruk(brief) != null;
	}

	public static Brief setTegenhouden(Brief brief, boolean tegenhouden)
	{
		if (brief != null)
		{
			brief.setTegenhouden(tegenhouden);
		}
		var afdrukbaarBrief = getBriefVoorPrintStatus(brief);
		if (afdrukbaarBrief != null)
		{
			afdrukbaarBrief.setTegenhouden(tegenhouden);
		}
		return afdrukbaarBrief;
	}

	public static boolean isTegengehouden(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.isTegenhouden();
		}
		return false;
	}

	public static Brief getBriefVoorPrintStatus(Brief brief)
	{
		if (brief != null)
		{
			brief = (Brief) Hibernate.unproxy(brief);
			if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
			{
				var projectBrief = clientBrief.getProjectBrief();
				if (projectBrief != null)
				{
					brief = projectBrief;
				}
			}
			return brief;
		}
		return null;
	}

	public static Date geefDatumVoorGebeurtenisoverzicht(Brief brief)
	{
		if (BriefUtil.isGegenereerd(brief))
		{
			var afdrukDatum = getVerstuurdVoorAfdrukkenMoment(brief);
			if (afdrukDatum != null)
			{
				return afdrukDatum;
			}
		}

		var mergedBrieven = getMergedBrieven(brief);
		if (mergedBrieven != null)
		{
			return mergedBrieven.getCreatieDatum();
		}

		return brief.getCreatieDatum();
	}

	public static Date getVerstuurdVoorAfdrukkenMoment(Brief brief)
	{
		var mergedBrieven = getMergedBrieven(brief);
		if (mergedBrieven != null && mergedBrieven.getPrintDatum() != null)
		{
			return mergedBrieven.getPrintDatum();
		}

		var briefVoorPrintStatus = getBriefVoorPrintStatus(brief);
		return briefVoorPrintStatus != null && briefVoorPrintStatus.getVerstuurdVoorAfdrukkenOp() != null ?
			DateUtil.toUtilDate(briefVoorPrintStatus.getVerstuurdVoorAfdrukkenOp()) :
			isAfgedrukteMigratieOfDirectPrintenBrief(briefVoorPrintStatus) ? briefVoorPrintStatus.getCreatieDatum() : null;
	}

	public static boolean isVerstuurdVoorAfdrukken(Brief brief)
	{
		return isAfgedrukteMigratieOfDirectPrintenBrief(brief) || getVerstuurdVoorAfdrukkenMoment(brief) != null;
	}

	public static boolean isGegenereerd(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.isGegenereerd();
		}
		return false;
	}

	private static boolean isAfgedrukteMigratieOfDirectPrintenBrief(Brief brief)
	{
		return isGegenereerd(brief) && getBriefVoorPrintStatus(brief).getMergedBrieven() == null && isMigratieOfDirectPrintenBrief(brief);
	}

	private static boolean isMigratieOfDirectPrintenBrief(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief == null)
		{
			return false;
		}
		var entityHistory = EntityAuditUtil.getEntityHistory(brief, ApplicationContextProvider.getApplicationContext().getBean(EntityManager.class), false);
		if (CollectionUtils.isEmpty(entityHistory))
		{
			return true;
		}
		var eersteGegenereerdeRevisieInfo = entityHistory.stream()
			.sorted(Comparator.comparingLong(BriefUtil::geefRevisieTimestamp))
			.filter(BriefUtil::isGegenereerdeRevisie)
			.findFirst()
			.map(EntityAuditUtil::getRevisionInfo)
			.orElse(null);

		return isGebruikerOfClientRevisie(eersteGegenereerdeRevisieInfo);
	}

	private static long geefRevisieTimestamp(Object auditRow)
	{
		var revisionInfo = EntityAuditUtil.getRevisionInfo(auditRow);
		return revisionInfo != null ? revisionInfo.getTimestamp() : Long.MAX_VALUE;
	}

	private static boolean isGegenereerdeRevisie(Object auditRow)
	{
		var revisieBrief = EntityAuditUtil.<Brief> getRevisionEntity(auditRow);
		return revisieBrief != null && revisieBrief.isGegenereerd();
	}

	private static boolean isGebruikerOfClientRevisie(ScreenitRevisionEntity revisionInfo)
	{
		return revisionInfo != null && (revisionInfo.getClient() != null || revisionInfo.getOrganisatieMedewerker() != null);
	}

	public static BezwaarBrief maakBezwaarBrief(Client client, BriefType type, Date creatieMoment, boolean vragenOmHandtekening)
	{
		var brief = new BezwaarBrief();
		vulBrief(brief, type, false, creatieMoment);
		brief.setClient(client);
		brief.setVragenOmHandtekening(vragenOmHandtekening);
		return brief;
	}

	public static AlgemeneBrief maakAlgemeneBrief(Client client, BriefType type, Date creatieMoment)
	{
		var brief = new AlgemeneBrief();
		vulBrief(brief, type, false, creatieMoment);
		brief.setClient(client);
		client.getAlgemeneBrieven().add(brief);
		return brief;
	}

	public static CervixRegioBrief maakRegioBrief(ScreeningOrganisatie so, BriefType type, Date creatieMoment, CervixHuisarts arts)
	{
		var brief = new CervixRegioBrief();
		vulBrief(brief, type, false, creatieMoment);
		brief.setHuisarts(arts);
		brief.setRegio(so);
		return brief;
	}

	public static <B extends ClientBrief<?, ?, ?>> B maakBvoBrief(Client client, BriefType type, Date creatieMoment, boolean gegenereerd)
	{
		B brief = maakBrief(type);
		vulBrief(brief, type, gegenereerd, creatieMoment);
		brief.setClient(client);

		return brief;
	}

	private static <B extends ClientBrief<?, ?, ?>> B maakBrief(BriefType type)
	{
		var bevolkingsonderzoeken = Arrays.asList(type.getOnderzoeken());
		if (bevolkingsonderzoeken.equals(List.of(Bevolkingsonderzoek.MAMMA)))
		{
			return (B) new MammaBrief();
		}
		else if (bevolkingsonderzoeken.equals(List.of(Bevolkingsonderzoek.COLON)))
		{
			return (B) new ColonBrief();
		}
		else if (bevolkingsonderzoeken.equals(List.of(Bevolkingsonderzoek.CERVIX)))
		{
			return (B) new CervixBrief();
		}
		throw new IllegalStateException("Deze methode is niet geschikt voor brieftype " + type);
	}

	public static ProjectBrief maakProjectBrief(ProjectClient pClient, ProjectBriefActie actie, Date creatieDatum)
	{
		var pBrief = new ProjectBrief();
		pBrief.setGegenereerd(false);
		pBrief.setCreatieDatum(creatieDatum);
		pBrief.setProjectClient(pClient);
		pBrief.setClient(pClient.getClient());
		pBrief.setDefinitie(actie);
		return pBrief;
	}

	public static <B extends Brief> void vulBrief(B brief, BriefType type, boolean gegenereerd, Date creatieMoment)
	{
		brief.setCreatieDatum(creatieMoment);
		brief.setGegenereerd(gegenereerd);
		brief.setBriefType(type);
	}

	public static String maakKenmerk(Brief brief)
	{
		return brief != null && brief.getId() != null ? "K" + Long.toHexString(brief.getId()).toUpperCase() : null;
	}

	public static String maakTestParagonKenmerk()
	{
		return "T" + Long.toHexString(System.currentTimeMillis()).toUpperCase();
	}

	public static String maakParagonKenmerk(Brief brief)
	{
		var briefKenmerk = maakKenmerk(brief);
		return (briefKenmerk != null ? briefKenmerk : "") + "=" + Long.toHexString(System.currentTimeMillis()).toUpperCase();
	}

	public static boolean isTegenhoudenMogelijk(Brief brief)
	{
		return !BriefType.getCervixZasBrieven().contains(brief.getBriefType());
	}
}
