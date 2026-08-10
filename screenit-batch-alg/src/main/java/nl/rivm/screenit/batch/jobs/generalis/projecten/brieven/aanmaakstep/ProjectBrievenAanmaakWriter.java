package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProjectBrievenAanmaakWriter extends BaseWriter<ProjectBriefActie>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private BaseBriefService briefService;

	@Override
	public void write(ProjectBriefActie item) throws Exception
	{
		var clienten = projectService.getValideClientenVanProject(item.getProject(), item);
		switch (item.getType())
		{
		case DATUM:
		case VANAF_DATUM:
			createDatumBrieven(item, clienten);
			break;
		case XDAGENNAY:
		case XMETY:
			createXYBrief(item, clienten);
			break;
		default:
			break;
		}
	}

	private void createDatumBrieven(ProjectBriefActie actie, List<ProjectClient> clienten)
	{
		for (var client : clienten)
		{
			if (!isDeBriefAlGegenereerdVoorDezeClient(client, actie) && ProjectUtil.isEinde1eCorrespondentieCheck(currentDateSupplier.getDate(), client))
			{
				briefService.maakProjectBrief(client, actie, null);
			}
		}
	}

	private void createXYBrief(ProjectBriefActie actie, List<ProjectClient> clienten)
	{
		var verstuurdOp = currentDateSupplier.getLocalDateTime();
		if (actie.getType() == ProjectBriefActieType.XDAGENNAY)
		{
			verstuurdOp = verstuurdOp.minusDays(actie.getAantalDagen());
		}
		for (var pClient : clienten)
		{
			var client = pClient.getClient();
			var type = actie.getBriefType();

			ScreeningRonde laatsteCervixScreeningsRonde = null, laatsteColonScreeningsRonde = null, laatsteMammaScreeningsRonde = null;
			if (client.getCervixDossier() != null)
			{
				laatsteCervixScreeningsRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			}

			if (client.getColonDossier() != null)
			{
				laatsteColonScreeningsRonde = client.getColonDossier().getLaatsteScreeningRonde();
			}

			if (client.getMammaDossier() != null)
			{
				laatsteMammaScreeningsRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			}

			for (var brief : briefService.getClientBrieven(client))
			{
				var screeningRonde = brief.getScreeningRonde();
				var briefZonderRondeOfBijRondeClientDossiers = screeningRonde == null
					|| screeningRonde.equals(laatsteCervixScreeningsRonde)
					|| screeningRonde.equals(laatsteColonScreeningsRonde)
					|| screeningRonde.equals(laatsteMammaScreeningsRonde);

				var magXProjectBriefMaken = type.equals(brief.getBriefType()) && briefZonderRondeOfBijRondeClientDossiers
					&& !isDeBriefAlGegenereerdVoorDezeClient(pClient, actie) && ProjectUtil.isEinde1eCorrespondentieCheck(currentDateSupplier.getDate(), pClient);
				if (actie.getType() == ProjectBriefActieType.XDAGENNAY)
				{
					var verstuurdVoorAfdrukkenMoment = BriefUtil.getVerstuurdVoorAfdrukkenMoment(brief);
					magXProjectBriefMaken &= verstuurdVoorAfdrukkenMoment != null
						&& DateUtil.toLocalDate(verstuurdVoorAfdrukkenMoment).atStartOfDay().isBefore(verstuurdOp);
				}
				else if (actie.getType() == ProjectBriefActieType.XMETY)
				{
					magXProjectBriefMaken &= DateUtil.toLocalDate(brief.getCreatieDatum()).atStartOfDay().isBefore(verstuurdOp);
				}

				if (magXProjectBriefMaken)
				{
					briefService.maakProjectBrief(pClient, actie, null);
				}
			}
		}
	}

	private static boolean isDeBriefAlGegenereerdVoorDezeClient(ProjectClient pClient, ProjectBriefActie actie)
	{
		return actie != null && pClient.getBrieven().stream().anyMatch(b -> actie.equals(b.getDefinitie()));
	}

}
