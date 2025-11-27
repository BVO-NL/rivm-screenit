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

import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.colon.ColonFitAnalyseResultaatRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonStudieRegistratieService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.impl.ProjectUitslagenUploadException;
import nl.rivm.screenit.util.ProjectUtil;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ColonStudieRegistratieServiceImpl implements ColonStudieRegistratieService
{
	@Lazy
	@Autowired
	private ColonBaseFitService fitService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	@Lazy
	private ClientService clientService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Autowired
	private ColonFitAnalyseResultaatRepository fitAnalyseResultaatRepository;

	@Override
	public Boolean heraanmeldenIndienNodig(ColonFitRegistratie studieRegistratie)
	{
		var clientIsHeraangemeld = false;
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(studieRegistratie);

		if (uitnodiging != null)
		{
			var projectclient = ProjectUtil.getHuidigeProjectClient(uitnodiging.getScreeningRonde().getDossier().getClient(), currentDateSupplier.getDate());
			if (projectclient != null && ProjectUtil.isClientActiefInProject(projectclient, currentDateSupplier.getDate()))
			{
				fitService.bepaalEnSetHeraanmeldenTekstKey(studieRegistratie);
				if (studieRegistratie.getHeraanmeldenTekstKey() != null)
				{
					fitService.heraanmelden(studieRegistratie.getScreeningRonde());
					clientIsHeraangemeld = true;
				}
			}
		}
		return clientIsHeraangemeld;
	}

	@Override
	@Transactional
	public void projectClientInactiverenBijVergelijkendOnderzoek(ColonScreeningRonde screeningRonde)
	{
		var client = screeningRonde.getDossier().getClient();
		var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (projectClient != null
			&& ColonOnderzoeksVariant.VERGELIJKEND.name().equals(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_ONDERZOEKSVARIANT)))
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.INACTIVATIE_VERGELIJKEND_ONDERZOEK, Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	@Transactional
	public void controleerUitslagenbestandOpFouten(ColonFitRegistratie studieRegistratie, ProjectBestand uitslagenbestand) throws ProjectUitslagenUploadException
	{
		geefFoutBijInactiefInProject(uitslagenbestand, studieRegistratie);
		geefFoutBijUploadenFit(studieRegistratie);
		geefFoutAlsFitNietVerwerktIs(studieRegistratie);
		geefFoutBijAfgerondeRonde(studieRegistratie);
		geefFoutAlsOngunstigeUitslagVerstuurdIs(studieRegistratie);
		geefFoutBijVerstrekenWachttijd(studieRegistratie);
	}

	private void geefFoutBijInactiefInProject(ProjectBestand uitslagenBestand, ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		var client = studieRegistratie.getScreeningRonde().getDossier().getClient();
		var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (projectClient == null || uitslagenBestand != null && !projectClient.getProject().equals(uitslagenBestand.getProject()))
		{
			throw new ProjectUitslagenUploadException("De cliënt is niet actief in dit project");
		}
	}

	private void geefFoutAlsFitNietVerwerktIs(ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(studieRegistratie);
		var reguliereTest = uitnodiging.getGekoppeldeFitRegistratie();

		if (!reguliereTest.getStatus().equals(ColonFitRegistratieStatus.UITGEVOERD))
		{
			if (!fitAnalyseResultaatRepository.findByBarcodeAndAnalyseResultaatSetStatusNot(reguliereTest.getBarcode(), ColonFitAnalyseResultaatSetStatus.VERWERKT).isEmpty())
			{
				throw new ProjectUitslagenUploadException("De FIT/Gold is wel geanalyseerd, maar nog niet verwerkt");
			}
			throw new ProjectUitslagenUploadException("De FIT/Gold is niet ontvangen/geanalyseerd");
		}
	}

	private void geefFoutBijUploadenFit(ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		if (!studieRegistratie.getType().equals(ColonFitType.STUDIE))
		{
			throw new ProjectUitslagenUploadException("De test is geen studietest");
		}
	}

	private void geefFoutBijAfgerondeRonde(ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		if (studieRegistratie.getScreeningRonde().getStatus().equals(ScreeningRondeStatus.AFGEROND))
		{
			throw new ProjectUitslagenUploadException("De ronde van de studietest is afgerond");
		}
	}

	private void geefFoutAlsOngunstigeUitslagVerstuurdIs(ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		var afspraak = studieRegistratie.getScreeningRonde().getLaatsteAfspraak();
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(studieRegistratie);
		var reguliereFit = uitnodiging.getGekoppeldeFitRegistratie();
		if (ColonFitRegistratieUtil.isGunstig(reguliereFit) && afspraak != null)
		{
			throw new ProjectUitslagenUploadException("De intake afspraak is ingepland, de uitslag van de studietest kan niet gewijzigd worden");
		}
	}

	private void geefFoutBijVerstrekenWachttijd(ColonFitRegistratie studieRegistratie) throws ProjectUitslagenUploadException
	{
		var uitnodiging = ColonFitRegistratieUtil.getUitnodiging(studieRegistratie);
		if (uitnodiging != null && uitnodiging.getUitgesteldeUitslagDatum() != null && uitnodiging.getUitgesteldeUitslagDatum().before(currentDateSupplier.getDate()))
		{
			throw new ProjectUitslagenUploadException("De wachtperiode is verstreken");
		}
	}

	@Override
	@Transactional
	public void verwerkRegistratie(ColonFitRegistratie studieRegistratie)
	{
		fitService.setStatus(studieRegistratie, ColonFitRegistratieStatus.UITGEVOERD);
		studieRegistratie.setVerwerkingsDatum(currentDateSupplier.getDate());

		uitnodigingService.verwijderUitgesteldeUitslagDatum(studieRegistratie.getUitnodigingExtra());

		hibernateService.saveOrUpdateAll(studieRegistratie);
	}
}
