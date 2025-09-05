
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekFilter;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenOrganisatieOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.repository.algemeen.AbstractAfgeslotenOvereenkomstRepository;
import nl.rivm.screenit.repository.algemeen.AfgeslotenMedewerkerOvereenkomstRepository;
import nl.rivm.screenit.repository.algemeen.AfgeslotenOrganisatieOvereenkomstRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieRepository;
import nl.rivm.screenit.repository.algemeen.OvereenkomstRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.AbstractAfgeslotenOvereenkomstSpecification;
import nl.rivm.screenit.specification.algemeen.AfgeslotenMedewerkerOvereenkomstSpecification;
import nl.rivm.screenit.specification.algemeen.AfgeslotenOrganisatieOvereenkomstSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.specification.algemeen.OvereenkomstSpecification;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.OvereenkomstSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.OvereenkomstSpecification.filterOrganisatieType;
import static nl.rivm.screenit.specification.algemeen.OvereenkomstSpecification.heeftOvereenkomstIn;
import static nl.rivm.screenit.specification.algemeen.OvereenkomstSpecification.isActief;

@Slf4j
@Service
public class OvereenkomstServiceImpl implements OvereenkomstService
{

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private OvereenkomstRepository overeenkomstRepository;

	@Autowired
	private AfgeslotenMedewerkerOvereenkomstRepository afgeslotenMedewerkerOvereenkomstRepository;

	@Autowired
	private AfgeslotenOrganisatieOvereenkomstRepository afgeslotenOrganisatieOvereenkomstRepository;

	@Autowired
	private AbstractAfgeslotenOvereenkomstRepository abstractAfgeslotenOvereenkomstRepository;

	@Autowired
	private OrganisatieRepository organisatieRepository;

	@Override
	@Transactional
	public void saveOrUpdateOvereenkomst(Overeenkomst overeenkomst, UploadDocument uploadDocument, Account account)
	{
		if (OvereenkomstType.ZAKELIJKE_OVEREENKOMST == overeenkomst.getOvereenkomst())
		{
			overeenkomst.setOrganisatieType(OrganisatieType.HUISARTS);
		}

		var nieuwUploadDocument = false;
		if (overeenkomst.getDocument() == null)
		{
			nieuwUploadDocument = true;
			overeenkomst.setDocument(new UploadDocument());
			overeenkomst.getDocument().setActief(Boolean.TRUE);
		}

		if (uploadDocument != null)
		{
			try
			{
				overeenkomst.setLaatsteUpdateDocument(currentDateSupplier.getDate());
				overeenkomst.getDocument().setContentType(uploadDocument.getContentType());
				overeenkomst.getDocument().setNaam(uploadDocument.getNaam());
				overeenkomst.getDocument().setFile(uploadDocument.getFile());

				if (nieuwUploadDocument)
				{
					uploadDocumentService.saveOrUpdate(overeenkomst.getDocument(), FileStoreLocation.COLON_OVEREENKOMST);
				}
				else
				{
					uploadDocumentService.update(overeenkomst.getDocument());
				}
			}
			catch (Exception e)
			{
				LOG.error("Error wirting file", e);
			}
		}

		if (overeenkomst.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEMAAKT, account);
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEWIJZIGD, account);

			for (var afgeslotenOvereenkomst : overeenkomst.getAfgeslotenOvereenkomsten())
			{
				afgeslotenOvereenkomst.setNieuwereOvereenkomst(true);
				afgeslotenOvereenkomst.setAkkoordDatum(null);
				hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
				verstuurOvereenkomstMail(afgeslotenOvereenkomst);
			}
		}

		hibernateService.saveOrUpdate(overeenkomst);

		if (OvereenkomstType.ZAKELIJKE_OVEREENKOMST == overeenkomst.getOvereenkomst())
		{
			cervixHuisartsSyncService.sendData(overeenkomst);
		}
	}

	@Override
	public List<Overeenkomst> getOvereenkomsten(Boolean actief, long first, long size, Sort sort)
	{
		return overeenkomstRepository.findWith(filterActief(actief), q -> q.sortBy(sort)).all(first, size);
	}

	@Override
	public long countOvereenkomsten(Boolean actief)
	{
		return overeenkomstRepository.count(filterActief(actief));
	}

	@Override
	@Transactional
	public void updateOvereenkomst(Overeenkomst overeenkomst, Account account)
	{
		if (Boolean.TRUE.equals(overeenkomst.getActief()))
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEACTIVEERD, account);

			for (var afgeslotenOvereenkomst : overeenkomst.getAfgeslotenOvereenkomsten())
			{
				afgeslotenOvereenkomst.setEindDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
			}
		}
		else
		{

			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_MODEL_GEINACTIVEERD, account);
		}

		hibernateService.saveOrUpdate(overeenkomst);
	}

	@Override
	public List<AfgeslotenMedewerkerOvereenkomst> getAfgeslotenMedewerkerOvereenkomsten(Medewerker zoekObject, Boolean actief, Long first, Long size, Sort sort)
	{
		return afgeslotenMedewerkerOvereenkomstRepository.findWith(
				AbstractAfgeslotenOvereenkomstSpecification.<AfgeslotenMedewerkerOvereenkomst> filterActief(actief, currentDateSupplier.getDate())
					.and(AfgeslotenMedewerkerOvereenkomstSpecification.heeftMedewerker(zoekObject)), q -> q.sortBy(sort))
			.all(first, size);
	}

	@Override
	public long countAfgeslotenMedewerkerOvereenkomsten(Medewerker zoekObject, Boolean actief)
	{
		return afgeslotenMedewerkerOvereenkomstRepository.count(
			AbstractAfgeslotenOvereenkomstSpecification.<AfgeslotenMedewerkerOvereenkomst> filterActief(actief, currentDateSupplier.getDate())
				.and(AfgeslotenMedewerkerOvereenkomstSpecification.heeftMedewerker(zoekObject)));
	}

	@Override
	public List<AfgeslotenOrganisatieOvereenkomst> getAfgeslotenOrganisatieOvereenkomsten(Organisatie zoekObject, Boolean actief, Long first, Long size, Sort sort)
	{
		return afgeslotenOrganisatieOvereenkomstRepository.findWith(
				AbstractAfgeslotenOvereenkomstSpecification.<AfgeslotenOrganisatieOvereenkomst> filterActief(actief, currentDateSupplier.getDate())
					.and(AfgeslotenOrganisatieOvereenkomstSpecification.heeftOrganisatie(zoekObject)), q -> q.sortBy(sort))
			.all(first, size);
	}

	@Override
	public long countAfgeslotenOrganisatieOvereenkomsten(Organisatie zoekObject, Boolean actief)
	{
		return afgeslotenOrganisatieOvereenkomstRepository.count(
			AbstractAfgeslotenOvereenkomstSpecification.<AfgeslotenOrganisatieOvereenkomst> filterActief(actief, currentDateSupplier.getDate())
				.and(AfgeslotenOrganisatieOvereenkomstSpecification.heeftOrganisatie(zoekObject)));
	}

	@Override
	public List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes)
	{
		return overeenkomstRepository.findAll(filterOrganisatieType(organisatieType)
			.and(isActief())
			.and(heeftOvereenkomstIn(List.of(overeenkomstTypes))));
	}

	@Override
	@Transactional
	public void saveOrUpdateOvereenkomst(AbstractAfgeslotenOvereenkomst overeenkomst, UploadDocument uploadDocument, Account account)
	{
		var genereerCode = false;
		var verstuurMail = false;
		if (overeenkomst.getId() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_TOEGEVOEGD, account);
			overeenkomst.setVolgnummer(getVolgnummerOvereenkomst());
			genereerCode = true;
			verstuurMail = overeenkomst.isTeAccoderen();
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_GEWIJZIGD, account);
			hibernateService.getHibernateSession().evict(overeenkomst);
			var oudeAbstractAfgeslotenOvereenkomst = (AbstractAfgeslotenOvereenkomst) hibernateService.load(Hibernate.getClass(overeenkomst),
				overeenkomst.getId());
			if (!oudeAbstractAfgeslotenOvereenkomst.getOvereenkomst().equals(overeenkomst.getOvereenkomst())
				|| !oudeAbstractAfgeslotenOvereenkomst.getStartDatum().equals(overeenkomst.getStartDatum()))
			{
				overeenkomst.setAkkoordDatum(null);
				genereerCode = true;
			}

			if (overeenkomst.isTeAccoderen() && !oudeAbstractAfgeslotenOvereenkomst.isTeAccoderen())
			{
				verstuurMail = true;
			}

			hibernateService.getHibernateSession().evict(oudeAbstractAfgeslotenOvereenkomst);
		}

		if (genereerCode)
		{
			var code = new StringBuilder();
			code.append(overeenkomst.getOvereenkomst().getNaam());
			code.append(".");
			if (overeenkomst.getScreeningOrganisatie().getRegioCode() != null)
			{
				code.append(overeenkomst.getScreeningOrganisatie().getRegioCode());
				code.append(".");
			}

			if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst afgeslotenKwaliteitsOvereenkomst)
			{
				code.append(afgeslotenKwaliteitsOvereenkomst.getMedewerker().getId());
			}
			else if (overeenkomst instanceof AfgeslotenOrganisatieOvereenkomst afgeslotenOvereenkomst)
			{
				code.append(afgeslotenOvereenkomst.getOrganisatie().getId());
			}

			code.append(".");
			code.append(new SimpleDateFormat("yyMM").format(overeenkomst.getStartDatum()));
			code.append(".");
			code.append(overeenkomst.getVolgnummer());
			overeenkomst.setCode(code.toString());
		}

		if (uploadDocument != null)
		{
			try
			{
				overeenkomst.setGescandDocument(uploadDocument);
				uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.COLON_OVEREENKOMST);
			}
			catch (Exception e)
			{
				LOG.error("Error wirting file", e);
			}
		}

		hibernateService.saveOrUpdate(overeenkomst);

		if (verstuurMail)
		{
			verstuurOvereenkomstMail(overeenkomst);
		}
	}

	private int getVolgnummerOvereenkomst()
	{
		return (int) abstractAfgeslotenOvereenkomstRepository.count(
			AbstractAfgeslotenOvereenkomstSpecification.heeftStartDatumInJaar(currentDateSupplier.getLocalDateTime().getYear())) + 1;
	}

	protected void verstuurOvereenkomstMail(AbstractAfgeslotenOvereenkomst overeenkomst)
	{
		Medewerker medewerker = null;
		String emailUA = null;
		if (overeenkomst instanceof AfgeslotenOrganisatieOvereenkomst afgeslotenOvereenkomst)
		{
			var organisatie = afgeslotenOvereenkomst.getOrganisatie();
			emailUA = organisatie.getEmail();
			medewerker = organisatie.getGemachtigde();
		}
		else if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst afgeslotenKwaliteitsOvereenkomst)
		{
			medewerker = afgeslotenKwaliteitsOvereenkomst.getMedewerker();
		}

		if (medewerker != null)
		{

			var content = "";
			var isUitstrijkendArts = overeenkomst.getOvereenkomst().getOrganisatieType() == OrganisatieType.HUISARTS;
			if (isUitstrijkendArts)
			{
				content = preferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL_ZVUA.name(), "{link}");
			}
			else
			{
				content = preferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL.name(), "{link}");
			}
			var link = applicationUrl;
			var aanhef = "";
			if (medewerker.getAanhef() != null)
			{
				aanhef = " " + medewerker.getAanhef().getNaam();
			}

			var titel = "";
			if (medewerker.getTitel() != null)
			{
				titel = " " + medewerker.getTitel().getNaam();
			}

			var achternaam = "";
			if (StringUtils.isNotBlank(medewerker.getAchternaam()))
			{
				achternaam = " " + medewerker.getAchternaam();
			}

			var tussenvoegsel = "";
			if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
			{
				tussenvoegsel = " " + medewerker.getTussenvoegsel();
			}

			var voorletters = "";
			if (StringUtils.isNotBlank(medewerker.getVoorletters()))
			{
				voorletters = " " + medewerker.getVoorletters();
			}

			content = content.replaceAll("\\{link\\}", link);
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{titel\\}", titel);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);

			var emailAdres = medewerker.getEmailwerk();
			if (StringUtils.isNotBlank(medewerker.getEmailextra()))
			{
				emailAdres = medewerker.getEmailextra();
			}
			else if (isUitstrijkendArts)
			{
				emailAdres = emailUA;
			}

			if (StringUtils.isNotBlank(emailAdres))
			{
				var onderwerp = "";
				if (isUitstrijkendArts)
				{
					onderwerp = preferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT_ZVUA.name(), "Zakelijke voorwaarden gewijzigd");
				}
				else
				{
					onderwerp = preferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT.name(), "Overeenkomst gewijzigd");
				}
				mailService.queueMailAanProfessional(emailAdres, onderwerp, content);
			}
		}
	}

	@Override
	public List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(OrganisatieMedewerker organisatieMedewerker)
	{
		return abstractAfgeslotenOvereenkomstRepository.findAll(getTeAccoderenOvereenkomstenSpecification(organisatieMedewerker));
	}

	@Override
	public long countTeAccoderenOvereenkomsten(OrganisatieMedewerker organisatieMedewerker)
	{
		return abstractAfgeslotenOvereenkomstRepository.count(getTeAccoderenOvereenkomstenSpecification(organisatieMedewerker));
	}

	private Specification<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomstenSpecification(OrganisatieMedewerker organisatieMedewerker)
	{
		return AbstractAfgeslotenOvereenkomstSpecification.isNietGeaccodeerd()
			.and(AbstractAfgeslotenOvereenkomstSpecification.heeftEinddatumVanaf(currentDateSupplier.getDate()))
			.and(AfgeslotenMedewerkerOvereenkomstSpecification.heeftMedewerker(organisatieMedewerker.getMedewerker())
				.or(AfgeslotenOrganisatieOvereenkomstSpecification.heeftOrganisatie(organisatieMedewerker.getOrganisatie())
					.and(AfgeslotenOrganisatieOvereenkomstSpecification.heeftGemachtigde(organisatieMedewerker.getMedewerker()))
				)
			);
	}

	@Override
	@Transactional
	public void accodeerOvereenkomsten(OrganisatieMedewerker organisatieMedewerker, Account account)
	{
		for (var afgeslotenOvereenkomst : getTeAccoderenOvereenkomsten(organisatieMedewerker))
		{
			afgeslotenOvereenkomst.setAkkoordDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(afgeslotenOvereenkomst);
			logService.logGebeurtenis(LogGebeurtenis.OVEREENKOMST_GEACCODEERD, account);
		}
	}

	@Override
	public List<Organisatie> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, Sort sort, int first, int count)
	{
		var specification = getSpecificationVoorZoekenAfgeslotenOvereenkomsten(filter);
		return organisatieRepository.findWith(specification, q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter)
	{
		return organisatieRepository.count(getSpecificationVoorZoekenAfgeslotenOvereenkomsten(filter));
	}

	private Specification<Organisatie> getSpecificationVoorZoekenAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter)
	{
		return OrganisatieSpecification.filterNaamContaining(filter.getOrganisatieNaam())
			.and(OrganisatieSpecification.filterOrganisatieType(filter.getOrganisatieType()))
			.and(OrganisatieSpecification.filterUziAbonneeNummer(filter.getOrganisatieUra()))
			.and(OrganisatieSpecification.filterOvereenkomst(filter.getOvereenkomst(), filter.getLopendeDatum()))
			.and(OrganisatieSpecification.filterParent(filter.getRegio()))
			.and(OrganisatieSpecification.filterAdres(filter.getOrganisatiePlaats(), filter.getOrganisatiePostcode()));
	}

	@Override
	public List<AfgeslotenOrganisatieOvereenkomst> getAfgeslotenOvereenkomstenVanOrganisatie(OvereenkomstZoekFilter filter, Organisatie organisatie)
	{
		ExtendedSpecification<AfgeslotenOrganisatieOvereenkomst> specification = AfgeslotenOrganisatieOvereenkomstSpecification.heeftOrganisatie(organisatie);
		if (filter.getLopendeDatum() != null)
		{
			specification = specification.and(AbstractAfgeslotenOvereenkomstSpecification.bevatPeildatum(filter.getLopendeDatum()));
		}
		return afgeslotenOrganisatieOvereenkomstRepository.findAll(specification);
	}

	@Override
	public List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst()
	{
		return overeenkomstRepository.findAll(OvereenkomstSpecification.heeftOvereenkomstType(OvereenkomstType.OVEREENKOMST));
	}

	@Override
	public boolean isErAlEenZakelijkOvereenkomst(Overeenkomst overeenkomst)
	{
		var specification = OvereenkomstSpecification.heeftOvereenkomstType(OvereenkomstType.ZAKELIJKE_OVEREENKOMST);
		if (overeenkomst != null && overeenkomst.getId() != null)
		{
			specification = specification.and(HibernateObjectSpecification.heeftNietId(overeenkomst.getId()));
		}
		return overeenkomstRepository.exists(specification);
	}
}
