package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;
import java.time.LocalDate;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.algemeen.BezwaarService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.OnderzoeksresultatenActie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.BezwaarMomentRepository;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.algemeen.OnderzoeksresultatenActieRepository;
import nl.rivm.screenit.service.BaseBezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.BezwaarUtil;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftBsn;
import static nl.rivm.screenit.util.DateUtil.isGeboortedatumGelijk;

@RequiredArgsConstructor
@Slf4j
@Service
public class BezwaarServiceImpl implements BezwaarService
{
	private final UploadDocumentService uploadDocumentService;

	private final LogService logService;

	private final BaseBezwaarService baseBezwaarService;

	private final ClientRepository clientRepository;

	private final OnderzoeksresultatenActieRepository onderzoeksresultatenActieRepository;

	private final BezwaarMomentRepository bezwaarMomentRepository;

	private final BriefService briefService;

	private final BriefHerdrukkenService briefHerdrukkenService;

	@Override
	@Transactional
	public void bezwaarBRPIntrekken(Client client, MultipartFile briefBestand) throws IOException, IllegalStateException
	{
		try
		{
			var account = ScreenitSession.get().getIngelogdAccount();
			var uploadDocument = uploadDocumentService.multipartToUploadDocument(briefBestand);
			uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.BEZWAAR, client.getId());

			var huidigeMoment = client.getLaatstVoltooideBezwaarMoment();

			var nieuweBezwaarMoment = new BezwaarMoment();
			nieuweBezwaarMoment.setClient(client);
			nieuweBezwaarMoment.setBezwaarBrief(uploadDocument);
			nieuweBezwaarMoment.setManier(ClientContactManier.DIRECT);
			nieuweBezwaarMoment.getBezwaren().addAll(huidigeMoment.getBezwaren().stream().filter(bezwaar -> BezwaarType.GEEN_OPNAME_UIT_BPR != bezwaar.getType()).toList());

			baseBezwaarService.bezwaarAfronden(nieuweBezwaarMoment, account);

			logService.logGebeurtenis(LogGebeurtenis.CLIENT_BEZWAAR_BRP_INGETROKKEN, account, client);

		}
		catch (IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden bij het intrekken van het Bezwaar BRP.", e);
			throw e;
		}
	}

	@Override
	public List<Client> getClientenMetBezwaarBrp(String bsn, LocalDate geboortedatum)
	{
		var account = ScreenitSession.get().getIngelogdAccount();
		logService.logGebeurtenis(LogGebeurtenis.CLIENT_BEZWAAR_BRP_GEZOCHT, account, "Clienten in extra beveiligde omgeving opgevraagd");

		return clientRepository.findAll(heeftBsn(bsn).with(Client_.persoon))
			.stream()
			.filter(client -> BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_OPNAME_UIT_BPR) && isGeboortedatumGelijk(geboortedatum, client))
			.toList();
	}

	@Override
	public boolean ondertekendeBezwaarBriefVervangen(UploadDocument nieuwDocument, BezwaarMoment bezwaarMoment, UploadDocument huidigDocument)
	{
		var account = ScreenitSession.get().getIngelogdAccount();

		bezwaarMoment.setBezwaarBrief(null);
		var isVervangen = ondertekendeBriefVervangen(nieuwDocument, bezwaarMoment.getClient(), huidigDocument, account);
		if (isVervangen)
		{
			bezwaarMoment.setBezwaarBrief(nieuwDocument);
			bezwaarMomentRepository.save(bezwaarMoment);
			return true;
		}

		return false;
	}

	@Override
	public boolean ondertekendeOnderzoeksresultatenBriefVervangen(UploadDocument nieuwDocument, OnderzoeksresultatenActie actie)
	{
		var huidigDocument = actie.getGetekendeBrief();
		var account = ScreenitSession.get().getIngelogdAccount();

		actie.setGetekendeBrief(null);
		var isVervangen = ondertekendeBriefVervangen(nieuwDocument, actie.getClient(), huidigDocument, account);
		if (isVervangen)
		{
			actie.setGetekendeBrief(nieuwDocument);
			onderzoeksresultatenActieRepository.save(actie);
			return true;
		}
		return false;
	}

	@Override
	@Transactional
	public List<BezwaarBrief> verstuurBevestigingsbrievenNogmaals(OnderzoeksresultatenActie actie, Account ingelogdAccount)
	{
		var bevestigingsbrieven = briefService.getOorspronkelijkeBevestigingsbrieven(actie);
		briefHerdrukkenService.opnieuwAanmaken(bevestigingsbrieven, ingelogdAccount);
		return bevestigingsbrieven;
	}

	private boolean ondertekendeBriefVervangen(UploadDocument nieuwDocument, Client client, UploadDocument huidigDocument, Account account)
	{
		uploadDocumentService.delete(huidigDocument);
		try
		{
			uploadDocumentService.saveOrUpdate(nieuwDocument, FileStoreLocation.BEZWAAR, client.getId());
		}
		catch (IOException e)
		{
			LOG.error("Fout bij uploaden van een bezwaar formulier: ", e);
			return false;
		}

		logService.logGebeurtenis(LogGebeurtenis.VERVANGEN_DOCUMENT, account, client, "Ondertekende brief is vervangen.");
		return true;
	}
}
