package nl.rivm.screenit.service.cervix.impl;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.edi.model.MedVryOut;
import nl.rivm.screenit.edi.model.OutboundMessageData;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.MedVryOntvanger;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.cervix.CervixEdiService;
import nl.rivm.screenit.service.cervix.enums.CervixEdiVerstuurStatus;
import nl.rivm.screenit.service.impl.EdiServiceBaseImpl;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixLocatieUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixEdiServiceImpl extends EdiServiceBaseImpl implements CervixEdiService
{
	@Autowired
	@Qualifier(value = "ediAfleverAdres")
	private String ediAfleverAdres;

	@Override
	public void verstuurMedVry(CervixHuisartsBericht huisartsBericht, Account ingelogdAccount)
	{
		var locatie = huisartsBericht.getHuisartsLocatie();
		if (CervixLocatieUtil.klantnummerNietGeverifieerd(locatie))
		{
			huisartsBericht.setStatus(CervixHuisartsBerichtStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
			hibernateService.saveOrUpdate(huisartsBericht);
			return;
		}

		var berichtInhoud = maakBerichtInhoud(huisartsBericht);

		var transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		var medVry = maakMedVry(huisartsBericht);
		zetPatient(huisartsBericht, medVry);
		var sender = zetZender(huisartsBericht, medVry);
		zetInhoud(berichtInhoud, huisartsBericht.getBerichtType(), medVry, transactionId);
		zetOntvanger(medVry, locatie);

		var foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);

		updateHuisartsBerichtNaVerzenden(huisartsBericht, berichtInhoud, StringUtils.isBlank(foutmelding));

		var logGebeurtenis = bepaalLoggebeurtenisVoorHuisartsBericht(huisartsBericht);
		var melding = getLoggingTekst(huisartsBericht.getHuisartsLocatie(), huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, ingelogdAccount);
	}

	@Override
	public CervixEdiVerstuurStatus verstuurMedVryNaarExtraHuisartsLocatie(CervixHuisartsBericht huisartsBericht, CervixHuisartsLocatie extraLocatie, Account ingelogdAccount)
	{
		if (CervixLocatieUtil.klantnummerNietGeverifieerd(extraLocatie))
		{
			return CervixEdiVerstuurStatus.KLANTNUMMER_NIET_GEVERIFIEERD;
		}

		var transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		var medVry = maakMedVry(huisartsBericht);
		zetPatient(huisartsBericht, medVry);
		var sender = zetZender(huisartsBericht, medVry);
		zetInhoud(maakBerichtInhoud(huisartsBericht), huisartsBericht.getBerichtType(), medVry, transactionId);
		zetOntvanger(medVry, extraLocatie);

		var foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);
		var succesvol = StringUtils.isBlank(foutmelding);

		updateHuisartsBerichtNaVerzendenExtraHuisarts(huisartsBericht, extraLocatie, succesvol);

		var logGebeurtenis = succesvol ? LogGebeurtenis.HUISARTSBERICHT_OPNIEUW_VERSTUURD : LogGebeurtenis.HUISARTS_BERICHT_NIET_VERZONDEN;
		var melding = getLoggingTekst(extraLocatie, huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, ingelogdAccount);

		return succesvol ? CervixEdiVerstuurStatus.VERSTUURD : CervixEdiVerstuurStatus.VERSTUREN_MISLUKT;
	}

	@Override
	public void verstuurKlantnummerVerificatieMedVry(CervixHuisartsBericht huisartsBericht)
	{
		var context = new MailMergeContext();
		context.putValue(MailMergeContext.CONTEXT_HA_LOCATIE, huisartsBericht.getHuisartsLocatie());
		context.putValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS, huisartsBericht.getHuisartsLocatie().getHuisarts());
		huisartsBericht.setBerichtInhoud(merge(context, huisartsBericht.getBerichtType()));

		var transactionId = Long.toString(currentDateSupplier.getDate().getTime());

		var medVry = maakMedVry(huisartsBericht);
		zetOntvanger(medVry, huisartsBericht.getHuisartsLocatie());
		zetInhoud(huisartsBericht.getBerichtInhoud(), huisartsBericht.getBerichtType(), medVry, transactionId);
		var sender = zetZender(huisartsBericht, medVry);
		var foutmelding = verstuur(huisartsBericht, transactionId, medVry, sender);

		var logGebeurtenis = StringUtils.isBlank(foutmelding) ? LogGebeurtenis.CERVIX_ZORGMAIL_VERIFICATIE_HUISARTSBERICHT_VERSTUURD
			: LogGebeurtenis.CERVIX_ZORGMAIL_VERIFICATIE_HUISARTSBERICHT_VERSTUREN_MISLUKT;
		var melding = getLoggingTekst(huisartsBericht.getHuisartsLocatie(), huisartsBericht.getBerichtType(), foutmelding,
			huisartsBericht.getScreeningsOrganisatie().getEnovationEdiAdres(), medVry.getReceiverId());
		schrijfLogGebeurtenis(logGebeurtenis, huisartsBericht, melding, null);
	}

	private String maakBerichtInhoud(CervixHuisartsBericht huisartsBericht)
	{
		var context = new MailMergeContext();

		CervixUitstrijkje uitstrijkje;
		if (huisartsBericht.getUitstrijkje() != null)
		{
			uitstrijkje = huisartsBericht.getUitstrijkje();
		}
		else
		{
			uitstrijkje = huisartsBericht.getLabformulier().getUitstrijkje();
			context.setBmhkLaboratorium(huisartsBericht.getLabformulier().getLaboratorium());
		}

		context.setClient(huisartsBericht.getClient());
		context.setCervixUitnodiging(uitstrijkje.getUitnodiging());
		context.setBrief(uitstrijkje.getBrief());

		return merge(context, huisartsBericht.getBerichtType());
	}

	private String verstuur(CervixHuisartsBericht huisartsBericht, String transactionId, MedVryOut medVry, OrganisatieMedewerker sender)
	{
		var outboundMessageData = new OutboundMessageData<MedVryOut>(medVry);
		outboundMessageData.setSubject(medVry.getSubject());
		outboundMessageData.setAddress(medVry.getMail());

		var foutmelding = verzendCheck(medVry, huisartsBericht.getScreeningsOrganisatie());

		try
		{
			var mailVerzenden = manipulateEmailadressen(sender, outboundMessageData);

			if (StringUtils.isBlank(foutmelding) && !MailVerzenden.UIT.equals(mailVerzenden)
				&& !ediMessageService.sendMedVry(sender, sender.getMedewerker().getEmailextra(), outboundMessageData, transactionId))
			{
				foutmelding = "Probleem met versturen MedVry";
			}
		}
		catch (Exception e)
		{
			foutmelding = e.getMessage();
		}

		if (StringUtils.isBlank(foutmelding))
		{
			LOG.debug("Er is succesvol een EDI bericht verzonden voor HuisartsBericht met ID: " + huisartsBericht.getId());
		}
		else
		{
			LOG.error("Het is niet gelukt een EDI bericht versturen voor HuisartsBericht met ID: " + huisartsBericht.getId() + ". " + foutmelding);
		}

		return foutmelding;
	}

	private void updateHuisartsBerichtNaVerzenden(CervixHuisartsBericht huisartsBericht, String berichtInhoud, boolean verzendenSuccesvol)
	{
		if (verzendenSuccesvol)
		{
			huisartsBericht.setBerichtInhoud("");
			huisartsBericht.setStatus(getNieuweStatusNaSuccesvolVerzenden(huisartsBericht));
		}
		else
		{
			huisartsBericht.setBerichtInhoud(berichtInhoud);
			huisartsBericht.setStatus(getNieuweStatusNaVerzendenMislukt(huisartsBericht));
		}
		huisartsBericht.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(huisartsBericht);
	}

	private CervixHuisartsBerichtStatus getNieuweStatusNaSuccesvolVerzenden(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case KLANTNUMMER_NIET_GEVERIFIEERD:
		case AANGEMAAKT:
		case VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.VERSTUURD;
		case VERSTUURD:
		case OPNIEUW_VERSTUURD:
		case OPNIEUW_VERSTUREN_MISLUKT:
			huisartsBericht.setEenOpnieuwVerzondenBericht(true);
			return CervixHuisartsBerichtStatus.OPNIEUW_VERSTUURD;
		default:
			throw new IllegalStateException();
		}
	}

	private CervixHuisartsBerichtStatus getNieuweStatusNaVerzendenMislukt(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case KLANTNUMMER_NIET_GEVERIFIEERD:
		case AANGEMAAKT:
		case VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.VERSTUREN_MISLUKT;
		case VERSTUURD:
		case OPNIEUW_VERSTUURD:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return CervixHuisartsBerichtStatus.OPNIEUW_VERSTUREN_MISLUKT;
		default:
			throw new IllegalStateException();
		}
	}

	private LogGebeurtenis bepaalLoggebeurtenisVoorHuisartsBericht(CervixHuisartsBericht huisartsBericht)
	{
		switch (huisartsBericht.getStatus())
		{
		case VERSTUURD:
			return LogGebeurtenis.HUISARTS_BERICHT_VERZONDEN;
		case OPNIEUW_VERSTUURD:
			return LogGebeurtenis.HUISARTSBERICHT_OPNIEUW_VERSTUURD;
		case VERSTUREN_MISLUKT:
		case OPNIEUW_VERSTUREN_MISLUKT:
			return LogGebeurtenis.HUISARTS_BERICHT_NIET_VERZONDEN;
		default:
			throw new IllegalStateException();
		}
	}

	private void updateHuisartsBerichtNaVerzendenExtraHuisarts(CervixHuisartsBericht huisartsBericht, CervixHuisartsLocatie locatie, boolean verzendenSuccesvol)
	{
		if (verzendenSuccesvol)
		{
			huisartsBericht.setExtraHuisartsLocatie(locatie);
			huisartsBericht.setExtraHuisartsLocatieVerstuurdDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(huisartsBericht);
		}
	}

	private void schrijfLogGebeurtenis(LogGebeurtenis logGebeurtenis, CervixHuisartsBericht huisartsBericht, String melding, Account ingelogdAccount)
	{
		var dashboardOrganisaties = addLandelijkeBeheerOrganisatie(new ArrayList<>());
		dashboardOrganisaties.add(huisartsBericht.getScreeningsOrganisatie());
		logService.logGebeurtenis(logGebeurtenis, dashboardOrganisaties, ingelogdAccount, huisartsBericht.getClient(), melding, Bevolkingsonderzoek.CERVIX);
	}

	private String getLoggingTekst(CervixHuisartsLocatie huisartsLocatie, HuisartsBerichtType berichtType, String foutmelding, String afzender, String ontvanger)
	{
		var logtekst = new StringBuilder();
		if (huisartsLocatie != null)
		{
			logtekst.append("Huisarts: ");
			logtekst.append(NaamUtil.getNaamHuisarts(huisartsLocatie.getHuisarts()));
			logtekst.append(", ");
			logtekst.append("Locatie: ");
			logtekst.append(huisartsLocatie.getNaam());
			logtekst.append(", ");
		}
		return getAlgemeneLoggingTekst(berichtType, foutmelding, afzender, ontvanger, logtekst);
	}

	private void zetOntvanger(MedVryOut medVryOut, CervixHuisartsLocatie huisartsLocatie)
	{
		var ontvanger = new MedVryOntvanger(huisartsLocatie, ediAfleverAdres);
		medVryOut.setOntvanger(ontvanger);
		medVryOut.setReceiverId(huisartsLocatie.getZorgmailklantnummer());
		medVryOut.setMail(ontvanger.getEdiMailAdres());
	}
}
