package nl.rivm.screenit.batch.service.impl;

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

import java.lang.reflect.InvocationTargetException;
import java.util.Date;

import jakarta.persistence.EntityManager;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.generalis.gba.exception.GbaImportException;
import nl.rivm.screenit.batch.jobs.generalis.gba.wrappers.GbaValidatieWrapper;
import nl.rivm.screenit.batch.service.GbaService;
import nl.rivm.screenit.batch.service.GbaVraagService;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.enums.DatumPrecisie;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.IndicatieGeheim;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.gba.GbaVerwerkingEntry;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.model.vertrouwdverbonden.Vo107Bericht;
import nl.rivm.screenit.model.vertrouwdverbonden.enums.Land;
import nl.rivm.screenit.model.vertrouwdverbonden.enums.GbaRubriek;
import nl.rivm.screenit.model.vertrouwdverbonden.enums.Vo107_ArecordVeld;
import nl.rivm.screenit.model.vertrouwdverbonden.enums.VoxBrecordVeld;
import nl.rivm.screenit.model.vertrouwdverbonden.utils.VoxHelper;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.service.BaseGbaVraagService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.TransgenderService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import com.google.common.base.Strings;

@Service
@RequiredArgsConstructor
@Slf4j
public class GbaServiceImpl implements GbaService
{
	private static final String WA11 = "Wa11";

	private final EntityManager entityManager;

	private final ClientService clientService;

	private final LogService logService;

	private final GemeenteRepository gemeenteRepository;

	private final HibernateService hibernateService;

	private final CoordinatenService coordinatenService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final TransgenderService transgenderService;

	private final BaseGbaVraagService baseGbaVraagService;

	private final GbaVraagService gbaVraagService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void importVo107Bericht(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog) throws GbaImportException
	{
		Client client = null;
		try
		{

			var eref = bericht.getString(Vo107_ArecordVeld.EREF);
			LOG.debug("Verwerken vo107-bericht met EREF: {}", eref);

			if (bericht.getBerichtType().equalsIgnoreCase("dt01") || bericht.getBerichtType().equalsIgnoreCase("dw01"))
			{

				verwerkTabelRegel(bericht);
			}
			else
			{
				var bsn = bericht.getBsn();
				var bsnBRecord = getStringUitBericht(bericht, GbaRubriek.PERS_BSN);
				var anummerARecord = bericht.getString(Vo107_ArecordVeld.ANR);
				var oorspronkelijkBsn = bericht.getOorspronkelijkBsn();

				if (Strings.isNullOrEmpty(bsn) || bericht.isVerstrekking() && Strings.isNullOrEmpty(bsnBRecord))
				{
					var foutRegel = new GbaFoutRegel();
					var foutString = "Bericht geskipt: Geen BSN gevonden in A of B records. EREF: " + eref;
					foutRegel.setFout(foutString);
					foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
					foutRegel.setVerwerkingsLog(verwerkingLog);
					verwerkingLog.getFouten().add(foutRegel);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEEN_BSN, clientService.getScreeningOrganisatieVan(client), null, null, foutString);

					return;
				}

				if (!Strings.isNullOrEmpty(oorspronkelijkBsn))
				{
					client = clientService.getClientByBsn(oorspronkelijkBsn);
					var clientMetNieuwBsn = clientService.getClientByBsn(bsn);
					Client afgevoerdeClientMetNieuwBsn = null;

					if (clientMetNieuwBsn == null)
					{
						afgevoerdeClientMetNieuwBsn = clientService.getLaatstAfgevoerdeClient(bsn);
					}

					var oorspronkelijkeClientHeeftDossier = client != null && clientService.heeftDossierMetRondeOfAfmelding(client);
					var afgevoerdeClientMetNieuwBsnHeeftDossier = afgevoerdeClientMetNieuwBsn != null
						&& clientService.heeftDossierMetRondeOfAfmelding(afgevoerdeClientMetNieuwBsn);

					if (!bsn.equals(oorspronkelijkBsn) && !oorspronkelijkeClientHeeftDossier && afgevoerdeClientMetNieuwBsnHeeftDossier)
					{
						if (client != null)
						{
							wisselAnummers(afgevoerdeClientMetNieuwBsn, client);
							verwijderClient(client, verwerkingLog, eref, anummerARecord, false);
						}
						client = afgevoerdeClientMetNieuwBsn;
					}
					else if (clientMetNieuwBsn != null && client != null && !bsn.equals(oorspronkelijkBsn))
					{
						var foutmelding = "Bericht geskipt: Wijziging van bsn van " + oorspronkelijkBsn + " naar " + bsn
							+ " niet mogelijk, omdat er al andere client met het nieuwe bsn bestaat (anummer " + clientMetNieuwBsn.getPersoon().getAnummer() + "). "
							+ getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_BSN_NIET_OVEREEN, clientService.getScreeningOrganisatieVan(clientMetNieuwBsn), clientMetNieuwBsn,
							foutmelding);
						createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);

						return;
					}

				}
				else
				{
					client = clientService.getClientByBsn(bsn); 
				}

				if (client == null)
				{
					client = clientService.getClientByBsnFromNg01Bericht(bsn, anummerARecord);
				}

				var isVerwijderBericht = "Ng01".equalsIgnoreCase(bericht.getBerichtType());
				var clientIsVerwijderd = client != null && GbaStatus.AFGEVOERD.equals(client.getGbaStatus());
				if (client != null)
				{
					var validatieWrapper = valideerGevondenClient(client, bericht, verwerkingLog, isVerwijderBericht, clientIsVerwijderd);
					clientIsVerwijderd = validatieWrapper.isClientIsVerwijderd();
					if (validatieWrapper.isStopVerwerking())
					{
						return;
					}
					if (clientIsVerwijderd && !isVerwijderBericht)
					{
						client = null;
					}
				}

				if (client == null && bericht.isVerstrekking())
				{
					client = clientService.getLaatstAfgevoerdeClient(bsn);
					if (client != null && !anummerARecord.equals(client.getPersoon().getAnummer()))
					{
						if (verstrekkingErrorClientMetAnderAnummerBestaatAl(bericht, verwerkingLog, client))
						{
							return;
						}
						client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
						client.getPersoon().setBsn(bsn);
						hibernateService.saveOrUpdate(client);
						entityManager.flush();
						var logmelding = "Client is heractiveerd met anummer wijziging naar: " + getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_HERACTIVATIE, clientService.getScreeningOrganisatieVan(client), client, logmelding);
					}
				}

				if (bericht.isVerstrekking())
				{
					verwerkVerstrekking(bericht, verwerkingLog, client);
				}
				else if (bericht.isMutatie())
				{
					verwerkMutatie(bericht, verwerkingLog, bsn, client);
				}
				else if (isNullBericht(bericht))
				{
					gbaVraagService.verwerkNullBericht(bsn, client, bericht);
				}
				else if (isVerwijderBericht)
				{

					if (client != null && clientIsVerwijderd)
					{
						var foutmelding = "Bericht geskipt: Ng01 bericht maar client is reeds eerder verwijderd. " + getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutmelding);
						createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
					}
					else if (client != null)
					{
						verwijderClient(client, verwerkingLog, eref, anummerARecord, true);
					}
					else
					{
						createFout(null, verwerkingLog, "Ng01 voor onbekende burger ontvangen, EREF: " + eref, GbaFoutCategorie.INHOUDELIJK);
					}
				}
				else
				{
					LOG.error("Bericht niet verwerkt, onbekend bericht: " + bericht.getBerichtType() + " EREF: " + eref);
				}
			}
			if (client != null)
			{
				clientService.actiesNaUpdateWithGba(client);
			}
			entityManager.flush();
		}
		catch (Exception e)
		{
			var foutmelding = "Berichtverwerking gestopt vanwege fout door bericht: " + getFoutmelding(bericht, verwerkingLog, client);
			LOG.error("Berichtverwerking gestopt vanwege fout door bericht", e);

			TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();

			var exception = new GbaImportException(foutmelding, e);
			if (client != null)
			{
				exception.setClientId(client.getId());
			}
			throw exception;
		}
	}

	private boolean isNullBericht(Vo107Bericht bericht)
	{

		return "Null".equalsIgnoreCase(bericht.getBerichtType());
	}

	private void wisselAnummers(Client clientA, Client clientB)
	{
		var anummerClientA = clientA.getPersoon().getAnummer();
		var anummerClientB = clientB.getPersoon().getAnummer();
		clientA.getPersoon().setAnummer(null);
		clientB.getPersoon().setAnummer(null);
		hibernateService.saveOrUpdate(clientA);
		hibernateService.saveOrUpdate(clientB);
		entityManager.flush();
		clientA.getPersoon().setAnummer(anummerClientB);
		clientB.getPersoon().setAnummer(anummerClientA);
		hibernateService.saveOrUpdate(clientA);
		hibernateService.saveOrUpdate(clientB);
		entityManager.flush();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void logGbaImportError(GbaImportException e, GbaVerwerkingsLog verwerkingLog)
	{
		Client client = null;
		if (e.getClientId() != null)
		{
			client = hibernateService.get(Client.class, e.getClientId());
		}

		logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_FOUT, clientService.getScreeningOrganisatieVan(client), client,
			e.getMessage());
		createFout(null, verwerkingLog, e.getMessage(), GbaFoutCategorie.PROCES);
	}

	private void verwijderClient(Client client, GbaVerwerkingsLog verwerkingLog, String eref, String anummerARecord, boolean aantalBurgersBijwerken)
	{
		var oudeClientBsn = client.getPersoon().getBsn();
		var nieuweClientBsn = clientService.getVoorNg01EenNieuweBsn(oudeClientBsn);
		if (nieuweClientBsn != null)
		{
			plaatsBSNGewijzigdMarker(client, oudeClientBsn, nieuweClientBsn);
			client.getPersoon().setBsn(nieuweClientBsn);
		}
		if (aantalBurgersBijwerken)
		{
			verwerkingLog.setAantalBijgewerkteBugers(verwerkingLog.getAantalBijgewerkteBugers() + 1);
		}
		client.setGbaStatus(GbaStatus.AFGEVOERD);
		client.setRedenIntrekkenGbaIndicatieDoorBvo(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN);
		var aantalIndicatiesIngetrokkenAfgevoerd = verwerkingLog.getAantalIndicatiesIngetrokkenAfgevoerd();
		verwerkingLog.setAantalIndicatiesIngetrokkenAfgevoerd(aantalIndicatiesIngetrokkenAfgevoerd != null ? aantalIndicatiesIngetrokkenAfgevoerd + 1 : 1);
		hibernateService.saveOrUpdate(client);
		entityManager.flush();

		var goedeString = "Bericht Verwerkt: bsn aangepast naar " + nieuweClientBsn + ", EREF " + eref + ", a-nummer uit bericht: " + anummerARecord
			+ ", anummer afgevoerde cliënt: " + client.getPersoon().getAnummer();
		logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_VERWIJDERD_VAN_PERSOONSLIJST, client, goedeString);

		if (getScreeningOrganisatie(client) != null && aantalBurgersBijwerken)
		{
			var verwerkingEntry = getOrCreateEntry(verwerkingLog, client);
			verwerkingEntry.setAantalBijgewerkteBugers(verwerkingEntry.getAantalBijgewerkteBugers() + 1);
		}
	}

	private GbaValidatieWrapper valideerGevondenClient(Client client, Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, boolean isVerwijderBericht,
		boolean clientIsVerwijderd)
	{
		var anummerARecord = bericht.getString(Vo107_ArecordVeld.ANR);
		var bsn = bericht.getBsn();

		if (client != null)
		{

			if (GbaStatus.BEZWAAR.equals(client.getGbaStatus()))
			{
				verwerkBerichtVoorClientMetBezwaarBrp(client, bericht, verwerkingLog);
				return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
			}

			registreerMutatie(client, bericht);

			var isWijzigAnummerBericht = WA11.equalsIgnoreCase(bericht.getBerichtType());
			var anummerClient = client.getPersoon().getAnummer();

			if (clientIsVerwijderd && !isVerwijderBericht && anummerClient != null && 
				(isWijzigAnummerBericht || anummerClient.equals(anummerARecord)))
			{
				if (bericht.isMutatie() || bericht.isVerstrekking())
				{
					client.getPersoon().setBsn(bsn);
					client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
					var foutString =
						"Verstrekking of mutatie bericht ontvangen voor burger die van persoonslijst verwijderd is. Verwijderindicatie wordt nu weer opgeheven en het juiste bsn teruggezet. "
							+ getFoutmelding(bericht, verwerkingLog, client) + ", berichttype " + bericht.getBerichtType();
					clientIsVerwijderd = false;
					logService.logGebeurtenis(LogGebeurtenis.GBA_OPHEFFEN_VERWIJDERINDICATIE, clientService.getScreeningOrganisatieVan(client), client, foutString);
				}
				else
				{
					var foutRegel = new GbaFoutRegel();
					var foutString = "Bericht geskipt: Bericht binnengekregen voor burger die van persoonslijst verwijderd is, "
						+ getFoutmelding(bericht, verwerkingLog, client) + ", berichttype " + bericht.getBerichtType();
					foutRegel.setFout(foutString);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutString);
					foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
					foutRegel.setClient(client.getId());
					foutRegel.setVerwerkingsLog(verwerkingLog);
					verwerkingLog.getFouten().add(foutRegel);

					return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
				}
			}

			var anummerBRecord = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
			if (!clientIsVerwijderd && anummerClient != null &&
				isWijzigAnummerBericht && !anummerClient.equals(anummerBRecord) ||
				!isWijzigAnummerBericht && !anummerClient.equals(anummerARecord))
			{
				var foutRegel = new GbaFoutRegel();
				var foutString = "Bericht geskipt: A-nummer in header van bericht en a-nummer van client (gevonden op basis van bsn) komen niet overeen. ";
				if (isWijzigAnummerBericht)
				{
					foutString = "Bericht geskipt: A-nummer in de b-record van bericht(Wa11) en a-nummer van client (gevonden op basis van bsn) komen niet overeen.";
				}
				logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(client), client, foutString);
				foutString += getFoutmelding(bericht, verwerkingLog, client);
				foutRegel.setFout(foutString);
				foutRegel.setFoutCategorie(GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
				foutRegel.setClient(client.getId());
				foutRegel.setVerwerkingsLog(verwerkingLog);
				verwerkingLog.getFouten().add(foutRegel);

				return GbaValidatieWrapper.stopVerwerking(clientIsVerwijderd);
			}

		}
		return new GbaValidatieWrapper(clientIsVerwijderd, false);
	}

	private void verwerkBerichtVoorClientMetBezwaarBrp(Client client, Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog)
	{
		if (isNullBericht(bericht))
		{
			gbaVraagService.verwerkNullBericht(client.getPersoon().getBsn(), client, bericht);
		}
		else
		{
			var string = "Bericht genegeerd: Client heeft bezwaar gemaakt tegen BRP. " + getFoutmelding(bericht, verwerkingLog, client);
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_BEZWAAR, clientService.getScreeningOrganisatieVan(client), client, string);
			gbaVraagService.onverwachtBerichtBijBezwaarBrp(client);
		}
	}

	protected String getFoutmelding(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		var bestandsnaam = "onbekend";
		if (!verwerkingLog.getBestanden().isEmpty())
		{
			bestandsnaam = verwerkingLog.getBestanden().get(verwerkingLog.getBestanden().size() - 1).getNaam();
		}

		var geboorteDatum = getDateUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		var foutmelding = "Record met eref " + bericht.getString(Vo107_ArecordVeld.EREF) + " in bestand " + bestandsnaam + ", bsn uit bericht: "
			+ bericht.getString(Vo107_ArecordVeld.SOFINR) + ", a-nummer uit bericht: " + bericht.getString(Vo107_ArecordVeld.ANR);

		var gebDatumAdded = false;
		if (client != null && client.getPersoon() != null && client.getPersoon().getGeboortedatum() != null)
		{
			foutmelding += ". Gegevens gevonden in client, bsn: " + client.getPersoon().getBsn() + ", a-nummer: " + client.getPersoon().getAnummer();

			if (client.getPersoon().getGeboortedatum() != null)
			{
				foutmelding += ", geb. datum : " + DateUtil.formatShortDate(client.getPersoon().getGeboortedatum());
				gebDatumAdded = true;
			}
		}

		if (!gebDatumAdded)
		{
			foutmelding += ", geb. datum uit bericht: ";
			if (geboorteDatum != null)
			{
				foutmelding += DateUtil.formatShortDate(geboorteDatum);
			}
			else
			{
				foutmelding += "geen";
			}
		}

		return foutmelding;
	}

	private void verwerkMutatie(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, String bsn, Client client)
	{
		if (client != null)
		{

			if (bericht.getBerichtType().equals(WA11))
			{
				var anummer = bericht.getString(Vo107_ArecordVeld.ANR);
				var otherClientWithSameAnummer = clientService.getClientByAnummer(anummer);
				if (otherClientWithSameAnummer != null)
				{
					var foutmelding = "Bericht(Wa11) geskipt: Client met bsn " + bsn + " en anummer " + anummer
						+ " kan niet gewijzigd worden, omdat er al een client met dit anummer (bsn " + otherClientWithSameAnummer.getPersoon().getBsn() + ") aanwezig is. "
						+ getFoutmelding(bericht, verwerkingLog, client);
					logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(otherClientWithSameAnummer), otherClientWithSameAnummer,
						foutmelding);
					createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
					return;
				}
			}
			verwerkBericht(bericht, verwerkingLog, client);
		}
		else
		{
			baseGbaVraagService.verzoekVerwijderIndicatieOnbekendeClient(bsn);
		}
	}

	private boolean verstrekkingErrorClientMetAnderAnummerBestaatAl(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		var anummer = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
		var otherClientWithSameAnummer = clientService.getClientByAnummer(anummer);
		if (otherClientWithSameAnummer != null)
		{
			var foutmelding = "Bericht geskipt: Client met bsn " + bericht.getBsn() + " en anummer " + anummer
				+ " kan niet aangemaakt worden, omdat er al een client met dit anummer (bsn " + otherClientWithSameAnummer.getPersoon().getBsn() + ") aanwezig is. "
				+ getFoutmelding(bericht, verwerkingLog, client);
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_SKIP, clientService.getScreeningOrganisatieVan(otherClientWithSameAnummer), otherClientWithSameAnummer,
				foutmelding);
			createFout(null, verwerkingLog, foutmelding, GbaFoutCategorie.OVERIG);
			return true;
		}
		return false;
	}

	private void verwerkVerstrekking(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		if (client == null)
		{
			if (verstrekkingErrorClientMetAnderAnummerBestaatAl(bericht, verwerkingLog, null))
			{
				return;
			}
			verwerkingLog.setAantalNieuweBurgers(verwerkingLog.getAantalNieuweBurgers() + 1);

			var nieuweClient = vulNieuweClient(bericht, verwerkingLog);

			registreerMutatie(nieuweClient, bericht);

			hibernateService.saveOrUpdate(nieuweClient);
			gbaVraagService.gbaVraagAfrondenVoorMutatieOfVerstrekking(nieuweClient, null, true, true);
		}
		else
		{
			verwerkBericht(bericht, verwerkingLog, client);
		}
	}

	private Client vulNieuweClient(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog)
	{
		var client = new Client();
		var persoon = new Persoon();
		persoon.setClient(client);
		client.setPersoon(persoon);
		client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		vulPersoonsGegevens(client, bericht, verwerkingLog, true);
		verwerkAdres(bericht, verwerkingLog, client);

		var clientScreeningorganisatie = clientService.getScreeningOrganisatieVan(client);

		if (client.getPersoon().getGeboortedatum() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEEN_GEBOORTEDATUM, clientScreeningorganisatie, null,
				"Verstrekking bevat geen geboortedatum, bsn: " + bericht.getBsn());
		}

		if (transgenderService.isNieuweClientAdhocPlaatsingTransgender(client.getPersoon()))
		{
			logService.logGebeurtenis(LogGebeurtenis.GBA_ADHOC_IMPORT_TRANSGENDER, clientScreeningorganisatie, client);
		}
		return client;
	}

	private void verwerkBericht(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{
		verwerkingLog.setAantalBijgewerkteBugers(verwerkingLog.getAantalBijgewerkteBugers() + 1);

		if (getScreeningOrganisatie(client) != null)
		{
			var verwerkingEntry = getOrCreateEntry(verwerkingLog, client);
			verwerkingEntry.setAantalBijgewerkteBugers(verwerkingEntry.getAantalBijgewerkteBugers() + 1);
		}

		var oudeGbaStatus = client.getGbaStatus();

		var persoonsGegevensGewijzigd = vulPersoonsGegevens(client, bericht, verwerkingLog, false);
		var adresGewijzigd = verwerkAdres(bericht, verwerkingLog, client);

		gbaVraagService.gbaVraagAfrondenVoorMutatieOfVerstrekking(client, oudeGbaStatus, persoonsGegevensGewijzigd, adresGewijzigd);

		setIndicatieAanwezig(client);
		hibernateService.saveOrUpdate(client);
	}

	private void registreerMutatie(Client client, Vo107Bericht bericht)
	{
		var gbaMutatie = new GbaMutatie();
		gbaMutatie.setMutatieDatum(currentDateSupplier.getDate());
		client.setLaatsteGbaMutatie(gbaMutatie);
		gbaMutatie.setTypeBericht(bericht.getBerichtType());
		gbaMutatie.setBerichtEref(bericht.getString(Vo107_ArecordVeld.EREF));
		client.getGbaMutaties().add(gbaMutatie);
	}

	private void setIndicatieAanwezig(Client client)
	{
		if (!GbaStatus.PUNT_ADRES.equals(client.getGbaStatus()))
		{
			client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
		}
		client.setRedenIntrekkenGbaIndicatieDoorBvo(RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN);
	}

	private GbaVerwerkingEntry getOrCreateEntry(GbaVerwerkingsLog verwerkingLog, Client client)
	{
		GbaVerwerkingEntry verwerkingEntry = null;
		for (var entry : verwerkingLog.getEntries())
		{
			if (entry.getScreeningOrganisatie().equals(getScreeningOrganisatie(client)))
			{
				verwerkingEntry = entry;
			}
		}

		if (verwerkingEntry == null)
		{
			verwerkingEntry = new GbaVerwerkingEntry();
			verwerkingEntry.setScreeningOrganisatie(getScreeningOrganisatie(client));
			verwerkingEntry.setVerwerkingsLog(verwerkingLog);
			verwerkingLog.getEntries().add(verwerkingEntry);
		}
		return verwerkingEntry;
	}

	private boolean verwerkAdres(Vo107Bericht bericht, GbaVerwerkingsLog verwerkingLog, Client client)
	{

		var persoon = client.getPersoon();
		var adres = persoon.getGbaAdres();
		if (adres == null)
		{
			adres = new BagAdres();
		}

		var adresGegevensGewijzigd = vulAdresMetGbaGegevens(adres, bericht, client, verwerkingLog);
		adresGegevensGewijzigd |= changeProperty(adres, "postcodeCoordinaten", coordinatenService.getCoordinaten(adres), true);
		persoon.setGbaAdres(adres);

		hibernateService.saveOrUpdate(adres);
		hibernateService.saveOrUpdate(persoon);

		var isTijdelijkGbaAdresVerwijderd = false;

		if (".".equals(StringUtils.trim(adres.getStraat())) && StringUtils.isNotBlank(getStringUitBericht(bericht, GbaRubriek.VERBP_AAND_GEG_ONDERZOEK)))
		{
			client.setGbaStatus(GbaStatus.PUNT_ADRES);
		}
		else
		{
			if (GbaStatus.PUNT_ADRES.equals(client.getGbaStatus()))
			{
				client.setGbaStatus(GbaStatus.INDICATIE_AANWEZIG);
			}
			if (AdresUtil.isOnvolledigAdres(adres))
			{
				if (adres.getGbaGemeente() != null && !adres.getGbaGemeente().getCode().equals(Gemeente.RNI_CODE) && client.getPersoon().getDatumVertrokkenUitNederland() == null)
				{
					var melding = "Lege postcode (08.11.60) en/of lege huisnummer (08.11.20) en/of lege woonplaats (08.11.70) en/of gevulde locatieomschrijving (08.12.10) "
						+ getFoutmelding(bericht, verwerkingLog, client);
					logService.logGebeurtenis(LogGebeurtenis.GBA_ADRES_ONVOLLEDIG, clientService.getScreeningOrganisatieVan(client), client, melding);
				}
			}
			else
			{
				var tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
				if (tijdelijkGbaAdres != null)
				{
					if (tijdelijkGbaAdres.getId() != null)
					{
						isTijdelijkGbaAdresVerwijderd = true;
						var melding = "Automatisch verwijderd. " + getFoutmelding(bericht, verwerkingLog, client);
						logService.logGebeurtenis(LogGebeurtenis.GBA_TIJDELIJK_ADRES, clientService.getScreeningOrganisatieVan(client), client, melding);
						hibernateService.delete(tijdelijkGbaAdres);
					}
					persoon.setTijdelijkGbaAdres(null);
					hibernateService.saveOrUpdate(persoon);
				}
			}
			setGbaAdresGewijzigdMarker(client, adresGegevensGewijzigd, isTijdelijkGbaAdresVerwijderd);
		}
		return adresGegevensGewijzigd;
	}

	private void setGbaAdresGewijzigdMarker(Client client, boolean adresGewijzigd, boolean tijdelijkGbaVerwijderd)
	{

		var huidigeMutatie = getHuidigeGbaMutatie(client);
		if (huidigeMutatie != null && client.getMammaDossier() != null
			&& (client.getMammaDossier().getLaatsteScreeningRonde() != null || client.getMammaDossier().getLaatsteAfmelding() != null)
			&& (adresGewijzigd || tijdelijkGbaVerwijderd))
		{
			huidigeMutatie.setAanvullendeInformatie(Constants.MAMMA_ADRES_GEWIJZIGD_MARKER + huidigeMutatie.getAanvullendeInformatie());
		}
	}

	private void plaatsBSNGewijzigdMarker(Client client, String oorspronkelijkBsn, String nieuweBsn)
	{
		var huidigeMutatie = getHuidigeGbaMutatie(client);

		if (huidigeMutatie != null && client.getMammaDossier() != null
			&& (client.getMammaDossier().getLaatsteScreeningRonde() != null || client.getMammaDossier().getLaatsteAfmelding() != null)
			&& !StringUtils.contains(huidigeMutatie.getAanvullendeInformatie(), Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER))
		{
			huidigeMutatie.setAanvullendeInformatie(
				String.format("|%s:%s,%s|%s", Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER, oorspronkelijkBsn, nieuweBsn, huidigeMutatie.getAanvullendeInformatie()));
		}
	}

	private GbaMutatie getHuidigeGbaMutatie(Client client)
	{
		if (client == null)
		{
			return null;
		}
		return !client.getGbaMutaties().isEmpty() ? client.getGbaMutaties().get(client.getGbaMutaties().size() - 1) : null;
	}

	private void plaatsIMSGegevensGewijzigdMarker(Client client)
	{

		plaatsMammaMarker(client, Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER);
	}

	private void plaatsMammaMarker(Client client, String marker)
	{
		var huidigeMutatie = getHuidigeGbaMutatie(client);
		if (huidigeMutatie != null && client.getMammaDossier() != null && client.getMammaDossier().getLaatsteScreeningRonde() != null
			&& !StringUtils.contains(huidigeMutatie.getAanvullendeInformatie(), marker))
		{
			huidigeMutatie.setAanvullendeInformatie(marker + huidigeMutatie.getAanvullendeInformatie());
		}
	}

	private void verwerkTabelRegel(Vo107Bericht bericht)
	{
		if (bericht.getRubriekMap().containsKey(GbaRubriek.TITEL_CODE.getNummer()))
		{
			var titelCode = getStringUitBericht(bericht, GbaRubriek.TITEL_CODE);
			var clienten = clientService.getClientenMetTitel(titelCode);

			for (var client : clienten)
			{
				client.getPersoon().setTitel(getStringUitBericht(bericht, GbaRubriek.TITEL_OMSCHRIJVING));
			}

			hibernateService.saveOrUpdateAll(clienten);
		}
		else if (bericht.getRubriekMap().containsKey(GbaRubriek.GEMEENTE_CODE.getNummer()))
		{
			var gemeenteCode = getStringUitBericht(bericht, GbaRubriek.GEMEENTE_CODE);
			var gemeente = gemeenteRepository.findOneByCode(gemeenteCode).orElse(null);

			if (gemeente == null)
			{
				gemeente = new Gemeente();
				gemeente.setCode(gemeenteCode);
			}

			var naam = getStringUitBericht(bericht, GbaRubriek.GEMEENTE_NAAM);
			var beginDatum = getDateUitBericht(bericht, GbaRubriek.GEMEENTE_BEGINDATUM);
			var eindDatum = getDateUitBericht(bericht, GbaRubriek.GEMEENTE_EINDDATUM);

			var nieuweGemeenteCode = getStringUitBericht(bericht, GbaRubriek.NIEUWE_GEMEENTE_CODE);

			if (naam != null)
			{
				gemeente.setNaam(naam);
			}

			if (beginDatum != null)
			{
				gemeente.setBeginDatum(beginDatum);
			}

			if (eindDatum != null)
			{
				gemeente.setEindDatum(eindDatum);
			}

			if (nieuweGemeenteCode != null)
			{
				var nieuweGemeente = gemeenteRepository.findOneByCode(nieuweGemeenteCode).orElse(null);
				gemeente.setOpvolgGemeente(nieuweGemeente);
			}

			hibernateService.saveOrUpdate(gemeente);
		}
	}

	private boolean vulAdresMetGbaGegevens(BagAdres adres, Vo107Bericht bericht, Client client, GbaVerwerkingsLog verwerkingsLog)
	{

		var huisnummerString = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISNR);
		var postcode = getStringUitBericht(bericht, GbaRubriek.VERBP_POSTCODE);
		var huisletter = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISLETTER);
		var toevoeging = getStringUitBericht(bericht, GbaRubriek.VERBP_HUISNRTOEV);
		var aanduiding = getStringUitBericht(bericht, GbaRubriek.VERBP_AAND_HUISNR);
		var straat = getStringUitBericht(bericht, GbaRubriek.VERBP_STRAATNAAM);
		var gemeenteDeel = getStringUitBericht(bericht, GbaRubriek.VERBP_GEMEENTEDEEL);
		var woonplaats = getStringUitBericht(bericht, GbaRubriek.VERBP_WOONPLAATS);
		var locatieBeschrijving = getStringUitBericht(bericht, GbaRubriek.VERBP_LOC_BESCHR);
		var gemeenteCode = getCodeUitBericht(bericht, GbaRubriek.VERBP_GEMEENTE_INSCHR);
		var naamOpenbareRuimte = getStringUitBericht(bericht, GbaRubriek.VERBP_NAAMOPENBARERUIMTE);
		var identificatieCodeVerblijfplaats = getStringUitBericht(bericht, GbaRubriek.VERBP_IDVERBLIJFPLAATS);
		var identificatieCodeNummerAanduiding = getStringUitBericht(bericht, GbaRubriek.VERBP_IDNUMMERAANDUIDING);

		var verstrekking = bericht.isVerstrekking();

		adres.setLand(Land.NEDERLAND);

		Integer huisnummer = null;
		if (!Strings.isNullOrEmpty(huisnummerString))
		{
			huisnummer = Integer.valueOf(huisnummerString);
		}

		var adresGegevensGewijzigd = changeProperty(adres, "huisnummer", huisnummer, verstrekking || huisnummerString != null);
		adresGegevensGewijzigd |= changeProperty(adres, "postcode", PostcodeFormatter.formatPostcode(postcode, false), verstrekking || postcode != null);

		if (StringUtils.isBlank(huisletter))
		{
			adresGegevensGewijzigd |= changeProperty(adres, "huisletter", huisletter, verstrekking);
		}
		else
		{
			adresGegevensGewijzigd |= changeProperty(adres, "huisletter", huisletter.substring(0, 1), true);
		}

		adresGegevensGewijzigd |= changeProperty(adres, "huisnummerToevoeging", toevoeging, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "huisnummerAanduiding", aanduiding, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "straat", straat, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "gemeentedeel", gemeenteDeel, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "plaats", woonplaats, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "locatieBeschrijving", locatieBeschrijving, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "naamOpenbareRuimte", naamOpenbareRuimte, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "identificatieCodeVerblijfplaats", identificatieCodeVerblijfplaats, verstrekking);

		adresGegevensGewijzigd |= changeProperty(adres, "identificatieCodeNummerAanduiding", identificatieCodeNummerAanduiding, verstrekking);

		Gemeente gemeente = null;
		if (gemeenteCode != null)
		{
			gemeente = getOrCreateGemeente(bericht, client, verwerkingsLog, gemeenteCode);
		}
		adresGegevensGewijzigd |= changeProperty(adres, "gbaGemeente", gemeente, verstrekking || gemeenteCode != null);
		return adresGegevensGewijzigd;
	}

	private Gemeente getOrCreateGemeente(Vo107Bericht bericht, Client client, GbaVerwerkingsLog verwerkingsLog, String gemeenteCode)
	{
		var gemeente = gemeenteRepository.findOneByCode(gemeenteCode).orElse(null);
		if (gemeente == null)
		{
			gemeente = new Gemeente();
			gemeente.setCode(gemeenteCode);
			gemeente.setNaam(getStringUitBericht(bericht, GbaRubriek.VERBP_GEMEENTE_INSCHR));

			hibernateService.saveOrUpdate(client);

			createFout(client, verwerkingsLog, "Gemeente was niet bekend, toegevoegd. Gemeentecode: " + gemeenteCode, GbaFoutCategorie.INHOUDELIJK);
			logService.logGebeurtenis(LogGebeurtenis.GBA_IMPORT_GEMEENTE_TOEGEVOEGD, "Gemeente was niet bekend, toegevoegd. Gemeentecode: " + gemeenteCode);
			hibernateService.saveOrUpdate(gemeente);
		}
		return gemeente;
	}

	private boolean vulPersoonsGegevens(Client client, Vo107Bericht bericht, GbaVerwerkingsLog verwerkingsLog, boolean isNieuw)
	{
		var persoonsGegevensGewijzigd = false;

		var bsn = getStringUitBericht(bericht, GbaRubriek.PERS_BSN);
		var anummer = getStringUitBericht(bericht, GbaRubriek.PERS_A_NUMMER);
		var arecordAnummer = bericht.getString(Vo107_ArecordVeld.ANR);
		var tussenvoegselGeslachtsnaam = getStringUitBericht(bericht, GbaRubriek.PERS_VOORV_GESLACHTSNAAM);
		var geslachtsnaam = getStringUitBericht(bericht, GbaRubriek.PERS_GESLACHTSNAAM);
		var voornaam = getStringUitBericht(bericht, GbaRubriek.PERS_VOORNAMEN_01_02_10);
		var naamgebruik = getNaamGebruikUitBericht(bericht, GbaRubriek.PERS_NAAMGEBRUIK);
		var adelijkeTitel = getStringUitBericht(bericht, GbaRubriek.PERS_TITELPREDIKAAT);
		var codeTitel = getCodeUitBericht(bericht, GbaRubriek.PERS_TITELPREDIKAAT);

		var geboorteDatum = getDateUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		var geboorteDatumPrecisie = getDatumPrecisieUitBericht(bericht, GbaRubriek.PERS_GEBOORTEDATUM);
		var overlijdensDatum = getDateUitBericht(bericht, GbaRubriek.OVL_DATUM_OVERLIJDEN);

		var geslacht = getGeslachtUitBericht(bericht, GbaRubriek.PERS_GESLACHTSAANDUIDING);

		var datumVertrekUitNederland = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_VERTREK_NED);
		var datumVestigingInNederland = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_VESTIGING_NED);
		var geheim = getStringUitBericht(bericht, GbaRubriek.INSCH_INDICATIE_GEHEIM);
		var datumAanvangAdreshouding = getDateUitBericht(bericht, GbaRubriek.VERBP_DATUM_AANV_ADRESH);

		var registerGemeenteAkte = getCodeUitBericht(bericht, GbaRubriek.OVL_REGISTERGEMEENTE_AKTE);
		var akteNummerOverlijden = getStringUitBericht(bericht, GbaRubriek.OVL_AKTENUMMER);

		var verstrekking = bericht.isVerstrekking();

		var persoon = client.getPersoon();

		if (!Strings.isNullOrEmpty(bsn))
		{
			var oorspronkelijkBsn = persoon.getBsn();
			persoonsGegevensGewijzigd |= changeProperty(persoon, "bsn", bsn, false);
			if (persoonsGegevensGewijzigd)
			{
				plaatsBSNGewijzigdMarker(client, oorspronkelijkBsn, bsn);
			}
		}

		if (!Strings.isNullOrEmpty(arecordAnummer) && bericht.getBerichtType().equals(WA11))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "anummer", arecordAnummer, false);
		}
		else if (!Strings.isNullOrEmpty(anummer))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "anummer", anummer, false);
		}

		var isAchternaamGewijzigd = changeProperty(persoon, "achternaam", geslachtsnaam, verstrekking);
		persoonsGegevensGewijzigd |= isAchternaamGewijzigd;
		var imsGegevensGewijzigd = isAchternaamGewijzigd;

		var isTussenvoegselGewijzigd = changeProperty(persoon, "tussenvoegsel", tussenvoegselGeslachtsnaam, verstrekking);
		persoonsGegevensGewijzigd |= isTussenvoegselGewijzigd;
		imsGegevensGewijzigd |= isTussenvoegselGewijzigd;

		var isVoornaamGewijzigd = changeProperty(persoon, "voornaam", voornaam, verstrekking);
		persoonsGegevensGewijzigd |= isVoornaamGewijzigd;
		imsGegevensGewijzigd |= isVoornaamGewijzigd;

		persoonsGegevensGewijzigd |= changeProperty(persoon, "naamGebruik", naamgebruik, verstrekking);

		if (adelijkeTitel != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titel", adelijkeTitel, verstrekking);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titelCode", codeTitel, verstrekking || codeTitel != null);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titel", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "titelCode", null, true);
		}

		if (overlijdensDatum != null)
		{
			var isOverlijdensdatumGewijzigd = changeProperty(persoon, "overlijdensdatum", overlijdensDatum, verstrekking);
			persoonsGegevensGewijzigd |= isOverlijdensdatumGewijzigd;
			imsGegevensGewijzigd |= isOverlijdensdatumGewijzigd;
		}
		else if (getStringUitBericht(bericht, GbaRubriek.OVL_DATUM_OVERLIJDEN) != null || verstrekking)
		{
			var isOverlijdingsdatumGewijzigd = changeProperty(persoon, "overlijdensdatum", null, true);
			persoonsGegevensGewijzigd |= isOverlijdingsdatumGewijzigd;
			imsGegevensGewijzigd |= isOverlijdingsdatumGewijzigd;
		}

		var isGeslachtGewijzigd = changeProperty(persoon, "geslacht", geslacht, verstrekking || getStringUitBericht(bericht, GbaRubriek.PERS_GESLACHTSAANDUIDING) != null);
		persoonsGegevensGewijzigd |= isGeslachtGewijzigd;
		imsGegevensGewijzigd |= isGeslachtGewijzigd;

		if (geheim != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "indicatieGeheim", IndicatieGeheim.getByCode(geheim), verstrekking);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "indicatieGeheim", null, true);
		}

		if (StringUtils.isBlank(registerGemeenteAkte))
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "registerGemeenteAkteOverlijden", null, true);
		}
		else
		{
			var gemeente = getOrCreateGemeente(bericht, client, verwerkingsLog, registerGemeenteAkte);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "registerGemeenteAkteOverlijden", gemeente, verstrekking);
		}

		persoonsGegevensGewijzigd |= changeProperty(persoon, "akteNummerOverlijden", akteNummerOverlijden, verstrekking);

		if (datumAanvangAdreshouding != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAanvangAdreshouding", datumAanvangAdreshouding, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_AANV_ADRESH) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAanvangAdreshouding", null, true);
		}

		if (datumVestigingInNederland != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVestigingNederland", datumVestigingInNederland, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_VESTIGING_NED) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVestigingNederland", null, true);
		}

		if (datumVertrekUitNederland != null)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVertrokkenUitNederland", datumVertrekUitNederland, verstrekking);
		}
		else if (getStringUitBericht(bericht, GbaRubriek.VERBP_DATUM_VERTREK_NED) != null || verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumVertrokkenUitNederland", null, true);
		}

		var partnerschap = bericht.getMeestRecentPartnerschap();
		if (partnerschap != null)
		{

			if (verstrekking || partnerschap.isGeslachtsnaamPartnerInBericht())
			{
				persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", partnerschap.getGeslachtsnaamPartner(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", partnerschap.getVoorvoegselPartner(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", partnerschap.getPartnerschapSluiting(), true);
				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", partnerschap.getPartnerschapEinde(), true);
			}
			else if (partnerschap.isPartnerschapEindeInBericht())
			{
				if (partnerschap.isGeslachtsnaamPartnerInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", partnerschap.getGeslachtsnaamPartner(), true);
				}

				if (partnerschap.isVoorvoegselPartnerInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", partnerschap.getVoorvoegselPartner(), true);
				}

				if (partnerschap.isPartnerschapSluitingInBericht())
				{
					persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", partnerschap.getPartnerschapSluiting(), true);
				}

				persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", partnerschap.getPartnerschapEinde(), true);
			}
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerAchternaam", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "partnerTussenvoegsel", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumAangaanPartnerschap", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "datumOntbindingPartnerschap", null, true);
		}

		if (Strings.isNullOrEmpty(geslachtsnaam) && isNieuw)
		{
			persoon.setAchternaam("");

			hibernateService.saveOrUpdate(client);
			createFout(client, verwerkingsLog, "Verstrekking bevat geen geslachtsnaam, bsn: " + bsn, GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
		}

		if (geboorteDatum != null)
		{
			var isGeboortedatumGewijzigd = changeProperty(persoon, "geboortedatum", geboorteDatum, verstrekking);
			persoonsGegevensGewijzigd |= isGeboortedatumGewijzigd;
			imsGegevensGewijzigd |= isGeboortedatumGewijzigd;

			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatumPrecisie", geboorteDatumPrecisie, verstrekking);
		}
		else if (isNieuw)
		{

			hibernateService.saveOrUpdate(client);
			createFout(client, verwerkingsLog, "Verstrekking bevat geen geboortedatum, bsn: " + bsn, GbaFoutCategorie.INHOUDELIJK_ERNGSTIG);
		}
		else if (verstrekking)
		{
			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatum", null, true);
			persoonsGegevensGewijzigd |= changeProperty(persoon, "geboortedatumPrecisie", null, true);
		}

		if (imsGegevensGewijzigd)
		{
			plaatsIMSGegevensGewijzigdMarker(client);
		}
		return persoonsGegevensGewijzigd;
	}

	private boolean changeProperty(Object target, String property, Object newValue, boolean blankOnNull)
	{
		var propertyChanged = false;
		if (newValue != null || blankOnNull)
		{
			try
			{
				if (newValue != null && newValue.toString().trim().isEmpty())
				{
					newValue = null;
				}
				var oldValue = PropertyUtils.getProperty(target, property);
				propertyChanged = oldValue == null && newValue != null || oldValue != null && !oldValue.equals(newValue);

				if (propertyChanged)
				{

					if (newValue == null && oldValue != null && oldValue.toString().trim().isEmpty())
					{
						propertyChanged = false;
					}
					PropertyUtils.setProperty(target, property, newValue);
				}
			}
			catch (SecurityException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
			{
				LOG.error("changeProperty fail: {}", property, e);
			}
		}
		return propertyChanged;
	}

	public void createFout(Client client, GbaVerwerkingsLog verwerkingsLog, String fout, GbaFoutCategorie foutcat)
	{
		var gbaFoutRegel = new GbaFoutRegel();
		if (client != null)
		{
			gbaFoutRegel.setClient(client.getId());
		}
		gbaFoutRegel.setFout(fout);
		gbaFoutRegel.setFoutCategorie(foutcat);
		gbaFoutRegel.setVerwerkingsLog(verwerkingsLog);
		verwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	private NaamGebruik getNaamGebruikUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (!Strings.isNullOrEmpty(stringValue))
		{
			stringValue = stringValue.trim();
			return NaamGebruik.getNaamGebruikByGba(stringValue);
		}

		return null;
	}

	private String getStringUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var rubriek = bericht.getSingleRubriek(gbaRubriek.getNummer());
		if (rubriek == null)
		{
			return null;
		}
		return rubriek.getWaarde(VoxBrecordVeld.INH);
	}

	private String getCodeUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var rubriek = bericht.getSingleRubriek(gbaRubriek.getNummer());
		if (rubriek == null)
		{
			return null;
		}
		return rubriek.getWaarde(VoxBrecordVeld.CODE);
	}

	private Date getDateUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var stringValue = getStringUitBericht(bericht, gbaRubriek);
		if (stringValue == null || stringValue.startsWith("0000"))
		{
			return null;
		}
		return VoxHelper.convertToDate(stringValue);
	}

	private DatumPrecisie getDatumPrecisieUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (stringValue != null && stringValue.endsWith("0000"))
		{
			return DatumPrecisie.JAAR;
		}
		else if (stringValue != null && stringValue.endsWith("00"))
		{
			return DatumPrecisie.MAAND;
		}

		return DatumPrecisie.VOLLEDIG;
	}

	private Geslacht getGeslachtUitBericht(Vo107Bericht bericht, GbaRubriek gbaRubriek)
	{
		var stringValue = getStringUitBericht(bericht, gbaRubriek);

		if (stringValue == null)
		{
			return null;
		}

		if (stringValue.startsWith("O"))
		{
			return Geslacht.ONBEKEND;
		}
		else if (stringValue.startsWith("M"))
		{
			return Geslacht.MAN;
		}
		else if (stringValue.startsWith("V"))
		{
			return Geslacht.VROUW;
		}
		return null;
	}

	private Long getScreeningOrganisatie(Client client)
	{
		if (client == null || client.getPersoon() == null || client.getPersoon().getGbaAdres() == null || client.getPersoon().getGbaAdres().getGbaGemeente() == null
			|| client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie() == null)
		{
			return null;
		}

		return client.getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie().getId();
	}
}
