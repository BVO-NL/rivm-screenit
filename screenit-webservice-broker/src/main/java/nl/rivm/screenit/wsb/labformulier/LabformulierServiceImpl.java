package nl.rivm.screenit.wsb.labformulier;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.time.LocalDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.regex.Pattern;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.specification.cervix.CervixBMHKLaboratoriumSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.ws.labformulier.Labformulier;
import nl.rivm.screenit.ws.labformulier.LabformulierDate;
import nl.rivm.screenit.ws.labformulier.LabformulierService;
import nl.rivm.screenit.ws.labformulier.LabformulierServiceException_Exception;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebResult;
import jakarta.jws.WebService;
import jakarta.xml.ws.RequestWrapper;
import jakarta.xml.ws.ResponseWrapper;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@WebService(targetNamespace = "http://screenit.rivm.nl/", name = "LabformulierService")
@Slf4j
@RequiredArgsConstructor
public class LabformulierServiceImpl implements LabformulierService
{

	private final HibernateService hibernateService;

	private final CervixLabformulierService labformulierService;

	private final BmhkLaboratoriumRepository bmhkLaboratoriumRepository;

	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService preferenceService;

	private final LogService logService;

	private Integer afkapwaardeLabformulier;

	@Override
	@WebResult(name = "return")
	@RequestWrapper(localName = "labformulierScanned", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.Labformulier")
	@WebMethod
	@ResponseWrapper(localName = "labformulierScannedResponse", targetNamespace = "http://screenit.rivm.nl/", className = "nl.rivm.screenit.LabformulierScannedResponse")
	@Transactional(propagation = Propagation.REQUIRED)
	public void labformulierScanned(@WebParam(name = "labformulier") Labformulier scan) throws LabformulierServiceException_Exception
	{
		try
		{
			if (scan.getScanDatum() == null)
			{
				throw new LabformulierServiceException_Exception("scanDatum is null");
			}
			if (scan.getObjid() == null)
			{
				throw new LabformulierServiceException_Exception("objid is null");
			}
			else if (scan.getObjid().isEmpty())
			{
				throw new LabformulierServiceException_Exception("objid is empty");
			}
			if (scan.getBarcodes().isEmpty())
			{
				throw new LabformulierServiceException_Exception("barcodes is empty");
			}
			if (scan.getLabIdScanner() == null)
			{
				throw new LabformulierServiceException_Exception("labIdScanner is null");
			}
			else if (scan.getLabIdScanner().isEmpty())
			{
				throw new LabformulierServiceException_Exception("labIdScanner is empty");
			}
			if (scan.getDatumUitstrijkje() == null)
			{
				throw new LabformulierServiceException_Exception("datumUitstrijkje is null");
			}
			if (scan.getDatumLaatsteMenstruatie() == null)
			{
				throw new LabformulierServiceException_Exception("datumLaatsteMenstruatie is null");
			}
			var objid = new HashMap<String, Object>();
			objid.put("objid", scan.getObjid());
			if (hibernateService.getUniqueByParameters(CervixLabformulier.class, objid) != null)
			{
				throw new LabformulierServiceException_Exception("objid not unique");
			}
			var bmhkLaboratorium = bmhkLaboratoriumRepository.findOne(CervixBMHKLaboratoriumSpecification.heeftUserIdScanner(scan.getLabIdScanner()))
				.orElseThrow(() -> new LabformulierServiceException_Exception("Er kon geen lab worden gevonden bij labIdScanner"));
			afkapwaardeLabformulier = preferenceService.getInteger(PreferenceKey.AFKAPWAARDE_LABFORMULIER.name());
			if (afkapwaardeLabformulier == null)
			{
				throw new LabformulierServiceException_Exception("Interne fout: Afkapwaarde labformulier niet geconfigureerd");
			}

			var labformulier = new CervixLabformulier();
			labformulier.setLaboratorium(bmhkLaboratorium);
			labformulier.setScanDatum(scan.getScanDatum().toGregorianCalendar().getTime());
			labformulier.setObjid(scan.getObjid());

			var barcodes = scan.getBarcodes();
			var monsterIdAanwezig = false;
			for (var barcode : barcodes)
			{
				if (barcode.contains(Constants.LOCATIEID + "="))
				{
					var id = barcode.replace(Constants.LOCATIEID + "=", "").trim();
					labformulier.setHuisartsLocatie(hibernateService.get(CervixHuisartsLocatie.class, Long.valueOf(id)));
				}
				else if (barcode.contains("AGB="))
				{
					var id = barcode.replace("AGB=", "").trim();
					labformulier.setHuisartsLocatie(hibernateService.get(CervixHuisartsLocatie.class, Long.valueOf(id)));
				}
				else if (Pattern.matches("[0-9]+", barcode))
				{
					if (!monsterIdAanwezig)
					{
						labformulier.setBarcode(barcode.trim());
						monsterIdAanwezig = true;
					}
					else
					{
						labformulier.setBarcode(null);
					}
				}
			}

			labformulier.setDatumUitstrijkje(getDate(scan.getDatumUitstrijkje()));
			labformulier.setKlachtenGeen(getVink(scan.isKlachtenGeen(), scan.getKlachtenGeenConf()));
			labformulier.setKlachtenContactbloedingen(getVink(scan.isKlachtenContactbloedingen(), scan.getKlachtenContactbloedingenConf()));
			labformulier.setKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(
				getVink(scan.isKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(), scan.getKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaakConf()));
			labformulier.setKlachtenIntermenstrueelBloedverlies(getVink(scan.isKlachtenIntermenstrueelBloedverlies(), scan.getKlachtenIntermenstrueelBloedverliesConf()));
			labformulier.setKlachtenPostmenopauzaalBloedverlies(getVink(scan.isKlachtenPostmenopauzaalBloedverlies(), scan.getKlachtenPostmenopauzaalBloedverliesConf()));
			labformulier.setKlachtenAndersNamelijk(getVinkTekst(scan.isKlachtenAndersNamelijk(), scan.getKlachtenAndersNamelijkConf(), scan.isKlachtenAndersNamelijkTekst(),
				scan.getKlachtenAndersNamelijkTekstConf()));
			labformulier.setMenstruatieNormaal(getVink(scan.isMenstruatieNormaal(), scan.getMenstruatieNormaalConf()));
			labformulier.setMenstruatieGeenMenstruatie(getVink(scan.isMenstruatieGeenMenstruatie(), scan.getMenstruatieGeenMenstruatieConf()));
			labformulier.setMenstruatieMenopauze(getVink(scan.isMenstruatieMenopauze(), scan.getMenstruatieMenopauzeConf()));
			labformulier.setMenstruatiePostmenopauze(getVink(scan.isMenstruatiePostmenopauze(), scan.getMenstruatiePostmenopauzeConf()));
			labformulier.setDatumLaatsteMenstruatie(getDate(scan.getDatumLaatsteMenstruatie()));
			labformulier.setAnticonceptieGeen(getVink(scan.isAnticonceptieGeen(), scan.getAnticonceptieGeenConf()));
			labformulier.setAnticonceptiePil(getVink(scan.isAnticonceptiePil(), scan.getAnticonceptiePilConf()));
			labformulier.setAnticonceptieIudKoper(getVink(scan.isAnticonceptieIudKoper(), scan.getAnticonceptieIudKoperConf()));
			labformulier.setAnticonceptieIudMirena(getVink(scan.isAnticonceptieIudMirena(), scan.getAnticonceptieIudMirenaConf()));
			labformulier.setAnticonceptieAnders(getVink(scan.isAnticonceptieAnders(), scan.getAnticonceptieAndersConf()));
			labformulier
				.setGebruikHormonenJaVanwegeOvergangsklachten(getVink(scan.isGebruikHormonenJaVanwegeOvergangsklachten(), scan.getGebruikHormonenJaVanwegeOvergangsklachtenConf()));
			labformulier.setGebruikHormonenJaVanwegeBorstkanker(getVink(scan.isGebruikHormonenJaVanwegeBorstkanker(), scan.getGebruikHormonenJaVanwegeBorstkankerConf()));
			labformulier.setGebruikHormonenJaVanwege(getVinkTekst(scan.isGebruikHormonenJaVanwege(), scan.getGebruikHormonenJaVanwegeConf(), scan.isGebruikHormonenJaVanwegeTekst(),
				scan.getGebruikHormonenJaVanwegeTekstConf()));
			labformulier.setGebruikHormonenGeen(getVink(scan.isGebruikHormonenGeen(), scan.getGebruikHormonenGeenConf()));
			labformulier.setAspectCervixNormaal(getVink(scan.isAspectCervixNormaal(), scan.getAspectCervixNormaalConf()));
			labformulier.setAspectCervixNietGezien(getVink(scan.isAspectCervixNietGezien(), scan.getAspectCervixNietGezienConf()));
			labformulier
				.setAspectCervixAbnormaalOfVerdachtePortio(getVinkTekst(scan.isAspectCervixAbnormaalOfVerdachtePortio(), scan.getAspectCervixAbnormaalOfVerdachtePortioConf(),
					scan.isAspectCervixAbnormaalOfVerdachtePortioTekst(), scan.getAspectCervixAbnormaalOfVerdachtePortioTekstConf()));
			labformulier.setOpmerkingen(getVink(scan.isOpmerkingenTekst(), scan.getOpmerkingenTekstConf()));

			labformulier.setStatus(CervixLabformulierStatus.GESCAND);
			labformulier.setStatusDatum(dateSupplier.getDate());

			hibernateService.saveOrUpdate(labformulier);
			labformulierService.koppelEnBewaarLabformulier(labformulier);

			logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_GESCAND, new LogEvent("Laboratorium: " + labformulier.getLaboratorium().getNaam()
				+ ", Monster-id: " + labformulier.getBarcode()
				+ ", ObjectID: " + labformulier.getObjid()), Bevolkingsonderzoek.CERVIX);
		}
		catch (Exception e)
		{
			LOG.error("Exceptie tijdens labformulier scanned", e);
			if (e.getClass().equals(LabformulierServiceException_Exception.class))
			{
				throw (LabformulierServiceException_Exception) e;
			}
			else
			{
				throw new LabformulierServiceException_Exception("Interne fout: " + e.getMessage());
			}
		}
	}

	private boolean getVink(Boolean value, Integer confidence)
	{
		return confidence != null && confidence >= afkapwaardeLabformulier ? value : false;
	}

	private boolean getVinkTekst(Boolean value, Integer confidence, Boolean tekstValue, Integer tekstConfidence)
	{
		return Boolean.TRUE.equals(value) && confidence != null && confidence >= afkapwaardeLabformulier
			|| Boolean.TRUE.equals(tekstValue) && tekstConfidence != null && tekstConfidence >= afkapwaardeLabformulier;
	}

	private Date getDate(LabformulierDate labformulierDate)
	{
		var dagConf = labformulierDate.getDagConf();
		Integer dag = null;
		if (dagConf != null && dagConf >= afkapwaardeLabformulier)
		{
			try
			{
				dag = Integer.parseInt(labformulierDate.getDag());
				if (dag < 1 && dag > 31)
				{
					dag = null;
				}
			}
			catch (NullPointerException | NumberFormatException ignored)
			{
			}
		}

		var maandConf = labformulierDate.getMaandConf();
		Integer maand = null;
		if (maandConf != null && maandConf >= afkapwaardeLabformulier)
		{
			try
			{
				maand = Integer.parseInt(labformulierDate.getMaand());
				if (maand < 1 && maand > 12)
				{
					maand = null;
				}
			}
			catch (NullPointerException | NumberFormatException e)
			{
			}
		}

		var jaarConf = labformulierDate.getJaarConf();
		Integer jaar = null;
		if (jaarConf != null && jaarConf >= afkapwaardeLabformulier)
		{
			try
			{
				jaar = Integer.parseInt(labformulierDate.getJaar()) + 2000;
				if (jaar < 2014 && jaar > 2099)
				{
					jaar = null;
				}
			}
			catch (NullPointerException | NumberFormatException e)
			{
			}
		}

		var datumConf = labformulierDate.getDatumConf();

		if (datumConf != null && datumConf >= afkapwaardeLabformulier)
		{
			if (dag == null || dagConf < datumConf)
			{
				try
				{
					var datumDag = Integer.parseInt(labformulierDate.getDatum().substring(0, 2));
					if (!(datumDag < 1 && datumDag > 31))
					{
						dag = datumDag;
					}
				}
				catch (NullPointerException | IndexOutOfBoundsException | NumberFormatException ignored)
				{
				}
			}

			if (maand == null || maandConf < datumConf)
			{
				try
				{
					var datumMaand = Integer.parseInt(labformulierDate.getDatum().substring(2, 4));
					if (!(datumMaand < 1 && datumMaand > 12))
					{
						maand = datumMaand;
					}
				}
				catch (NullPointerException | IndexOutOfBoundsException | NumberFormatException ignored)
				{
				}
			}

			if (jaar == null || jaarConf < datumConf)
			{
				try
				{
					var datumJaar = Integer.parseInt(labformulierDate.getDatum().substring(6, 8)) + 2000;
					if (!(datumJaar < 2014 && datumJaar > 2099))
					{
						jaar = datumJaar;
					}
				}
				catch (NullPointerException | IndexOutOfBoundsException | NumberFormatException ignored)
				{
				}
			}
		}

		if (dag != null && maand != null && jaar != null)
		{
			try
			{
				return DateUtil.toUtilDate(LocalDateTime.of(jaar, maand, dag, 0, 0));
			}
			catch (Exception ignored)
			{
			}
		}
		return null;
	}
}
