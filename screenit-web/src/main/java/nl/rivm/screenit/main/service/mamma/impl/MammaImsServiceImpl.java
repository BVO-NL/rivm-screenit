package nl.rivm.screenit.main.service.mamma.impl;

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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.mamma.MammaImsErrorType;
import nl.rivm.screenit.main.service.mamma.MammaImsService;
import nl.rivm.screenit.mamma.imsapi.model.FhirCodableConcept;
import nl.rivm.screenit.mamma.imsapi.model.FhirCoding;
import nl.rivm.screenit.mamma.imsapi.model.FhirContext;
import nl.rivm.screenit.mamma.imsapi.model.FhirFocus;
import nl.rivm.screenit.mamma.imsapi.model.FhirIdentifiableEntity;
import nl.rivm.screenit.mamma.imsapi.model.FhirIdentifier;
import nl.rivm.screenit.mamma.imsapi.model.FhirImagingStudy;
import nl.rivm.screenit.mamma.imsapi.model.FhirPatientStudy;
import nl.rivm.screenit.mamma.imsapi.model.FhirUserSession;
import nl.rivm.screenit.mamma.imsapi.model.FhirWorklistItem;
import nl.rivm.screenit.mamma.imsapi.service.MammaImsLaunchUrlGenerator;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammobridgeFocusMode;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true) 
@RequiredArgsConstructor
public class MammaImsServiceImpl implements MammaImsService
{
	private static final ObjectMapper mapper = new ObjectMapper();

	private final HibernateService hibernateService;

	private final LogService logService;

	private final SimplePreferenceService preferenceService;

	private final MammaImsLaunchUrlGenerator launchUrlGenerator;

	@Override
	public String createDesktopSyncMessage(Medewerker medewerker, MammobridgeRole role, Long huidigeOnderzoekId, List<Long> komendeBeoordelingIds, MammobridgeFocusMode focusMode)
	{
		var userSession = createUserSession(medewerker, role);

		var huidigeOnderzoek = hibernateService.get(MammaOnderzoek.class, huidigeOnderzoekId);

		userSession.setFocus(createFocus(huidigeOnderzoek, focusMode));

		var worklist = createWorklist(komendeBeoordelingIds, focusMode);

		var upcomingCasesContext = createUpcomingCasesContext(worklist);
		upcomingCasesContext.setLaunchUrl(desktopSyncLaunchUrl(medewerker, role, getBsn(huidigeOnderzoek), getAccessionNumber(huidigeOnderzoek, focusMode)));
		userSession.setContext(upcomingCasesContext);

		return toJsonString(userSession);
	}

	@Override
	public String createEmptyDesktopSyncMessage(Medewerker medewerker, MammobridgeRole role)
	{
		var userSession = createUserSession(medewerker, role);
		userSession.setFocus(createEmptyFocus());
		userSession.setContext(createUpcomingCasesContext(Collections.emptyList()));
		return toJsonString(userSession);
	}

	@Override
	public String createAllImagesSeenMessage(Medewerker medewerker, MammobridgeRole role, MammaOnderzoek onderzoek, MammobridgeFocusMode focusMode)
	{
		var userSession = createUserSession(medewerker, role);
		var focus = createFocus(onderzoek, focusMode);
		var context = createContext("layoutImages", "requestLayoutsImagesSeenCurrentFocus");
		userSession.setFocus(focus);
		userSession.setContext(context);
		return toJsonString(userSession);
	}

	@Override
	public FhirUserSession parseFhirMessage(String json) throws IOException
	{
		return mapper.readValue(json, FhirUserSession.class);
	}

	@Override
	public String createLogonMessage(Medewerker medewerker, MammobridgeRole role)
	{
		var userSession = createUserSession(medewerker, role);
		userSession.setContext(createLogonContext(medewerker, role));
		return toJsonString(userSession);
	}

	@Override
	public String createLogoffMessage(Medewerker medewerker, MammobridgeRole role)
	{
		var userSession = createUserSession(medewerker, role);
		userSession.setContext(createLogoffContext());

		return toJsonString(userSession);
	}

	private FhirUserSession createUserSession(Medewerker medewerker, MammobridgeRole role)
	{
		var userSession = new FhirUserSession();
		userSession.setUser(createUser(medewerker, role));
		return userSession;
	}

	private FhirFocus createFocus(MammaOnderzoek onderzoek, MammobridgeFocusMode focusMode)
	{
		var focus = new FhirFocus();
		setPatientStudy(focus, onderzoek, focusMode);
		return focus;
	}

	private FhirFocus createEmptyFocus()
	{
		var focus = new FhirFocus();
		focus.setPatient(createPatient(""));
		focus.setImagingStudy(createStudy(""));
		return focus;
	}

	private List<FhirWorklistItem> createWorklist(List<Long> komendeBeoordelingIds, MammobridgeFocusMode focusMode)
	{
		List<FhirWorklistItem> worklist = new ArrayList<>();
		for (var beoordelingId : komendeBeoordelingIds)
		{
			worklist.add(createWorklistItem(beoordelingId, focusMode));
		}
		return worklist;
	}

	private FhirWorklistItem createWorklistItem(Long beoordelingId, MammobridgeFocusMode focusMode)
	{
		var beoordeling = hibernateService.get(MammaBeoordeling.class, beoordelingId);
		var worklistItem = new FhirWorklistItem();
		setPatientStudy(worklistItem, beoordeling.getOnderzoek(), focusMode);
		return worklistItem;
	}

	private void setPatientStudy(FhirPatientStudy patientStudy, MammaOnderzoek onderzoek, MammobridgeFocusMode focusMode)
	{
		patientStudy.setPatient(createPatient(getBsn(onderzoek)));
		patientStudy.setImagingStudy(createStudy(getAccessionNumber(onderzoek, focusMode)));
	}

	private String getBsn(MammaOnderzoek onderzoek)
	{
		return getClient(onderzoek).getPersoon().getBsn();
	}

	private Client getClient(MammaOnderzoek onderzoek)
	{
		return getScreeningRonde(onderzoek).getDossier().getClient();
	}

	private MammaScreeningRonde getScreeningRonde(MammaOnderzoek onderzoek)
	{
		return onderzoek.getAfspraak().getUitnodiging().getScreeningRonde();
	}

	private String getAccessionNumber(MammaOnderzoek onderzoek, MammobridgeFocusMode focusMode)
	{
		var screeningRonde = getScreeningRonde(onderzoek);
		return switch (focusMode)
		{
			case ALLEEN_BVO_BEELDEN -> Long.toString(screeningRonde.getUitnodigingsNr());
			case INCLUSIEF_UPLOAD_BEELDEN -> Long.toString(bepaalAccNrInclusiefUploads(screeningRonde));
		};
	}

	private Long bepaalAccNrInclusiefUploads(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getUploadBeeldenVerzoeken().stream()
			.filter(uv -> uv.getStatus() == MammaUploadBeeldenVerzoekStatus.VERWERKT)
			.flatMap(uv -> uv.getUploadPogingen().stream())
			.filter(up -> up.getIlmStatus() == MammaMammografieIlmStatus.BESCHIKBAAR)
			.max(Comparator.comparing(MammaUploadBeeldenPoging::getCreatieDatum))
			.map(MammaUploadBeeldenPoging::getAccessionNumber)
			.orElse(screeningRonde.getUitnodigingsNr());
	}

	private FhirIdentifiableEntity createUser(Medewerker username, MammobridgeRole role)
	{
		var user = new FhirIdentifiableEntity();
		user.setIdentifier(createIdentifier("ScreenIT", username.getGebruikersnaam()));
		user.getIdentifier().setType(createRole(role));
		return user;
	}

	private FhirCodableConcept createRole(MammobridgeRole role)
	{
		var type = new FhirCodableConcept();
		type.setCoding(new FhirCoding());
		type.getCoding().setCode(role.getIds7Role());
		type.getCoding().setSystem("ScreenIT");
		return type;
	}

	private FhirIdentifiableEntity createPatient(String bsn)
	{
		var patient = new FhirIdentifiableEntity();
		patient.setIdentifier(createIdentifier("NLMINBIZA", bsn));
		return patient;
	}

	private FhirImagingStudy createStudy(String accessionNumber)
	{
		var study = new FhirImagingStudy();
		study.setAccession(createIdentifier("ScreenIT", accessionNumber));
		study.setIdentifier(createIdentifier("ScreenIT", accessionNumber));
		return study;
	}

	private FhirContext createUpcomingCasesContext(List<FhirWorklistItem> worklist)
	{
		var context = createContext("Worklist", "UpcomingCases");
		context.setWorklist(worklist);
		return context;
	}

	private FhirContext createLogonContext(Medewerker medewerker, MammobridgeRole role)
	{
		var context = createContext("Session", "LogOn");
		context.setLaunchUrl(logonLaunchUrl(medewerker, role));
		return context;
	}

	private FhirContext createLogoffContext()
	{
		return createContext("Session", "LogOff");
	}

	private FhirContext createContext(String type, String value)
	{
		var context = new FhirContext();
		context.setType(type);
		context.setValue(value);
		return context;
	}

	private FhirIdentifier createIdentifier(String system, String value)
	{
		var identifier = new FhirIdentifier();
		identifier.setSystem(system);
		identifier.setValue(value);
		return identifier;
	}

	private String logonLaunchUrl(Medewerker medewerker, MammobridgeRole role)
	{
		var launchUrlPassword = preferenceService.getString(PreferenceKey.MAMMA_IMS_LAUNCH_URL_PASSWORD.name());
		boolean launchUrlSha1Mode = preferenceService.getBoolean(PreferenceKey.MAMMA_IMS_LAUNCH_URL_SHA1_MODE.name(), true);

		return launchUrlGenerator.generateLoginLaunchUrl(launchUrlPassword, medewerker.getGebruikersnaam(), role.getIds7Role(), launchUrlSha1Mode);
	}

	private String desktopSyncLaunchUrl(Medewerker medewerker, MammobridgeRole role, String bsn, String accessionNumber)
	{
		var launchUrlPassword = preferenceService.getString(PreferenceKey.MAMMA_IMS_LAUNCH_URL_PASSWORD.name());
		boolean launchUrlSha1Mode = preferenceService.getBoolean(PreferenceKey.MAMMA_IMS_LAUNCH_URL_SHA1_MODE.name(), true);

		return launchUrlGenerator.generateDesktopSyncLaunchUrl(launchUrlPassword, medewerker.getGebruikersnaam(), role.getIds7Role(), bsn, accessionNumber, launchUrlSha1Mode);
	}

	private String toJsonString(FhirUserSession userSession)
	{
		try
		{
			var objectMapper = new ObjectMapper();
			return objectMapper.writeValueAsString(userSession);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalStateException("Cannot convert UserSession to Json", e);
		}
	}

	@Override
	public String handleError(String error, OrganisatieMedewerker organisatieMedewerker, StringResolver stringResolver, Long onderzoekId)
	{
		var errorType = MammaImsErrorType.findForCode(error);
		var foutmelding = stringResolver.resolveString(errorType.getMeldingProperty());
		logService.logGebeurtenis(errorType.getLogGebeurtenis(), new LogEvent(foutmelding + " (" + error + ")"), organisatieMedewerker, getClientVoorLogGebeurtenis(onderzoekId));
		return foutmelding;
	}

	private Client getClientVoorLogGebeurtenis(Long onderzoekId)
	{
		return onderzoekId != null ? getClient(hibernateService.get(MammaOnderzoek.class, onderzoekId)) : null;
	}
}
