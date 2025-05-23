package nl.rivm.screenit.huisartsenportaal.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import java.time.Duration;
import java.time.Instant;
import java.util.List;

import jakarta.persistence.EntityManagerFactory;

import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordWijzigenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;

import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.RevisionType;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;

@Component
public class WachtwoordWijzigenValidator extends BaseWachtwoordValidator<WachtwoordWijzigenDto>
{

	@Autowired
	private EntityManagerFactory entityManagerFactory;

	@Autowired
	private AuthenticatieService authenticatieService;

	@Override
	public void validateTarget(WachtwoordWijzigenDto target, Errors errors)
	{
		Huisarts huisarts = getIngelogdeHuisarts();
		if (target.getOudeWachtwoord() == null && huisarts.getInlogCode() == null)
		{
			errors.reject("error.oudepassword.null", "Het huidige wachtwoord dient ingevuld te zijn.");
		}
		if (huisarts != null && target.getNieuweWachtwoord() != null && target.getNieuweWachtwoordControle() != null
			&& (target.getOudeWachtwoord() != null || huisarts.getInlogCode() != null) && InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{
			controleerOudeWachtwoordEnControleWachtwoord(huisarts, target, errors);
			controleerGebruikersnaamInWachtwoord(huisarts.getGebruikersnaam(), target.getNieuweWachtwoord(), errors);
			controleerTekenEisen(target.getNieuweWachtwoord(), errors);
			controleerHergebruikOudeWachtwoord(target.getNieuweWachtwoord(), errors);
		}
	}

	void controleerOudeWachtwoordEnControleWachtwoord(Huisarts huisarts, WachtwoordWijzigenDto target, Errors errors)
	{
		String encodedPassword = huisarts.getPassword();
		if (huisarts.getInlogCode() == null)
		{
			if (!authenticatieService.controleerWachtwoord(target.getOudeWachtwoord(), encodedPassword))
			{
				errors.reject("error.oudepassword", "Het huidige wachtwoord is onjuist.");
			}
		}
		if (!target.getNieuweWachtwoord().equals(target.getNieuweWachtwoordControle()))
		{
			errors.reject("error.password.equals", "De twee nieuwe wachtwoorden zijn niet gelijk.");
		}
	}

	void controleerHergebruikOudeWachtwoord(String nieuwWachtwoord, Errors errors)
	{
		var vorigeWachtwoorden = getVorigeWachtwoordenAfgelopenJaren(getIngelogdeHuisarts(), 2);
		if (vorigeWachtwoorden != null && vorigeWachtwoorden.stream().anyMatch(oudeWachtwoord -> authenticatieService.controleerWachtwoord(nieuwWachtwoord, oudeWachtwoord)))
		{
			errors.reject("error.password.hergebruik", "Het nieuwe wachtwoord mag niet gelijk zijn aan een van de laatste 2 wachtwoorden.");
		}
	}

	protected List<String> getVorigeWachtwoordenAfgelopenJaren(Huisarts huisarts, int jaren)
	{
		AuditReader auditReader = AuditReaderFactory.get(entityManagerFactory.createEntityManager());
		AuditQuery query = auditReader.createQuery().forRevisionsOfEntity(huisarts.getClass(), false, true);

		query.add(AuditEntity.id().eq(huisarts.getHuisartsportaalId()));
		query.add(AuditEntity.revisionType().eq(RevisionType.MOD));
		query.add(AuditEntity.revisionProperty("timestamp").gt(Instant.now().minus(Duration.ofDays(365L * jaren)).toEpochMilli()));

		return (List<String>) query.getResultList().stream().map((Object auditRow) ->
			{
				Huisarts huisartsAtRevision = (Huisarts) ((Object[]) auditRow)[0];
				return huisartsAtRevision.getPassword();
			})
			.limit(2)
			.toList();
	}

}
