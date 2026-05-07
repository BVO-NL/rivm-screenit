package nl.rivm.screenit.batch.jobs.generalis.gba.transgenderstep;

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

import jakarta.persistence.criteria.JoinType;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.generalis.gba.abstractindicatieverwijderenvoordoelgroepstep.AbstractIndicatieVerwijderenVoorDoelgroepReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.colon.ColonDossierSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.valtBuitenLeeftijd;

@Component
@RequiredArgsConstructor
public class TransgenderIndicatieVerwijderenReader extends AbstractIndicatieVerwijderenVoorDoelgroepReader
{
	private static final int MAANDEN_NA_SIGNALERING_BRIEF = 6;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		return super.createSpecification()
			.and(heeftSignaleringGenderBriefOuderDanZesMaanden())
			.and(heeftSelectieblokkadeOpBkEnBmhk())
			.and(valtBuitenDkLeeftijdOfHeeftDefinitieveAfmelding());
	}

	private Specification<Client> heeftSignaleringGenderBriefOuderDanZesMaanden()
	{
		var peilmoment = currentDateSupplier.getLocalDate().minusMonths(MAANDEN_NA_SIGNALERING_BRIEF);
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(AlgemeneBrief.class);
			subquery.select(cb.literal(1L)).where(
				cb.and(
					cb.equal(subRoot.get(AlgemeneBrief_.client), r),
					cb.equal(subRoot.get(AlgemeneBrief_.briefType), BriefType.CLIENT_SIGNALERING_GENDER),
					cb.lessThan(subRoot.get(AlgemeneBrief_.creatieDatum), DateUtil.toUtilDate(peilmoment))
				)
			);
			return cb.exists(subquery);
		};
	}

	private Specification<Client> heeftSelectieblokkadeOpBkEnBmhk()
	{
		var heeftBkSelectieblokkade = DossierSpecification.heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).with(Client_.mammaDossier, JoinType.LEFT);
		var heeftBmhkSelectieblokkade = DossierSpecification.heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE)
			.with(Client_.cervixDossier, JoinType.LEFT);
		return heeftBkSelectieblokkade.and(heeftBmhkSelectieblokkade);
	}

	private Specification<Client> valtBuitenDkLeeftijdOfHeeftDefinitieveAfmelding()
	{
		var dkMinLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		var dkMaxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		var peildatum = currentDateSupplier.getLocalDate();
		var heeftDefinitieveAfmeldingDk = DossierSpecification.<ColonDossier> isAangemeld(false)
			.and(ColonDossierSpecification.heeftGeenLopendeLaatsteScreeningronde())
			.with(Client_.colonDossier, JoinType.LEFT);
		return valtBuitenLeeftijd(dkMinLeeftijd, dkMaxLeeftijd, peildatum).with(Client_.persoon).or(heeftDefinitieveAfmeldingDk);
	}
}
