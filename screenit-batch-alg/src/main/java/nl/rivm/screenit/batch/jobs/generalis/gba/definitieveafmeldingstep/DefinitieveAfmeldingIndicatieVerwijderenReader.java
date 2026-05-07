package nl.rivm.screenit.batch.jobs.generalis.gba.definitieveafmeldingstep;

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

import java.util.Collections;
import java.util.List;

import jakarta.persistence.criteria.JoinType;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.generalis.gba.abstractindicatieverwijderenvoordoelgroepstep.AbstractIndicatieVerwijderenVoorDoelgroepReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftCervixDossier;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftMammaDossier;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.isAangemeld;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.valtBuitenLeeftijd;

@Component
@RequiredArgsConstructor
public class DefinitieveAfmeldingIndicatieVerwijderenReader extends AbstractIndicatieVerwijderenVoorDoelgroepReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		return super.createSpecification().and(isVoorAlleBvosBinnenDoelgroepAfgemeld());
	}

	private Specification<Client> isVoorAlleBvosBinnenDoelgroepAfgemeld()
	{
		var peildatum = currentDateSupplier.getLocalDate();

		var bmhkMinLeeftijd = CervixLeeftijdcategorie.minimumLeeftijd() - 1;
		var bmhkMaxLeeftijd = CervixLeeftijdcategorie._70.getLeeftijd();
		var bkMinLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name()) - 1;
		var bkMaxLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		var dkMinLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name()) - 1;
		var dkMaxLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		var alleMinLeeftijden = List.of(bmhkMinLeeftijd, bkMinLeeftijd, dkMinLeeftijd);
		var alleMaxLeeftijden = List.of(bmhkMaxLeeftijd, bkMaxLeeftijd, dkMaxLeeftijd);

		var clientValtBuitenBmhkDoelgroepleeftijd = valtBuitenLeeftijd(bmhkMinLeeftijd, bmhkMaxLeeftijd, peildatum).withRoot(getPersoonJoin());
		var clientValtBuitenBkDoelgroepleeftijd = valtBuitenLeeftijd(bkMinLeeftijd, bkMaxLeeftijd, peildatum).withRoot(getPersoonJoin());
		var clientValtBuitenDkDoelgroepleeftijd = valtBuitenLeeftijd(dkMinLeeftijd, dkMaxLeeftijd, peildatum).withRoot(getPersoonJoin());

		var clientValtBuitenAlleDoelgroepen = valtBuitenLeeftijd(Collections.min(alleMinLeeftijden), Collections.max(alleMaxLeeftijden), peildatum).withRoot(getPersoonJoin());

		var indicatieNietNodigVoorCervix = clientValtBuitenBmhkDoelgroepleeftijd.or(isCervixAfgemeldOfGeenDossier());
		var indicatieNietNodigVoorMamma = clientValtBuitenBkDoelgroepleeftijd.or(isMammaAfgemeldOfGeenDossier());
		var indicatieNietNodigVoorColon = clientValtBuitenDkDoelgroepleeftijd.or(isColonAfgemeld());

		var clientValtBuitenAlleDoelgroepenEnIsAfgemeldVoorAlleBVOs = clientValtBuitenAlleDoelgroepen.and(isCervixAfgemeldOfGeenDossier()).and(isMammaAfgemeldOfGeenDossier())
			.and(isColonAfgemeld());

		var binnenMinstensEenDoelgroepEnGeenIndicatieNodig = Specification.not(clientValtBuitenAlleDoelgroepen).and(indicatieNietNodigVoorCervix).and(indicatieNietNodigVoorMamma)
			.and(indicatieNietNodigVoorColon);

		return clientValtBuitenAlleDoelgroepenEnIsAfgemeldVoorAlleBVOs.or(binnenMinstensEenDoelgroepEnGeenIndicatieNodig);
	}

	private static ExtendedSpecification<Client> isCervixAfgemeldOfGeenDossier()
	{
		return isAangemeld(false).with(Client_.cervixDossier, JoinType.LEFT)
			.or(not(heeftCervixDossier()));
	}

	private static ExtendedSpecification<Client> isMammaAfgemeldOfGeenDossier()
	{
		return isAangemeld(false).with(Client_.mammaDossier, JoinType.LEFT)
			.or(not(heeftMammaDossier()));
	}

	private static ExtendedSpecification<Client> isColonAfgemeld()
	{
		return isAangemeld(false).with(Client_.colonDossier);
	}
}
