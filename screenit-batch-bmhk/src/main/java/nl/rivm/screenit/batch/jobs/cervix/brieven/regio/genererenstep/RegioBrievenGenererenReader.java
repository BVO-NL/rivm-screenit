package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.specification.algemeen.BriefSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.cervix.CervixRegioBriefSpecification.heeftScreeningsorganisatieId;

@Component
public class RegioBrievenGenererenReader extends BaseSpecificationScrollableResultReader<CervixRegioBrief>
{
	@Autowired
	private BaseBriefService briefService;

	@Override
	protected Specification<CervixRegioBrief> createSpecification()
	{
		var screeningsorganisatieId = briefService.isOverbruggingssituatieParagonStarted() ?
			null :
			getStepExecutionContext().getLong(RegioBrievenGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
		return heeftScreeningsorganisatieId(screeningsorganisatieId)
			.and(BriefSpecification.isNietGegenereerd())
			.and(BriefSpecification.isNietVervangen())
			.and(BriefSpecification.isNietTegengehouden())
			.and(BriefSpecification.heeftBriefType(BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS));
	}
}
