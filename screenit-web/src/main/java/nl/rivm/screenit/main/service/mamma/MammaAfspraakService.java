package nl.rivm.screenit.main.service.mamma;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaBulkVerzettenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

public interface MammaAfspraakService
{
	Map<MammaScreeningsEenheid, List<LocalDate>> getAfspraakDatums(MammaBlokkade blokkade);

	Date getDatumEersteGeplandeAfspraak(Long standplaatsPeriodeId);

	Date getDatumLaatsteGeplandeAfspraak(Long standplaatsPeriodeId);

	void bulkVerzetten(IMammaBulkVerzettenFilter filter, List<MammaAfspraak> afspraken, Account account, LocalDate date);

	void verzetAfsprakenNaarStandplaatsPlusBrievenKlaarzettenVoorAfdrukken(Map<Long, List<Long>> afsprakenTeVerplatsen, Account account);

	boolean kortVoorVolgendeRonde(MammaAfspraak afspraak);

	String controleerAfspraakInAndereLocatie(MammaKandidaatAfspraakDto kandidaatAfspraakDto, MammaDossier dossier);

	boolean magBevestigingsbriefAanmaken(MammaAfspraak afspraak);
}
