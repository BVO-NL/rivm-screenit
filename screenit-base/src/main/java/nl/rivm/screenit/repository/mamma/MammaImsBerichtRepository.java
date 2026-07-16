package nl.rivm.screenit.repository.mamma;

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

import java.util.List;

import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;

public interface MammaImsBerichtRepository extends BaseJpaRepository<MammaIMSBericht>
{
	default boolean isBerichtAlOntvangen(String messageId)
	{
		return existsByMessageId(messageId);
	}

	boolean existsByMessageId(String messageId);

	@Query("""
		SELECT m.id
		FROM MammaIMSBericht m
		WHERE m.berichtStatus = nl.rivm.screenit.model.berichten.enums.BerichtStatus.NIEUW
		ORDER BY m.id ASC""")
	List<Long> getAlleNietVerwerkteImsBerichtIds();
}
