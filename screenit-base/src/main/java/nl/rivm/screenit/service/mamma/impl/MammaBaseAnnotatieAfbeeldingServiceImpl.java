package nl.rivm.screenit.service.mamma.impl;

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

import jakarta.persistence.EntityManager;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;
import nl.rivm.screenit.service.mamma.MammaBaseAnnotatieAfbeeldingService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MammaBaseAnnotatieAfbeeldingServiceImpl implements MammaBaseAnnotatieAfbeeldingService
{
	private final EntityManager entityManager;

	@Override
	@Transactional
	public void updateIconenInAfbeelding(List<MammaAnnotatieIcoon> iconen, MammaAnnotatieAfbeelding afbeelding)
	{
		var afbeeldingIconen = afbeelding.getIconen();
		afbeeldingIconen.clear();

		iconen.forEach(icoon ->
		{
			icoon.setAfbeelding(afbeelding);
			if (icoon.getId() == null)
			{
				afbeeldingIconen.add(icoon);
			}
			else
			{
				afbeeldingIconen.add(entityManager.merge(icoon));
			}
		});
	}
}
