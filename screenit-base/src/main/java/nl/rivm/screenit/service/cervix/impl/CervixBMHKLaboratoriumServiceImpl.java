package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.repository.algemeen.GemeenteRepository;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.service.cervix.CervixBMHKLaboratoriumService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CervixBMHKLaboratoriumServiceImpl implements CervixBMHKLaboratoriumService
{

	@Autowired
	private GemeenteRepository gemeenteRepository;

	@Autowired
	private BmhkLaboratoriumRepository bmhkLaboratoriumRepository;

	@Override
	@Transactional
	public void saveOrUpdateLaboratorium(BMHKLaboratorium laboratorium, List<Gemeente> mogelijkeGemeentes)
	{
		List<Gemeente> gekoppeldeGemeentes = laboratorium.getGemeentes();
		gekoppeldeGemeentes.forEach(gemeente -> gemeente.setBmhkLaboratorium(laboratorium));
		gemeenteRepository.saveAll(gekoppeldeGemeentes);

		mogelijkeGemeentes.stream()
			.filter(gemeente -> !gekoppeldeGemeentes.contains(gemeente) && laboratorium.equals(gemeente.getBmhkLaboratorium()))
			.forEach(gemeente -> gemeente.setBmhkLaboratorium(null));
		gemeenteRepository.saveAll(mogelijkeGemeentes);

		bmhkLaboratoriumRepository.save(laboratorium);
	}
}
