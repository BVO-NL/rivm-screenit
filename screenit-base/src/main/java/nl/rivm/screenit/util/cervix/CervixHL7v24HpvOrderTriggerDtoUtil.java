package nl.rivm.screenit.util.cervix;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.messagequeue.dto.CervixHL7v24HpvOrderTriggerDto;

import org.hibernate.Hibernate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHL7v24HpvOrderTriggerDtoUtil
{
	public static CervixHL7v24HpvOrderTriggerDto maakHpvOrderTriggerDto(CervixMonster monster, boolean cancelOrder)
	{
		var triggerDto = new CervixHL7v24HpvOrderTriggerDto();
		triggerDto.setClazz(((CervixMonster) Hibernate.unproxy(monster)).getClass());
		triggerDto.setMonsterId(monster.getId());
		triggerDto.setCancelOrder(cancelOrder);
		return triggerDto;
	}
}
