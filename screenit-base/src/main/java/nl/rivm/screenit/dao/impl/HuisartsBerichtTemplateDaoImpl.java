package nl.rivm.screenit.dao.impl;

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

import nl.rivm.screenit.dao.HuisartsBerichtTemplateDao;
import nl.rivm.screenit.model.HuisartsBerichtTemplate;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class HuisartsBerichtTemplateDaoImpl extends AbstractAutowiredDao implements HuisartsBerichtTemplateDao
{

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public HuisartsBerichtTemplate getTemplateByType(HuisartsBerichtType type)
	{
		Criteria crit = getSession().createCriteria(HuisartsBerichtTemplate.class);
		crit.add(Restrictions.eq("berichtType", type));

		return (HuisartsBerichtTemplate) crit.uniqueResult();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(HuisartsBerichtTemplate template)
	{
		this.getSession().saveOrUpdate(template);
	}

}
