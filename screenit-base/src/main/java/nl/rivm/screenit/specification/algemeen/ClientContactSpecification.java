package nl.rivm.screenit.specification.algemeen;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContactActie_;
import nl.rivm.screenit.model.ClientContact_;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientContactSpecification
{
	public static Specification<ClientContact> heeftClient(Client client)
	{
		return (r, q, cb) -> cb.equal(r.get(ClientContact_.client), client);
	}

	public static Specification<ClientContact> heeftClientId(Long clientId)
	{
		return (r, q, cb) -> cb.equal(r.get(ClientContact_.client).get(AbstractHibernateObject_.id), clientId);
	}

	public static Specification<ClientContact> heeftOpmerking()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(ClientContact_.opmerking));
	}

	public static Specification<ClientContact> zonderActieType(ClientContactActieType type)
	{
		return (root, query, cb) ->
		{
			var subquery = query.subquery(Long.class);
			var actie = subquery.from(ClientContactActie.class);
			subquery.select(actie.get(AbstractHibernateObject_.id))
				.where(cb.equal(actie.get(ClientContactActie_.contact), root), cb.equal(actie.get(ClientContactActie_.type), type));
			return cb.not(cb.exists(subquery));
		};
	}

}
