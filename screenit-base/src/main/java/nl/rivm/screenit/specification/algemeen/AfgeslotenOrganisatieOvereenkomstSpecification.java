package nl.rivm.screenit.specification.algemeen;

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

import jakarta.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class AfgeslotenOrganisatieOvereenkomstSpecification
{
	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> heeftGemachtigde(Gebruiker gebruiker)
	{
		return (r, q, cb) ->
		{
			var organisatieRoot = treat(r, AfgeslotenInstellingOvereenkomst.class, cb);
			var organisatieJoin = join(organisatieRoot, AfgeslotenInstellingOvereenkomst_.instelling, JoinType.LEFT);
			return cb.equal(organisatieJoin.get(Instelling_.gemachtigde), gebruiker);
		};
	}

	public static <O extends AbstractAfgeslotenOvereenkomst> ExtendedSpecification<O> heeftOrganisatie(Instelling organisatie)
	{
		return (r, q, cb) ->
		{
			var organisatieRoot = treat(r, AfgeslotenInstellingOvereenkomst.class, cb);
			return cb.equal(organisatieRoot.get(AfgeslotenInstellingOvereenkomst_.instelling), organisatie);
		};
	}
}
