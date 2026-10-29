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

import java.time.LocalDateTime;

import jakarta.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens_;
import nl.rivm.screenit.model.algemeen.OverdrachtType;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OverdrachtPersoonsgegevensSpecification
{
	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftClient(Client client)
	{
		return (r, q, cb) -> cb.equal(r.get(OverdrachtPersoonsgegevens_.client), client);
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftDownloadVerzoek(MammaDownloadOnderzoekenVerzoek verzoek)
	{
		return (r, q, cb) -> cb.equal(r.get(OverdrachtPersoonsgegevens_.mammaDownloadOnderzoekenVerzoek), verzoek);
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftStatus(AanvraagBriefStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(OverdrachtPersoonsgegevens_.status), status);
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftOpenstaandVerzoek(Client client)
	{
		return heeftClient(client).and(ExtendedSpecification.not(heeftStatus(AanvraagBriefStatus.VERWERKT)));
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftType(OverdrachtType type)
	{
		return (r, q, cb) -> cb.equal(r.get(OverdrachtPersoonsgegevens_.type), type);
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftGeenDataDump()
	{
		return (r, q, cb) -> cb.isNull(r.get(OverdrachtPersoonsgegevens_.dataDumpGegenereerdOp));
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftDataDump()
	{
		return (r, q, cb) -> cb.and(cb.isNotNull(r.get(OverdrachtPersoonsgegevens_.dataDumpGegenereerdOp)),
			cb.isNotNull(r.get(OverdrachtPersoonsgegevens_.gegenereerdeDataDump)));
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> heeftNogTeVerwijderenDataDump(LocalDateTime grensDatum)
	{
		return (r, q, cb) -> cb.and(
			cb.isNotNull(r.get(OverdrachtPersoonsgegevens_.gegenereerdeDataDump)),
			cb.lessThan(r.get(OverdrachtPersoonsgegevens_.dataDumpGegenereerdOp), grensDatum));
	}

	public static ExtendedSpecification<OverdrachtPersoonsgegevens> moetDataDumpGenereren()
	{
		return (r, q, cb) ->
		{
			var verzoekJoin = join(r, OverdrachtPersoonsgegevens_.mammaDownloadOnderzoekenVerzoek, JoinType.LEFT);
			var bkBeelden = r.get(OverdrachtPersoonsgegevens_.bkBeelden);
			return cb.or(cb.and(
				cb.isTrue(bkBeelden),
				cb.equal(verzoekJoin.get(MammaDownloadOnderzoekenVerzoek_.status), BestandStatus.VERWERKT)), cb.isFalse(r.get(OverdrachtPersoonsgegevens_.bkBeelden)));
		};
	}
}
