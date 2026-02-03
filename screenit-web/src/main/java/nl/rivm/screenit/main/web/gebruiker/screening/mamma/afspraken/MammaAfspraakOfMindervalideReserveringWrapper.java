package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.time.LocalTime;

import lombok.AccessLevel;
import lombok.Getter;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaMindervalideReservering;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

@Getter
public class MammaAfspraakOfMindervalideReserveringWrapper implements Serializable, IDetachable
{
	@Getter(AccessLevel.NONE)
	private final IModel<MammaAfspraak> afspraakModel;

	private final LocalTime vanafTijd;

	private final boolean isMindervalideReserveringOpVanafTijd;

	public MammaAfspraakOfMindervalideReserveringWrapper(MammaAfspraak afspraak)
	{
		this(afspraak, false);
	}

	public MammaAfspraakOfMindervalideReserveringWrapper(MammaAfspraak afspraak, boolean isMindervalideReserveringOpVanafTijd)
	{
		this.afspraakModel = ModelUtil.sModel(afspraak);
		this.vanafTijd = DateUtil.toLocalTime(afspraak.getVanaf());
		this.isMindervalideReserveringOpVanafTijd = isMindervalideReserveringOpVanafTijd;
	}

	public MammaAfspraakOfMindervalideReserveringWrapper(MammaMindervalideReservering mindervalideReservering)
	{
		this.afspraakModel = null;
		this.vanafTijd = mindervalideReservering.getVanaf();
		this.isMindervalideReserveringOpVanafTijd = true;
	}

	public MammaAfspraak getAfspraak()
	{
		return ModelUtil.nullSafeGet(afspraakModel);
	}

	public boolean isLegeMindervalideReservering()
	{
		return isMindervalideReserveringOpVanafTijd && afspraakModel == null;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(afspraakModel);
	}
}
