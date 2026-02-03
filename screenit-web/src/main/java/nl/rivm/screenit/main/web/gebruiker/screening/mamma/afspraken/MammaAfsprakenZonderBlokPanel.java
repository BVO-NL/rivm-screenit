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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class MammaAfsprakenZonderBlokPanel extends GenericPanel<MammaScreeningsEenheid>
{
	public MammaAfsprakenZonderBlokPanel(String id, IModel<MammaScreeningsEenheid> screeningsEenheidModel, LocalDate datum,
		HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken,
		boolean magVerzetten, boolean magBulkVerzetten, IModel<List<MammaAfspraak>> afsprakenModel)
	{
		super(id, screeningsEenheidModel);

		setVisible(!afsprakenModel.getObject().isEmpty());
		var afsprakenWrappersModel = new DetachableListModel<>(
			afsprakenModel.getObject().stream().map(MammaAfspraakOfMindervalideReserveringWrapper::new).toList());

		add(new MammaAfsprakenBlokPanel("afspraken", afsprakenWrappersModel, selectedAfspraken, datum, magVerzetten, magBulkVerzetten));
	}
}
