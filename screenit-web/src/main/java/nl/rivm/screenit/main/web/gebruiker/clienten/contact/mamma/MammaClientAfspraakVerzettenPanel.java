package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

public class MammaClientAfspraakVerzettenPanel extends MammaClientAfspraakWijzigenPanel
{

	private static final long serialVersionUID = 1L;

	private MammaAfspraakKiezenPanel afspraakKiezenPanel;

	public MammaClientAfspraakVerzettenPanel(String id, MammaAfspraak afspraak)
	{
		super(id, afspraak);
		add(new MammaAfspraakPanel("huidigeAfspraakPanel", afspraak, false));

		afspraakKiezenPanel = new MammaAfspraakKiezenPanel("afspraakKiezenPanel", ModelUtil.sModel(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient()));
		add(afspraakKiezenPanel);
	}

	@Override
	public void validate()
	{
		afspraakKiezenPanel.validate();
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		return afspraakKiezenPanel.getOpslaanMeldingen();
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		return afspraakKiezenPanel.getOpslaanObjecten();
	}
}
