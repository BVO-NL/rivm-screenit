package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import nl.rivm.screenit.service.cervix.CervixLabformulierService;

import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class CervixLabformulierPanel extends Panel
{
	@SpringBean
	private CervixLabformulierService labformulierService;

	public CervixLabformulierPanel(String id, String objid)
	{
		super(id);
		if (labformulierService.betreftEenS3Labformulier(objid))
		{
			add(new S3LabformulierViewerContainer("labformulier", objid));
		}
		else
		{
			add(new EmptyPanel("labformulier"));
		}
	}

}
