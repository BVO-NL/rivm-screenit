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

import java.util.Date;

import nl.topicuszorg.wicket.component.object.PdfObjectContainer;

import org.apache.wicket.IRequestListener;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.resource.IResource.Attributes;

public class S3LabformulierViewerContainer extends PdfObjectContainer implements IRequestListener
{
	private final String objid;

	public S3LabformulierViewerContainer(String id, String objid)
	{
		super(id);
		this.objid = objid;
	}

	@Override
	protected void onInitialize()
	{

		setValue(DATA_ATTRIBUTE, urlForListener(null).toString() + "&random=" + new Date().getTime());
		super.onInitialize();
	}

	@Override
	public final void onRequest()
	{
		var resource = new S3ScannedFormulierViewerResourceExternal(objid);
		var a = new Attributes(RequestCycle.get().getRequest(), RequestCycle.get().getResponse(), null);
		resource.respond(a);
	}

	@Override
	public boolean rendersPage()
	{
		return false;
	}
}
