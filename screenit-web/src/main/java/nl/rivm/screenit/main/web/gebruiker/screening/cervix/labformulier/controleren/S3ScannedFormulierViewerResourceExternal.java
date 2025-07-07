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

import java.io.IOException;
import java.io.InputStream;
import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.ScannedFormulierViewerResourceExternal;
import nl.rivm.screenit.service.LabformulierenS3FileService;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class S3ScannedFormulierViewerResourceExternal extends ScannedFormulierViewerResourceExternal
{
	@SpringBean
	private LabformulierenS3FileService labformulierenS3FileService;

	public S3ScannedFormulierViewerResourceExternal(String filePath)
	{
		super(filePath, false, Duration.ofMinutes(30));
		Injector.get().inject(this);
	}

	@Override
	protected InputStream getInputStream() throws IOException
	{
		return labformulierenS3FileService.loadAsStream(getBestandReferentie());
	}
}
