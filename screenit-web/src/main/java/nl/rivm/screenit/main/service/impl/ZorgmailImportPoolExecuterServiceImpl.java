package nl.rivm.screenit.main.service.impl;

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

import java.io.File;
import java.io.FileInputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.ZorgmailImportPoolExecuterService;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.DatabaseRunner;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ZorgmailImportService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ZorgmailImportPoolExecuterServiceImpl implements ZorgmailImportPoolExecuterService
{
	private ExecutorService executorService;

	@Autowired
	private ZorgmailImportService importService;

	@Autowired
	private LogService logService;

	@Autowired
	private DatabaseRunner databaseRunner;

	public ZorgmailImportPoolExecuterServiceImpl()
	{
		executorService = Executors.newSingleThreadExecutor();
	}

	@Override
	public void startImport(final File csvFile, final Boolean ediAdresOverschrijven)
	{
		executorService.submit(() ->
		{
			try (FileInputStream xlsStream = new FileInputStream(csvFile))
			{
				databaseRunner.runInSessionOnly(() -> importService.importHandmatigAdresboek(xlsStream, ediAdresOverschrijven));
			}
			catch (Exception e) 
			{
				LOG.error("Er is een onvoorziene crash geweest in de Zorgmail Import Thread, deze is nu gesloten. {}", e.getMessage(), e);
				logService.logGebeurtenis(LogGebeurtenis.HUISARTS_IMPORT_FOUT,
					"Importeren van Huisartsen Adresboek is mislukt. Er is een crash opgetreden, alle gemaakte wijzigingen zijn terugggedraaid.", Bevolkingsonderzoek.COLON);
			}
		});
	}
}
