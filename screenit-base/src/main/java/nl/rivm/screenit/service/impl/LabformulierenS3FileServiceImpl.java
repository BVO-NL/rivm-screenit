package nl.rivm.screenit.service.impl;

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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.LabformulierenS3FileService;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class LabformulierenS3FileServiceImpl extends AbstractS3FileServiceImpl implements LabformulierenS3FileService
{
	@Qualifier("labformulierenS3bucketEndpointOverride")
	private final String s3bucketEndpointOverride;

	@Qualifier("labformulierenS3bucketAccessId")
	private final String s3bucketAccessId;

	@Qualifier("labformulierenS3bucketAccessSecret")
	private final String s3bucketAccessSecret;

	@Qualifier("labformulierenS3bucketRegion")
	private final String s3bucketRegion;

	@Qualifier("labformulierenS3bucketName")
	private final String s3bucketName;

	@Qualifier("labformulierenOmgevingNaam")
	private final String omgevingNaam;

	@Qualifier("labformulierenS3bucketEnabled")
	private final boolean labformulierenS3bucketEnabled;

	@Override
	protected String getS3bucketEndpointOverride()
	{
		return s3bucketEndpointOverride;
	}

	@Override
	protected String getS3bucketAccessId()
	{
		return s3bucketAccessId;
	}

	@Override
	protected String getS3bucketAccessSecret()
	{
		return s3bucketAccessSecret;
	}

	@Override
	protected String getS3bucketRegion()
	{
		return s3bucketRegion;
	}

	@Override
	protected String getS3bucketName()
	{
		return s3bucketName;
	}

	@Override
	public void afterPropertiesSet()
	{
		LOG.info("Labformulieren S3 Fileservice staat " + (labformulierenS3bucketEnabled ? "aan" : "uit"));
		if (labformulierenS3bucketEnabled)
		{
			super.afterPropertiesSet();
		}
	}

	@Override
	public String getOmgevingPadPrefix()
	{
		return omgevingNaam;
	}

	@Override
	public void save(String fullFilePath, InputStream content, Long contentLength) throws IOException
	{
		if (labformulierenS3bucketEnabled)
		{
			super.save(fullFilePath, content, contentLength);
		}
	}

	@Override
	public void save(String fullFilePath, File tempFile) throws IOException
	{
		if (labformulierenS3bucketEnabled)
		{
			super.save(fullFilePath, tempFile);
		}
	}

	@Override
	public boolean exists(String fullFilePath)
	{
		return labformulierenS3bucketEnabled && super.exists(fullFilePath);
	}

	@Override
	public File load(String fullFilePath)
	{
		return labformulierenS3bucketEnabled ? super.load(fullFilePath) : null;
	}

	@Override
	public InputStream loadAsStream(String fullFilePath) throws IOException
	{
		return labformulierenS3bucketEnabled ? super.loadAsStream(fullFilePath) : null;
	}

	@Override
	public boolean delete(String fullFilePath)
	{
		return labformulierenS3bucketEnabled && super.delete(fullFilePath);
	}

	@Override
	public boolean deleteQuietly(String fullFilePath)
	{
		return labformulierenS3bucketEnabled && deleteQuietly(fullFilePath);
	}

	@Override
	public void cleanDirectory(String directory) throws IOException
	{
		if (labformulierenS3bucketEnabled)
		{
			super.cleanDirectory(directory);
		}
	}

	@Override
	public void deleteDirectory(String directory) throws IOException
	{
		if (labformulierenS3bucketEnabled)
		{
			super.deleteDirectory(directory);
		}
	}

	@Override
	public void deleteFileOrDirectory(String bestand) throws IOException
	{
		if (labformulierenS3bucketEnabled)
		{
			super.deleteFileOrDirectory(bestand);
		}
	}

	@Override
	public List<String> listFiles(String directory) throws IOException
	{
		return labformulierenS3bucketEnabled ? super.listFiles(directory) : List.of();
	}

	@Override
	public List<String> listFilesGesorteerd(String directory) throws IOException
	{
		return labformulierenS3bucketEnabled ? super.listFilesGesorteerd(directory) : List.of();
	}

	@Override
	public boolean labformulierenS3bucketEnabled()
	{
		return labformulierenS3bucketEnabled;
	}
}
