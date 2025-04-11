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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.FileService;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@ConditionalOnProperty(value = "s3.enabled", havingValue = "true")
@Primary
public class S3FileServiceImpl extends AbstractS3FileServiceImpl implements FileService
{

	public S3FileServiceImpl(@Qualifier("s3bucketEndpointOverride") String s3bucketEndpointOverride, @Qualifier("s3bucketAccessId") String s3bucketAccessId,
		@Qualifier("s3bucketAccessSecret") String s3bucketAccessSecret, @Qualifier("s3bucketRegion") String s3bucketRegion, @Qualifier("s3bucketName") String s3bucketName)
	{
		LOG.info("S3 Fileservice");
		this.s3bucketEndpointOverride = s3bucketEndpointOverride;
		this.s3bucketAccessId = s3bucketAccessId;
		this.s3bucketAccessSecret = s3bucketAccessSecret;
		this.s3bucketRegion = s3bucketRegion;
		this.s3bucketName = s3bucketName;
	}

	protected final String s3bucketEndpointOverride;

	protected final String s3bucketAccessId;

	protected final String s3bucketAccessSecret;

	protected final String s3bucketRegion;

	protected final String s3bucketName;

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
}
