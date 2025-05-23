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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.FileService;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.InitializingBean;

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;
import software.amazon.awssdk.services.s3.model.Delete;
import software.amazon.awssdk.services.s3.model.DeleteObjectsRequest;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.HeadObjectRequest;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request;
import software.amazon.awssdk.services.s3.model.ObjectIdentifier;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.model.S3Exception;
import software.amazon.awssdk.services.s3.model.S3Object;
import software.amazon.awssdk.services.s3.paginators.ListObjectsV2Iterable;

import static org.apache.commons.lang3.StringUtils.chomp;

@Slf4j
public abstract class AbstractS3FileServiceImpl implements FileService, InitializingBean
{
	private S3Client s3;

	abstract String getS3bucketEndpointOverride();

	abstract String getS3bucketAccessId();

	abstract String getS3bucketAccessSecret();

	abstract String getS3bucketRegion();

	abstract String getS3bucketName();

	@Override
	public boolean exists(String fullFilePath)
	{
		try
		{
			return s3.headObject(HeadObjectRequest.builder()
				.bucket(getS3bucketName())
				.key(getS3Path(fullFilePath))
				.build()).sdkHttpResponse().isSuccessful();
		}
		catch (S3Exception e)
		{
			return false;
		}
	}

	@Override
	public void save(String fullFilePath, File tempFile) throws IOException
	{
		save(fullFilePath, new FileInputStream(tempFile), tempFile.length());
	}

	@Override
	public void save(String fullFilePath, InputStream content, Long contentLength) throws IOException
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om op te slaan");
		}
		try
		{
			var objectResponse = s3.putObject(PutObjectRequest
				.builder()
				.bucket(getS3bucketName())
				.key(getS3Path(fullFilePath)).build(), RequestBody.fromInputStream(content, contentLength));

			if (!objectResponse.sdkHttpResponse().isSuccessful())
			{
				throw new IOException("HTTP error bij uploaden bestand " + fullFilePath + " naar S3 (code=" + objectResponse.sdkHttpResponse().statusCode() + ")");
			}
		}
		catch (S3Exception e)
		{
			throw new IOException("Fout bij uploaden van bestand " + fullFilePath + " naar S3", e);
		}
	}

	@Override
	public File load(String fullFilePath)
	{
		try
		{
			var file = File.createTempFile(FilenameUtils.getBaseName(fullFilePath), "." + FilenameUtils.getExtension(fullFilePath));
			LOG.debug("Tijdelijk bestand {} aangemaakt van S3 bestand {}", file.getPath(), fullFilePath);
			try (InputStream documentStream = loadAsStream(fullFilePath))
			{
				Files.write(file.toPath(), IOUtils.toByteArray(documentStream), StandardOpenOption.WRITE);
			}
			return file;
		}
		catch (Exception e)
		{
			LOG.error("Fout bij downloaden van bestand {} als 'File' uit S3 door {}", fullFilePath, e.getMessage(), e);
			return null;
		}
	}

	@Override
	public InputStream loadAsStream(String fullFilePath) throws IOException
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			return null;
		}
		try
		{
			return s3.getObject(GetObjectRequest
				.builder()
				.bucket(getS3bucketName())
				.key(getS3Path(fullFilePath))
				.build());
		}
		catch (S3Exception e)
		{
			LOG.error("Fout bij downloaden van bestand {} uit S3 door {}", fullFilePath, e.getMessage(), e);
			throw new IOException(e);
		}
	}

	@Override
	public boolean delete(String fullFilePath)
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			return false;
		}
		try
		{
			var key = ObjectIdentifier.builder().key(getS3Path(fullFilePath)).build();
			var request = DeleteObjectsRequest
				.builder()
				.bucket(getS3bucketName())
				.delete(Delete
					.builder()
					.objects(key)
					.build())
				.build();
			var response = s3.deleteObjects(request);
			return response.sdkHttpResponse().isSuccessful();
		}
		catch (S3Exception e)
		{
			LOG.error("Fout bij verwijderen van bestand {} uit S3 door {}", fullFilePath, e.getMessage(), e);
		}
		return false;
	}

	@Override
	public boolean deleteQuietly(String fullFilePath)
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			return false;
		}
		try
		{
			var key = ObjectIdentifier.builder().key(getS3Path(fullFilePath)).build();
			var request = DeleteObjectsRequest
				.builder()
				.bucket(getS3bucketName())
				.delete(Delete
					.builder()
					.objects(key)
					.build())
				.build();
			var response = s3.deleteObjects(request);
			return response.sdkHttpResponse().isSuccessful();
		}
		catch (S3Exception e)
		{

		}
		return false;
	}

	@Override
	public void cleanDirectory(String directory) throws IOException
	{
		deleteDirectory(directory);
	}

	@Override
	public void deleteDirectory(String directory) throws IOException
	{
		var filesInDirectory = listFiles(directory);
		filesInDirectory.forEach(this::delete);
	}

	@Override
	public void deleteFileOrDirectory(String bestand) throws IOException
	{

		delete(bestand);
	}

	@Override
	public List<String> listFiles(String directory) throws IOException
	{
		try
		{
			var response = getObjectsFromDirectory(directory);
			if (response == null)
			{
				return new ArrayList<>();
			}
			return response.stream().flatMap(page -> page.contents().stream()).map(S3Object::key).collect(Collectors.toList());
		}
		catch (S3Exception e)
		{
			LOG.error("Fout bij ophalen van bestandlijst in map {} uit S3 door {}", directory, e.getMessage(), e);
			throw new IOException(e);
		}
	}

	@Override
	public List<String> listFilesGesorteerd(String directory) throws IOException
	{
		var response = getObjectsFromDirectory(directory);
		if (response == null)
		{
			return new ArrayList<>();
		}
		try
		{
			return response.stream().flatMap(page -> page.contents().stream()).sorted(Comparator.comparing(S3Object::lastModified).reversed()).map(S3Object::key).collect(
				Collectors.toList());
		}
		catch (S3Exception e)
		{
			LOG.error("Fout bij ophalen van bestandlijst in map {} uit S3 door {}", directory, e.getMessage(), e);
			throw new IOException(e);
		}
	}

	private ListObjectsV2Iterable getObjectsFromDirectory(String directory)
	{
		if (StringUtils.isBlank(directory))
		{
			return null;
		}
		var request = ListObjectsV2Request
			.builder()
			.bucket(getS3bucketName())
			.prefix(getS3Path(directory))
			.build();
		return s3.listObjectsV2Paginator(request);
	}

	@Override
	public void afterPropertiesSet()
	{
		final var basicCredentials = AwsBasicCredentials.create(getS3bucketAccessId(), getS3bucketAccessSecret());

		s3 = S3Client
			.builder()
			.credentialsProvider(StaticCredentialsProvider.create(basicCredentials))
			.region(Region.of(getS3bucketRegion()))
			.applyMutation(this::setEndpointOverrideIfPresent)
			.build();
	}

	private void setEndpointOverrideIfPresent(final S3ClientBuilder s3ClientBuilder) throws IllegalArgumentException
	{
		var s3bucketEndpointOverride = getS3bucketEndpointOverride();
		if (StringUtils.isNotBlank(s3bucketEndpointOverride))
		{
			try
			{
				final var endpointOverrideURI = URI.create(s3bucketEndpointOverride);
				s3ClientBuilder.endpointOverride(endpointOverrideURI);
			}
			catch (final IllegalArgumentException e)
			{
				LOG.error("Invalid endpoint override URI", e);
				throw e;
			}
		}
	}

	private String getS3Path(String path)
	{
		var formattedPath = path.replace("\\", "/").replaceAll("\\\\", "/").replaceAll("//", "/");
		return formattedPath.endsWith("/") ? chomp(formattedPath) : formattedPath;
	}
}
