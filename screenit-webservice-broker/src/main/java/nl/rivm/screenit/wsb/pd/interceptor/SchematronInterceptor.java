package nl.rivm.screenit.wsb.pd.interceptor;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import lombok.extern.slf4j.Slf4j;

import net.sf.saxon.TransformerFactoryImpl;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.ws.providedocument.ProvideDocument;
import nl.rivm.screenit.ws.providedocument.ProvideDocument.DocumentMetaData;
import nl.rivm.screenit.wsb.pd.PdConstants;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.binding.soap.interceptor.AbstractSoapInterceptor;
import org.apache.cxf.binding.xml.XMLFault;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.Phase;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class SchematronInterceptor extends AbstractSoapInterceptor
{
	private final Map<BerichtType, Map<String, Transformer>> transformers = new HashMap<>();

	private String currentSchematronVersionPathMapping = null;

	private final Map<String, String> schematronPathMapping = new HashMap<>();

	@Autowired
	@Qualifier("schematronLocation")
	private String schematronLocation;

	@Autowired
	private SimplePreferenceService preferenceService;

	public SchematronInterceptor()
	{
		super(Phase.PRE_INVOKE);
	}

	@Override
	public void handleMessage(SoapMessage message) throws Fault
	{
		LOG.info("Start schematron validatie");
		var messageContentsList = message.getContent(List.class);
		DocumentMetaData metaData = null;
		String cda = null;
		Object ping = null;
		String projectVersion = null;
		if (messageContentsList != null)
		{
			for (var messageContent : messageContentsList)
			{
				if (messageContent instanceof ProvideDocument)
				{
					var provideDocument = (ProvideDocument) messageContent;
					metaData = provideDocument.getDocumentMetaData();
					var document = provideDocument.getDocument();
					if (document != null)
					{
						cda = new String(document);
					}
					ping = provideDocument.getPing();
				}
			}
		}
		if (ping != null)
		{
			LOG.info("Ping ontvangen. Geen validatie.");
			return;
		}
		if (metaData == null)
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed: kan geen meta data in SOAP vinden.");
		}
		if (cda == null)
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed: kan geen cda document in SOAP vinden.");
		}

		var project = metaData.getProject();
		if (project != null)
		{
			projectVersion = project.getId() + "." + project.getVersion();
		}
		if (StringUtils.isBlank(projectVersion))
		{
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "No projectversion found in meta data in SOAP.");
		}
		try
		{
			init(projectVersion);
		}
		catch (TransformerException | IOException e)
		{
			LOG.error("Init failed", e);
			throw new XMLFault("init failed" + e.getMessage());
		}

		var result = new StringWriter();
		var clinicalDocumentTemplateIds = metaData.getClinicalDocumentTemplateIds();
		String documentTemplateIdSOAP = null;
		if (CollectionUtils.isNotEmpty(clinicalDocumentTemplateIds))
		{
			documentTemplateIdSOAP = StringUtils.trim(clinicalDocumentTemplateIds.get(0));
		}
		validateCandidate(new StreamSource(new StringReader(cda)), new StreamResult(result), metaData.getClinicalDocumentCode().getCode(), documentTemplateIdSOAP, projectVersion);

		var printableResult = result.toString().trim().replace("\n\n", "").replace("\n", " ");

		if (StringUtils.isNotBlank(printableResult))
		{
			LOG.error("Validation failed");
			throw new XMLFault(Constants.XML_FAULT_PREFIX + "Schematron validation failed:" + printableResult);
		}
	}

	private void init(String projectVersion) throws TransformerException, IOException
	{
		var newSchematronPathMapping = preferenceService.getString(PreferenceKey.INTERNAL_WSB_SCHEMATRON_VERSIONPATHMAPPING.name());
		if (StringUtils.isNotBlank(newSchematronPathMapping))
		{
			if (!newSchematronPathMapping.equals(currentSchematronVersionPathMapping))
			{
				currentSchematronVersionPathMapping = newSchematronPathMapping.trim();
				schematronPathMapping.clear();
			}
		}
		if (schematronPathMapping.isEmpty())
		{
			var schematronPathMappings = currentSchematronVersionPathMapping.split(";");
			for (var schematronPathMappingParts : schematronPathMappings)
			{
				var schematronPathMappingPart = schematronPathMappingParts.trim().split("\\|");
				if (schematronPathMappingPart.length == 2)
				{
					schematronPathMapping.put(schematronPathMappingPart[0].trim(), schematronPathMappingPart[1].trim());
				}
			}
		}
		if (StringUtils.isBlank(schematronPathMapping.get(projectVersion)))
		{
			LOG.error("Schematron " + projectVersion + " not found in mapping ('schematron/versionpathmapping').");
			throw new XMLFault("Schematron version " + projectVersion + " (project.id + '.' + project.version) unknown.");
		}
	}

	private byte[] compileSchematron(final String projectVersionPath, String type) throws TransformerException, IOException
	{
		final var closeables = new HashSet<Closeable>();
		try
		{
			var tf = new TransformerFactoryImpl();
			tf.setURIResolver((href, base) ->
			{
				if (base.contains("include") || href.contains("include"))
				{
					var completeIncludePath = schematronLocation + "/" + projectVersionPath + "/" + href;
					try
					{
						var fileInputStream = new FileInputStream(completeIncludePath);
						closeables.add(fileInputStream);
						return new StreamSource(fileInputStream);
					}
					catch (FileNotFoundException e)
					{
						LOG.error("Fout bij openen van bestand: " + completeIncludePath, e);
					}
				}
				else if (href.equals("iso_schematron_skeleton_for_saxon.xsl"))
				{
					var inputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_schematron_skeleton_for_saxon.xsl");
					closeables.add(inputStream);
					return new StreamSource(inputStream);
				}
				var inputStream = SchematronInterceptor.class.getResourceAsStream(base + href);
				closeables.add(inputStream);
				return new StreamSource(inputStream);
			});

			LOG.info("Running pre-process transform #1");
			var schematronInputStream = new FileInputStream(schematronLocation + "/" + projectVersionPath + "/" + type);
			byte[] interim1 = transformStep(closeables, tf, schematronInputStream, "/schematron-transform/iso_dsdl_include.xsl");

			LOG.info("Running pre-process transform #2");
			byte[] interim2 = transformStep(closeables, tf, new ByteArrayInputStream(interim1), "/schematron-transform/iso_abstract_expand.xsl");

			LOG.info("Transforming schema to XSLT");
			byte[] interim3 = transformStep(closeables, tf, new ByteArrayInputStream(interim2), "/schematron-transform/iso_svrl_for_xslt2.xsl");

			return interim3;
		}
		finally
		{
			for (var closeable : closeables)
			{
				try
				{
					closeable.close();
				}
				catch (Exception e)
				{
				}
			}
		}
	}

	@NotNull
	private static byte[] transformStep(HashSet<Closeable> closeables, TransformerFactoryImpl tf, InputStream inputStreamToTransform,
		String transformerFileName) throws TransformerException, IOException
	{
		try (var baos = new ByteArrayOutputStream(); var transformerInputStream = SchematronInterceptor.class.getResourceAsStream(transformerFileName))
		{
			var xsltSource = new StreamSource(transformerInputStream);
			var transformer = tf.newTransformer(xsltSource);
			transformer.transform(new StreamSource(inputStreamToTransform), new StreamResult(baos));
			closeables.add(inputStreamToTransform);
			return baos.toByteArray();
		}
	}

	private void validateCandidate(Source sourceToValidate, Result result, String docType, String templateId, String projectVersion)
	{
		try
		{
			var tf = new TransformerFactoryImpl();
			try (var svrlTransformerInputStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/svrl_transform.xsl"))
			{
				var svrlTransformer = tf.newTransformer(new StreamSource(svrlTransformerInputStream));

				LOG.debug("Applying XSLT to candidate");
				var transformer = getTransformer(docType, templateId, projectVersion);
				try (var baos = new ByteArrayOutputStream())
				{
					transformer.transform(sourceToValidate, new StreamResult(baos));

					try (var svrlResultTransformerInputStream = new ByteArrayInputStream(baos.toByteArray()))
					{
						svrlTransformer.transform(new StreamSource(svrlResultTransformerInputStream), result);
					}
				}
			}
		}
		catch (Exception e)
		{
			throw new IllegalStateException("Transformation failure: " + e.getMessage(), e);
		}
	}

	private Transformer getTransformer(String docType, String templateId, final String projectVersion) throws TransformerException, IOException
	{
		var closeables = new HashSet<Closeable>();

		var projectVersionContext = schematronPathMapping.get(projectVersion);
		BerichtType berichtType;

		if (PdConstants.DOC_TYPE_MDL.equals(docType))
		{
			berichtType = BerichtType.MDL_VERSLAG;
		}
		else if (PdConstants.DOC_TYPE_PA.equals(docType) && (templateId == null || templateId.equals(PdConstants.TEMPLATE_ID_PA_DK)))
		{
			berichtType = BerichtType.PA_LAB_VERSLAG;
		}
		else if (PdConstants.DOC_TYPE_PA.equals(docType) && PdConstants.TEMPLATE_ID_PA_CYTOLOGY.equals(templateId))
		{
			berichtType = BerichtType.CERVIX_CYTOLOGIE_VERSLAG;
		}
		else
		{
			throw new XMLFault("Schematron validation failed: document type " + docType + " in codeSystem " + PdConstants.OID_DOC_TYPE + " onbekend");
		}

		var tf = createTransformerFactory(closeables, projectVersionContext);
		var transformer = createOrGetTransformer(projectVersion, closeables, tf, berichtType);

		for (var closeable : closeables)
		{
			try
			{
				closeable.close();
			}
			catch (Exception e)
			{
			}
		}

		return transformer;
	}

	private Transformer createOrGetTransformer(String projectVersion, HashSet<Closeable> closeables, TransformerFactoryImpl tf, BerichtType berichtType)
		throws TransformerException, IOException
	{
		var berichtTypeTransformers = this.transformers.computeIfAbsent(berichtType, k -> new HashMap<>());
		var projectVersionContext = schematronPathMapping.get(projectVersion);
		var transformer = berichtTypeTransformers.get(projectVersionContext);
		if (transformer == null)
		{
			LOG.info("Start compiling schematron for {}, project version {}", berichtType.getNaam(), projectVersion);
			var compiledSchematron = compileSchematron(getSchematronPath(projectVersionContext), getSchematron(projectVersionContext, berichtType));
			LOG.info("Compiling schematron for {} finished, project version {}", berichtType.getNaam(), projectVersion);
			var byteArrayInputStream = new ByteArrayInputStream(compiledSchematron);
			closeables.add(byteArrayInputStream);
			var xsltSource = new StreamSource(byteArrayInputStream);
			transformer = tf.newTransformer(xsltSource);
			berichtTypeTransformers.put(projectVersionContext, transformer);
		}
		return transformer;
	}

	@NotNull
	private TransformerFactoryImpl createTransformerFactory(HashSet<Closeable> closeables, String projectVersionContext)
	{
		var tf = new TransformerFactoryImpl();
		tf.setURIResolver((href, base) ->
		{
			if (base.contains("include") || href.contains("include"))
			{
				var completeIncludePath = schematronLocation + "/" + getSchematronPath(projectVersionContext) + "/" + href;
				try
				{
					var fileInputStream = new FileInputStream(completeIncludePath);
					closeables.add(fileInputStream);
					return new StreamSource(fileInputStream);
				}
				catch (FileNotFoundException e)
				{
					LOG.error("Fout bij openen van bestand: " + completeIncludePath, e);
				}
			}
			else if (href.equals("iso_schematron_skeleton_for_saxon.xsl"))
			{
				var resourceAsStream = SchematronInterceptor.class.getResourceAsStream("/schematron-transform/iso_schematron_skeleton_for_saxon.xsl");
				closeables.add(resourceAsStream);
				return new StreamSource(resourceAsStream);
			}
			var resourceAsStream = SchematronInterceptor.class.getResourceAsStream(base + href);
			closeables.add(resourceAsStream);
			return new StreamSource(resourceAsStream);
		});
		return tf;
	}

	private String getSchematronPath(String projectVersionContext)
	{
		var splittedContext = projectVersionContext.split(",");
		return splittedContext[0];
	}

	private String getSchematron(String projectVersionContext, BerichtType berichtType)
	{
		String keyword = null;
		switch (berichtType)
		{
		case MDL_VERSLAG:
			keyword = "-mdl";
			break;
		case PA_LAB_VERSLAG:
			keyword = "-pa";
			break;
		case CERVIX_CYTOLOGIE_VERSLAG:
			keyword = "-bmhk";
			break;
		case MAMMA_PA_FOLLOW_UP_VERSLAG:
			keyword = "-fuppa";
			break;
		}

		String[] splittedContext = projectVersionContext.split(",");
		for (int i = 1; i < splittedContext.length; i++)
		{
			var schematronEntry = splittedContext[i];
			if (schematronEntry.contains(keyword))
			{
				return schematronEntry;
			}
		}
		return null;
	}
}
