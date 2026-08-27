package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

import jakarta.annotation.PostConstruct;
import jakarta.persistence.Column;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.VerwerkCdaBerichtContentService;
import nl.rivm.screenit.hl7v3.cda.helper.CDAHelper;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;
import nl.rivm.screenit.model.berichten.XPathMapping;
import nl.rivm.screenit.model.berichten.cda.CdaConstants;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.HibernateService;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

@Service
@Slf4j
public class VerwerkCdaBerichtContentServiceImpl implements VerwerkCdaBerichtContentService
{
	@Autowired
	private BaseVerslagService verslagService;

	@Autowired
	private HibernateService hibernateService;

	private static class ConceptExceptionContext
	{

		private final BaseVerslagService verslagService;

		private final XPath xpath;

		private final Node node;

		private final Field field;

		private final String xpathValue;

		private ConceptExceptionContext(BaseVerslagService verslagService, Field field, XPath xpath, Node node, String xpathValue)
		{
			this.verslagService = verslagService;
			this.field = field;
			this.xpath = xpath;
			this.node = node;
			this.xpathValue = xpathValue;
		}
	}

	private enum XpathAlternativeMapping
	{

		PROTOCOL_COLON_BIOPT_PER_POLIEP("hl7:act/hl7:specimen/hl7:specimenRole/hl7:id/@extension")
			{
				@Override
				Map<String, String> getAlternatives()
				{
					return Map.of(

						"""
							/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/
							hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]""",

						"hl7:organizer/hl7:specimen/hl7:specimenRole/hl7:id/@extension", 

						"""
							/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/
							hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]""",

						"hl7:organizer/hl7:specimen/hl7:specimenRole/hl7:id/@extension" 
					);
				}
			};

		private final String xpathReferenceValue;

		XpathAlternativeMapping(String xpathReferenceValue)
		{
			this.xpathReferenceValue = xpathReferenceValue;
		}

		abstract Map<String, String> getAlternatives();

		String getXpathReferenceValue()
		{
			return xpathReferenceValue;
		}
	}

	private enum ConceptExceptionHandler
	{
		PERIODE_VERVOLG_SCOPIE("15", VerslagGeneratie.V11)
			{
				@Override
				Object getValue(ConceptExceptionContext context) throws XPathExpressionException
				{
					DSValue dsValue = null;
					var quantityNode = (Node) context.xpath.compile(context.xpathValue).evaluate(context.node, XPathConstants.NODE);
					var value = context.xpath.compile("@value").evaluate(quantityNode);
					var unit = context.xpath.compile("@unit").evaluate(quantityNode);
					if (StringUtils.isNotBlank(unit) && StringUtils.isNotBlank(value))
					{
						try
						{
							var codeValue = new BigDecimal(value).intValue();
							var code2Jaar = "13";
							var code3Jaar = "14";
							var code5Jaar = "15";
							String code = null;
							if (unit.equals("maand") || unit.equals("mo"))
							{
								if (codeValue > 0 && codeValue <= 12)
								{
									code = String.valueOf(codeValue);
								}
								else if (codeValue == 24)
								{
									code = code2Jaar;
								}
								else if (codeValue == 36)
								{
									code = code3Jaar;
								}
								else if (codeValue == 60)
								{
									code = code5Jaar;
								}
							}
							else if (unit.equals("jaar"))
							{
								if (codeValue == 1)
								{
									code = String.valueOf(12);
								}
								else if (codeValue == 2)
								{
									code = code2Jaar;
								}
								else if (codeValue == 3)
								{
									code = code3Jaar;
								}
								else if (codeValue == 5)
								{
									code = code5Jaar;
								}
							}
							if (code != null)
							{
								dsValue = context.verslagService.getDsValue(code, "2.16.840.1.113883.2.4.3.36.77.5.226", "vs_periode_vervolg");
								if (dsValue == null)
								{
									dsValue = context.verslagService.getDsValue(code, "2.16.840.1.113883.2.4.3.36.77.5.226", "vs_periode_vervolg_surveillance");
								}
							}
						}
						catch (NumberFormatException e)
						{
							LOG.error("Fout bij vertalen surveillancecoloscopie van cda waarde naar intvalue {}: {}", value, e.getMessage());
						}
					}
					return dsValue;
				}
			},
		CERVIX_COS("275", VerslagGeneratie.V11)
			{
				@Override
				Object getValue(ConceptExceptionContext context) throws XPathExpressionException
				{
					DSValue dsValue = null;
					var booleanNode = (Node) context.xpath.compile(context.xpathValue).evaluate(context.node, XPathConstants.NODE);
					var booleanValue = getBooleanValue(booleanNode);
					if (BooleanUtils.isTrue(booleanValue))
					{
						dsValue = getCosDSValue(context, "1"); 
					}
					else if (BooleanUtils.isFalse(booleanValue))
					{
						dsValue = getCosDSValue(context, "2");  
					}

					return dsValue;
				}

				private DSValue getCosDSValue(ConceptExceptionContext context, String code)
				{
					return context.verslagService.getDsValue(code, "2.16.840.1.113883.2.4.3.36.77.11.268", "vs_COS");
				}
			};

		private final String shortConceptId;

		private final VerslagGeneratie[] generaties;

		ConceptExceptionHandler(String shortConceptId, VerslagGeneratie... generaties)
		{
			this.shortConceptId = shortConceptId;
			this.generaties = generaties;
		}

		abstract Object getValue(ConceptExceptionContext context) throws XPathExpressionException;

		static ConceptExceptionHandler convertFromCode(String code, VerslagGeneratie generatie)
		{
			for (var handler : values())
			{
				if (handler.shortConceptId.equals(code)
					&& (handler.generaties == null || handler.generaties.length == 0 || Arrays.asList(handler.generaties).contains(generatie)))
				{
					return handler;
				}
			}
			return null;
		}

	}

	private enum PostConceptValueConverter
	{
		DUMMY("", null)
			{
				@Override
				Object getValue(Object value)
				{
					return value;
				}

			},

		;

		private final String shortConceptId;

		private final VerslagGeneratie generatie;

		PostConceptValueConverter(String shortConceptId, VerslagGeneratie generatie)
		{
			this.shortConceptId = shortConceptId;
			this.generatie = generatie;
		}

		abstract Object getValue(Object value);

		static PostConceptValueConverter convertFromCode(String code, VerslagType verslagType)
		{
			for (var converter : values())
			{
				if (converter.shortConceptId.equals(code)
					&& (converter.generatie == null || converter.generatie.equals(verslagType.getHuidigeGeneratie())))
				{
					return converter;
				}
			}
			return null;
		}
	}

	@PostConstruct
	public void init()
	{
	}

	@Override
	public void verwerkVerslagContent(Verslag verslag, Class<? extends VerslagContent> rootClazz)
	{
		try
		{

			var domFactory = DocumentBuilderFactory.newInstance("com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl", null);
			domFactory.setNamespaceAware(true);
			var builder = domFactory.newDocumentBuilder();
			var doc = builder.parse(IOUtils.toInputStream(verslag.getOntvangenBericht().getXmlBericht(), StandardCharsets.UTF_8));

			var factory = XPathFactory.newInstance();
			var xpath = factory.newXPath();
			xpath.setNamespaceContext(new UniversalNamespaceCache(doc, "hl7"));

			var generatie = VerslagProjectVersionMapping.get().getGeneratie(verslag.getOntvangenBericht().getProjectVersion(), verslag.getType());

			var verslagContent = maakEnVulVerslagDeel(doc, xpath, rootClazz, verslag, "", verslag.getType(), generatie);
			var oldVerslagContent = verslag.getVerslagContent();
			if (oldVerslagContent != null)
			{
				oldVerslagContent.setVerslag(null);
				hibernateService.delete(oldVerslagContent);
			}
			verslag.setVerslagContent(verslagContent);
			verslagContent.setVersie(generatie);
			verslagContent.setVerslag(verslag);
		}
		catch (NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException | SAXException | IOException | ParserConfigurationException
			   | XPathExpressionException e)
		{
			LOG.error("Fout bij vertaling van bericht naar model classes", e);
			throw new IllegalArgumentException("Fout bij vertaling van CDA bericht naar verslag in DB");
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private <T, S> T maakEnVulVerslagDeel(Node node, XPath xpath, Class<T> rootClazz, S parent, String rootXPath, VerslagType verslagType, VerslagGeneratie generatie)
		throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException, XPathExpressionException
	{
		var verslagDeel = rootClazz.getDeclaredConstructor().newInstance();
		var declaredFields = rootClazz.getDeclaredFields();

		for (var declaredField : declaredFields)
		{
			var declaringType = declaredField.getType();
			var fieldName = declaredField.getName();
			var vraagElement = declaredField.getAnnotation(VraagElement.class);
			if (vraagElement != null && vraagElement.useInCda())
			{
				var xpathMapping = getXpathMapping(vraagElement, verslagType, generatie);
				if (declaredField.isAnnotationPresent(OneToOne.class))
				{
					PropertyUtils.setProperty(verslagDeel, fieldName, maakEnVulVerslagDeel(node, xpath, declaringType, verslagDeel, rootXPath, verslagType, generatie));

				}
				else if (declaredField.isAnnotationPresent(OneToMany.class) || declaredField.isAnnotationPresent(ManyToMany.class))
				{
					if (xpathMapping != null)
					{
						var nieuwRootXPath = xpathMapping.value();
						if (isRootPartOfCurrentXpath(rootXPath, nieuwRootXPath))
						{
							nieuwRootXPath = removeRootFromXpath(rootXPath, nieuwRootXPath);
						}
						else if (!(node instanceof Document))
						{
							LOG.warn("root xpath " + rootXPath + " niet in nieuwXpath " + nieuwRootXPath + " voor " + declaredField);
						}
						var nodeSet = xpath.compile(nieuwRootXPath).evaluate(node, XPathConstants.NODESET);
						var property = PropertyUtils.getProperty(verslagDeel, fieldName);
						if (nodeSet instanceof NodeList nodeList && property instanceof List list)
						{
							Class paramType = getType(declaredField);
							var extension = xpathMapping.extension();

							for (var i = 0; i < nodeList.getLength(); i++)
							{
								LOG.debug("element " + (i + 1) + " van " + nodeList.getLength() + " van veld " + declaredField);
								var itemNode = nodeList.item(i);
								var attributes = itemNode.getAttributes();
								var negationIndNode = attributes.getNamedItem("negationInd");
								if (negationIndNode == null || !"true".equals(negationIndNode.getNodeValue()))
								{
									if (!HibernateObject.class.isAssignableFrom(paramType) || paramType.equals(DSValue.class))
									{
										var value = getValue(itemNode, xpath, ".", extension, declaredField, verslagType);
										if (value != null)
										{
											list.add(value);
										}
										else
										{
											LOG.warn("geen value voor element " + (i + 1) + " van veld " + declaredField);
										}
									}
									else
									{
										list.add(maakEnVulVerslagDeel(itemNode, xpath, paramType, verslagDeel, nieuwRootXPath, verslagType, generatie));
									}
								}
							}
						}
						else
						{
							LOG.warn("geen nodes voor " + declaredField);
						}
					}
					else
					{
						LOG.warn("xToMany: geen xpath voor " + declaredField + " code " + vraagElement.conceptId() + " verslagType " + verslagType.name());
					}
				}
				else if (declaredField.isAnnotationPresent(ManyToOne.class))
				{
					if (declaringType.equals(DSValue.class))
					{
						PropertyUtils.setProperty(verslagDeel, fieldName, getValue(node, xpath, declaredField, rootXPath, verslagType, generatie));
					}
				}
				else if (declaredField.getType().equals(Quantity.class) || declaredField.getType().equals(NullFlavourQuantity.class)
					|| declaredField.isAnnotationPresent(Column.class))
				{
					PropertyUtils.setProperty(verslagDeel, fieldName, getValue(node, xpath, declaredField, rootXPath, verslagType, generatie));
				}

			}
			else if (Hibernate.getClass(parent).equals(declaringType))
			{
				PropertyUtils.setProperty(verslagDeel, fieldName, parent);
			}
		}
		return verslagDeel;
	}

	private String removeRootFromXpath(String rootXPath, String nieuwRootXPath)
	{
		if (nieuwRootXPath.substring(rootXPath.length()).startsWith("["))
		{
			nieuwRootXPath = "self::node()" + nieuwRootXPath.substring(rootXPath.length());
		}
		else
		{
			nieuwRootXPath = nieuwRootXPath.substring(rootXPath.length() + 1);
		}
		return nieuwRootXPath;
	}

	private XPathMapping getXpathMapping(VraagElement vraagElement, VerslagType verslagType, VerslagGeneratie generatie)
	{
		XPathMapping mapping = null;
		if (vraagElement != null)
		{

			String xpath = null;
			var xpaths = vraagElement.xpaths();
			if (generatie != null && !generatie.isHuidigeGeneratie(verslagType) && xpaths.length == 2)
			{
				xpath = xpaths[1];
			}
			else if (xpaths.length >= 1)
			{
				xpath = xpaths[0];
			}

			if (xpath != null)
			{
				var extension = "";
				if (xpath.contains("|"))
				{
					var splittedXpath = xpath.split("\\|");
					xpath = splittedXpath[0];
					extension = splittedXpath[1];
				}
				mapping = new XPathMapping(xpath, extension);
			}
		}
		return mapping;
	}

	private Class<?> getType(Field declaredField)
	{
		var type = declaredField.getGenericType();
		if (type instanceof ParameterizedType)
		{
			var pt = (ParameterizedType) type;
			for (var t : pt.getActualTypeArguments())
			{
				return (Class<?>) t;
			}
			return null;
		}
		else
		{
			return declaredField.getType();
		}
	}

	private Object getValue(Node node, XPath xpath, Field declaredField, String rootXPath, VerslagType verslagType, VerslagGeneratie generatie)
		throws XPathExpressionException, InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		Object returnValue = null;
		var vraagElement = declaredField.getAnnotation(VraagElement.class);
		var xpathMapping = getXpathMapping(vraagElement, verslagType, generatie);
		if (xpathMapping != null && StringUtils.isNotBlank(xpathMapping.value()))
		{
			var xpathValue = xpathMapping.value();
			if (isRootPartOfCurrentXpath(rootXPath, xpathValue))
			{
				xpathValue = removeRootFromXpath(rootXPath, xpathValue);
			}
			else if (!(node instanceof Document))
			{
				var correctionSuccess = false;

				top:
				for (var mapping : XpathAlternativeMapping.values())
				{
					var alternatives = mapping.getAlternatives();
					for (var altMapping : alternatives.entrySet())
					{
						var alternativeXpath = altMapping.getKey();
						if (xpathValue.startsWith(alternativeXpath))
						{
							var referenceValue = xpath.compile(mapping.getXpathReferenceValue()).evaluate(node);
							var alternativeReferenceValue = altMapping.getValue();
							node = (Node) xpath.compile(alternativeXpath + "[" + alternativeReferenceValue + "='" + referenceValue + "']").evaluate(node.getOwnerDocument(),
								XPathConstants.NODE);
							xpathValue = xpathValue.substring(alternativeXpath.length() + 1);
							correctionSuccess = true;
							break top;
						}
					}
				}

				if (!correctionSuccess)
				{
					LOG.warn("correctionSuccess=false: root xpath " + rootXPath + " niet in xpathValue " + xpathValue + " voor " + declaredField);
				}
			}

			var handler = ConceptExceptionHandler.convertFromCode(vraagElement.conceptId(), generatie);
			if (handler != null)
			{
				returnValue = handler.getValue(new ConceptExceptionContext(verslagService, declaredField, xpath, node, xpathValue));
			}
			else
			{
				returnValue = getValue(node, xpath, xpathValue, xpathMapping.extension(), declaredField, verslagType);
			}

			if (vraagElement.isVerplicht() && returnValue == null)
			{
				LOG.info("geen value voor " + declaredField);
			}
		}
		else
		{
			var handler = ConceptExceptionHandler.convertFromCode(vraagElement.conceptId(), generatie);
			if (handler != null)
			{
				returnValue = handler.getValue(new ConceptExceptionContext(verslagService, declaredField, xpath, node, ""));
			}
			else
			{
				LOG.warn("geen xpath voor " + declaredField + " code " + vraagElement.conceptId() + " verslagType " + verslagType.name());
			}
		}

		var converter = PostConceptValueConverter.convertFromCode(vraagElement.conceptId(), verslagType);
		if (converter != null)
		{
			returnValue = converter.getValue(returnValue);
		}
		return returnValue;
	}

	private boolean isRootPartOfCurrentXpath(String rootXPath, String xpathValue)
	{
		return StringUtils.isNotBlank(rootXPath) && !rootXPath.equals(xpathValue) && xpathValue.contains(rootXPath);
	}

	private Object getValue(Node node, XPath xpath, String xpathValue, String extension, Field declaredField, VerslagType verslagType)
		throws XPathExpressionException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException
	{
		Object returnValue = null;
		var type = getType(declaredField);
		if (Boolean.class.equals(type))
		{
			var booleanNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			returnValue = getBooleanValue(booleanNode);
		}
		else if (String.class.equals(type))
		{
			if (StringUtils.isNotBlank(extension))
			{
				if (!xpathValue.endsWith("/"))
				{
					xpathValue += "/";
				}
				xpathValue += extension;
			}
			returnValue = xpath.compile(xpathValue).evaluate(node);
		}
		else if (Date.class.equals(type))
		{
			returnValue = getDateValue(node, xpath, xpathValue);
		}
		else if (Quantity.class.equals(type))
		{
			var quantity = (Quantity) type.getDeclaredConstructor().newInstance();
			var quantityNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			quantity.setValue(xpath.compile("@value").evaluate(quantityNode));
			quantity.setUnit(xpath.compile("@unit").evaluate(quantityNode));
			if (StringUtils.isNotBlank(quantity.getValue()))
			{
				returnValue = quantity;
			}
		}
		else if (NullFlavourQuantity.class.equals(type))
		{
			var quantity = (NullFlavourQuantity) type.getDeclaredConstructor().newInstance();
			var quantityNode = (Node) xpath.compile(xpathValue.replace("[not(@nullFlavor)]", "")).evaluate(node, XPathConstants.NODE);
			quantity.setValue(xpath.compile("@value").evaluate(quantityNode));
			quantity.setUnit(xpath.compile("@unit").evaluate(quantityNode));
			if (quantityNode != null && StringUtils.isBlank(quantity.getValue()))
			{
				quantity.setUnit(null);
				quantity.setValue(null);
				var noNullFlavour = isNoNullFlavour(quantityNode.getAttributes());
				quantity.setNullFlavour(!Boolean.TRUE.equals(noNullFlavour));
			}
			if (StringUtils.isNotBlank(quantity.getValue()) || quantity.getNullFlavour() != null)
			{
				returnValue = quantity;
			}
		}
		else if (DSValue.class.equals(type))
		{
			var dsValueSet = declaredField.getAnnotation(DSValueSet.class);
			var dsZoekObject = new DSValue();
			var dsNode = (Node) xpath.compile(xpathValue).evaluate(node, XPathConstants.NODE);
			dsZoekObject.setCode(xpath.compile("@code").evaluate(dsNode));
			dsZoekObject.setCodeSystem(xpath.compile("@codeSystem").evaluate(dsNode));
			dsZoekObject.setValueSetName(dsValueSet.name());

			dsZoekObjectCorrectie(declaredField, verslagType, dsZoekObject);

			returnValue = zoekDsValue(xpath, dsNode, dsZoekObject, verslagService);
		}
		else if (List.class.equals(type))
		{
			returnValue = xpath.compile(xpathValue).evaluate(node, XPathConstants.NODESET);
		}
		return returnValue;
	}

	private static Object zoekDsValue(XPath xpath, Node node, DSValue zoekValue, BaseVerslagService verslagService) throws XPathExpressionException
	{
		DSValue returnValue = null;
		returnValue = verslagService.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
		if (returnValue == null)
		{
			zoekValue.setCode(xpath.compile("@nullFlavor").evaluate(node));
			if ("OTH".equals(zoekValue.getCode()) || "UNK".equals(zoekValue.getCode()) || "ASKU".equals(zoekValue.getCode()))
			{
				zoekValue.setCodeSystem("2.16.840.1.113883.5.1008");
				returnValue = verslagService.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
				if (returnValue == null)
				{
					zoekValue.setValueSetName(CdaConstants.CDA_NULL_FLAVOR_VALUESET_NAME);
					returnValue = verslagService.getDsValue(zoekValue.getCode(), zoekValue.getCodeSystem(), zoekValue.getValueSetName());
				}
			}
		}
		return returnValue;
	}

	private void dsZoekObjectCorrectie(Field declaredField, VerslagType verslagType, DSValue zoekValue)
	{

		if (zoekValue.getCode() != null)
		{
			var vraagElement = declaredField.getAnnotation(VraagElement.class);
			switch (vraagElement.conceptId())
			{
			case "110":

				switch (zoekValue.getCode())
				{
				case "183654001":
					zoekValue.setCode("183851006");
					break;
				case "73761001:408730004=64695001":
					zoekValue.setCode("73761001:260870009=64695001");
					break;
				case "410410006:408730004=428119001":
					zoekValue.setCode("428119001:363589002=73761001");
					break;
				}
				break;
			case "130":

				switch (zoekValue.getCode())
				{
				case "277163006":
					zoekValue.setCode("89452002");
					break;
				case "82375006":
					zoekValue.setCode("428054006");
					break;
				case "4":
					zoekValue.setCode("67401000119103:116676008=61647009");
					zoekValue.setCodeSystem("2.16.840.1.113883.6.96");
					break;
				case "128653004":
					zoekValue.setCode("449855005");
					break;
				case "35917007":
					zoekValue.setCode("408645001");
					break;

				}
				break;
			case "164":

				switch (zoekValue.getCode())
				{
				case "269533000:408729009=415684004":
					zoekValue.setCode("162572001:246090004=269533000");
					break;
				}
			}
		}
	}

	private static Boolean getBooleanValue(Node booleanNode)
	{
		if (booleanNode == null)
		{
			return null;
		}
		var attributes = booleanNode.getAttributes();
		if (attributes == null)
		{
			return null;
		}
		var typeNode = attributes.getNamedItem("xsi:type");
		var valueNode = attributes.getNamedItem("value");
		Boolean returnValue;
		if (typeNode != null && valueNode != null && typeNode.getNodeValue().equals("BL")) 
		{
			returnValue = "true".equals(valueNode.getNodeValue());
		}
		else
		{
			returnValue = isNoNullFlavour(attributes);
		}
		return returnValue;
	}

	private static Boolean isNoNullFlavour(NamedNodeMap attributes)
	{
		Boolean returnValue;
		var nullFlavorNode = attributes.getNamedItem("nullFlavor");
		if (nullFlavorNode != null && ("UNK".equals(nullFlavorNode.getNodeValue()) || "NA".equals(nullFlavorNode.getNodeValue())))
		{
			returnValue = null;
		}
		else
		{
			var negationIndNode = attributes.getNamedItem("negationInd");
			returnValue = !(negationIndNode != null && "true".equals(negationIndNode.getNodeValue()));
		}
		return returnValue;
	}

	private static Date getDateValue(Node node, XPath xpath, String xpathValue) throws XPathExpressionException
	{
		Date returnValue = null;
		var dateValue = xpath.compile(xpathValue + "/@value").evaluate(node);
		try
		{
			returnValue = CDAHelper.converCdaDateStringToDate(dateValue);
		}
		catch (ParseException e)
		{
			LOG.error("Fout bij parsen van value naar datum met xpath " + xpathValue, e);
		}
		return returnValue;
	}
}
