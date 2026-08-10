package nl.rivm.screenit.hl7v3.cda.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-ws-common
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

import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import jakarta.xml.bind.JAXBElement;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.hl7v3.cda.ClinicalDocument;
import nl.rivm.screenit.hl7v3.cda.EN;
import nl.rivm.screenit.hl7v3.cda.II;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedAuthor;
import nl.rivm.screenit.hl7v3.cda.POCDMT000040AssignedEntity;
import nl.rivm.screenit.hl7v3.cda.ST;

import org.apache.commons.beanutils.PropertyUtils;

@Slf4j
public class CDAHelper
{

	private CDAHelper()
	{
	}

	public static String getExtension(String oidRoot, List<? extends II> ids)
	{
		String idExtension = null;

		if (ids != null)
		{
			for (var id : ids)
			{
				if (id.getRoot() != null && id.getRoot().equals(oidRoot))
				{
					idExtension = id.getExtension();
					break;
				}
			}
		}
		return idExtension;
	}

	public static String getRootExtension(II id)
	{
		var rootExtension = id.getRoot().trim();
		if (id.getExtension() != null && !id.getExtension().trim().equals(""))
		{
			rootExtension += "." + id.getExtension().trim();
		}
		return rootExtension;
	}

	public static boolean hasRootOidId(String oidRoot, List<? extends II> ids)
	{
		var result = false;

		if (ids != null)
		{
			for (var id : ids)
			{
				if (id.getRoot() != null && id.getRoot().equals(oidRoot))
				{
					result = true;
					break;
				}
			}
		}
		return result;
	}

	public static <T extends ST> String getNamePart(List<? extends EN> list, Class<T> declaredClazz, int maxLength, String defaultValue)
	{
		String name = null;
		if (list != null)
		{
			for (var item : list)
			{
				var namePart = getNamePart(item, declaredClazz);
				if (namePart != null && namePart.trim().length() > 0)
				{
					name = namePart.trim();
				}
			}
		}
		if (name == null && defaultValue != null)
		{
			name = defaultValue;
		}
		if (name != null && maxLength > 0 && name.length() > maxLength)
		{
			name = name.substring(0, maxLength);
		}
		return name;
	}

	public static <T extends ST> String getNamePart(EN item, Class<T> declaredClazz)
	{
		var content = item.getContent();

		String result = null;
		for (var contentItem : content)
		{
			if (contentItem instanceof JAXBElement)
			{
				@SuppressWarnings("unchecked")
				var jaxbElement = (JAXBElement<T>) contentItem;
				if (jaxbElement.getDeclaredType().equals(declaredClazz))
				{
					var namePartItem = jaxbElement.getValue();
					var namePartItemContent = namePartItem.getContent();
					if (namePartItemContent != null && !namePartItemContent.isEmpty())
					{
						if (result != null)
						{
							result += " ";
						}
						else
						{
							result = "";
						}
						result += namePartItemContent.get(0).toString();
					}
				}
			}
			else if (declaredClazz == null && contentItem instanceof String)
			{
				if (result != null)
				{
					result += " ";
				}
				else
				{
					result = "";
				}
				result += contentItem.toString();
			}
		}
		return result;
	}

	public static POCDMT000040AssignedAuthor getAssigendAuthor(ClinicalDocument cda)
	{
		if (cda.getAuthors().isEmpty())
		{
			return null;
		}
		else
		{
			return cda.getAuthors().get(0).getAssignedAuthor();
		}
	}

	public static POCDMT000040AssignedEntity getAssigendEntity(ClinicalDocument cda)
	{
		var documentationOves = cda.getDocumentationOves();
		POCDMT000040AssignedEntity assignedAuthor = null;
		if (documentationOves != null && !documentationOves.isEmpty())
		{
			var serviceEvent = documentationOves.get(0).getServiceEvent();
			if (serviceEvent != null)
			{
				var performers = serviceEvent.getPerformers();
				if (performers != null && !performers.isEmpty())
				{
					assignedAuthor = performers.get(0).getAssignedEntity();
				}
			}
		}
		return assignedAuthor;
	}

	public static <T> T getFirstValue(Object root, String path)
	{
		List<T> values = getAllValues(root, path);
		if (values.isEmpty())
		{
			return null;
		}
		else
		{
			return values.get(0);
		}
	}

	public static String getFirstValueNotNull(Object root, String path)
	{
		List<String> values = getAllValues(root, path);
		if (values.isEmpty())
		{
			return "onbekend";
		}
		else
		{
			return values.get(0);
		}
	}

	public static <T> List<T> getAllValues(Object root, String path)
	{
		var values = new ArrayList<T>();
		if (root != null && path != null && !path.trim().isEmpty())
		{
			getValueFromChild(root, new ArrayList<>(Arrays.asList(path.split(":"))), values);
		}
		return values;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static <T> void getValueFromChild(Object bean, List<String> pathElements, List<T> values)
	{
		var element = pathElements.get(0);
		try
		{

			if (element.contains("["))
			{
				var oid = element.substring(element.indexOf('[') + 1, element.indexOf(']'));
				element = element.substring(0, element.indexOf('['));
				for (var splittedOid : oid.split("\\|"))
				{
					getValuesFromProperty(bean, new ArrayList<>(pathElements), values, element, splittedOid);
				}
			}
			else
			{
				getValuesFromProperty(bean, pathElements, values, element, null);
			}
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | StringIndexOutOfBoundsException e)
		{
			LOG.error("error bij ophalen van element " + element, e);
		}
	}

	private static <T> void getValuesFromProperty(Object bean, List<String> pathElements, List<T> values, String element, String oid)
		throws IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		var simpleProperty = PropertyUtils.getSimpleProperty(bean, element);
		if (simpleProperty != null)
		{
			if (pathElements.size() == 1)
			{
				if (simpleProperty instanceof List)
				{
					values.addAll((List<T>) simpleProperty);
				}
				else
				{
					values.add((T) simpleProperty);
				}
			}
			else
			{
				pathElements.remove(0);

				if (simpleProperty instanceof List list)
				{
					for (var simplePropertyElement : list)
					{
						if (hasTemplateId(simplePropertyElement, oid))
						{
							getValueFromChild(simplePropertyElement, new ArrayList<>(pathElements), values);
						}
					}
				}
				else if (hasTemplateId(simpleProperty, oid))
				{
					getValueFromChild(simpleProperty, new ArrayList<>(pathElements), values);
				}

			}
		}
	}

	@SuppressWarnings("unchecked")
	private static <T> boolean hasTemplateId(T simpleProperty, String oid)
	{
		var hasTemplateId = true;
		if (oid != null && !oid.trim().isEmpty())
		{
			try
			{
				var list = PropertyUtils.getProperty(simpleProperty, "templateIds");
				if (list instanceof List templateIds)
				{
					hasTemplateId = false;
					if (templateIds != null && !templateIds.isEmpty() && templateIds.get(0) instanceof II)
					{
						hasTemplateId = hasRootOidId(oid, templateIds);
					}
				}

			}
			catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
			{
				LOG.error("error bij zoeken naar oid " + oid + " in " + simpleProperty.toString(), e);
			}
		}
		return hasTemplateId;
	}

	public static Date converCdaDateStringToDate(String dateValue) throws ParseException
	{
		Date returnValue = null;
		if (dateValue != null && dateValue.trim().length() > 0)
		{
			var dateFormat = "";
			if (dateValue.length() >= 8)
			{
				dateFormat = "yyyyMMdd";
			}
			if (dateValue.length() >= 10)
			{
				dateFormat += "HH";
			}
			if (dateValue.length() >= 12)
			{
				dateFormat += "mm";
			}
			if (dateValue.length() >= 14)
			{
				dateFormat += "ss";
			}
			if (dateValue.length() >= 16)
			{
				dateFormat += "SSS";
			}
			var formatter = new SimpleDateFormat(dateFormat);
			returnValue = formatter.parse(dateValue);
		}
		return returnValue;
	}

	public static String getUitvoerendeOrganisatieInformatie(ClinicalDocument cdaDocument)
	{
		var assignedAuthor = getAssigendEntity(cdaDocument);

		var uitvoerendeOrganisatie = "";
		if (assignedAuthor != null)
		{
			uitvoerendeOrganisatie = " (uitvoerder '" + getFirstValueNotNull(assignedAuthor, CommonCdaConstants.ORGANIZATION_NAME_SUBPATH) + "'; "
				+ CDAHelper.getUitvoerendeOrganisatieIds(assignedAuthor) + ")";
		}
		return uitvoerendeOrganisatie;
	}

	public static String getUitvoerendeOrganisatieIds(POCDMT000040AssignedEntity assignedAuthor)
	{
		var organisationId = new StringBuilder();

		if (assignedAuthor != null)
		{
			var representedOrganization = assignedAuthor.getRepresentedOrganization();
			if (representedOrganization != null && representedOrganization.getIds() != null)
			{
				for (var ii : representedOrganization.getIds())
				{
					if (ii.getRoot() != null)
					{
						var root = ii.getRoot();
						var extension = ii.getExtension();
						if (CommonCdaConstants.URA.equals(root))
						{
							if (extension != null && extension.trim().length() > 0)
							{
								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append("URA:");
								organisationId.append(extension);
							}
						}
						else
						{
							String organisationOid = null;
							if (root != null && root.trim().length() > 0)
							{
								organisationOid = root;
								if (extension != null && extension.trim().length() > 0)
								{
									organisationOid += "." + extension.trim();
								}
							}
							if (organisationId != null)
							{
								if (organisationId.length() > 0)
								{
									organisationId.append(", ");
								}
								organisationId.append(organisationOid);
							}
						}
					}
				}
			}
		}
		return organisationId.toString();
	}

	public static String getUzinummer(ClinicalDocument cda)
	{
		var assignedAuthor = CDAHelper.getAssigendEntity(cda);
		String uzinummer = null;
		if (assignedAuthor != null)
		{
			uzinummer = CDAHelper.getExtension(CommonCdaConstants.UZINUMMER, assignedAuthor.getIds());
		}
		return uzinummer;
	}

}
