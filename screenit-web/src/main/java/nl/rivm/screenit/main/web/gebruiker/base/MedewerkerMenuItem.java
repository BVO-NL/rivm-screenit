package nl.rivm.screenit.main.web.gebruiker.base;

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

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.Component;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;

public class MedewerkerMenuItem implements Serializable, IMenuItem
{

	private final String resourceTag;

	private boolean clickable = true;

	private final Class<? extends MedewerkerBasePage>[] targetPageClass;

	private final List<IMenuItem> subMenuItems;

	public MedewerkerMenuItem(String resourceTag, Class<? extends MedewerkerBasePage>... targetPageClass)
	{
		this(resourceTag, true, targetPageClass);
	}

	public MedewerkerMenuItem(String resourceTag, boolean clickable, Class<? extends MedewerkerBasePage>... targetPageClass)
	{
		this.resourceTag = resourceTag;
		this.targetPageClass = targetPageClass;
		this.clickable = clickable;
		this.subMenuItems = Collections.emptyList();
	}

	public MedewerkerMenuItem(String resourceTag, List<IMenuItem> contextMenuItems)
	{
		this.resourceTag = resourceTag;
		this.targetPageClass = null;
		this.subMenuItems = contextMenuItems;
	}

	public String getResourceTag()
	{
		return resourceTag;
	}

	public Class<? extends MedewerkerBasePage> getTargetPageClass()
	{
		if (targetPageClass != null)
		{
			for (Class<? extends MedewerkerBasePage> pageclass : targetPageClass)
			{
				if (Session.get().getAuthorizationStrategy().isInstantiationAuthorized(pageclass))
				{
					return pageclass;
				}
			}
		}
		return null;
	}

	public List<IMenuItem> getSubMenuItems()
	{
		return subMenuItems;
	}

	public IndicatingAjaxLink<?> createWicketLink(String markupId)
	{
		var link = new IndicatingAjaxLink<Void>(markupId)
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(getTargetPageClass());
			}
		};

		link.setEnabled(clickable);
		return link;
	}

	public static Class<? extends MedewerkerBasePage> getTargetPageClass(MedewerkerMenuItem hoofdMenuItem)
	{
		Class<? extends MedewerkerBasePage> targetPageClass = null;
		if (hoofdMenuItem.getTargetPageClass() != null)
		{
			targetPageClass = hoofdMenuItem.getTargetPageClass();
		}
		else
		{
			MedewerkerMenuItem contextMenuItem = getEerstBeschikbareSubMenuItem(hoofdMenuItem);
			if (contextMenuItem != null)
			{
				targetPageClass = contextMenuItem.getTargetPageClass();
			}
		}
		return targetPageClass;
	}

	private static MedewerkerMenuItem getEerstBeschikbareSubMenuItem(MedewerkerMenuItem hoofdMenuItem)
	{
		if (CollectionUtils.isNotEmpty(hoofdMenuItem.getSubMenuItems()))
		{
			for (IMenuItem contextMenuItem : hoofdMenuItem.getSubMenuItems())
			{
				if (contextMenuItem instanceof MedewerkerMenuItem medewerkerMenuItem)
				{
					if (medewerkerMenuItem.getTargetPageClass() != null
						&& Session.get().getAuthorizationStrategy().isInstantiationAuthorized(medewerkerMenuItem.getTargetPageClass()))
					{
						return medewerkerMenuItem;
					}
				}
			}
		}
		return null;
	}

	public Component getPrefix(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}

	public Component getPostfix(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}

	public boolean isClickable()
	{
		return clickable;
	}
}
