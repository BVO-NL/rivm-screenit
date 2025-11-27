package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.uitnodigingversturen;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import jakarta.annotation.Nullable;

import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportageEntry;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportageProjectGroepEntry;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

public class UitnodigingVersturenVerslagPanel extends GenericPanel<ColonSelectieRapportage>
{
	public UitnodigingVersturenVerslagPanel(String id, final IModel<ColonSelectieRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));

		add(new PropertyListView<ColonSelectieRapportageEntry>("verstuurd", new IModel<List<ColonSelectieRapportageEntry>>()
		{
			@Override
			public List<ColonSelectieRapportageEntry> getObject()
			{
				List<ColonSelectieRapportageEntry> selectieRapportageEntries = model.getObject().getEntries();

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<ColonSelectieRapportageEntry>()
				{
					@Override
					public boolean apply(@Nullable ColonSelectieRapportageEntry input)
					{
						return SelectieType.UITNODIGING_VERSTUURD == input.getSelectieType();
					}
				}));
				Collections.sort(selectieRapportageEntries, new Comparator<ColonSelectieRapportageEntry>()
				{
					@Override
					public int compare(ColonSelectieRapportageEntry o1, ColonSelectieRapportageEntry o2)
					{
						return o1.getUitnodigingscategorie().name().compareTo(o2.getUitnodigingscategorie().name());
					}
				});
				return selectieRapportageEntries;
			}
		})
		{
			@Override
			protected void populateItem(ListItem<ColonSelectieRapportageEntry> item)
			{
				item.add(new EnumLabel<ColonUitnodigingscategorie>("uitnodigingscategorie"));
				item.add(new Label("aantal"));
			}
		});

		add(new PropertyListView<ColonSelectieRapportageProjectGroepEntry>("groep", new IModel<List<ColonSelectieRapportageProjectGroepEntry>>()
		{
			@Override
			public List<ColonSelectieRapportageProjectGroepEntry> getObject()
			{
				List<ColonSelectieRapportageProjectGroepEntry> selectieRapportageEntries = model.getObject().getProjectGroepen();
				selectieRapportageEntries = new ArrayList<>(selectieRapportageEntries);

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<ColonSelectieRapportageProjectGroepEntry>()
				{
					@Override
					public boolean apply(@Nullable ColonSelectieRapportageProjectGroepEntry input)
					{
						return input.getProjectGroep() != null;
					}
				}));
				Collections.sort(selectieRapportageEntries, new Comparator<ColonSelectieRapportageProjectGroepEntry>()
				{
					@Override
					public int compare(ColonSelectieRapportageProjectGroepEntry o1, ColonSelectieRapportageProjectGroepEntry o2)
					{
						return o1.getProjectGroep().getProject().getNaam().compareTo(o2.getProjectGroep().getProject().getNaam());
					}
				});
				return selectieRapportageEntries;
			}
		})
		{
			@Override
			protected void populateItem(ListItem<ColonSelectieRapportageProjectGroepEntry> item)
			{
				item.add(new Label("projectGroep.project.naam"));
				item.add(new Label("projectGroep.naam"));
				item.add(new Label("aantal"));
			}
		});
	}
}
