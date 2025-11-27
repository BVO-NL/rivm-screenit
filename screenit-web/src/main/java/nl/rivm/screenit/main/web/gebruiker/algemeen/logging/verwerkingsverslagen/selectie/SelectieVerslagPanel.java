package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.selectie;

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
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportageGewijzigdGebiedEntry;
import nl.rivm.screenit.model.verwerkingverslag.colon.ColonSelectieRapportageProjectGroepEntry;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

public class SelectieVerslagPanel extends GenericPanel<ColonSelectieRapportage>
{
	public SelectieVerslagPanel(String id, final IModel<ColonSelectieRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));

		add(new PropertyListView<ColonSelectieRapportageEntry>("gemaakt", new IModel<List<ColonSelectieRapportageEntry>>()
		{
			@Override
			public List<ColonSelectieRapportageEntry> getObject()
			{
				var selectieRapportageEntries = model.getObject().getEntries();

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<ColonSelectieRapportageEntry>()
				{
					@Override
					public boolean apply(@Nullable ColonSelectieRapportageEntry input)
					{
						return input.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT;
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
				var entry = item.getModelObject();
				item.add(new EnumLabel<ColonUitnodigingscategorie>("uitnodigingscategorie"));
				item.add(new Label("aantal"));
				item.add(new Label("gepusht", getPushString(entry)));
			}
		});

		add(new PropertyListView<ColonSelectieRapportageProjectGroepEntry>("groep", new IModel<List<ColonSelectieRapportageProjectGroepEntry>>()
		{
			@Override
			public List<ColonSelectieRapportageProjectGroepEntry> getObject()
			{
				var selectieRapportageEntries = model.getObject().getProjectGroepen();

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<ColonSelectieRapportageProjectGroepEntry>()
				{
					@Override
					public boolean apply(@Nullable ColonSelectieRapportageProjectGroepEntry input)
					{
						return input.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT;
					}
				}));
				Collections.sort(selectieRapportageEntries, new Comparator<ColonSelectieRapportageProjectGroepEntry>()
				{
					@Override
					public int compare(ColonSelectieRapportageProjectGroepEntry o1, ColonSelectieRapportageProjectGroepEntry o2)
					{
						var projectGroep1 = o1.getProjectGroep().getProject().getNaam() + "/" + o1.getProjectGroep().getNaam();
						var projectGroep2 = o2.getProjectGroep().getProject().getNaam() + "/" + o2.getProjectGroep().getNaam();
						return projectGroep1.compareTo(projectGroep2);
					}
				});
				return selectieRapportageEntries;
			}
		})
		{
			@Override
			protected void populateItem(ListItem<ColonSelectieRapportageProjectGroepEntry> item)
			{
				var groepEntry = item.getModelObject();
				if (groepEntry.getDagenNogTeGaan() == 0 && item.getModelObject().getClientenNogTeGaan() > 0)
				{
					item.add(new AttributeAppender("class", Model.of("error")));
				}
				item.add(new Label("dagenNogTeGaan"));
				item.add(new Label("clientenNogTeGaan"));
				item.add(new Label("projectGroep.naam"));
				item.add(new Label("projectGroep.project.naam"));
				item.add(new Label("aantal"));
				if (item.getModelObject().getWaarvanGepusht() > 0)
				{
					item.add(new Label("gepusht", String.format("(waarvan gepusht: %s)", item.getModelObject().getWaarvanGepusht())));
				}
				else
				{
					item.add(new EmptyPanel("gepusht"));
				}
			}
		});

		List<ColonSelectieRapportageGewijzigdGebiedEntry> list = new ArrayList<>(model.getObject().getGewijzigdeGebieden());
		Collections.sort(list, new PropertyComparator("uitnodigingsGebied.naam", false, true));
		add(new ListView<ColonSelectieRapportageGewijzigdGebiedEntry>("gewijzigdeGebieden", ModelUtil.listRModel(list, false))
		{
			@Override
			protected void populateItem(ListItem<ColonSelectieRapportageGewijzigdGebiedEntry> item)
			{
				item.setModel(new CompoundPropertyModel<ColonSelectieRapportageGewijzigdGebiedEntry>(item.getModel()));
				item.add(new Label("uitnodigingsGebied.naam"));
				if (Integer.valueOf(Integer.MAX_VALUE).equals(item.getModelObject().getPercentage()))
				{

					item.add(new Label("percentage", "&#8734;").setEscapeModelStrings(false));
				}
				else
				{
					item.add(new Label("percentage"));
				}
			}
		});
	}

	private static String getPushString(ColonSelectieRapportageEntry entry)
	{
		var tekst = "";
		if (entry.getWaarvanGepusht() != null && entry.getWaarvanGepusht() > 0)
		{
			tekst = "(waarvan gepusht: " + entry.getWaarvanGepusht() + ")";
		}
		return tekst;
	}
}
