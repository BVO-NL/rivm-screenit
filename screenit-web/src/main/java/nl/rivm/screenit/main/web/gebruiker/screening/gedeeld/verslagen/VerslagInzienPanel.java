package nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen;

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

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VerslagContent_;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.wicket.Component;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.hibernate.Hibernate;
import org.jetbrains.annotations.NotNull;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

public abstract class VerslagInzienPanel<T extends Verslag<?, ?>> extends GenericPanel<T>
{

	public static final String VALUE_COMPONENT_ID = "value";

	protected VerslagInzienPanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new EnumLabel<>("type"));
		add(new Label(propertyChain(MdlVerrichting_.VERSLAG_CONTENT, VerslagContent_.VERSIE), getModelObject().getVerslagContent().getVersie().name()));

		var fields = getSelectedFields();

		add(new ListView<>("list", new ListModel<>(fields))
		{
			@Override
			protected void populateItem(ListItem<String> item)
			{
				var longFieldName = item.getModelObject();
				var shortFieldName = longFieldName.substring(longFieldName.lastIndexOf(".") + 1);
				var parentFieldName = longFieldName.substring(0, longFieldName.lastIndexOf("."));
				var parentModel = new PropertyModel<>(VerslagInzienPanel.this.getModel(), parentFieldName);
				var clazz = Hibernate.unproxy(ModelProxyHelper.deproxy(parentModel.getObject())).getClass();
				addField(item, clazz, shortFieldName, parentModel);
			}
		});

		add(new ClazzFieldsFragment("allFields", new PropertyModel<>(getModel(), "verslagContent")).setVisible(ScreenitSession.get().checkPermission(Recht.TESTEN, Actie.INZIEN)));
	}

	protected abstract @NotNull List<String> getSelectedFields();

	private void addField(ListItem<String> item, Class<?> clazz, String fieldName, IModel<?> defaultModel)
	{
		var field = FieldUtils.getDeclaredField(clazz, fieldName, true);
		var vraagElement = field.getAnnotation(VraagElement.class);
		if (vraagElement != null)
		{
			item.add(new Label("fieldName", vraagElement.displayName()));
			item.add(getValueField(field, defaultModel));
		}
		else
		{
			item.setVisible(false);
			item.add(new EmptyPanel("fieldName"));
			item.add(new EmptyPanel(VALUE_COMPONENT_ID));
		}
	}

	private Component getValueField(Field field, IModel<?> defaultModel)
	{
		var declaringType = field.getType();
		var fieldName = field.getName();
		var vraagElement = field.getAnnotation(VraagElement.class);
		var objectPropertyModel = new PropertyModel<>(defaultModel, fieldName);
		if (vraagElement != null && vraagElement.useInCda() && objectPropertyModel.getObject() != null &&
			StringUtils.isNotBlank(objectPropertyModel.getObject().toString()))
		{
			if (field.isAnnotationPresent(OneToOne.class))
			{
				return new ClazzFieldsFragment(VALUE_COMPONENT_ID, objectPropertyModel);
			}
			else if (field.isAnnotationPresent(OneToMany.class) || field.isAnnotationPresent(ManyToMany.class))
			{
				return new ListFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
			}
			else if (field.isAnnotationPresent(ManyToOne.class))
			{
				if (declaringType.equals(DSValue.class))
				{
					return new TextFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName + ".displayNameNl"));
				}
			}
			else if (declaringType.equals(Quantity.class))
			{
				return new QuantityFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
			}
			else if (declaringType.equals(NullFlavourQuantity.class))
			{
				return new NullFlavourQuantityFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
			}
			else if (field.isAnnotationPresent(Column.class))
			{
				if (String.class.equals(declaringType))
				{
					return new TextFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
				}
				else if (Date.class.equals(declaringType))
				{
					return new DateFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
				}
				else if (Boolean.class.equals(declaringType))
				{
					return new BooleanFragment(VALUE_COMPONENT_ID, new PropertyModel<>(defaultModel, fieldName));
				}
			}
		}

		return new EmptyPanel(VALUE_COMPONENT_ID).setVisible(false);
	}

	private class ClazzFieldsFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentClazzFields";

		public ClazzFieldsFragment(String id, IModel modelObject)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, modelObject);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(getFieldListView());
		}

		private WebMarkupContainer getFieldListView()
		{
			var modelObject = ClazzFieldsFragment.this.getDefaultModelObject();
			var clazz = Hibernate.unproxy(ModelProxyHelper.deproxy(modelObject)).getClass();
			var declaredFields = Arrays.stream(clazz.getDeclaredFields()).map(Field::getName).toList();
			return new ListView<>("clazzFields", declaredFields)
			{
				@Override
				protected void populateItem(final ListItem<String> item)
				{
					addField(item, clazz, item.getModelObject(), ClazzFieldsFragment.this.getDefaultModel());
				}
			};
		}
	}

	private class BooleanFragment extends Fragment
	{

		private static final String MARKUP_ID = "fragmentBoolean";

		public BooleanFragment(String id, IModel<Boolean> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(new BooleanLabel("boolean", BooleanFragment.this.getDefaultModel()));
		}
	}

	private class TextFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentText";

		public TextFragment(String id, IModel<String> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(new Label("text", TextFragment.this.getDefaultModelObjectAsString()));
		}

	}

	private class DateFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentDatum";

		public DateFragment(String id, IModel<Date> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(new Label("datum", DateUtil.formatForPattern(Constants.DEFAULT_DATE_FORMAT, (Date) DateFragment.this.getDefaultModelObject())));
		}
	}

	private class QuantityFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentQuantity";

		public QuantityFragment(String id, IModel<Quantity> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			var quantity = (Quantity) QuantityFragment.this.getDefaultModelObject();
			add(new Label("quantity", quantity.getValue()));
			add(new Label("unit", quantity.getUnit()));
		}
	}

	private class NullFlavourQuantityFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentNullFlavourQuantity";

		public NullFlavourQuantityFragment(String id, IModel<NullFlavourQuantity> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			var nullFlavourQuantity = (NullFlavourQuantity) NullFlavourQuantityFragment.this.getDefaultModelObject();
			add(new Label("quantity", nullFlavourQuantity.getValue()));
			var isNullFlavour = BooleanUtils.isTrue(nullFlavourQuantity.getNullFlavour());
			add(new Label("unit", nullFlavourQuantity.getUnit()).setVisible(!isNullFlavour));
			add(new WebMarkupContainer("nullFlavour").setVisible(isNullFlavour));
		}
	}

	private class ListFragment extends Fragment
	{
		private static final String MARKUP_ID = "fragmentList";

		public ListFragment(String id, IModel<List> model)
		{
			super(id, MARKUP_ID, VerslagInzienPanel.this, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			var defaultModel = ListFragment.this.getDefaultModel();
			add(new ListView<>("list", (IModel<List<Object>>) defaultModel)
			{

				@Override
				protected void populateItem(ListItem item)
				{
					var itemModel = item.getModel();
					var itemModelObjectType = itemModel.getObject().getClass();
					if (itemModelObjectType.equals(DSValue.class))
					{
						item.add(new TextFragment("item", new PropertyModel<>(itemModel, "displayNameNl")));
					}
					else
					{
						item.add(new ClazzFieldsFragment("item", itemModel));
					}
				}
			}).setVisible(!((List<?>) defaultModel.getObject()).isEmpty());
		}
	}
}
