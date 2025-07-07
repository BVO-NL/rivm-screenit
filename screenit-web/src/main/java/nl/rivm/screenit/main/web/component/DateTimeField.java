package nl.rivm.screenit.main.web.component;

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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.markup.html.form.FormComponentPanel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class DateTimeField extends FormComponentPanel<Date>
{

	private DatePicker<Date> datePicker;

	private TimeField timeField;

	public DateTimeField(String id)
	{
		this(id, null);
	}

	public DateTimeField(String id, IModel<Date> model)
	{
		super(id, model);

		setType(Date.class);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		datePicker = newDatePicker("datePicker", getModel());
		add(datePicker);
		timeField = newTimeField("timeField", getModel());
		add(timeField);
	}

	protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
	{
		return DatePickerHelper.newDatePicker(wicketId, model);
	}

	protected TimeField newTimeField(String wicketId, IModel<Date> model)
	{
		return new TimeField(wicketId, model);
	}

	@Override
	public void convertInput()
	{
		if (getDatePicker().getConvertedInput() == null && getDatePicker().getModelObject() == null && getTimeField().getConvertedInput() == null
			&& getTimeField().getModelObject() == null)
		{
			invalid();
		}
		else
		{

			LocalDateTime datum;
			if (getDatePicker().getConvertedInput() != null)
			{
				datum = DateUtil.toLocalDateTime(getDatePicker().getConvertedInput());
			}
			else
			{
				datum = DateUtil.toLocalDateTime(getDatePicker().getModelObject());
			}

			LocalDateTime tijd;
			if (getTimeField().getConvertedInput() != null)
			{
				tijd = DateUtil.toLocalDateTime(getTimeField().getConvertedInput());
			}
			else
			{
				tijd = DateUtil.toLocalDateTime(getTimeField().getModelObject());
			}

			setConvertedInput(DateUtil.toUtilDate(datum
				.withHour(tijd.getHour())
				.withMinute(tijd.getMinute())
				.withSecond(0)
				.withNano(0)
			));
		}
	}

	protected DatePicker<Date> getDatePicker()
	{
		return datePicker;
	}

	protected TimeField getTimeField()
	{
		return timeField;
	}
}
