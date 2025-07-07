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

import org.apache.wicket.model.IModel;

public class TimeField extends BaseTimeField<Date>
{

	private LocalDateTime dateTime;

	public TimeField(String id)
	{
		this(id, null);
	}

	public TimeField(String id, IModel<Date> model)
	{
		super(id, model);
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		setType(Date.class);

		var d = (Date) getDefaultModelObject();
		if (d != null)
		{
			dateTime = DateUtil.toLocalDateTime(d);
			setHours(Integer.valueOf(dateTime.getHour()));
			setMinutes(Integer.valueOf(dateTime.getMinute()));
		}
		else
		{
			dateTime = null;
			setHours(null);
			setMinutes(null);
		}
	}

	@Override
	public void convertInput()
	{
		var dt = getDefaultModelObject() != null ? DateUtil.toLocalDateTime((Date) getDefaultModelObject()) : LocalDateTime.now();
		var h = getHoursField().getConvertedInput();
		var m = getMinutesField().getConvertedInput();

		try
		{
			if (h != null)
			{
				dt = dt.withHour(h);
			}
			if (m != null)
			{
				dt = dt.withMinute(m);
			}
			else
			{
				dt = dt.withMinute(0);
			}
			dt = dt.withSecond(0).withNano(0);

			setConvertedInput(DateUtil.toUtilDate(dt));
		}
		catch (RuntimeException e)
		{
			invalid();
		}
	}

	public Date getDate()
	{
		return DateUtil.toUtilDate(dateTime);
	}

	public void setDate(Date dateTime)
	{
		if (dateTime == null)
		{
			this.dateTime = null;
			setDefaultModelObject(null);
			setHours(null);
			setMinutes(null);
			return;
		}

		this.dateTime = DateUtil.toLocalDateTime(dateTime);
		setDefaultModelObject(dateTime);

		var h = getHours();
		var m = getMinutes();

		if (h != null)
		{
			this.dateTime = this.dateTime.withHour(h);
			if (m != null)
			{
				this.dateTime = this.dateTime.withMinute(m.intValue());
			}
			else
			{
				this.dateTime = this.dateTime.withMinute(0);
			}
		}
		setDefaultModelObject(DateUtil.toUtilDate(this.dateTime));
	}
}
