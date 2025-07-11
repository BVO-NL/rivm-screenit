package nl.rivm.screenit.model.cervix;

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

import java.util.Date;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "cervix", name = "bulk_upload")
public class CervixBulkUpload extends AbstractHibernateObject
{

	@OneToOne(optional = false)
	private UploadDocument document;

	@Temporal(TemporalType.TIMESTAMP)
	private Date uploadDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker gebruiker;

	public UploadDocument getDocument()
	{
		return document;
	}

	public void setDocument(UploadDocument document)
	{
		this.document = document;
	}

	public Date getUploadDatum()
	{
		return uploadDatum;
	}

	public void setUploadDatum(Date uploadDatum)
	{
		this.uploadDatum = uploadDatum;
	}

	public InstellingGebruiker getGebruiker()
	{
		return gebruiker;
	}

	public void setGebruiker(InstellingGebruiker gebruiker)
	{
		this.gebruiker = gebruiker;
	}
}
