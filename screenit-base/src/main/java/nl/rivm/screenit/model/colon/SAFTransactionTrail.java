
package nl.rivm.screenit.model.colon;

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

import java.io.Serial;
import java.util.Date;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "colon")
public class SAFTransactionTrail extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.EAGER)
	private ScannedAntwoordFormulier scannedAntwoordFormulier;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumTijd;

	private String gebruiker;

	private String transactionId;

	public Date getDatumTijd()
	{
		return datumTijd;
	}

	public void setDatumTijd(Date datumTijd)
	{
		this.datumTijd = datumTijd;
	}

	public String getGebruiker()
	{
		return gebruiker;
	}

	public void setGebruiker(String gebruiker)
	{
		this.gebruiker = gebruiker;
	}

	public String getTransactionId()
	{
		return transactionId;
	}

	public void setTransactionId(String transactionId)
	{
		this.transactionId = transactionId;
	}

	public ScannedAntwoordFormulier getScannedAntwoordFormulier()
	{
		return scannedAntwoordFormulier;
	}

	public void setScannedAntwoordFormulier(ScannedAntwoordFormulier scannedAntwoordFormulier)
	{
		this.scannedAntwoordFormulier = scannedAntwoordFormulier;
	}
}
