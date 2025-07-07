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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;
import org.hibernate.envers.RelationTargetAuditMode;

@Entity
@Proxy
@Audited
@Getter
@Setter
public class CervixCytologieVerslag extends CervixVerslag<CervixCytologieVerslagContent>
{
	@OneToOne(mappedBy = "cytologieVerslag", optional = false, fetch = FetchType.LAZY)
	private CervixUitstrijkje uitstrijkje;

	@OneToOne(optional = false, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	private CervixCytologieVerslagContent verslagContent;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private CervixCytologieUitslag cytologieUitslag;

	@ElementCollection
	@Temporal(TemporalType.TIMESTAMP)
	@CollectionTable(schema = "cervix", name = "cytologie_verslag_herzieningen_ontvangen")
	@NotAudited
	private List<Date> herzieningenOntvangen = new ArrayList<>();

	@NotAudited
	@Column(nullable = false)
	private String patholoogNaam;

	public BMHKLaboratorium getLaboratorium()
	{
		return (BMHKLaboratorium) HibernateHelper.deproxy(getUitvoerderOrganisatie());
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		setUitvoerderOrganisatie(laboratorium);
	}

}
