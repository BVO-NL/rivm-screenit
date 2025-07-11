package nl.rivm.screenit.model.project;

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
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
public class ProjectInactiveerDocument extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY)
	private UploadDocument uploadDocument;

	@Column(nullable = true)
	private String dynamischeInactiveerReden;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Project project;

	@OneToMany(mappedBy = "projectInactiveerDocument", fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private List<ProjectClient> projectClienten;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker geuploadDoor;

	public UploadDocument getUploadDocument()
	{
		return uploadDocument;
	}

	public void setUploadDocument(UploadDocument uploadDocument)
	{
		this.uploadDocument = uploadDocument;
	}

	public String getDynamischeInactiveerReden()
	{
		return dynamischeInactiveerReden;
	}

	public void setDynamischeInactiveerReden(String dynamischeInactiveerReden)
	{
		this.dynamischeInactiveerReden = dynamischeInactiveerReden;
	}

	public Project getProject()
	{
		return project;
	}

	public void setProject(Project project)
	{
		this.project = project;
	}

	public List<ProjectClient> getProjectClienten()
	{
		return projectClienten;
	}

	public void setProjectClienten(List<ProjectClient> projectClienten)
	{
		this.projectClienten = projectClienten;
	}

	public InstellingGebruiker getGeuploadDoor()
	{
		return geuploadDoor;
	}

	public void setGeuploadDoor(InstellingGebruiker geuploadDoor)
	{
		this.geuploadDoor = geuploadDoor;
	}

}
