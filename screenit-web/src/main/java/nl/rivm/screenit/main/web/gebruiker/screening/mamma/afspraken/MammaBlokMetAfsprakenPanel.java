package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.Hibernate;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import static nl.rivm.screenit.util.mamma.MammaMindervalideUtil.isMindervalideAfspraakMetReservering;
import static nl.rivm.screenit.util.mamma.MammaMindervalideUtil.isMindervalideReserveringOnbezet;

public class MammaBlokMetAfsprakenPanel extends GenericPanel<MammaCapaciteitBlok>
{
	@SpringBean
	private HibernateService hibernateService;

	public MammaBlokMetAfsprakenPanel(String id, IModel<MammaCapaciteitBlok> capaciteitBlokModel, HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken,
		LocalDate currentDay,
		boolean magVerzetten, boolean magBulkVerzetten)
	{
		super(id, capaciteitBlokModel);

		var capaciteitBlok = capaciteitBlokModel.getObject();
		hibernateService.reload(capaciteitBlok);

		var blok = new WebMarkupContainer("blok");
		add(blok);

		blok.add(new EnumLabel<MammaCapaciteitBlokType>("blokType"));

		blok.add(DateLabel.forDatePattern("vanaf", "HH:mm"));
		blok.add(DateLabel.forDatePattern("tot", "HH:mm"));

		var factor = getFactor(capaciteitBlok);
		blok.add(new Label("vrijeCapaciteit", getVrijeCapaciteit(capaciteitBlok, factor)));
		blok.add(new Label("beschikbareCapaciteit", getBeschikbareCapaciteit(capaciteitBlok, factor)));

		var afsprakenEnMindervalideReserveringenModel = new DetachableListModel<>(getAfsprakenEnMindervalideReserveringen(capaciteitBlokModel));
		blok.add(new MammaAfsprakenBlokPanel("afspraken", afsprakenEnMindervalideReserveringenModel, selectedAfspraken, currentDay, magVerzetten, magBulkVerzetten));
	}

	private BigDecimal getFactor(MammaCapaciteitBlok capaciteitBlok)
	{
		var screeningOrganisatie = (ScreeningOrganisatie) Hibernate.unproxy(capaciteitBlok.getScreeningsEenheid().getBeoordelingsEenheid().getParent().getRegio());
		return MammaFactorType.GEEN.getFactor(screeningOrganisatie);
	}

	private static String getVrijeCapaciteit(MammaCapaciteitBlok capaciteitBlok, BigDecimal factor)
	{
		var vrijeCapaciteitRegulier = capaciteitBlok.getVrijeCapaciteit();
		var vrijeCapaciteit = vrijeCapaciteitRegulier.divide(factor, 5, RoundingMode.HALF_UP);
		return vrijeCapaciteit.setScale(1, RoundingMode.HALF_UP).toString();
	}

	private static String getBeschikbareCapaciteit(MammaCapaciteitBlok capaciteitBlok, BigDecimal factor)
	{
		var beschikbareCapaciteitRegulier = capaciteitBlok.getBeschikbareCapaciteit();
		var beschikbareCapaciteit = beschikbareCapaciteitRegulier.divide(factor, 5, RoundingMode.HALF_UP);
		return BigDecimalUtil.decimalToString(beschikbareCapaciteit, 1);
	}

	private List<MammaAfspraakOfMindervalideReserveringWrapper> getAfsprakenEnMindervalideReserveringen(IModel<MammaCapaciteitBlok> capaciteitBlokModel)
	{
		var blok = capaciteitBlokModel.getObject();

		var mindervalideReserveringen = blok.getMindervalideReserveringen().stream()
			.filter(r -> isMindervalideReserveringOnbezet(r, blok.getAfspraken()))
			.map(MammaAfspraakOfMindervalideReserveringWrapper::new)
			.toList();

		List<MammaAfspraakOfMindervalideReserveringWrapper> afsprakenEnMindervalideReserveringen = new ArrayList<>(mindervalideReserveringen);

		var afspraken = blok.getAfspraken().stream()
			.filter(a -> a.getStatus() == MammaAfspraakStatus.GEPLAND)
			.map(a -> new MammaAfspraakOfMindervalideReserveringWrapper(a, isMindervalideAfspraakMetReservering(a, blok.getMindervalideReserveringen())))
			.toList();

		afsprakenEnMindervalideReserveringen.addAll(afspraken);
		afsprakenEnMindervalideReserveringen.sort(Comparator.comparing(MammaAfspraakOfMindervalideReserveringWrapper::getVanafTijd)
			.thenComparing(MammaAfspraakOfMindervalideReserveringWrapper::isMindervalideReserveringOpVanafTijd, Comparator.reverseOrder()));

		return afsprakenEnMindervalideReserveringen;
	}
}
