package nl.rivm.screenit.main.util;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.UnaryOperator;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.functionalinterfaces.TriFunction;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.model.IModel;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BriefOmschrijvingUtil
{
	public static List<String> getBrievenOmschrijvingen(List<? extends ClientBrief> brieven)
	{
		List<String> brievenStrings = new ArrayList<>();
		brieven.sort(new BriefCreatieDatumComparator());
		for (var brief : brieven)
		{
			brievenStrings.add(getBriefOmschrijving(brief) + ", ");
		}
		return brievenStrings;
	}

	public static String getBriefOmschrijving(Brief brief)
	{
		var formatter = new SimpleDateFormat("dd-MM-yyyy");
		var builder = new StringBuilder(brief.getBriefType().getWeergaveNaam())
			.append("(").append(formatter.format(brief.getCreatieDatum()));
		if (brief instanceof ClientBrief<?, ?, ?> clientBrief)
		{
			var herdrukBrief = BriefUtil.getHerdruk(clientBrief);
			if (herdrukBrief != null)
			{
				builder.append(", herdruk van ").append(formatter.format(herdrukBrief.getCreatieDatum()));
			}
		}
		if (BriefUtil.isTegengehouden(brief))
		{
			builder.append(", tegengehouden");
		}
		if (brief.isVervangen())
		{
			builder.append(", vervangen");
		}
		return builder.append(")").toString();
	}

	public static void addExtraOmschrijving(StringBuilder omschrijving, Brief brief, UnaryOperator<String> getString)
	{
		var gebeurtenis = bepaalTypeGebeurtenis(brief);

		omschrijving.append(" (");
		omschrijving.append(getString.apply("label.formulier." + gebeurtenis.name().toLowerCase()));
		omschrijving.append(": ");
		omschrijving.append(brief.getBriefType().getWeergaveNaam());
		if (StringUtils.isNotBlank(brief.getTemplateNaam()))
		{
			omschrijving.append(", ");
			omschrijving.append(brief.getTemplateNaam());
		}
		if (brief instanceof BezwaarBrief && ((BezwaarBrief) brief).isVragenOmHandtekening())
		{
			omschrijving.append(" - ");
			omschrijving.append(getString.apply("label.formulier.handtekeningvergeten"));
		}
		omschrijving.append(")");
	}

	public static TypeGebeurtenis bepaalTypeGebeurtenis(Brief brief)
	{
		if (BriefUtil.isGegenereerd(brief))
		{
			return BriefUtil.isVerstuurdVoorAfdrukken(brief) ? TypeGebeurtenis.BRIEF_AFGEDRUKT : TypeGebeurtenis.BRIEF_KLAARGEZET;
		}
		else if (BriefUtil.isTegengehouden(brief))
		{
			return TypeGebeurtenis.BRIEF_TEGENHOUDEN;
		}
		else
		{
			return TypeGebeurtenis.BRIEF_AANGEMAAKT;
		}
	}

	public static String verwerkExtraOmschrijvingen(String[] extraOmschrijvingen, TriFunction<String, IModel<?>, String, String> getString)
	{
		var extraOmschrijving = "";
		if (extraOmschrijvingen == null)
		{
			return extraOmschrijving;
		}

		extraOmschrijvingen = Arrays.stream(extraOmschrijvingen).filter(Objects::nonNull).toArray(String[]::new);
		var index = 0;
		var aantal = extraOmschrijvingen.length;
		for (var omschrijving : extraOmschrijvingen)
		{
			if (StringUtils.isNotBlank(extraOmschrijving))
			{
				if (extraOmschrijving.trim().endsWith(":"))
				{
					if (!extraOmschrijving.endsWith(":"))
					{
						extraOmschrijving += " ";
					}
					extraOmschrijving += " ";
				}
				else if (index < aantal)
				{
					extraOmschrijving += ", ";
				}
			}
			else
			{
				extraOmschrijving = "(";
			}
			extraOmschrijving += getString.apply(omschrijving, null, omschrijving).trim();
			index++;
		}
		if (StringUtils.isNotBlank(extraOmschrijving))
		{
			extraOmschrijving += ")";
		}
		return extraOmschrijving;
	}
}
