package nl.rivm.screenit.main.web.gebruiker.screening.cervix.cytologie;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.VerslagInzienPanel;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;

import org.apache.wicket.model.IModel;
import org.jetbrains.annotations.NotNull;

import static nl.rivm.screenit.model.cervix.CervixCytologieVerslag_.VERSLAG_CONTENT;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts_.CONCLUSIE;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts_.PROTOCOLLAIR_VERSLAG;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk_.CNUMMER_LABORATORIUM;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk_.COS;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk_.COS_PLATFORM;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk_.MONSTER_BMHK;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk_.SCREENINGSADVIES_HERHALING;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieMonsterBmhk_.MONSTER_IDENTIFICATIE;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerrichting_.EINDE_VERRICHTING;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent_.CYTOLOGIE_UITSLAG_BVO_BMHK;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent_.CYTOLOGIE_UITSLAG_BVO_BMHK_TBV_HUISARTS;
import static nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent_.VERRICHTING;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

public class CervixCytologieVerslagInzienPanel extends VerslagInzienPanel<CervixCytologieVerslag>
{
	public CervixCytologieVerslagInzienPanel(String id, IModel<CervixCytologieVerslag> model)
	{
		super(id, model);
	}

	@Override
	protected @NotNull List<String> getSelectedFields()
	{
		return List.of(propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK, MONSTER_BMHK, MONSTER_IDENTIFICATIE),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK_TBV_HUISARTS, PROTOCOLLAIR_VERSLAG),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK_TBV_HUISARTS, CONCLUSIE),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK, COS),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK, COS_PLATFORM),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK, CNUMMER_LABORATORIUM),
			propertyChain(VERSLAG_CONTENT, CYTOLOGIE_UITSLAG_BVO_BMHK, SCREENINGSADVIES_HERHALING),
			propertyChain(VERSLAG_CONTENT, VERRICHTING, EINDE_VERRICHTING));
	}
}
