package nl.rivm.screenit.main.web.gebruiker.screening.colon.verslagen;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.verslagen.VerslagInzienPanel;
import nl.rivm.screenit.model.colon.MdlVerslag;

import org.apache.wicket.model.IModel;
import org.jetbrains.annotations.NotNull;

import static nl.rivm.screenit.model.colon.MdlVerslag_.VERSLAG_CONTENT;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlColoscopieMedischeObservatie_.DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEK;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.PERIODE_VERVOLG_SCOPIE;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg_.PERIODE_VERVOLG_SURVEILLANCE;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlVerrichting_.AANVANG_VERRICHTING;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent_.COLOSCOPIE_MEDISCHE_OBSERVATIE;
import static nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent_.VERRICHTING;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
public class ColonMdlVerslagInzienPanel extends VerslagInzienPanel<MdlVerslag>
{

	public ColonMdlVerslagInzienPanel(String id, IModel<MdlVerslag> model)
	{
		super(id, model);
	}

	protected @NotNull List<String> getSelectedFields()
	{
		return List.of(propertyChain(VERSLAG_CONTENT, VERRICHTING, AANVANG_VERRICHTING),
			propertyChain(VERSLAG_CONTENT, COLOSCOPIE_MEDISCHE_OBSERVATIE, DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG, DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEK),
			propertyChain(VERSLAG_CONTENT, COLOSCOPIE_MEDISCHE_OBSERVATIE, DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG, PERIODE_VERVOLG_SURVEILLANCE),
			propertyChain(VERSLAG_CONTENT, COLOSCOPIE_MEDISCHE_OBSERVATIE, DEFINITIEF_VERVOLGBELEID_VOOR_BEVOLKINGSONDERZOEKG, PERIODE_VERVOLG_SCOPIE));
	}
}
