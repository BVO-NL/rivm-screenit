package nl.rivm.screenit.main.service.impl;

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

import nl.rivm.screenit.main.service.KwaliteitscontroleLabService;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet_;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.repository.colon.ColonFITRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLControleBarcodeRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLExterneControleBarcodeRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLInterneControleBarcodeRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLInterneControleSetRepository;
import nl.rivm.screenit.repository.colon.ColonSKMLSentineelControleBarcodeRepository;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
public class KwaliteitscontroleLabServiceImpl implements KwaliteitscontroleLabService
{
	@Autowired
	private ColonSKMLInterneControleBarcodeRepository skmlInterneControleBarcodeRepository;

	@Autowired
	private ColonSKMLExterneControleBarcodeRepository skmlExterneControleBarcodeRepository;

	@Autowired
	private ColonSKMLSentineelControleBarcodeRepository skmlSentineelControleBarcodeRepository;

	@Autowired
	private ColonSKMLControleBarcodeRepository skmlControleBarcodeRepository;

	@Autowired
	private ColonSKMLInterneControleSetRepository skmlInterneControleSetRepository;

	@Autowired
	private ColonFITRepository fitRepository;

	@Override
	public SKMLInterneControleSet laagOfHoogSample(Instelling instelling)
	{
		var laatsteControleBarcode = skmlInterneControleBarcodeRepository.findFirstByLaboratoriumIdOrderByIdDesc(instelling.getId());
		var alleControleSets = skmlInterneControleSetRepository.findAll(Sort.by(SKMLInterneControleSet_.VOLGORDE));

		SKMLInterneControleSet volgendeSet = null;

		if (laatsteControleBarcode.isPresent())
		{
			var foundSet = false;
			for (var controleSet : alleControleSets)
			{
				if (controleSet.getQbaseId() != null && StringUtils.isNotBlank(controleSet.getControleTekst()))
				{
					if (foundSet)
					{
						volgendeSet = controleSet;
						break;
					}
					if (zelfdeQbaseIdEnVolgorde(controleSet, laatsteControleBarcode.get()))
					{
						foundSet = true;
					}
				}
			}

		}

		if (volgendeSet == null)
		{
			for (var set : alleControleSets)
			{
				if (set.getQbaseId() != null && StringUtils.isNotBlank(set.getControleTekst()))
				{
					volgendeSet = set;
					break;
				}
			}
		}
		return volgendeSet;
	}

	private boolean zelfdeQbaseIdEnVolgorde(SKMLInterneControleSet set, SKMLInterneControleBarcode laatsteControleBarcode)
	{
		return set.getQbaseId().equals(laatsteControleBarcode.getQbaseId()) && set.getVolgorde().equals(laatsteControleBarcode.getVolgorde());
	}

	@Override
	public SKMLInterneControleBarcode getInterneControleBarcode(String barcode)
	{
		return skmlInterneControleBarcodeRepository.findByBarcode(barcode).orElse(null);
	}

	@Override
	public SKMLExterneControleBarcode getExterneControleBarcode(String barcode)
	{
		return skmlExterneControleBarcodeRepository.findByBarcode(barcode).orElse(null);
	}

	@Override
	public SKMLSentineelControleBarcode getSentineelControleBarcode(String barcode)
	{
		return skmlSentineelControleBarcodeRepository.findByBarcode(barcode).orElse(null);
	}

	@Override
	public boolean checkOfBarcodeAlBestaat(String barcode)
	{
		return skmlControleBarcodeRepository.existsByBarcode(barcode) || fitRepository.existsByBarcode(barcode);
	}
}
