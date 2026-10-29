package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Session;
import org.hibernate.query.NativeQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Slf4j
@Repository
public class RoosterDaoImpl implements RoosterDao
{
	public static final String[] HERVERWERKING_MARKERS = new String[] { "zonder conclusie", "zonder passende screeningsronde", "geen ongunstige uitslag" };

	@PersistenceContext
	private EntityManager entityManager;

	@Autowired
	private ColonBaseAfspraakService afspraakService;

	@Override
	public List<ColonAfspraakslotListViewWrapper> getAlleAfspraakslotsInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie)
	{
		var query = createAfspraakslotsQuery(filter, intakeLocatie, sortProperty, asc)
			.setTupleTransformer(this::maakAfspraakslotListViewWrapper);
		return query.list();
	}

	@Override
	public List<ColonAfspraakslotListViewWrapper> getAfspraakslots(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie)
	{
		var query = createAfspraakslotsQuery(filter, intakeLocatie, sortProperty, asc)
			.setTupleTransformer(this::maakAfspraakslotListViewWrapper);
		if (first >= 0)
		{
			query.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			query.setMaxResults(Ints.checkedCast(count));
		}

		return query.getResultList();
	}

	private NativeQuery<?> createAfspraakslotsQuery(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie, String sortProperty, boolean asc)
	{
		entityManager.flush();
		var selectFromQueryString = "select ";
		if (sortProperty == null)
		{
			selectFromQueryString += "count(afs.id) ";
		}
		else
		{
			selectFromQueryString += "ts.vanaf as \"startDatum\", ts.tot as \"eindDatum\", k.naam as \"kamer\", k.id as \"kamerId\", afs.id as \"afspraakslotId\", afs.capaciteit_mee_bepaald as \"capaciteitMeeBepaald\"";
		}
		selectFromQueryString += " from colon.afspraakslot afs "
			+ "join colon.tijdslot ts on afs.id=ts.id " 
			+ "join colon.intakekamer k on ts.kamer=k.id " 
			+ "join algemeen.organisatie il on il.id=k.intakelocatie "; 
		var whereQueryString = "where il.id=:intakelocatie and il.actief = true and k.actief = true ";

		var params = new HashMap<String, Object>();

		if (filter.getStatus() != null)
		{
			switch (filter.getStatus())
			{
			case INTAKE_GEPLAND:
				selectFromQueryString += "inner join colon.intakeafspraak ia on afs.id=ia.afspraakslot  ";
				whereQueryString += "and (ia.status=:status1 or ia.status=:status2) ";
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				break;
			case GEBRUIKT_VOOR_CAPACITEIT:
				whereQueryString += "and afs.capaciteit_mee_bepaald = true ";
				selectFromQueryString += "left outer join colon.intakeafspraak ia on afs.id=ia.afspraakslot  ";
				whereQueryString += "and (ia.status is null or (ia.status!=:status1 and ia.status!=:status2)) ";
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				whereQueryString += "and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				break;
			case VRIJ_TE_VERPLAATSEN:
				whereQueryString += "and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot=afs.id and (ia.status=:status1 or ia.status=:status2)) " 
					+ "and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				if (filter.isRekeningHoudenMetCapaciteitMeeBepaald())
				{
					whereQueryString += "and afs.capaciteit_mee_bepaald = false";
				}
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				break;
			case BLOKKADE:
				whereQueryString += "and EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' and b.kamer=k.id and b.vanaf<ts.tot and b.tot>ts.vanaf) ";
				break;
			default:
				break;
			}
		}
		whereQueryString += " and ts.tot>:vanaf and ts.vanaf<:tot ";
		var orderByQueryString = "";
		if (sortProperty != null)
		{
			orderByQueryString += "order by ";
			var supportedSortProperty = true;
			switch (sortProperty)
			{
			case "vanaf":
				orderByQueryString += "ts.vanaf ";
				break;
			case "tot":
				orderByQueryString += "ts.tot ";
				break;
			case "kamer":
				orderByQueryString += "k.naam ";
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty + " filter " + filter.getStatus());
				supportedSortProperty = false;
				break;
			}
			if (supportedSortProperty)
			{
				if (asc)
				{
					orderByQueryString += "asc";
				}
				else
				{
					orderByQueryString += "desc";
				}
			}
		}

		var nativeQuery = entityManager.createNativeQuery(selectFromQueryString + whereQueryString + orderByQueryString).unwrap(NativeQuery.class);
		nativeQuery
			.setParameter("tot", filter.getEindDatum())
			.setParameter("vanaf", filter.getStartDatum())
			.setParameter("intakelocatie", intakeLocatie.getId());

		for (var param : params.entrySet())
		{
			nativeQuery.setParameter(param.getKey(), param.getValue());
		}

		return nativeQuery;
	}

	private ColonAfspraakslotListViewWrapper maakAfspraakslotListViewWrapper(Object[] tuple, String[] aliases)
	{
		var wrapper = new ColonAfspraakslotListViewWrapper();
		for (var index = 0; index < aliases.length; index++)
		{
			var waarde = tuple[index];
			switch (aliases[index])
			{
			case "startDatum" -> wrapper.setStartDatum(waarde);
			case "eindDatum" -> wrapper.setEindDatum(waarde);
			case "kamer" -> wrapper.setKamer((String) waarde);
			case "kamerId" -> wrapper.setKamerId(((Number) waarde).longValue());
			case "afspraakslotId" -> wrapper.setAfspraakslotId(((Number) waarde).longValue());
			case "capaciteitMeeBepaald" -> wrapper.setCapaciteitMeeBepaald((Boolean) waarde);
			default -> throw new IllegalStateException("Onbekende alias " + aliases[index] + " voor afspraakslot");
			}
		}
		return wrapper;
	}

	private VrijSlotZonderKamer maakVrijSlotZonderKamer(Object[] tuple, String[] aliases)
	{
		var vrijSlot = new VrijSlotZonderKamer();
		for (var index = 0; index < aliases.length; index++)
		{
			var waarde = tuple[index];
			switch (aliases[index])
			{
			case "startTijd" -> vrijSlot.setStartTijd(waarde);
			case "eindTijd" -> vrijSlot.setEindTijd(waarde);
			case "intakelocatieId" -> vrijSlot.setIntakelocatieId(((Number) waarde).longValue());
			case "plaats" -> vrijSlot.setPlaats((String) waarde);
			case "naam" -> vrijSlot.setNaam((String) waarde);
			default -> throw new IllegalStateException("Onbekende alias " + aliases[index] + " voor vrij slot");
			}
		}
		return vrijSlot;
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc)
			.setTupleTransformer(this::maakVrijSlotZonderKamer);
		if (first >= 0)
		{
			query.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			query.setMaxResults(Ints.checkedCast(count));
		}
		return query.list();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc)
			.setTupleTransformer(this::maakVrijSlotZonderKamer);
		return query.getResultList();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, "niet", true)
			.setTupleTransformer(this::maakVrijSlotZonderKamer);
		return query.getResultList();
	}

	@Override
	public long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter)
	{
		var query = createVrijSlotZonderKamerQuery(filter, null, true);
		return ((Number) query.uniqueResult()).longValue();
	}

	private NativeQuery<?> createVrijSlotZonderKamerQuery(VrijSlotZonderKamerFilter filter, String sortProperty, boolean asc)
	{
		entityManager.flush();

		var querySB = new StringBuilder();

		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			querySB.append("""
				WITH blokkades as MATERIALIZED ( \
				   select vanaf, tot, kamer \
				   from colon.tijdslot b \
				   where b.type='BLOKKADE'\
				)""");

			if (filter.isDigitaleAfspraak() && afspraakService.isDigitaleAfspraakBeschikbaar())
			{

				querySB.append("""
					, eerste_vrije_slots AS MATERIALIZED ( \
					   SELECT k_sub.intakelocatie AS intakelocatie_id, DATE_TRUNC('week', ts_sub.vanaf) AS week, MIN(ts_sub.vanaf) AS min_vanaf \
					   FROM colon.tijdslot ts_sub \
					   JOIN colon.afspraakslot afs_sub ON afs_sub.id = ts_sub.id \
					   JOIN colon.intakekamer k_sub ON ts_sub.kamer = k_sub.id AND k_sub.actief = true \
					   WHERE ts_sub.type != 'BLOKKADE' \
					   AND NOT EXISTS(SELECT 1 FROM colon.intakeafspraak ia_sub WHERE ia_sub.afspraakslot = afs_sub.id AND (ia_sub.status = :status1 OR ia_sub.status = :status2)%s) \
					   AND NOT EXISTS(SELECT 1 FROM blokkades b_sub WHERE b_sub.kamer = ts_sub.kamer AND b_sub.vanaf < ts_sub.tot AND b_sub.tot > ts_sub.vanaf) \
					   AND DATE_TRUNC('week', ts_sub.vanaf) >= DATE_TRUNC('week', CAST(:vanaf AS TIMESTAMP)) \
					   AND DATE_TRUNC('week', ts_sub.vanaf) <= DATE_TRUNC('week', CAST(:totEnMet AS TIMESTAMP) - INTERVAL '1 day') \
					   GROUP BY k_sub.intakelocatie, DATE_TRUNC('week', ts_sub.vanaf) \
					)""".formatted(huidigeAfspraakClause("ia_sub", filter.getHuidigAfspraakId())));
			}
		}

		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(ts.vanaf, il.id))");
			}
			else
			{
				querySB.append(
					" select distinct ts.vanaf as \"startTijd\", ts.tot as \"eindTijd\", il.id as \"intakelocatieId\", a.plaats as \"plaats\", il.naam as \"naam\"");
			}
		}
		else
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(il.id))");
			}
			else
			{
				querySB.append(" select distinct il.id as \"intakelocatieId\", a.plaats as \"plaats\", il.naam as \"naam\"");
			}
		}

		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{
			querySB.append(" from colon.afspraakslot afs");
			querySB.append(" join colon.tijdslot ts on afs.id=ts.id");
			querySB.append(" join colon.intakekamer k on ts.kamer=k.id");
		}
		else
		{
			querySB.append(" from colon.intakekamer k");
		}
		querySB.append(" join algemeen.organisatie il on k.intakelocatie=il.id");
		querySB.append(" join gedeeld.adres a on il.adres=a.id");

		var params = new HashMap<String, Object>();
		querySB.append(" where il.actief = true");
		querySB.append(" and k.actief = true");
		if (!Boolean.TRUE.equals(filter.getAlleenIntakelocaties()))
		{

			if (!filter.isDigitaleAfspraak())
			{
				querySB.append(" and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot = afs.id and (ia.status=:status1 or ia.status=:status2)")
					.append(huidigeAfspraakClause("ia", filter.getHuidigAfspraakId()))
					.append(")");
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				voegHuidigeAfspraakIdToe(params, filter.getHuidigAfspraakId());
				querySB.append(" and not exists(select 1 from blokkades b where b.kamer=ts.kamer and b.vanaf<ts.tot and b.tot>ts.vanaf) ");
				querySB.append(" and ts.vanaf>:vanaf and ts.vanaf<:totEnMet");
			}
			else
			{

				querySB.append(" and DATE_TRUNC('week', ts.vanaf) >= DATE_TRUNC('week', CAST(:vanaf AS TIMESTAMP))");
				querySB.append(" and DATE_TRUNC('week', ts.vanaf) <= DATE_TRUNC('week', CAST(:totEnMet AS TIMESTAMP) - INTERVAL '1 day')");
			}
			params.put("vanaf", filter.getVanaf());
			params.put("totEnMet", DateUtil.plusDagen(filter.getTotEnMet(), 1));
			if (filter.getIntakelocatieId() != null)
			{
				querySB.append(" and il.id = :intakelocatieId");
				params.put("intakelocatieId", filter.getIntakelocatieId());
			}
			if (filter.getNietIntakelocatieId() != null)
			{
				querySB.append(" and il.id != :nietIntakelocatieId");
				params.put("nietIntakelocatieId", filter.getNietIntakelocatieId());
			}
		}
		if (StringUtils.isNotBlank(filter.getNaam()))
		{
			querySB.append(" and il.naam ILIKE '%' || :naam || '%'");
			params.put("naam", filter.getNaam());
		}
		if (StringUtils.isNotBlank(filter.getPlaats()))
		{
			querySB.append(" and plaats ILIKE '%' || :plaats || '%'");
			params.put("plaats", filter.getPlaats());
		}

		if (afspraakService.isDigitaleAfspraakBeschikbaar())
		{
			if (filter.isDigitaleAfspraak())
			{
				params.put("status1", ColonAfspraakStatus.GEPLAND.name());
				params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
				voegHuidigeAfspraakIdToe(params, filter.getHuidigAfspraakId());
				querySB.append(
					" and exists(select 1 from algemeen.organisatie_parameter op where op.organisatie = il.id and op.key = 'COLON_DIGITALE_INTAKE_ENABLED' and lower(op.value) = 'true')");

				querySB.append(
					" and exists(select 1 from eerste_vrije_slots evs where evs.intakelocatie_id = il.id and evs.min_vanaf = ts.vanaf)");
			}
			if (filter.isAsaScoreBovenDrie())
			{
				querySB.append(
					" and exists(select 1 from algemeen.organisatie_parameter op where op.organisatie = il.id and op.key = 'COLON_CLIENTEN_HOGE_ASA_SCORE_NIET_BEHANDELEN' and lower(op.value) = 'false')");
			}
		}

		if (sortProperty != null && !sortProperty.equals("niet"))
		{
			querySB.append(" order by");
			switch (sortProperty)
			{
			case "vanaf":
				querySB.append(" \"startTijd\"");
				break;
			case "naam":
				querySB.append(" \"naam\"");
				break;
			case "plaats":
				querySB.append(" \"plaats\"");
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty);
				break;
			}
			if (asc)
			{
				querySB.append(" asc");
			}
			else
			{
				querySB.append(" desc");
			}
		}

		var query = entityManager.createNativeQuery(querySB.toString()).unwrap(NativeQuery.class);
		for (var param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		return query;
	}

	private String huidigeAfspraakClause(String afspraakAlias, Long huidigeAfspraakId)
	{

		return huidigeAfspraakId != null ? " and " + afspraakAlias + ".id != :huidigeAfspraakId" : "";
	}

	private void voegHuidigeAfspraakIdToe(Map<String, Object> params, Long huidigeAfspraakId)
	{
		if (huidigeAfspraakId != null)
		{
			params.put("huidigeAfspraakId", huidigeAfspraakId);
		}
	}

	@Override
	public List<ColonIntakekamer> getKamers(LocalDateTime startTijd, Long intakelocatieId)
	{
		return getKamers(startTijd, intakelocatieId, null);
	}

	@Override
	public List<ColonIntakekamer> getKamers(LocalDateTime startTijd, Long intakelocatieId, Long huidigeAfspraakId)
	{
		var querySB = new StringBuilder();

		querySB.append("select {k.*}");

//fc342e9d-beb4-47c3-8871-ed20adc415a2
		querySB.append(" from colon.afspraakslot afs");
		querySB.append(" join colon.tijdslot ts on afs.id=ts.id");
		querySB.append(" join colon.intakekamer k on ts.kamer=k.id");

//fc342e9d-beb4-47c3-8871-ed20adc415a2
		var params = new HashMap<String, Object>();
		querySB.append(" and k.actief = true");
		querySB.append(" and not exists(select id from colon.intakeafspraak ia where ia.afspraakslot = afs.id and (ia.status=:status1 or ia.status=:status2)")
			.append(huidigeAfspraakClause("ia", huidigeAfspraakId))
			.append(")");
		params.put("status1", ColonAfspraakStatus.GEPLAND.name());
		params.put("status2", ColonAfspraakStatus.UITGEVOERD.name());
		voegHuidigeAfspraakIdToe(params, huidigeAfspraakId);
		querySB.append(
			" and not EXISTS(select b.id from colon.tijdslot b where b.type='BLOKKADE' "
				+ "and b.kamer=ts.kamer and b.vanaf<ts.tot and b.tot>ts.vanaf)");
		querySB.append(" and ts.vanaf=:startTijd");
		params.put("startTijd", startTijd);
		querySB.append(" and k.intakelocatie = :intakelocatieId");
		params.put("intakelocatieId", intakelocatieId);

		var query = entityManager.unwrap(Session.class).createNativeQuery(querySB.toString()).addEntity("k", ColonIntakekamer.class);
		for (var param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		return query.list();
	}

	@Override
	public long getAfspraakslotsCount(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie)
	{
		var query = createAfspraakslotsQuery(filter, intakeLocatie, null, true);

		return ((Number) query.uniqueResult()).longValue();
	}
}
