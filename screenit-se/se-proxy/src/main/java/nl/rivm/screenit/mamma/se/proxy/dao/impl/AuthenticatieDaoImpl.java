package nl.rivm.screenit.mamma.se.proxy.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.dao.AuthenticatieDao;
import nl.rivm.screenit.mamma.se.proxy.model.IngelogdeMedewerkerDto;
import nl.rivm.screenit.mamma.se.proxy.model.LoginContext;
import nl.rivm.screenit.mamma.se.proxy.model.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class AuthenticatieDaoImpl extends BaseDaoImpl implements AuthenticatieDao
{
	@Autowired
	private ConfiguratieService configuratieService;

	@Override
	public IngelogdeMedewerkerDto getIngelogdeMedewerker(LoginContext loginContext)
	{
		var query = "SELECT IM.gebruikersnaam, IM.wachtwoord, IM.laatste_inlog, IM.yubikey_public, IM.login_response, IMaccount_id " +
			"FROM INGELOGDE_MEDEWERKER IM " +
			"WHERE IM.gebruikersnaam=? " +
			"AND IM.wachtwoord=? " +
			"AND IM.yubikey_public=?;";

		try (Connection dbConnection = getConnection();
			PreparedStatement statement = dbConnection.prepareStatement(query))
		{
			statement.setString(1, loginContext.getGebruikersnaam());
			statement.setString(2, loginContext.getEncryptedWachtwoord());
			statement.setString(3, loginContext.getYubikeyIdentificatie());
			ResultSet resultSet = statement.executeQuery();
			if (resultSet.next())
			{
				return getIngelogdeMedewerkerFromResultSet(resultSet);
			}
		}
		catch (SQLException e)
		{
			LOG.warn("Er is een probleem met het ophalen van een ingelogde medewerker met id {}: {}", loginContext.getAccountId(), e.getMessage());
			throw new IllegalStateException("Ophalen van ingelogde medewerker ging fout.");
		}
		return null;
	}

	@Override
	public void insertOrUpdateIngelogdeMedewerker(IngelogdeMedewerkerDto ingelogdeMedewerkerDto)
	{
		var sql = "INSERT INTO INGELOGDE_MEDEWERKER(gebruikersnaam, wachtwoord, laatste_inlog, yubikey_public, login_response, account_id)" +
			" VALUES (?, ?, ?, ? ,?, ?)" +
			" ON CONFLICT(account_id)" +
			" DO UPDATE SET gebruikersnaam = ?, wachtwoord = ?, laatste_inlog = ?, yubikey_public = ?, login_response = ?, account_id = ?;";
		try (Connection connection = getConnection();
			PreparedStatement insertStatement = connection.prepareStatement(sql))
		{
			insertStatement.setString(1, ingelogdeMedewerkerDto.getGebruikersnaam());
			insertStatement.setString(2, ingelogdeMedewerkerDto.getWachtwoord());
			insertStatement.setString(3, ingelogdeMedewerkerDto.getLaatsteInlog().toString());
			insertStatement.setString(4, ingelogdeMedewerkerDto.getYubikeyIdentificatie());
			insertStatement.setString(5, ingelogdeMedewerkerDto.getLoginResponse());
			insertStatement.setLong(6, ingelogdeMedewerkerDto.getAccountId());

			insertStatement.setString(7, ingelogdeMedewerkerDto.getGebruikersnaam());
			insertStatement.setString(8, ingelogdeMedewerkerDto.getWachtwoord());
			insertStatement.setString(9, ingelogdeMedewerkerDto.getLaatsteInlog().toString());
			insertStatement.setString(10, ingelogdeMedewerkerDto.getYubikeyIdentificatie());
			insertStatement.setString(11, ingelogdeMedewerkerDto.getLoginResponse());
			insertStatement.setLong(12, ingelogdeMedewerkerDto.getAccountId());
			insertStatement.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Er ging iets fout bij toevoegen van een ingelogde medewerker met id {}: {}, na het draaien van de lokale filler moet je handmatig de"
				+ " tabel ingelogde_medewerker legen in de SE database in bestand se-proxy/db/screenit-se.db", ingelogdeMedewerkerDto.getAccountId(), e.getMessage());
			throw new IllegalStateException("Toevoegen van ingelogde medewerker ging fout.");
		}
	}

	@Override
	public void updateIngelogdeMedewerker(IngelogdeMedewerkerDto ingelogdeMedewerkerDto)
	{
		var sql = "UPDATE INGELOGDE_MEDEWERKER " +
			"SET gebruikersnaam = ?, wachtwoord = ?, laatste_inlog = ?, yubikey_public = ?, login_response = ?, account_id = ? " +
			"WHERE gebruikersnaam = ? ;";
		try (Connection connection = getConnection();
			PreparedStatement insertStatement = connection.prepareStatement(sql))
		{
			insertStatement.setString(1, ingelogdeMedewerkerDto.getGebruikersnaam());
			insertStatement.setString(2, ingelogdeMedewerkerDto.getWachtwoord());
			insertStatement.setString(3, ingelogdeMedewerkerDto.getLaatsteInlog().toString());
			insertStatement.setString(4, ingelogdeMedewerkerDto.getYubikeyIdentificatie());
			insertStatement.setString(5, ingelogdeMedewerkerDto.getLoginResponse());
			insertStatement.setLong(6, ingelogdeMedewerkerDto.getAccountId());
			insertStatement.setString(7, ingelogdeMedewerkerDto.getGebruikersnaam());
			insertStatement.execute();
		}
		catch (SQLException e)
		{
			LOG.warn("Er ging iets fout bij updaten van een ingelogde medewerker met id {}: {}", ingelogdeMedewerkerDto.getAccountId(), e.getMessage());
			throw new IllegalStateException("Updaten van ingelogde medewerker ging fout.");
		}
	}

	@Override
	public Long getAccountIdFromUsername(String gebruikersnaam)
	{
		var query = "SELECT IM.account_id " +
			"FROM INGELOGDE_MEDEWERKER IM " +
			"WHERE IM.gebruikersnaam=?;";

		try (Connection dbConnection = getConnection();
			PreparedStatement statement = dbConnection.prepareStatement(query))
		{
			statement.setString(1, gebruikersnaam);
			ResultSet resultSet = statement.executeQuery();
			if (resultSet.next())
			{
				return resultSet.getLong(1);
			}
		}
		catch (SQLException e)
		{
			throw new IllegalStateException("Ophalen van ingelogde medewerker ging fout.");
		}
		return null;
	}

	@Override
	public void verwijderOudeIngelogdeMedewerkers()
	{
		int termijn = configuratieService.getConfiguratieIntegerValue(SeConfiguratieKey.SE_MAX_OFFLINE_INLOG_PERIODE);

		String sql = "DELETE FROM INGELOGDE_MEDEWERKER WHERE laatste_inlog < ?;";
		try (Connection connection = getConnection();
			PreparedStatement removeTransactie = connection.prepareStatement(sql))
		{
			removeTransactie.setString(1, DateUtil.getCurrentDateTime().toLocalDate().minusDays(termijn).toString());
			removeTransactie.execute();
		}
		catch (SQLException e)
		{
			LOG.error("Er is een probleem met het verwijderen van de oude ingelogde medewerkers: {}", e.getMessage());
		}
	}

	private IngelogdeMedewerkerDto getIngelogdeMedewerkerFromResultSet(ResultSet resultSet) throws SQLException
	{
		return new IngelogdeMedewerkerDto(resultSet.getString(1), resultSet.getString(2), LocalDate.parse(resultSet.getString(3)),
			resultSet.getString(4), resultSet.getString(5), resultSet.getLong(6));
	}
}
