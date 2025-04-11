package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import java.time.LocalDate;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import javax.crypto.SecretKey;

import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.JwtService;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

@Service
public class JwtServiceImpl implements JwtService
{
	@Value("${spring.security.jwt.secret-key}")
	private String secretKey;

	@Value("${spring.security.jwt.expiration-time}")
	private long jwtExpiration;

	private final static long CLOCK_SKEW_SECONDS = 60;

	public String extractSubject(String token)
	{
		return extractClaim(token, Claims::getSubject);
	}

	public <T> T extractClaim(String token, Function<Claims, T> claimsResolver)
	{
		final Claims claims = extractAllClaims(token);
		return claimsResolver.apply(claims);
	}

	public String generateToken(UserDetails userDetails)
	{
		var huisarts = (Huisarts) userDetails;
		var extraClaims = new HashMap<String, Object>();
		extraClaims.put("userId", huisarts.getHuisartsportaalId().toString());
		return generateToken(extraClaims, userDetails);
	}

	public String generateToken(Map<String, Object> extraClaims, UserDetails userDetails)
	{
		return buildToken(extraClaims, userDetails, jwtExpiration);
	}

	private String buildToken(
		Map<String, Object> extraClaims,
		UserDetails userDetails,
		long expiration
	)
	{
		return Jwts
			.builder()
			.claims()
			.add(extraClaims)
			.subject(userDetails.getUsername())
			.issuedAt(new Date(System.currentTimeMillis()))
			.expiration(new Date(System.currentTimeMillis() + expiration))
			.and()
			.signWith(getSignInKey())
			.compact();
	}

	public boolean isTokenValid(String token, UserDetails userDetails)
	{
		final String username = extractSubject(token);
		return (username.equals(userDetails.getUsername())) && !isTokenExpired(token);
	}

	private boolean isTokenExpired(String token)
	{
		return extractExpiration(token).isBefore(LocalDate.now());
	}

	private LocalDate extractExpiration(String token)
	{
		var expiration = extractClaim(token, Claims::getExpiration);
		return DateUtil.toLocalDate(expiration);
	}

	private Claims extractAllClaims(String token)
	{
		return Jwts
			.parser()
			.clockSkewSeconds(CLOCK_SKEW_SECONDS)
			.verifyWith(getSignInKey())
			.build()
			.parseSignedClaims(token)
			.getPayload();
	}

	private SecretKey getSignInKey()
	{
		byte[] keyBytes = Decoders.BASE64.decode(secretKey);
		return Keys.hmacShaKeyFor(keyBytes);
	}
}
