package nl.rivm.screenit.huisartsenportaal.exception;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.CredentialsExpiredException;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.server.ResponseStatusException;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import io.jsonwebtoken.ExpiredJwtException;

@Slf4j
@AllArgsConstructor
@Order(Ordered.HIGHEST_PRECEDENCE)
@RestControllerAdvice
public class GlobalExceptionHandler
{
	private static final ObjectMapper objectMapper = new ObjectMapper();

	@ExceptionHandler(UserNotFoundException.class)
	public ResponseEntity<String> handleUserNotFoundException(UserNotFoundException ex)
	{
		var node = getMessageObject(ex);
		return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(node.toString());
	}

	@ExceptionHandler(BadCredentialsException.class)
	public ResponseEntity<String> handleBadCredentialsException(BadCredentialsException ex)
	{
		var node = getMessageObject(ex);
		return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(node.toString());
	}

	@ExceptionHandler(CredentialsExpiredException.class)
	public ResponseEntity<String> handleCredentialsExpiredException(CredentialsExpiredException ex)
	{
		var node = getMessageObject(ex);
		return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(node.toString());
	}

	@ExceptionHandler(ExpiredJwtException.class)
	public ResponseEntity<String> handleExpiredJwtException(ExpiredJwtException ex)
	{
		var node = getMessageObject(ex, "Uw sessie is verlopen. Om door te gaan dient u opnieuw in te loggen.");
		return ResponseEntity.status(HttpStatus.FORBIDDEN).body(node.toString());
	}

	@ExceptionHandler(ResponseStatusException.class)
	public ResponseEntity<String> handleResponseStatusException(ResponseStatusException ex)
	{
		var node = getMessageObject(ex);
		return ResponseEntity.status(HttpStatus.FORBIDDEN).body(node.toString());
	}

	@ExceptionHandler(InternalAuthenticationServiceException.class)
	protected ResponseEntity<Object> handleInternalAuthenticationServiceException(InternalAuthenticationServiceException ex)
	{
		var node = getMessageObject(ex);
		node.put("exception", ex.getCause().getClass().getSimpleName());
		return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(node.toString());
	}

	@ExceptionHandler(ValidatieException.class)
	protected ResponseEntity<Object> handleValidatieException(ValidatieException ex)
	{
		LOG.error(ex.getMessage(), ex);
		var node = getMessageObject(ex, ex.getValidatieMessage());
		return ResponseEntity.badRequest().body(node.toString());
	}

	@ExceptionHandler(IllegalStateException.class)
	protected ResponseEntity<Object> handleIllegalStateException(IllegalStateException ex)
	{
		var node = getMessageObject(ex);
		return ResponseEntity.badRequest().body(node.toString());
	}

	@ExceptionHandler(Exception.class)
	protected ResponseEntity<Object> handleAllExceptions(Exception ex)
	{
		LOG.error(ex.getMessage(), ex);
		var node = getMessageObject(ex, "Er ging iets mis");
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(node.toString());
	}

	public static ObjectNode getMessageObject(Exception ex, String message)
	{
		var node = getMessageObject(ex);
		if (message != null)
		{
			node.put("message", message);
		}
		return node;
	}

	public static ObjectNode getMessageObject(Exception ex)
	{
		var message = ex.getMessage();
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		node.put("exception", ex.getClass().getSimpleName());
		return node;
	}

}
