package nl.rivm.screenit.main.exception;

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

import java.io.IOException;
import java.util.stream.Collectors;

import jakarta.persistence.EntityNotFoundException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.util.ScreenitObjectMapper;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;

import org.apache.wicket.Application;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.multipart.support.MissingServletRequestPartException;
import org.springframework.web.servlet.resource.NoResourceFoundException;

@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
@ControllerAdvice
public class GlobalExceptionHandler
{
	private final ScreenitObjectMapper objectMapper;

	private final String applicationEnvironment;

	public GlobalExceptionHandler(ScreenitObjectMapper objectMapper, @Qualifier("applicationEnvironment") String applicationEnvironment)
	{
		this.objectMapper = objectMapper;
		this.applicationEnvironment = applicationEnvironment;
	}

	@ExceptionHandler(ValidatieException.class)
	public ResponseEntity<String> handleValidatieException(ValidatieException ex)
	{
		var message = ex.getFormattedMessage(GlobalExceptionHandler.this::getString);
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		if (ex instanceof FeestdagValidatieException)
		{
			node.set("afspraakslots", objectMapper.valueToTree(((FeestdagValidatieException) ex).getParameters()));
		}
		return ResponseEntity.unprocessableContent().body(node.toString());
	}

	@ExceptionHandler(BeperkingException.class)
	public ResponseEntity<String> handleBeperkingException(BeperkingException ex) throws IOException
	{
		var messages = ex.getExceptions().stream().map(exception -> exception.getFormattedMessage(GlobalExceptionHandler.this::getString)).collect(
			Collectors.toList());
		messages.forEach(LOG::error);
		var messagesArrayNode = objectMapper.valueToTree(messages);
		var node = objectMapper.createObjectNode();
		node.set("messages", messagesArrayNode);
		node.put("beperkingType", ex.getBeperkingType().toString());
		return ResponseEntity.unprocessableContent().body(objectMapper.writeValueAsString(node));
	}

	@ExceptionHandler(BulkAanmakenException.class)
	public ResponseEntity<String> handleBulkAanmakenException(BulkAanmakenException ex)
	{
		return ResponseEntity.unprocessableContent().body(ex.toJson());
	}

	@ExceptionHandler(OpslaanVerwijderenTijdBlokException.class)
	public ResponseEntity<String> handleTijdBlokOverlapException(OpslaanVerwijderenTijdBlokException ex)
	{
		var message = getString(ex.getMessage());
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		node.put("additionalInfo", ex.getAdditionalMessageInfo());
		return ResponseEntity.unprocessableContent().body(node.toString());
	}

	@ExceptionHandler(BulkVerwijderenException.class)
	public ResponseEntity<String> handleBulkVerwijderenException(BulkVerwijderenException ex)
	{
		LOG.error(ex.getMessage());
		return ResponseEntity.unprocessableContent().body(ex.toJson());
	}

	@ExceptionHandler(EntityNotFoundException.class)
	public ResponseEntity<String> handleEntityNotFoundException(EntityNotFoundException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.NOT_FOUND, "De gevraagde gegevens zijn niet gevonden");
	}

	@ExceptionHandler(BestandNietGevondenException.class)
	public ResponseEntity<String> handleBestandNietGevondenException(BestandNietGevondenException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.NOT_FOUND, ex.getMessage());
	}

	@ExceptionHandler(BestandsnaamInGebruikException.class)
	public ResponseEntity<String> handleBestandsnaamInGebruikException(BestandsnaamInGebruikException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.CONFLICT, ex.getMessage());
	}

	@ExceptionHandler(OngeldigeBestandsTypeException.class)
	public ResponseEntity<String> handleOngeldigeBestandsTypeException(OngeldigeBestandsTypeException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.BAD_REQUEST, ex.getMessage());
	}

	@ExceptionHandler(MaxUploadSizeExceededException.class)
	public ResponseEntity<String> handleMaxUploadSizeExceededException(MaxUploadSizeExceededException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.CONTENT_TOO_LARGE, "Bestand is te groot om te uploaden");
	}

	@ExceptionHandler(IllegalStateException.class)
	protected ResponseEntity<String> handleIllegalStateException(IllegalStateException ex)
	{
		var message = getString(ex.getMessage());
		LOG.error(message);
		return maakMeldingResponse(HttpStatus.BAD_REQUEST, message);
	}

	@ExceptionHandler(NoResourceFoundException.class)
	protected ResponseEntity<String> handleNoResourceFoundException(NoResourceFoundException ex)
	{

		var message = ex.getBody().getDetail();
		if (message != null && (applicationEnvironment.equalsIgnoreCase("development") && message.contains(".map") || message.contains("favicon.ico")))
		{
			return ResponseEntity.ok().build();
		}
		LOG.error(ex.getMessage());
		return maakMeldingResponse(HttpStatus.NOT_FOUND, ex.getMessage());
	}

	@ExceptionHandler(MissingServletRequestParameterException.class)
	protected ResponseEntity<String> handleMissingServletRequestParameterException(MissingServletRequestParameterException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.BAD_REQUEST, ex.getMessage());
	}

	@ExceptionHandler(MissingServletRequestPartException.class)
	protected ResponseEntity<String> handleMissingServletRequestPartException(MissingServletRequestPartException ex)
	{
		LOG.warn(ex.getMessage());
		return maakMeldingResponse(HttpStatus.BAD_REQUEST, ex.getMessage());
	}

	@ExceptionHandler(Exception.class)
	protected ResponseEntity<String> handleAllExceptions(Exception ex)
	{
		var message = getString("error.algemene.controller.error");
		LOG.error(message, ex);
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
	}

	private ResponseEntity<String> maakMeldingResponse(HttpStatusCode status, String message)
	{
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		return ResponseEntity.status(status).body(node.toString());
	}

	private String getString(String key)
	{
		return Application.get().getResourceSettings().getLocalizer().getString(key, null);
	}
}
