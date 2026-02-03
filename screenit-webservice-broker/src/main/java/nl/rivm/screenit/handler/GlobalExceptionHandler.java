package nl.rivm.screenit.handler;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.http.HttpStatus;
import org.springframework.http.ProblemDetail;
import org.springframework.http.ResponseEntity;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
@Slf4j
@RequiredArgsConstructor
public class GlobalExceptionHandler
{
	private final ICurrentDateSupplier currentDateSupplier;

	@ExceptionHandler(IllegalArgumentException.class)
	public ResponseEntity<ProblemDetail> handleIncorrecteRequests(IllegalArgumentException exception)
	{
		LOG.warn("Er is een incorrect request binnen gekomen: {}, {}", exception.getClass().getSimpleName(), exception.getMessage());

		var problemDetail = ProblemDetail.forStatusAndDetail(HttpStatus.BAD_REQUEST, exception.getMessage());
		problemDetail.setTitle("Ongeldig request");
		problemDetail.setProperty("tijdstip", currentDateSupplier.getLocalDateTime());

		return ResponseEntity.badRequest().body(problemDetail);
	}

	@ExceptionHandler(HttpMediaTypeNotAcceptableException.class)
	public ResponseEntity<ProblemDetail> handleNietAcceptabelMediaType(HttpMediaTypeNotAcceptableException exception)
	{
		LOG.warn("Niet acceptabel/ondersteund media type: {}", exception.getMessage());

		var problemDetail = ProblemDetail.forStatus(HttpStatus.NOT_ACCEPTABLE);
		problemDetail.setTitle("Niet acceptabel");
		problemDetail.setDetail("Het gebruikte media type wordt niet ondersteund door dit endpoint.");
		problemDetail.setProperty("tijdstip", currentDateSupplier.getLocalDateTime());

		return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).body(problemDetail);
	}

	@ExceptionHandler(Exception.class)
	public ResponseEntity<ProblemDetail> handleOnverwachteFout(Exception exception)
	{
		LOG.error("Onverwachte fout", exception);

		var problemDetail = ProblemDetail.forStatus(HttpStatus.INTERNAL_SERVER_ERROR);
		problemDetail.setTitle("Interne fout");
		problemDetail.setDetail("Er is een onverwachte fout opgetreden.");
		problemDetail.setProperty("tijdstip", currentDateSupplier.getLocalDateTime());

		return ResponseEntity.internalServerError().body(problemDetail);
	}
}
