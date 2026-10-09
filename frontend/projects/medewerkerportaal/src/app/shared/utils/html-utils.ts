/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
import { SecurityContext } from '@angular/core'
import { DomSanitizer } from '@angular/platform-browser'

const HTML_PATROON = /<\/?[a-z][a-z0-9]*(?:\s[^<>]*)?\/?>|&(?:[a-z][a-z0-9]+|#\d+|#x[0-9a-f]+);/i

export const bevatHtml = (tekst: string | null | undefined): boolean => !!tekst && HTML_PATROON.test(tekst)

export const saneerHtml = (sanitizer: DomSanitizer, tekst: string | null | undefined): string => (tekst ? (sanitizer.sanitize(SecurityContext.HTML, tekst) ?? '') : '')
