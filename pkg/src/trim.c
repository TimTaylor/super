/* --------------------------------------------------------------------------
 Forked from glue at:

 Version: 1.8.0.9000;
 Commit: a3f80d678274ef634c10c2cb094c939b1543222a;
 URL: https://github.com/tidyverse/glue/commit/a3f80d678274ef634c10c2cb094c939b1543222a

 trim remains almost identical save:
 - Handling malloc failure.
 - size_t to R_xlen_t tweaks.
 -------------------------------------------------------------------------- */

/* --------------------------------------------------------------------------
 # MIT License

 Copyright (c) 2023 glue authors
 Copyright (c) 2024 super authors

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
 -------------------------------------------------------------------------- */

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>  /* for strlen, strspn, strncpy */

#define R_NO_REMAP
#include "R.h"
#include "Rinternals.h"

SEXP trim(SEXP x)
{
	R_xlen_t len = XLENGTH(x);
	SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
	for (R_xlen_t num = 0; num < len; ++num)
	{
		const char* xx = Rf_translateCharUTF8(STRING_ELT(x, num));
		size_t str_len = strlen(xx);
		char* str = malloc(sizeof(*str) * (str_len + 1));
		if (str == NULL)
		{
			Rf_error(
				"in %s(): Unable to allocate string of length %zu (line %d of %s)",
				__func__, str_len, __LINE__, __FILE__
			);
		}

		size_t start = 0;
		bool new_line = false;

		/* skip leading blanks on first line */
		while (start < str_len && (xx[start] == ' ' || xx[start] == '\t'))
		{
			++start;
		}

		/* Skip first newline */
		if (start < str_len && xx[start] == '\n')
		{
			new_line = true;
			++start;
		}

		size_t i = start;

		/* Ignore first line */
		if (!new_line)
		{
			while (i < str_len && xx[i] != '\n')
			{
				++i;
			}
			new_line = true;
		}

		size_t indent = 0;

		/* Maximum size of size_t */
		size_t min_indent = (size_t) - 1;

		/* find minimum indent */
		while (i < str_len)
		{
			if (xx[i] == '\n')
			{
				new_line = true;
				indent = 0;
			}
			else if (new_line)
			{
				if (xx[i] == ' ' || xx[i] == '\t')
				{
					++indent;
				}
				else
				{
					if (indent < min_indent)
					{
						min_indent = indent;
					}
					indent = 0;
					new_line = false;
				}
			}
			++i;
		}

		/* if string ends with '\n', `indent = 0` only because we made it so */
		if (xx[str_len - 1] != '\n' && new_line && indent < min_indent)
		{
			min_indent = indent;
		}

		new_line = true;
		i = start;
		size_t j = 0;

		/* copy the string removing the minimum indent from new lines */
		while (i < str_len)
		{
			if (xx[i] == '\n')
			{
				new_line = true;
			}
			else if (xx[i] == '\\' && i + 1 < str_len && xx[i + 1] == '\n')
			{
				new_line = true;
				i += 2;
				continue;
			} else if (new_line)
			{
				size_t skipped = strspn(xx + i, "\t ");
				/*
				 * if the line consists only of tabs and spaces, and if the line is
				 * shorter than min_indent, copy the entire line and proceed to the
				 * next
				 */
				if (*(xx + i + skipped) == '\n' && skipped < min_indent)
				{
					strncpy(str + j, xx + i, skipped);
					i += skipped;
					j += skipped;
				}
				else
				{
					if (i + min_indent < str_len && (xx[i] == ' ' || xx[i] == '\t'))
					{
						i += min_indent;
					}
				}
				new_line = false;
				continue;
			}
			str[j++] = xx[i++];
		}
		str[j] = '\0';

		/* Remove trailing whitespace up to the first newline */
		size_t end = j;
		while (j > 0)
		{
			if (str[j] == '\n')
			{
				end = j;
				break;
			}
			else if (str[j] == '\0' || str[j] == ' ' || str[j] == '\t')
			{
				--j;
			}
			else
			{
				break;
			}
		}
		str[end] = '\0';
		SET_STRING_ELT(out, num, Rf_mkCharCE(str, CE_UTF8));
		free(str);
		str = NULL;
	}

	UNPROTECT(1);
	return out;
}
