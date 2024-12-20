/* --------------------------------------------------------------------------
 Forked from glue at:

 Version: 1.8.0.9000;
 Commit: a3f80d678274ef634c10c2cb094c939b1543222a;
 URL: https://github.com/tidyverse/glue/commit/a3f80d678274ef634c10c2cb094c939b1543222a

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
#include <string.h>  /* for strlen */

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#define DELIM_OPEN '{'
#define DELIM_CLOSE '}'

/*
 Note: We use a static variable as the R_getVar function in 4.5.0 will error
       if it cannot find the variable in the environment. We must then use
       an on.exit function to clean up on error/user interrupt.
 */
static char* str;

/*
 Note: It looks like R 4.5.0 will gain a useful function R_getVar. This is a
       slight variation on that function that should keep things working in
       earlier releases. The only difference a user should see is a slight
       change in error messages.
 */
#if R_VERSION < R_Version(4, 5, 0)
static SEXP R_getVar(SEXP sym, SEXP rho, Rboolean inherits)
{
    if (TYPEOF(sym) != SYMSXP)
    {
        Rf_error("first argument to '%s' must be a symbol (line %d of %s).\n", __func__, __LINE__, __FILE__);
    }

    if (TYPEOF(rho) != ENVSXP)
    {
        Rf_error("second argument to '%s' must be an environment (line %d of %s).\n", __func__, __LINE__, __FILE__);
    }

    if (!inherits)
    {
        Rf_error("Backport error in %s() (line %d of %s).\n", __func__, __LINE__, __FILE__);
    }

    SEXP val = Rf_findVar(sym, rho);
    if (val == R_MissingArg)
    {
        Rf_error("Backport error in %s() (line %d of %s).\n", __func__, __LINE__, __FILE__);
    }
    else if (val == R_UnboundValue)
    {
        Rf_error("object '%s' not found", Rf_translateCharUTF8(PRINTNAME(sym)));
    }
    else if (TYPEOF(val) == PROMSXP)
    {
	    PROTECT(val);
	    val = Rf_eval(val, rho);
	    UNPROTECT(1);
    }
    return val;
}
#endif

/*
 Note: I added an additional PROTECT to the original implementation as I'm
 unsure if SET_VECTOR_ELT() could trigger gc. Need to look in to this at some
 point.
 */
static SEXP set(SEXP x, int i, SEXP val) {
    int protected = 0;
    R_xlen_t len = Rf_xlength(x);
    if (i >= len)
    {
        len *= 2;
        x = PROTECT(Rf_xlengthgets(x, len)); protected++;
    }
    SET_VECTOR_ELT(x, i, val);
    UNPROTECT(protected);
    return x;
}

static SEXP resize(SEXP out, R_xlen_t n)
{
    if (n == Rf_xlength(out))
    {
        return out;
    }
    return Rf_xlengthgets(out, n);
}

SEXP glue(SEXP x, SEXP env)
{
    str = NULL;
    enum state {TEXT, ESCAPE, DELIM};

    const char* xx = Rf_translateCharUTF8(STRING_ELT(x, 0));
    size_t str_len = strlen(xx);
    str = (char*) R_Calloc(str_len + 1, char);

    SEXP out = Rf_allocVector(VECSXP, 1);
    PROTECT_INDEX out_idx;
    PROTECT_WITH_INDEX(out, &out_idx);

    size_t j = 0;
    size_t k = 0;
    int delim_level = 0;
    size_t start = 0;
    enum state state = TEXT;
    enum state prev_state = TEXT;
    for (size_t i = 0; i < str_len; ++i)
    {
        switch (state)
        {
            case TEXT:
                if (xx[i] == DELIM_OPEN)
                {
                    /* check for open delim doubled */
                    if (xx[i + 1] == DELIM_OPEN)
                    {
                        i++;
                    }
                    else
                    {
                        state = DELIM;
                        delim_level = 1;
                        start = i + 1;
                        break;
                    }
                }
                if (xx[i] == DELIM_CLOSE && xx[i + 1] == DELIM_CLOSE)
                {
                    i++;
                }
                str[j++] = xx[i];
                break;

        case ESCAPE:
            state = prev_state;
            break;

        case DELIM:
            if (xx[i] == DELIM_OPEN)
            {
                ++delim_level;
            }
            else if (xx[i] == DELIM_CLOSE)
            {
                --delim_level;
            }

            if (delim_level == 0)
            {
                /* Get the current glue statement */
                SEXP expr = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(&xx[start], i - start, CE_UTF8)));
                SEXP result = PROTECT(R_getVar(Rf_installChar(STRING_ELT(expr, 0)), env, TRUE));

                /* text in between last glue statement */
                if (j > 0)
                {
                    str[j] = '\0';
                    SEXP str_ = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(str, j, CE_UTF8)));
                    REPROTECT(out = set(out, k++, str_), out_idx);
                    UNPROTECT(1);
                }

                REPROTECT(out = set(out, k++, result), out_idx);

                /* Clear the string buffer */
                memset(str, 0, j);
                j = 0;
                UNPROTECT(2);
                state = TEXT;
            }
            break;
        };
    }

    if (k == 0 || j > 0)
	{
        str[j] = '\0';
        SEXP str_ = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(str, j, CE_UTF8)));
        REPROTECT(out = set(out, k++, str_), out_idx);
        UNPROTECT(1);
    }

    if (state == DELIM)
    {
       Rf_error("Expecting '%c'", DELIM_CLOSE);
    }

    out = resize(out, k);
    UNPROTECT(1);
    return out;
}

SEXP glue_free(void)
{
    if (str != NULL)
    {
        R_Free(str);
    }
    str = NULL;
    return R_NilValue;
}
