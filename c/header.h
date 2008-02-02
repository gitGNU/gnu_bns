/* header.h  various utilities, limits, compiling alternatives ...  */

/*
 * Author: Benelli Marco <mbenelli@yahoo.com>
 * Date: Juny 2004
 *
 */

/*
 * Copyright (c) 2004 Benelli Marco
 *
 * This file is part of BNS.
 *
 * BNS is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * BNS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BNS; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */

#ifndef HEADER_H
#define HEADER_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifdef USE_GSL
#include <gsl/gsl_rng.h>
#endif /* USE_GSL */

/*
 * Define INLINE if compiler support it.
 *
 */

#ifdef HAS_INLINE
#define INLINE inline
#else
#define INLINE
#endif /* HAS_INLINE */

/*
 * Memory allocation: if _GNU_C_ make them as macros.
 *
 */

#define xmalloc(size)                \
({                                   \
  void *x = malloc ((size));         \
  assert (x);                        \
  x;                                 \
})

#define xrealloc(ptr, size)          \
({                                   \
  void *tmp = (ptr);                 \
  if (tmp)                           \
    tmp = realloc (tmp, (size));     \
  else                               \
    tmp = malloc ((size));           \
  assert (tmp);                      \
  tmp;                               \
})

/* void * */
/* xmalloc (size_t size) */
/* { */
/*   void *x = malloc (size); */
/*   assert (x); */
/*   return x;      */
/* } */

/* void * */
/* xrealloc (void *ptr, size_t size) */
/* { */
/*   void *tmp = (ptr); */
/*   if (tmp) */
/*     tmp = realloc (tmp, size); */
/*   else */
/*     tmp = malloc (size); */
/*   assert (tmp); */
/*   return tmp;                         */
/* } */

/* 
 * Booleans.
 *
 */

typedef int bool;

/*
 * Random number generator.
 *
 */

#ifdef USE_GSL

gsl_rng *random_number_generator;
#define GSL_RBN_INIT random_number_generator = gsl_rng_alloc (gsl_rng_taus)
#define SRAND(seed) gsl_rng_set (random_number_generator, (seed))
#define DICE(n) (bool) gsl_rng_uniform_int (random_number_generator, (n))
#define RANDOM() (double) gsl_rng_uniform (random_number_generator)

#else /* USE_GLS not defined, uses stdlib functions.  */

#define SRAND(seed) srand(seed)
#define DICE(n) (random () % n)
#define RANDOM() ((double) random() / RAND_MAX)

#endif /* USE_GSL*/

/*
 * Reading lines from a file, appending '\0'.
 * Return number of character read (including '\0') or 0 if EOF.
 *
 */

int
get_next_line (FILE *fd, char *s);

/*
 * Read a sequence of int value from BUFFER and put them in VALUE.
 * BUFFER and VALUE must be already allocated.
 *
 */

int
get_int_values (char *buffer, int *value);

/*
 * Cat PREFIX, ID, EXT > FILENAME
 * All char* must be already allocated.
 *
 */

char *
create_filename (char *filename, char *prefix, int id, char *ext);

/* 
 * Limits.
 *
 */

#define MAX_NODES 10000
#define MAX_K 32
#define MAX_BUFFER_SIZE 1024

#endif /* HEADER_H */
