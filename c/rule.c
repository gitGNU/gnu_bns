/* rule.c - boolean function for a node  */

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

#include "rule.h"

#define GET_LENGTH(f) 1 << (f)->inputs

struct rule_str
{
  int inputs;
  bool *f;
};

/*
 * Constructors.
 *
 */

rule
rule_alloc (rule self, int inputs)
{
  int length = 1 << inputs;

  assert (inputs >=0 && inputs < MAX_K);

  self = (rule) xmalloc (sizeof (struct rule_str));
  self->inputs = inputs;
  self->f = (bool*) xmalloc (length * sizeof (bool));
  return self;
}

rule
rule_realloc (rule self, int inputs)
{
  int length = 1 << inputs;

  assert (self && inputs >=0 && inputs < MAX_K);

  self->inputs = inputs;
  self->f = (bool*) xrealloc (self->f, length * sizeof (bool));
  return self;
}

/*
 * Init SELF at random.
 * BIAS is the weigth of 1, it must be between 0.0 and 1.0.
 *
 */

rule
rule_init (rule self, double bias)
{
  int i;
  int length = GET_LENGTH (self);

  assert (self && bias >= 0.0 && bias <= 1.0);

  for (i = 0; i < length; i++)
    if (RANDOM () < bias)
      self->f [i] = 1;
    else
      self->f [i] = 0;
  return self;
}

/*
 * Mutators.
 *
 */

void
rule_set (rule self, bool *value, int size)
{
  int i;
  int length = GET_LENGTH (self);

  assert (self && length == size);

  for (i = 0; i < length; i++)
    self->f [i] = value [i];
}

/*
 * Accessors.
 *
 */

bool
rule_get_value (rule self, int i)
{
  assert (i >= 0 && i < (1 << self->inputs));
  return self->f [i];
}

int
rule_get_length (rule self)
{
  return GET_LENGTH (self);
}

/*
 * Print on stdout.
 *
 */

void
rule_save (rule self, FILE *fd)
{
  int i;
  int length = GET_LENGTH (self);

  for (i = 0; i < length; i++)
    fprintf (fd, "%d", self->f [i]);
}

void
rule_print (rule self)
{
  int i;
  int length = GET_LENGTH (self);

  for (i = 0; i < length; i++)
    printf ("%d", self->f [i]);
}

/*
 * Destructor.
 *
 */

void
rule_free (rule self)
{
  free (self->f);
  free (self);
}
