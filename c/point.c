/* point.c - point represent the net state as an array of bools  */

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

#include "point.h"

struct point_str
{
  int n;
  bool *value;
};

point
point_alloc (point self, int n)
{
  assert (n < MAX_NODES);

  self = (point) xmalloc (sizeof (struct point_str));
  if (n == 0)
      self->value = NULL;
  else
    self->value = (bool*) xmalloc (n * sizeof (bool));
  self->n = n;
  return self;
}

point
point_init (point self)
{
  int i;
  
  assert (self);

  for (i = 0; i < self->n; i++)
    self->value [i] = DICE (2);
  return self;
}

point
point_init_from_string (point self, char*str, int n)
{
  int i;

  assert (self && !(n < self->n));

  for (i = 0; i < self->n ; i++)
    {
      if (str [i] == '0')
	self->value [i] = 0;
      else
	self->value [i] = 1;
    }
  return self;
}

point
point_clone (point self)
{
  point new = NULL;

  assert (self);

  new = point_alloc (new, point_get_n (self));
  point_copy (self, new);
  return new;
}

int
point_get_n (point self)
{
  return self->n;
}

bool
point_get_item (point self, int index)
{
  assert ((index >= 0) && (index < self->n));

  return self->value [index];
}

void
point_set_item (point self, int index, bool value)
{
  assert ((index >= 0) && (index < self->n));

  if (value == 0)
    self->value [index] = 0;
  else
    self->value [index] = 1;
}

void
point_flip_item (point self, int index)
{
  assert ((index >= 0) && (index < self->n));

  if (self->value [index] == 0)
    self->value [index] = 1;
  else
    self->value [index] = 0;
}

void
point_copy (point src, point dst)
{
  int i;

  assert (src->n == dst->n);

  for (i = 0; i < src->n; i++)
    dst->value [i] = src->value [i];
}

int
point_is_equal (point x, point y)
{
  int i;

  assert (x->n == y->n);

  if (x == y)
    return 1;
  for (i = 0; i < x->n; i++)
    if (x->value [i] != y->value [i])
      return 0;
  return 1;
}

point
point_find_max (point x, point y)
{
  int i;

  assert (x->n == y->n);

  for (i = 0; i < x->n; i++)
    {
      if (x->value [i] > y->value [i])
	return x;
      else if (x->value [i] < y->value [i])
	return y;
    }
  return x;
}

double
point_correlation (point x, point y)
{
  int i;
  int c = 0;

  assert (x->n == y->n);

  for (i = 0; i < x->n; i++)
    {
      if (x->value  [i] == y->value [i])
	c += 1;
      else
	c -= 1;
    }
  return (double) c / x->n;
}

void
point_print (point self)
{
  int i;

  if (!self)
    printf("NULL\n");
  else
    {
      for (i = 0; i < self->n; i++)
	printf ("%d", self->value [i]);
      printf ("\n");
    }
}

void
point_fprint (point self, FILE *fd)
{
  int i;

  if (!self)
    fprintf(fd, "NULL\n");
  else
    {
      for (i = 0; i < self->n; i++)
	fprintf (fd, "%d", self->value [i]);
      fprintf (fd, "\n");
    }
}

void
point_free (point self)
{
  if (self)
    {
      free (self->value);
      free (self);
    }
}
