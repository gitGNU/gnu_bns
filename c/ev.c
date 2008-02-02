/* ev.c - evolver for a rbn: may be SYN or ASYN  */

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

#include "ev.h"

struct ev_str
{
  bool *inputs;  /* Inputs of a node. */
  net nt;        /* Net to evolve.  */
  int type;      /* Type of updating: SYNC or ASYNC.  */
  point tmp;     /* used for checkin fp. */
};


/* Evolve X to NEXT synch. */

static point
run_syn (ev self, point x, point next)
{
  int i;
  int n = net_get_n (self->nt);

  for (i = 0; i < n; i++)
    ev_update_point_item (self, x, next, i);
  return next;
}

/* Evolve X to NEXT asynch. */

static point
run_asyn (ev self, point x, point next)
{
  int i, rnd;
  int n = net_get_n (self->nt);

  for (i = 0; i < n; i++)
    {
      rnd = DICE (n);
      ev_update_point_item (self, x, next, rnd);
      point_copy (next, x);
    }
  return next;
}

/* Public functions.  */

ev
ev_alloc (ev self, net nt, int type)
{
  int n = net_get_n (nt);
  int k = net_get_k (nt);

  assert (nt);

	if (k < 0)
		k = n;

  self = (ev) xmalloc (sizeof (struct ev_str));
  self->inputs = (bool*) xmalloc (k * sizeof (bool));
  self->tmp = point_alloc (self->tmp, n);
  self->nt = nt;
  self->type = type;
  return self;
}

int
ev_get_net_size (ev self)
{
  assert (self);

  return net_get_n (self->nt);
}

int
ev_get_type (ev self)
{
	return self->type;
}

point
ev_run (ev self, point x, point next)
{
  assert (self && x && next);

  if (self->type == SYN)
    return run_syn (self, x, next);
  else if (self->type == ASYN)
    return run_asyn (self, x, next);
  else
    {
      printf ("Error: updating type not defined.\n");
      exit (1);
    }
}

void
ev_update_point_item (ev self, point src, point dst, int i)
{
  int j;
  node node = net_get_node (self->nt, i);
  int k = node_get_k_in (node);
  int *in = node_get_inputs (node);

  /* Get ancestors and put them in SELF->INPUTS. */
  for (j = 0; j < k; j++)
    self->inputs [j] = point_get_item (src, in [j]);
  /* Evaluate NODE's boolean function and set DST's Ith components. */
  point_set_item (dst, i, node_get_next_value (node, self->inputs));
}

point
ev_check_fp (ev self, point x)
{
  int i;
  int n = net_get_n (self->nt);

  for (i = 0; i < n; i++)
    {
      ev_update_point_item (self, x, self->tmp, i);
      if (point_get_item (self->tmp, i) != point_get_item (x,i))
	return 0;
    }
  return point_clone (x);
}

void
ev_free (ev self)
{
  if (self)
    {
      free (self->tmp);
      free (self->inputs);
      free (self);
    }
}
