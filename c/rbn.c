/* Rbn engine : rbn.c - dynamical system  */

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

#include "rbn.h"

struct rbn_str
{
  point x0;           /* Initial state.  */
  point x;            /* Current state.  */
  point tmp;          /* used to check fixed points.  */
  net nt;
  ev evolver;
  int n_iter;         /* Iterations.  */
  int n;              /* Net's size.  */
  int fp_transient;
};

rbn
rbn_alloc (rbn self, int n, int k, double bias, int autoref,
	   int update, int n_iter)
{
  self = (rbn) xmalloc (sizeof (struct rbn_str));
  self->x0 = point_alloc (self->x0, n);
  point_init (self->x0);
  self->x = point_clone (self->x0);
  self->tmp = point_clone (self->x0);
  self->nt = net_alloc (self->nt, n, k);
  net_init (self->nt, bias, autoref);
  self->evolver = ev_alloc (self->evolver, self->nt, update);
  self->n_iter = n_iter;
  self->n = n;
  self->fp_transient = 0;
  return self;
}

rbn
rbn_alloc_from_net (rbn self, char*netname, int update, int n_iter)
{
  self = (rbn) xmalloc (sizeof (struct rbn_str));
  self->nt = net_build_from_file (self->nt, netname);
  self->n = net_get_n (self->nt);
  self->x0 = point_alloc (self->x0, self->n);
  point_init (self->x0);
  self->x = point_clone (self->x0);
  self->tmp = point_clone (self->x0);
  self->evolver = ev_alloc (self->evolver, self->nt, update);
  self->n_iter = n_iter;
  self->fp_transient = 0;
  return self;
}

rbn
rbn_init (rbn self)
{
  self->x0 = point_init (self->x0);
  point_copy (self->x0, self->x);
  net_reinit (self->nt);
  net_rewire (self->nt);
  return self;
}

rbn
rbn_reset (rbn self)
{
  point_copy (self->x0, self->x);
  return self;
}

point
rbn_get_x0 (rbn self)
{
  return self->x0;
}

int
rbn_get_n_iter (rbn self)
{
  return self->n_iter;
}

net
rbn_get_net (rbn self)
{
  return self->nt;
}

int
rbn_get_fp_transient (rbn self)
{
  return self->fp_transient;
}

ev
rbn_get_evolver (rbn self)
{
  return self->evolver;
}

rbn
rbn_set_x0 (rbn self, point x0)
{
  point_copy (x0, self->x0);
  point_copy (x0, self->x);
  return self;
}

rbn
rbn_set_net (rbn self, net nt)
{
  int update = ev_get_type (self->evolver);

  ev_free (self->evolver);
  net_free (self->nt);
  point_free (self->tmp);
  point_free (self->x);
  point_free (self->x0);
  self->n = net_get_n (nt);
  self->x0 = point_alloc (self->x0, self->n);
  self->x = point_clone (self->x0);
  self->tmp = point_clone (self->x0);
  self->nt = nt;
  self->evolver = ev_alloc (self->evolver, self->nt, update);
  return self;
}

point
rbn_next_point (rbn self)
{
  point_copy (self->x, self->tmp);
  ev_run (self->evolver, self->x, self->tmp);
  point_copy (self->tmp, self->x);
  return self->x;
}

/* 
 * Search for fixed points.
 * NB: Memory for "tmp" is allocated (if needed) by ev_check_fp().
 *
 */

point
rbn_search_fp (rbn self, int chkstp)
{
  int i;
  point tmp = 0;

  for (i = 0; i < self->n_iter; i++)
    {
      rbn_next_point (self);
      if (i > (chkstp - 1) && (i % chkstp) == 0)
	{
	  tmp = ev_check_fp (self->evolver, self->x);
	  if (tmp)
	    {
	      self->fp_transient = i;
	      return tmp;
	    }
	}
    }
  tmp = ev_check_fp (self->evolver, self->x);
  if (tmp)
    {
      self->fp_transient = self->n_iter;
      return tmp;
    }
  else
    return 0;
}

/*
 * Interface to ev_check_fp().
 *
 */

bool
rbn_check_fp (rbn self, point x)
{
  if (ev_check_fp (self->evolver, x))
    return 1;
  else
    return 0;
}

point *
rbn_dynamics (rbn self, point *dyn, int dyn_len)
{
  int i;

  assert (dyn && (dyn_len == self->n_iter));

  point_copy (self->x0, dyn [0]);
  for (i = 1; i < self->n_iter; i++)
    point_copy (rbn_next_point (self), dyn [i]);
  return dyn;
}

void
dyn_print (point *dyn, int length)
{
  int i;

  assert (dyn);

  for (i = 0; i < length; i++)
    point_print (dyn [i]);
}

void
rbn_free (rbn self)
{
  point_free (self->x0);
  point_free (self->x);
  point_free (self->tmp);
  net_free (self->nt);
  ev_free (self->evolver);
  free (self);
}

