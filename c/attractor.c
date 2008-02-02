/* attractor.h - attractor: same struct is used to cycles and looses  */

/*
 * Author: Benelli Marco <mbenelli@yahoo.com>
 * Date: Juny 2004
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
 */

#include "attractor.h"

#define DEFAULT_RANGE 300

struct attractor_str
{
  point *x;
  int *freq;
  point name;
  point tmp;
  int period;
  int basin;
  int range;
  int n_fixed_nodes;
  int *fixed_nodes;
  int n_floating_nodes;
  int *floating_nodes;
  int n_fixed_nodes_doc;
  bool is_attractor; /* 1 if SELF is an attractor, 0 otherwise. */
};

/* 
 * Private.
 *
 */

/*
 * Check if P is already contained in SELF. 
 *
 */

static int
already_exist (attractor self, point p)
{
  int i;

  assert (self && p);

  for (i = 0; i < self->period; i++)
    if (point_is_equal (self->x [i], p))
      {
	/* self->freq [i] ++; */
	return 1;
      }
  return 0;
}

/*
 * Add P to SELF. Realloc SELF.
 *
 */

static void
add_point (attractor self, point p)
{
  assert (self && p);

  self->period ++;
  self->x = (point *) xrealloc (self->x, self->period * sizeof (point));
  self->x [self->period - 1] = point_clone (p);
  /*
  self->freq = (int*) xrealloc (self->freq, self->period * sizeof (int));
  self->freq [self->period - 1] = 1;
  */
}

/*
 * Check if P can evolve to a point that isn't in SELF. 
 * Only next step is checked.
 * Return true if P can't evolve outside SELF (in one step), false otherwise.
 */

static bool
next_is_in (attractor self, rbn rbn, point p)
{
  int i;
  int j;
  int counter = 0;
  int n = point_get_n (p);
  point tmp = 0;
  ev evolver = rbn_get_evolver (rbn);

  assert (self && p);

  tmp = point_alloc (tmp, n);
  for (i = 0; i < n; i++)
    {
      point_copy (p, tmp);
      ev_update_point_item (evolver, p, tmp, i); /* Evolve Ith node.  */
      if (point_is_equal (tmp, p)) /* point isn't changed */
	counter ++; /* increment COUNTER and go to next node.*/
      else /* point is changed */
	for (j = 0; j < self->period; j++)
	  if (point_is_equal (tmp, self->x [j])) /* new point is in SELF->X */
	    counter ++; /* increment COUNTER and go to next node */
    }
  point_free (tmp);
  if (counter == n)
    return 1;
  else
    return 0;
}

/*
 * Return SELF's name (point with higher boolean value).
 *
 */

static point
eval_name (attractor self)
{
  int i;

  if (self->period == 0)
    return NULL;
  self->name = self->x [0];
  for (i = 1; i < self->period; i++)
    self->name = point_find_max (self->name, self->x [i]);
  return self->name;
}

/*
 * Identify nodes that have the same value in each point of SELF
 * and nodes that change their values.
 *
 */

static void
eval_fixed_nodes (attractor self)
{
  int i;
  int j;
  int n = point_get_n (self->x [0]);
  bool item;
  int counter;

  for (i = 0; i < n; i++)
    {
      counter = 1;
      item = point_get_item (self->x [0], i);
      for (j = 1; j < self->period; j ++)
	if (item == point_get_item (self->x [j], i))
	  counter ++;
      if (counter == self->period)
	/* Node is fixed.  */
	{
	  self->n_fixed_nodes ++;
	  self->fixed_nodes = (int*) xrealloc (self->fixed_nodes,
					       self->n_fixed_nodes
					       * sizeof (int));
	  self->fixed_nodes [self->n_fixed_nodes - 1] = i;
	}
      else
	/* Node is floating. */
	{
	  self->n_floating_nodes ++;
	  self->floating_nodes = (int*) xrealloc (self->floating_nodes,
						  self->n_floating_nodes
						  * sizeof (int));
	  self->floating_nodes [self->n_floating_nodes - 1] = i;
	}
    }
}

/*
 * Try to update fixed nodes and check if they are really fixed.
 * Note: it's a really weak test, usefull only to guarantee that a node
 * could not be thinked fixed only because it never updates.
 *
 */

static int
check_fixed_nodes (attractor self, rbn rbn, point p)
{
  int i;
  int fixed = self->n_fixed_nodes;
  int *v = self->fixed_nodes;
  int n = point_get_n (p);
  int counter = 0;
  ev evolver = rbn_get_evolver (rbn);
  point tmp = 0;

  tmp = point_alloc (tmp, n);
  for (i = 0; i < fixed; i++)
    {
      point_copy (p, tmp);
      ev_update_point_item (evolver, p, tmp, v [i]);
      if (point_get_item (p, v [i]) == point_get_item (tmp, v [i]))
	counter ++;
    }
  point_free (tmp);
  return counter;
}

/*
 * Given a point try do "add 1" as it floating nodes would be a binary number.
 * Fixed nodes are untouched.
 * Usefull to obtain each possible points with given fixed nodes.
 *
 */

static point
increment (attractor self, point x, int i)
{
  int index = self->floating_nodes [i];

  if (i == (self->n_floating_nodes - 1) 
      && (point_get_item (x, index) == 1))
    return x;
  if (point_get_item (x, index) == 0)
    {
      point_set_item (x, index, 1);
      return x;
    }
  else
    {
      point_set_item (x, index, 0);
      return increment (self, x, i + 1);
    }
}

/*
 * Window things. Used for searchin attractors in synchronous nets.
 *
 */

void
win_fill (point *win, rbn rbn, int size)
{
  int i = 0;

  win [0] = point_clone (rbn_get_x0 (rbn));
  for (i = 1; i < size; i++)
    win [i] = point_clone (rbn_next_point (rbn));
}

attractor
win_step (attractor self, point *win, rbn rbn, int size)
{
  int i;
  int j;
  point tmp;

  tmp = win [0];
  for (i = 0; i < size - 1; i++)
    win [i] = win [i + 1];
  win [size - 1] = tmp;
  win [size - 1] = rbn_next_point (rbn);
  for (i = size - 2; i >= 0; i--)
    if (point_is_equal (win [size - 1], win [i]))
      {
	for (j = i; j < size; j++)
	  add_point (self, win [j]);
	self->is_attractor = 1;
	self->name = eval_name (self);
	eval_fixed_nodes (self);
	return self;
      }
  self->is_attractor = 0;
  return NULL;
}

/*
 * Let X a point, evolve _one_ floating nodes and add resulting point to SELF.
 * Repeat for each floating nodes. 
 *
 */

static void
find_next (attractor self, rbn rbn, point x)
{
  int i;
  int n = self->n_floating_nodes;
  ev evolver = rbn_get_evolver (rbn);
  int *fn = self->floating_nodes;
  point tmp;

  tmp = point_clone (x);
  for (i = 0; i < n; i++)
    {
      point_copy (x, tmp);
      ev_update_point_item (evolver, x, tmp, fn [i]);
      if (!point_is_equal (tmp, x))
	if (!already_exist (self, tmp))
	  add_point (self, tmp);
    }
  point_free (tmp);
}

/*
 * Public (descripted in attractor.h).
 *
 */

attractor
attractor_alloc (attractor self)
{
  self = (attractor) xmalloc (sizeof (struct attractor_str));
  self->x = NULL;
  self->period = 0;
  self->basin = 0;
  self->range = DEFAULT_RANGE;
  self->is_attractor = 0;
  self->n_fixed_nodes = 0;
  self->fixed_nodes = NULL;
  self->n_floating_nodes = 0;
  self->floating_nodes = NULL;
  return self;
}

attractor
attractor_init (attractor self)
{
  int i;

  self->name = NULL;
  for (i = 0; i < self->period; i++)
    point_free (self->x [i]);
  self->period = 0;
  self->basin = 0;
  free (self->x);
  self->x = NULL;
  self->is_attractor = 0;
  self->n_fixed_nodes = 0;
  free (self->fixed_nodes);
  self->fixed_nodes = NULL;
  self->n_floating_nodes = 0;
  free (self->floating_nodes);
  self->floating_nodes = NULL;
  self->n_fixed_nodes_doc = 0;
  return self;
}

attractor
attractor_create_dummy (attractor self)
{
	point x = 0;
	
	x = point_alloc (x, 0);	
	/*	x = point_init_from_string (x, "11", 2); */

	self = attractor_alloc (self);
 	self = attractor_init (self);
	self->range = 0;
	add_point (self, x);
	self->name = eval_name (self);
	self->is_attractor = 1;
	self->period = 1;
	free (x);
	return self;
}

void
attractor_set_range (attractor self, int range)
{
  self->range = range;
}

void
attractor_set_is_attractor (attractor self, bool flag)
{
  self->is_attractor = flag;
}

void
attractor_set_basin (attractor self, int basin_size)
{
  self->basin = basin_size;
}

int
attractor_get_range (attractor self)
{
  return self->range;
}

int
attractor_get_period (attractor self)
{
  return self->period;
}

int
attractor_get_basin (attractor self)
{
  return self->basin;
}

int
attractor_get_fixed_nodes (attractor self)
{
  return self->n_fixed_nodes;
}

int
attractor_get_floating_nodes (attractor self)
{
  return self->n_floating_nodes;
}

bool
attractor_is_fixed_nodes_really_fixed (attractor self)
{
  return self->n_fixed_nodes_doc;
}

bool
attractor_get_is_attractor (attractor self)
{
  return self->is_attractor;
}

point
attractor_get_item (attractor self, int i)
{
  assert (i >= 0 && i <= self->period);

  return self->x [i];
}

int
attractor_get_item_freq (attractor self, int i)
{
  assert (i>=0 && i <= self->period);

  return self->freq [i];
}

point
attractor_get_name (attractor self)
{
	eval_name (self);
  return self->name;
}

attractor
attractor_search_loose (attractor self, point *dyn, int n)
{
  int i;
  int start = n - self->range;

  assert (start >= 0 && start <= n);

  for (i = start; i < n; i++)
    if (!already_exist (self, dyn [i]))
      add_point (self, dyn [i]);
  self->name = eval_name (self);
  eval_fixed_nodes (self);
  return self;
}

attractor
attractor_search_cycle2 (attractor self, rbn rbn, int win_size)
{
  int i;
  point *win = NULL;
  int n_iter = rbn_get_n_iter (rbn);

  win = (point *) xmalloc (win_size * sizeof (point));
  win_fill (win, rbn, win_size);
  for (i = win_size; i < n_iter; i++)
    if (win_step (self, win, rbn, win_size))
      {
	free (win);
	return self;
      }
  free (win);
  return NULL;
}

attractor
attractor_search_cycle (attractor self, point *dyn, int n)
{
  int i;
  int j;
  int start = n - self->range;

  assert (start >= 0 && start <= n);

  for (i = n - 2; i > start; i--)
    {
      if (point_is_equal (dyn [n - 1], dyn [i])) /* Attractor found.  */
	{
	  for (j = i; j < n - 1; j++)
	    add_point (self, dyn [j]);
	  self->is_attractor = 1;
	  self->name = eval_name (self);
	  eval_fixed_nodes (self);
	  return self;
	}
    }
  /* Attractor not found. */
  self->is_attractor = 0;
  return NULL;
}

void
attractor_check_loose (attractor self, rbn rbn)
{
  int i;
  int counter = 0;

  for (i = 0; i < self->period; i++)
    if (next_is_in (self, rbn, self->x [i]))
      counter ++;
    else
      break;

  if (counter == self->period)
    self->is_attractor = 1;
  else
    self->is_attractor = 0;

  counter = 0;

  for (i = 0; i < self->period;i++)
    if (self->n_fixed_nodes == check_fixed_nodes (self, rbn, self->x [i]))
      counter ++;
  if (counter == self->period)
    self->n_fixed_nodes_doc = 1;
  else
    self->n_fixed_nodes_doc = 0;
}

void
attractor_complete_loose (attractor self, rbn rbn)
{
  int i;
  int max = 1 << self->n_floating_nodes;
  
  for (i = 0; (i < self->period) && (i < max); i++)
    {
      find_next (self, rbn, self->x [i]);
    }
  self->is_attractor = 1;
  self->name = eval_name (self);
}

void
attractor_start_loose (attractor self, rbn rbn, point *dyn, int n_iter)
{
  attractor_init (self);
  attractor_search_loose (self, dyn, n_iter);
  attractor_check_loose (self, rbn);
}

void
attractor_start_cycle (attractor self, rbn rbn, point *dyn, int n_iter)
{
  attractor_init (self);
  self = attractor_search_cycle (self, dyn, n_iter);
/*   self = attractor_search_syn2 (self, rbn, DEFAULT_RANGE); */
}

void
attractor_print (attractor self)
{
  int i;

  for (i = 0; i < self->period; i++)
      point_print (self->x [i]);
  printf ("\n");
}

void
attractor_print_fixed_nodes (attractor self)
{
  int i;

  printf ("%d -> ", self->n_fixed_nodes);
  for (i = 0; i < self->n_fixed_nodes; i++)
    printf ("%d ", self->fixed_nodes [i]);
}

void
attractor_print_floating_nodes (attractor self)
{
  int i;

  printf ("%d -> ", self->n_floating_nodes);
  for (i = 0; i < self->n_floating_nodes; i++)
    printf ("%d ", self->floating_nodes [i]);
}

void
attractor_print_results (attractor self)
{
  printf ("Name: ");
  point_print (attractor_get_name (self));
  printf ("Period: %d\n", attractor_get_period (self));
  printf ("Every point reached? %d\n", attractor_get_is_attractor (self));
  printf ("Floating nodes: ");
  attractor_print_floating_nodes (self);
  printf ("\nFixed nodes are really fixed? %d\n",
	  attractor_is_fixed_nodes_really_fixed (self));
}

void
attractor_print_scm (attractor self, FILE *fd)
{
  point id = self->name;
  int period = self->period;
  int basin = self->basin;
  int n_fixed_nodes = self->n_fixed_nodes;
  int *fixed_nodes = self->fixed_nodes;
  int n_floating_nodes = self->n_floating_nodes;
  int *floating_nodes = self->floating_nodes;

  int i;

  fprintf (fd, "(\n(id . #(");
  for (i = 0; i < point_get_n (id); i++)
    fprintf (fd, "%d ", point_get_item (id, i));
  fprintf (fd, "))\n");
  fprintf (fd, "(period . %d)\n", period);
  fprintf (fd, "(basin . %d)\n", basin);
  fprintf (fd, "(fixed . (");
  for (i = 0; i < n_fixed_nodes; i++)
    fprintf (fd, "%d ", fixed_nodes [i]);
  fprintf (fd, "))\n");
  fprintf (fd, "(floating . (");
  for (i = 0; i < n_floating_nodes; i++)
    fprintf (fd, "%d ", floating_nodes [i]);
  fprintf (fd, "))\n)\n");
}

void
attractor_free (attractor self)
{
  int i;

  for (i = 0; i < self->period; i++)
    point_free (self->x [i]);
  free (self->x);
  free (self->freq);
  if (self->tmp)
    point_free (self->tmp);
  free (self->fixed_nodes);
  free (self->floating_nodes);
  free (self);
}
