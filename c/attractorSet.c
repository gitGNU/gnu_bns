/* attractorSet.c - set of attractor founded in one net  */

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

#include <math.h>
#include "attractorSet.h"

struct attractorSet_str
{
  int n;
  attractor *set;
  int *basins;
  double period;
  double period_sd;
  double fixed_nodes;
  double fixed_nodes_sd;
  double basin;
  double basin_sd;
};

/*
 * Private functions.
 *
 */

static void
add_new (attractorSet self, attractor x)
{
  self->n ++;
  self->set = (attractor*) xrealloc (self->set, self->n * sizeof (attractor));
  self->basins = (int*) xrealloc (self->basins, self->n * sizeof (int));
  self->set [self->n - 1] = x;
  self->basins [self->n - 1] = 1;
}

static bool
already_in (attractorSet self, attractor x)
{
  int i;
  point name = attractor_get_name (x);

  for (i = 0; i < self->n; i++)
    if (point_is_equal (name, attractor_get_name (self->set [i])))
      {
	self->basins [i] ++;
	return 1;
      }
  return 0;
}

static bool
is_in (point *set, int n, point p)
{
  int i;
  
  for (i = 0; i < n; i++)
    {
      if (point_is_equal (p, set [i]))
	return 1;
    }
  return 0;
}


/*
 * Pubblic functions.
 *
 */

attractorSet
attractorSet_alloc (attractorSet self)
{
  self = (attractorSet) xmalloc (sizeof (struct attractorSet_str));
  self->set = NULL;
  self->basins = NULL;
  self->n = 0;
  return self;
}

attractorSet
attractorSet_init (attractorSet self)
{
/*   int i; */

/*   for (i = 0; i < self->n; i++) */
/*     attractor_free (self->set [i]); */

  free (self->set);
  free (self->basins);
  self->set = NULL;
  self->basins = NULL;
  self->n = 0;
  self->period = 0.0;
  self->period_sd = 0.0;
  self->fixed_nodes = 0.0;
  self->fixed_nodes_sd = 0.0;
  self->basin = 0.0;
  self->basin_sd = 0.0;
  return self;
}

void
attractorSet_set_fixed_point (attractorSet self, int basin)
{
  attractor a = 0;
  a = attractor_create_dummy (a);
  attractorSet_add_if_new (self, a);
  attractor_set_basin (a, basin);
}

void
attractorSet_add_if_new (attractorSet self, attractor x)
{
  if (x && !already_in (self, x))
    add_new (self, x);
}

void
attractorSet_evaluate (attractorSet self)
{
  int n = self->n;
  attractor *set = self->set;
  int *basins = self->basins;

  int i;
  double x_i = 0.0;
  double y_i = 0.0;
  double z_i = 0.0;
  double x_2 = 0.0;
  double y_2 = 0.0;
  double z_2 = 0.0;

  for (i = 0; i < n; i++ )
    {
      x_i = attractor_get_period (set [i]);
      y_i = attractor_get_fixed_nodes (set [i]);
      z_i = basins [i];
      attractor_set_basin (set [i], basins [i]);
      x_2 += (x_i * x_i);
      y_2 += (y_i * y_i);
      z_2 += (z_i * z_i);
      self->period += x_i;
      self->fixed_nodes += y_i;
      self->basin += z_i;
    }
  /* Evaluate means. */
  self->period /= n;
  self->fixed_nodes /= n;
  self->basin /= n;
  /* Evaluate standard deviation from means. */
  if (n != 1)
    {
      self->period_sd =
	sqrt (((x_2 / n) - (self->period * self->period))
	      / (n - 1));
      self->fixed_nodes_sd =
	sqrt (((y_2 / n) - (self->fixed_nodes * self->fixed_nodes))
	      / (n - 1));
      self->basin_sd =
	sqrt (((z_2 / n) - (self->basin * self->basin))
	      / (n - 1));
    }
}

int
attractorSet_get_n (attractorSet self)
{
  return self->n;
}

attractor
attractorSet_get_item (attractorSet self, int i)
{
  return self->set [i];
}

double
attractorSet_get_period (attractorSet self)
{
  return self->period;
}

double
attractorSet_get_period_sd (attractorSet self)
{
  return self->period_sd;
}

double
attractorSet_get_fixed_nodes (attractorSet self)
{
  return self->fixed_nodes;
}

double
attractorSet_get_fixed_nodes_sd (attractorSet self)
{
  return self->fixed_nodes_sd;
}


double
attractorSet_get_basin (attractorSet self)
{
  return self->basin;
}

double
attractorSet_get_basin_sd (attractorSet self)
{
  return self->basin_sd;
}

int
attractorSet_get_basin_item (attractorSet self, int i)
{
  return self->basins [i];
}

int
attractorSet_get_tot_points (attractorSet self)
{
  int i;
  int tot = 0;

  for (i = 0; i < self->n; i++)
    tot += attractor_get_period (self->set [i]);
  return tot;
}

int
attractorSet_get_common_points (attractorSet self)
{
  int i, j;
  int c;
  int common = 0;
  point p;
  point *distinct;
  int distinct_size;

  if (self->n < 2)
    return 0;

  /* Initilize DISTINCT putting in it all point of SELF->SET [0].  */  
  c = attractor_get_period (self->set [0]);
  distinct_size = c;
  distinct = (point*) xmalloc (distinct_size * sizeof (point));
  for (i = 0; i < c; i++)
    distinct [i] = attractor_get_item (self->set [0], i);
  /* Check others attractors.  */
  for (i = 1; i < self->n; i++)
    {
      c = attractor_get_period (self->set [i]);
      for (j = 0; j < c; j++)
	{
	  p = attractor_get_item (self->set [i], j);
	  if (is_in (distinct, distinct_size, p))
	    common++;
	  else
	    {
	      distinct_size ++;
	      distinct = (point*) xrealloc (distinct, 
					    distinct_size * sizeof (point));
	      distinct [distinct_size - 1] = p;
	    }
	}
    }
  free (distinct);
  return common;
}

void
attractorSet_println (attractorSet self, FILE*fd)
{
  fprintf (fd, "%d \t\t%.2f %.2f\t\t%.2f %.2f\t\t %.2f %.2f\n",
	   self->n,
	   self->period, self->period_sd,
	   self->basin, self->basin_sd,
	   self->fixed_nodes, self->fixed_nodes_sd);
}

void
attractorSet_print_scm (attractorSet self, FILE*fd,
			char *prefix)
{
  int n = self->n;
  attractor *set = self->set;
  int *basins = self->basins;

  int i;

  fprintf (fd, "(define %s '(\n", prefix);
  for (i = 0; i < n; i++)
    {
      attractor_set_basin (set [i], basins [i]);
      attractor_print_scm (set [i], fd);
    }
  fprintf (fd, "))\n");
}

void
attractorSet_free (attractorSet self)
{
  free (self->set);
  free (self->basins);
  free (self);
}
