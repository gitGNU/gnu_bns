/* node.c - node in a net : it contains a RULE and some other things
 * It _NOT_ contain any sort of state: that's POINT's work.
 */

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

#include "node.h"

struct node_str
{
  int id;
  int k_in;
  int *in;
  int k_out;
  int *out;
  int linked;
  rule f;
};

node
node_alloc (node self, int id, int k_in)
{
  assert (k_in > -1);

  self = (node) xmalloc (sizeof (struct node_str));
  self->in = (int*) xmalloc (k_in * sizeof (int));
  self->out = (int*) xmalloc (sizeof (int));
  self->id = id;
  self->k_in = k_in;
  self->k_out = 0;
  self->linked = 0;
  self->f = rule_alloc (self->f , k_in);
  return self;
}

node
node_init (node self, double bias)
{
  assert (self);

  self->f = rule_init (self->f, bias);
  return self;
}

node
node_init_from_file (node self, FILE *fd)
{
  char buffer [MAX_BUFFER_SIZE];

  assert (self && fd);

  /* Read SELF inputs.  */
  get_next_line (fd, buffer);
  sscanf (buffer, "%d", &(self->k_in));
  get_next_line (fd, buffer);
  get_int_values (buffer, self->in);
  /* Read SELF outputs.  */
  get_next_line (fd, buffer);
  sscanf (buffer, "%d", &(self->k_out));
  if (self->k_out != 0)
    self->out = (int*) xrealloc (self->out, self->k_out * sizeof (int));
  get_next_line (fd, buffer);
  get_int_values (buffer, self->out);
  /* Realloc boolean rule.  */
  rule_realloc (self->f, self->k_in);

/*   free (buffer); */
  return self;
}

node
node_build_from_file (node self, int id, FILE *fd)
{
 char buffer [MAX_BUFFER_SIZE];

  assert (fd);
 	self = (node) xmalloc (sizeof (struct node_str));
	self->id = id;
  /* Read SELF inputs.  */
  get_next_line (fd, buffer);
  sscanf (buffer, "%d", &(self->k_in));
	self->in = (int*) xmalloc (self->k_in * sizeof(int));
  get_next_line (fd, buffer);
  get_int_values (buffer, self->in);
  /* Read SELF outputs.  */
  get_next_line (fd, buffer);
  sscanf (buffer, "%d", &(self->k_out));
  self->out = (int*) xmalloc (self->k_out * sizeof (int));
  get_next_line (fd, buffer);
  get_int_values (buffer, self->out);
  /* Alloc boolean rule.  */
  self->f = rule_alloc (self->f, self->k_in);

/*   free (buffer); */
  return self;
}

int
node_get_id (node self)
{
  return self->id;
}

int
node_get_k_in (node self)
{
  return self->k_in;
}

int *
node_get_inputs (node self)
{
  return self->in;
}

rule
node_get_rule (node self)
{
  return self->f;
}

void
node_reset_inputs (node self)
{
  self->linked = 0;
}

void
node_set_rule (node self, rule f)
{
  self->f = f;
}

int
node_set_input (node self, int id)
{
  int i;

  for (i = 0; i < self->linked; i++)
    {
      if (id == self->in [i])
			return 0;
    }
  if (self->linked < self->k_in)
    {
      self->in [self->linked ++] = id;
      return 1;
    }
  else
    {
      printf ("WARNING: Too many links at node %d \n", self->id);
      return 0;
    }
}

void
node_add_output (node self, int id)
{
  self->k_out ++;
  self->out = (int*) xrealloc (self->out, self->k_out * sizeof(int));
  self->out [self->k_out - 1] = id;
}

int
node_get_next_value (node self, bool *in_value)
{
  int i;
  int j = 0;

  assert (self);

  for (i = 0; i < self->k_in; i++)
    j = j + (in_value [i] << i);
  return rule_get_value (self->f, j);
}

void
node_print (node self)
{
  int i;

  assert (self);

  printf ("Node [%d] ; k_in: ", self->id);
  for (i = 0; i < self->k_in; i++)
    printf ("%d ", self->in [i]);
  printf ("\tk_out: ");
  for (i = 0; i < self->k_out; i++)
    printf ("%d ", self->out [i]);
}

void
node_save (node self, FILE *fd)
{
  int i;

  assert (self && fd);

  fprintf (fd, "%d\n",self->k_in);
  for (i = 0; i < self->k_in; i++)
    fprintf (fd, "%d ", self->in [i]);
  fprintf (fd, "\n");
  fprintf (fd, "%d\n",self->k_out);
  for (i = 0; i < self->k_out; i++)
    fprintf (fd, "%d ", self->out [i]);
  fprintf (fd, "\n");
}

void
node_free (node self)
{
  rule_free (self->f);
  free (self->in);
  free (self->out);
  free (self);
}
