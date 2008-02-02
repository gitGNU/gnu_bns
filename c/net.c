/* net.c - net is a set of N nodes: BIAS is P(1) for rules init,
 * AUTOREF represent possibilty for a node to be linked with
 * itself.
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

#include "net.h"

#define STR_LEN 100

struct net_str
{
  int n;
  int k;
  double bias;
  int autoref;
  node *nd;
};

/* Static functions (private) .  */
static void
net_rewire_allow_autoref (net self)
{
  int i, j, rnd, flag;

  assert (self);

  for (i = 0; i < self->n; i++)
    {
      j = 0;
      while (j < node_get_k_in (self->nd [i]))
	{
	  rnd = DICE (self->n);
	  flag = node_set_input (self->nd [i], rnd);
	  if (flag)
	    node_add_output (self->nd [rnd], i);
	  j += flag;
	}
    }
}

static void
net_rewire_forbid_autoref (net self)
{
  int i, j, flag, rnd;

  assert (self);

  for (i = 0; i < self->n; i++)
    {
      j = 0;
      while (j < node_get_k_in (self->nd [i]))
	{
	  rnd = DICE (self->n);
	  if (rnd != i)
	    {
	      flag = node_set_input (self->nd [i], rnd);
	      if (flag)
		node_add_output (self->nd [rnd], i);
	      j += flag;
	    }
	}
    }
}

static void
net_rewire_force_autoref (net self)
{
  int i, j, rnd;

  assert (self);

  for (i = 0; i < self->n; i++)
    {
      node_set_input (self->nd [i], node_get_id (self->nd [i]));
      node_add_output (self->nd [i], i);
      j = 1;
      while (j < node_get_k_in (self->nd [i]))
	{
	  rnd = DICE (self->n);
	  j += node_set_input (self->nd [i], rnd);
	  node_add_output (self->nd [rnd], i);
	}
    }
}


static net
net_realloc (net self)
{
  assert (self);

  self->nd = (node*) xrealloc (self->nd, self->n * sizeof (node));
  self = net_init (self, self->bias, self->autoref);
  return self;
}

/* Function defined in net.h (public).  */

net
net_alloc (net self, int n, int k)
{
  int i;

  self = (net) xmalloc (sizeof (struct net_str));
  self->n = n;
  self->k = k;
  self->nd = (node*) xmalloc (n * sizeof (node));
  for (i = 0; i < n; i++)
    self->nd [i] = node_alloc (self->nd [i], i, k);
  return self;
}
/*
net
net_alloc_unhomogeneus (net self, int n, int *k)
{
  int i;

  self = (net) xmalloc (sizeof (struct net_str));
  self->n = n;
  self->k = 0;
  for (i = 0; i < n; i++)
    self->k += k [i];
  self->k /= n;
  self->k ++;
  self->nd = (node*) xmalloc (n * sizeof (node));
  for (i = 0; i < n; i++)
    self->nd [i] = node_alloc (self->nd [i], i, k [i]);
  return self;
}*/

net
net_alloc_unhomogeneus (net self, int n)
{
  self = (net) xmalloc (sizeof (struct net_str));
  self->n = n;
  self->k = -1;
	self->bias = -1;
	self->autoref = UNKNOWN;
  self->nd = (node*) xmalloc (n * sizeof (node));
  return self;
}

net
net_init (net self, double bias, int autoref)
{
  int i;

  assert (self && autoref >= 0 && autoref <= 2);

  self->bias = bias;
  self->autoref = autoref;

  for (i = 0; i < self->n; i++)
    {
      self->nd [i] = node_init (self->nd [i], bias);
    }
  return self;
}

net
net_reinit (net self)
{
  return net_init (self, self->bias, self->autoref);
}

net
net_init_topology_from_file (net self, FILE *fd)
{
  int i;
  int n;
  int dummy;
  char buffer [MAX_BUFFER_SIZE];

  assert (self && fd);

  fscanf (fd, "%d\n", &n);
  fscanf (fd, "%d\n", &dummy);
  fscanf (fd, "%d\n", &dummy);

  assert (n == self->n);

  for (i = 0; i < n; i++)
    {
      self->nd [i] = node_init_from_file (self->nd [i], fd);
      get_next_line (fd, buffer);
    }
  return self;
}

net
net_build_topology_from_file (net self, FILE *fd)
{
  int i;
  int n;
  int dummy;
  char buffer [MAX_BUFFER_SIZE];

  assert (fd);

  fscanf (fd, "%d\n", &n);
  fscanf (fd, "%d\n", &dummy);
  fscanf (fd, "%d\n", &dummy);

  if (n == 0)
    return NULL;

  self = net_alloc_unhomogeneus (self, n);

  for (i = 0; i < n; i++)
    {
      self->nd [i] = node_build_from_file (self->nd [i], i, fd);
      get_next_line (fd, buffer);
    }
  return self;
}

net
net_init_rules_from_file (net self, FILE *fd)
{
  int i;
  int n = self->n;
  int c;
  int index;
  int *v = NULL;
  rule f;

  for (i = 0; i < n; i++)
    {
      index = 0;
      f = net_get_rule (self, i);
      while ((c = fgetc (fd)) != '\n')
	{
	  v = (int*) xrealloc (v, ++index * sizeof(int));
	  v [index - 1] = c - 48; /* Convert ascii value to 0 or 1. */
	}
      assert (index == rule_get_length (f));
      rule_set (f, v, index);
    }
  free (v);
  return self;
}

net
net_init_from_files (net self, char *prefix)
{
  char *top_str;
  char *rul_str;
  FILE *top;
  FILE *rul;

  assert (self && prefix);

  top_str = (char*) xmalloc (STR_LEN * sizeof (char));
  rul_str = (char*) xmalloc (STR_LEN * sizeof (char));

  top_str = strcpy (top_str, prefix);
  top_str = strcat (top_str, ".top");
  rul_str = strcpy (rul_str, prefix);
  rul_str = strcat (rul_str, ".rul");

  top = fopen (top_str, "r");
  rul = fopen (rul_str, "r");

  self = net_init_topology_from_file (self, top);
  self = net_init_rules_from_file (self, rul);

  fclose (top);
  fclose (rul);
  free (top_str);
  free (rul_str);

  return self;
}

net
net_build_from_file (net self, char *filename)
{
  char *top_str;
  char *rul_str;
  FILE *top;
  FILE *rul;

  assert (filename);

  top_str = (char*) xmalloc (STR_LEN * sizeof (char));
  rul_str = (char*) xmalloc (STR_LEN * sizeof (char));

  top_str = strcpy (top_str, filename);
  top_str = strcat (top_str, ".top");
  rul_str = strcpy (rul_str, filename);
  rul_str = strcat (rul_str, ".rul");

  top = fopen (top_str, "r");
  rul = fopen (rul_str, "r");

  self = net_build_topology_from_file (self, top);
  if (self == NULL)
    return NULL;
  self = net_init_rules_from_file (self, rul);

  fclose (top);
  fclose (rul);
  free (top_str);
  free (rul_str);

  return self;
}

void
net_rules_init (net self, double bias)
{
  int i;

  assert (self);

  for (i = 0; i < self->n; i++)
    rule_init (node_get_rule (self->nd [i]), bias);
}

net
net_set_n (net self, int n)
{
  assert (self && n < MAX_NODES);

  self->n = n;
  return net_realloc (self);
}

net
net_set_k (net self, int k)
{
  assert (self && k < MAX_K);

  self->k = k;
  return net_realloc (self);
}

int
net_get_n (net self)
{
  assert (self);

  return self->n;
}

int
net_get_k (net self)
{
  assert (self);

  return self->k;
}

node
net_get_node (net self, int i)
{
  assert (self && i >= 0 && i < self->n);

  return self->nd [i];
}

rule
net_get_rule (net self, int i)
{
  assert (self && i >= 0 && i < self->n);

  return node_get_rule (net_get_node (self, i));
}

void
net_rewire (net self)
{
  int i;

  assert (self);

  for (i = 0; i < self->n; i++)
    node_reset_inputs (self->nd [i]);
  if (self->autoref == ALLOWED)
    net_rewire_allow_autoref (self);
  else if (self->autoref == FORBIDDEN)
    net_rewire_forbid_autoref (self);
  else if (self->autoref == FORCED)
    net_rewire_force_autoref (self);
  else
    {
      printf ("Error: wiring style not spefied.\n");
      exit (1);
    }
}

void
net_print (net self)
{
  int i;

  for (i = 0; i < self->n; i++)
    {
      node_print (self->nd [i]);
      printf ("\t\t");
      printf ("f: ");
      rule_print (node_get_rule (self->nd [i]));
      printf ("\n");
    }
}

void
net_save_topology (net self, FILE *fd)
{
  int i;

  assert (self && fd);

  fprintf (fd, "%d\n%d\n%d\n\n", self->n, self->k, self->autoref);
  for (i = 0; i < self->n; i++)
    {
      node_save (self->nd [i], fd);
      fprintf (fd, "\n");
    }
}

void
net_save_rules (net self, FILE *fd)
{
  int i;

  assert (self && fd);

/*   fprintf (fd, "%d\n%d\n\n", self->n, self->k); */
  for (i = 0; i < self->n; i++)
    {
      rule_save (node_get_rule (self->nd [i]), fd);
      fprintf (fd, "\n");
    }
}

void
net_save (net self, char *prefix)
{
  char *top_str;
  char *rul_str;
  FILE *top;
  FILE *rul;

  assert (self && prefix);

  top_str = (char*) xmalloc (STR_LEN * sizeof (char));
  rul_str = (char*) xmalloc (STR_LEN * sizeof (char));

  top_str = strcpy (top_str, prefix);
  top_str = strcat (top_str, ".top");
  rul_str = strcpy (rul_str, prefix);
  rul_str = strcat (rul_str, ".rul");

  top = fopen (top_str, "w");
  rul = fopen (rul_str, "w");

  net_save_topology (self, top);
  net_save_rules (self, rul);

  fclose (top);
  fclose (rul);
  free (top_str);
  free (rul_str);
}

void
net_free (net self)
{
  int i;

  for (i = 0; i < self->n; i++)
    node_free (self->nd [i]);
  free (self->nd);
  free (self);
}
