/* lab.c : interface to all things */

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

#define MAXSIZE 1000
#define SIZE 100
#define MEDIUM_SIZE 25
#define SMALL_SIZE 5

#include <stdio.h>
#include <math.h>

#include "header.h"
#include "rbn.h"
#include "lab.h"

struct lab_str
{
  int n;
  int k;
  double p;
  int autoref;
  int update;
  int n_iter;
  int n_nets;
  int n_init;
  unsigned long seed;
  int chkstp;
  int threshold;
  char netname [MAXSIZE];
  char x0 [MAXSIZE];
  rbn syn;
  rbn asyn;
  attractor *looses;
  attractor *cycles;
  attractorSet loose_set;
  attractorSet cycle_set;
};

/* Private. */


static void
parse_command_line (lab self, int argc, char **argv)
{
  int i;
  char u;

  if (argc > 1)
    {
      for (i = 1; i < argc; i++)
	{
	  if (strcmp (argv [i], "-seed") == 0)
	    sscanf (argv [i + 1], "%ld", &self->seed);
	  if (strcmp (argv [i], "-n") == 0)
	    sscanf (argv [i + 1], "%d", &self->n);
	  if (strcmp (argv [i], "-k") == 0)
	    sscanf (argv [i + 1], "%d", &self->k);
	  if (strcmp (argv [i], "-p") == 0)
	    sscanf (argv [i + 1], "%lf", &self->p);
	  if (strcmp (argv [i], "-ar") == 0)
            sscanf (argv [i + 1], "%d", &self->autoref); 
	  if (strcmp (argv [i], "-i") == 0)
	    sscanf (argv [i + 1], "%d", &self->n_init);
	  if (strcmp (argv [i], "-s") == 0)
	    sscanf (argv [i + 1], "%d", &self->n_nets);
	  if (strcmp (argv [i], "-n_iter") == 0)
	    sscanf (argv [i + 1], "%d", &self->n_iter);
	  if (strcmp (argv [i], "-fn") == 0)
	    sscanf (argv [i + 1], "%d", &self->threshold);
	  if (strcmp (argv [i], "-net") == 0)
	    sscanf (argv [i + 1], "%s", self->netname);
	  if (strcmp (argv [i], "-init") == 0)
	    sscanf (argv [i + 1], "%s", self->x0);
	  if (strcmp (argv [i], "-u") == 0)
	    {
	      sscanf (argv [i + 1], "%c", &u);
	      if (u == 's')
		self->update = SYN;
	      else if (u == 'a')
		self->update = ASYN;
	      else
		{
		  printf ("Wrong argument for -u: must be 's' or 'a'\n");
		  exit (0);
		}
	    }
	}
    }
}

static double
mean (int *x, int n)
{
  int i;
  int mean = 0;

  for (i = 0; i < n; i++)
    mean += x [i];
  return mean / (double) n;
}

static double
std_dev (int *x, int n, double mean)
{
  int i;
  double x_2 = 0.0;

  for (i = 0; i < n; i++)
    x_2 += x [i]*  x [i];
  return sqrt (((x_2 / n) - (mean * mean)) / (n - 1));
}

static int
get_string_size (char *str)
{
  int size = 0;

  while ((str [size] != '\0') && (size < MAXSIZE))
    size++;
  if (size < MAXSIZE)
    return size;
  else
    return -1;
}

static char *
build_prefix (char *buffer, char *prefix, int n, int k)
{
  char n_k_str [MEDIUM_SIZE];

  sprintf (n_k_str, "%d_%d/%d_%d_",n, k, n, k);
  strcpy (buffer, prefix);
  strcat (buffer, n_k_str);
  return buffer;
}

static char *
build_name (char *buffer, char *prefix, int i)
{
  char id [SMALL_SIZE];
  int src_size = get_string_size (prefix);
  sprintf (id, "%03d", i);
  if ( (!(src_size < 0))
       && (SIZE > (src_size + SMALL_SIZE - 1)))
    {
      strcpy (buffer, prefix);
      strcat (buffer, id);
      return buffer;
    }
  else
    {
      perror ("In function build_name():");
      exit (1);
    }
}

static rbn
init_net (rbn sys, char *name)
{
  rbn_init (sys);
  if (name)
    net_init_from_files (rbn_get_net (sys), name);
  return sys;
}

static rbn
init_reduced_net (rbn sys, char *name)
{
  net nt = 0;

  assert (name);

  nt = net_build_from_file (nt, name);
  if (!nt)
    return NULL;
  rbn_set_net (sys, nt);
  return sys;
}

static point *
load_x0 (point *x0, int size, char *name)
{
  FILE *fd;
  int i;
  int index;
  char c;
  char line [MAXSIZE];
	char filename [MAXSIZE];
	
	strcpy (filename, name);
	strcat (filename, ".ini");
	
	/* printf ("%s\n", filename); */

  fd = fopen (filename, "r");
  for (i = 0; i < size; i++)
    {
      index = 0;
      while ((c = fgetc (fd)) != '\n')
				{
					line [index ++] = c;
				}
      line [index] = '\0';
      x0 [i] = point_alloc (x0 [i], index);
      x0 [i] = point_init_from_string (x0[i], line, index);
    }
  fclose (fd);
  return x0;
}

static void
build_scm_name (char *name, char *prefix)
{
  strcpy (name, prefix);
  strcat (name, ".scm");
}

static void
print_scm_results (lab self, char*netname, char *outname)
{
  FILE *out;
  attractorSet cycle_set = self->cycle_set;
  attractorSet loose_set = self->loose_set;

  build_scm_name (outname, netname);
  out = fopen (outname, "w");
  if (out == NULL)
    {
      perror ("In function lab_attractors_search() ");
      exit (1);
    }
  fprintf (out, ";;; Net file prefix: %s\n", netname);
  fprintf (out, ";;; ");
  lab_print_net_prms (self, out);
  fprintf (out, ";;; ");
  lab_print_stats_prms (self, out);
  fprintf (out, "\n");
  attractorSet_print_scm (cycle_set, out, "cyclic");
  attractorSet_print_scm (loose_set, out, "loose");
  fclose (out);
}

/* Public. */

lab
lab_alloc (lab self, int argc, char **argv)
{
  int j;

  /* Dynamical parameter. */
  self = (lab) xmalloc (sizeof (struct lab_str));
  self->n = 16;
  self->k = 2;
  self->p = 0.5;
  self->autoref = ALLOWED;
  self->n_iter = 10000;
  self->n_nets = 10;
  self->n_init = 10;
  self->seed = 1;
  self->chkstp = 1000;
  self->threshold = 4;

  parse_command_line (self, argc, argv);


#ifdef USE_GSL
  GSL_RBN_INIT;
#endif /* USE_GSL */

  SRAND (self->seed);

  /* Data structures. */
  self->syn = rbn_alloc (self->syn, self->n, self->k, self->p,
			 self->autoref, SYN, self->n_iter);
  self->asyn = rbn_alloc (self->syn, self->n, self->k, self->p,
			  self->autoref, ASYN, self->n_iter);

  self->looses = (attractor *) xmalloc (self->n_init * sizeof (attractor));
  for (j = 0; j < self->n_init; j ++)
    self->looses [j] = attractor_alloc (self->looses [j]);

  self->cycles = (attractor *) xmalloc (self->n_init * sizeof (attractor));
  for (j = 0; j < self->n_init; j ++)
    self->cycles [j] = attractor_alloc (self->cycles [j]);

  self->loose_set = attractorSet_alloc (self->loose_set);
  self->cycle_set = attractorSet_alloc (self->cycle_set);
  return self;
}

void
lab_print_net_prms (lab self, FILE *fd)
{
  fprintf (fd,
	   "N = %d, K = %d, P = %f, autoref = %d\n",
	   self->n,
	   self->k,
	   self->p,
	   self->autoref);
}

void
lab_print_stats_prms (lab self, FILE *fd)
{
  fprintf (fd,
	   "Iter = %d, Nets = %d, Inits = %d, Seed = %ld, Max Floating = %d\n",
	   self->n_iter,
	   self->n_nets,
	   self->n_init,
	   self->seed,
	   self->threshold);
}

void
lab_print_prms (lab self, FILE *fd)
{
  lab_print_net_prms (self, fd);
  lab_print_stats_prms (self, fd);
}

int
lab_build_net_sample (lab self, char*path)
{
  int n = self->n;
  int k = self->k;
  double p = self->p;
  int autoref = self->autoref;
  int n_nets = self->n_nets;

  int i = 0;
  net nt = 0;
  char prefix [SIZE];
  char netname [SIZE];

  build_prefix (prefix, path, n, k);
  nt = net_alloc (nt, n, k);
  nt = net_init (nt, p, autoref);

  for (i = 0; i < n_nets; i++)
    {
      build_name (netname, prefix, i);
      net_reinit (nt);
      net_rewire (nt);
/*       net_rules_init (nt, p); */
      net_save (nt, netname);
    }
  return 1;
}

int
lab_build_net_sample_from_topology (lab self, char *path)
{
  int n = self->n;
  int k = self->k;
  double p = self->p;
  int autoref = self->autoref;
  int n_nets = self->n_nets;

  int i;
  net nt = 0;
  char prefix [SIZE];
  char netname [SIZE];
  char top_str [SIZE];
  char rul_str [SIZE];
  FILE *topology_file;
  FILE *rule_file;

  build_prefix (prefix, path, n, k);
  nt = net_alloc (nt, n, k);
  nt = net_init (nt, p, autoref);

  for (i = 0; i < n_nets; i++)
    {
      build_name (netname, prefix, i);
      strcpy (top_str, netname);
      strcpy (rul_str, netname);
      
      topology_file = fopen (strcat (top_str, ".top"), "r");
      rule_file = fopen (strcat (rul_str, ".rul"), "w");
      net_build_topology_from_file (nt, topology_file);
      net_rules_init (nt, p);
      net_save_rules (nt, rule_file);
      fclose (topology_file);
      fclose (rule_file);
      topology_file = NULL;
      rule_file = NULL;
    }
  return 1;
}

int
lab_fixed_point_count (lab self)
{
  int n = self->n;
  int k_max = self->k;
  double p = self->p;
  int autoref = self->autoref;
  int update = self->update;
  int n_iter = self->n_iter;
  int n_nets = self->n_nets;
  int n_init = self->n_init;
  int chkstp = self->chkstp;

  int k;
  int i;
  int j;
  point x0;
  rbn sys;
  point flag = NULL;
  int counter = 0;
  int*fp;
  double m = 0.0;
  double s = 0.0;

  fp = (int*) xmalloc (n_nets * sizeof (int));

  for (k = 1; k < k_max + 1; k++)
    {
      x0 = NULL;
      sys = NULL;
      x0 = point_alloc (x0, n);
      sys = rbn_alloc (sys, n, k, p, autoref, update, n_iter);
      for (i = 0; i < n_nets; i++)
	{
	  counter = 0;
	  sys = rbn_init (sys);
	  for (j = 0; j < n_init; j++)
	    {
	      x0 = point_init (x0);
	      rbn_set_x0 (sys, x0);
	      flag = rbn_search_fp (sys, chkstp);
	      if (flag)
		counter ++;
	    }
	  fp [i] = counter;
	}
      m = mean (fp, n_nets);
      s = std_dev (fp, n_nets, m);
      printf ("%d\t%.2f\t%.2f\n", k, m / n_nets, s / n_nets);
      rbn_free (sys);
      point_free (x0);
    }
  free (fp);
  return 0;
}

int
lab_single_net_syn (lab self)
{
  int n = self->n;
  int n_iter = self->n_iter;
  char*netname = self->netname;
  int i = 0;
  rbn syn = self->syn;
  point x0 = 0;
  point *dyn = 0;

  init_reduced_net (syn, netname);
  n = net_get_n (rbn_get_net (syn));
  printf ("%d\n",n);

  dyn = (point*) xmalloc (n_iter * sizeof (point));
  for (i = 0; i < n_iter; i++)
    dyn [i] = point_alloc (dyn [i], n);

  x0 = point_alloc (x0, n);
  point_init_from_string (x0, self->x0, n);
  rbn_set_x0 (syn, x0);
  rbn_dynamics (syn, dyn, n_iter);
  dyn_print (dyn, n_iter);

  for (i = 0; i < n_iter; i++)
    point_free (dyn [i]);
  free (dyn);
  return 1;
}

void
lab_run_syn (lab self, point x0, point *dyn, attractor cycle)
{
  rbn syn = self->syn;
  int n_iter = self->n_iter;
  attractorSet cycle_set = self->cycle_set;

  rbn_set_x0 (syn, x0);
  rbn_dynamics (syn, dyn, n_iter);
  attractor_start_cycle (cycle, syn, dyn, n_iter);
  if (attractor_get_period (cycle) != 0)
    attractorSet_add_if_new (cycle_set, cycle);
}

void
lab_run_asyn (lab self, point x0, point *dyn, attractor loose)
{
  rbn asyn = self->asyn;
  int n_iter = self->n_iter;
  int threshold = self->threshold;
  attractorSet loose_set = self->loose_set;

  rbn_set_x0 (asyn, x0);
  rbn_dynamics (asyn, dyn, n_iter);
  attractor_start_loose (loose, asyn, dyn, n_iter);
  if (attractor_get_floating_nodes (loose) < threshold)
    {
      attractor_complete_loose (loose, asyn);
      attractorSet_add_if_new (loose_set, loose);
    }
}

int
lab_attractors_search (lab self, char *path, char*initpath)
{
  int n = self->n;
  int n_nets = self->n_nets;
  int n_init = self->n_init;
  rbn syn = self->syn;
  rbn asyn = self->asyn;
  int n_iter = self->n_iter;
/*   int threshold = self->threshold; */
  attractor *looses = self->looses;
  attractor *cycles = self->cycles;
  attractorSet loose_set = self->loose_set;
  attractorSet cycle_set = self->cycle_set;

  char prefix [SIZE];
  char netname [SIZE];
  char outname [SIZE];
/*   FILE *out; */
  point *x0;
  point *dyn;
  int i;
  int j;

/*   x0 = point_alloc (x0, n); */
  x0 = (point*) xmalloc (n_init *sizeof (point));
  x0 = load_x0 (x0, n_init, initpath);
/*   for (i = 0; i < n_init; i++) */
/*     point_print (x0[i]); */
  
  dyn = (point*) xmalloc (n_iter * sizeof (point));
  for (i = 0; i < n_iter; i++)
    dyn [i] = point_alloc (dyn [i], n);
  for (i = 0; i < SIZE -1; i++)
    netname [i] = ' ';
  netname [SIZE - 1] = '\0';

  build_prefix (prefix, path, self->n, self->k);

  for (i = 0; i < n_nets; i++)
    {
      build_name (netname, prefix, i);
      init_net (syn, netname);
      init_net (asyn, netname);
      attractorSet_init (loose_set);
      attractorSet_init (cycle_set);
      for (j = 0; j < n_init; j++)
	{
	  lab_run_syn (self, x0 [j], dyn, cycles [j]);
	  lab_run_asyn (self, x0 [j], dyn, looses [j]);
/* 	  point_init (x0); */
/* 	  /\* Syn *\/ */
/* 	  rbn_set_x0 (syn, x0 [j]); */
/* 	  rbn_dynamics (syn, dyn, n_iter); */
/* 	  attractor_start_cycle (cycles [j], syn, dyn, n_iter); */
/* 	  if (attractor_get_period (cycles [j]) != 0) */
/* 	    attractorSet_add_if_new (cycle_set, cycles [j]); */
/* 	  /\* Asyn *\/ */
/* 	  rbn_set_x0 (asyn, x0 [j]); */
/* 	  rbn_dynamics (asyn, dyn, n_iter); */
/* 	  attractor_start_loose (looses [j], asyn, dyn, n_iter); */
/* 	  if (attractor_get_floating_nodes (looses [j]) < threshold) */
/* 	    { */
/* 	      attractor_complete_loose (looses [j], asyn); */
/* 	      attractorSet_add_if_new (loose_set, looses [j]); */
/* 	    } */
	}
      attractorSet_evaluate (loose_set);
      attractorSet_evaluate (cycle_set);
      print_scm_results (self, netname, outname);
    }

  for (i = 0; i < n_iter; i++)
    point_free (dyn [i]);
  free (dyn);
/*   point_free (x0); */
  free (x0);
  return 0;
}

int
lab_attractors_search_reduced (lab self, char *path)
{
  int n = self->n;
  int n_nets = self->n_nets;
  int n_init = self->n_init;
  rbn syn = self->syn;
  rbn asyn = self->asyn;
  int n_iter = self->n_iter;
/*   int threshold = self->threshold; */
  attractor *looses = self->looses;
  attractor *cycles = self->cycles;
  attractorSet loose_set = self->loose_set;
  attractorSet cycle_set = self->cycle_set;

  char prefix [SIZE];
  char netname [SIZE];
  char outname [SIZE];
  point *x0;
  point *dyn;
  int i;
  int j;
  int k;

  for (i = 0; i < SIZE -1; i++)
    netname [i] = ' ';
  netname [SIZE - 1] = '\0';

  build_prefix (prefix, path, self->n, self->k);

  x0 = (point*) xmalloc (n_init *sizeof (point));
  x0 = load_x0 (x0, n_init,
		"/home/elianto/src/c/rbn/search/nets/32_2/decimated/000/32_2/32_2_d.ini");

  for (i = 0; i < n_nets; i++)
/*       build_name (netname, prefix, i); */
/*       attractorSet_init (loose_set); */
/*       attractorSet_init (cycle_set); */
      for (j = 0; j < n_init; j++)
	{
	  build_name (netname, prefix, j);
	  printf ("%s\n", netname);
	  point_print (x0[j]);
	  dyn = NULL;
	  attractorSet_init (loose_set);
	  attractorSet_init (cycle_set);
	  init_reduced_net (syn, netname);
	  init_reduced_net (asyn, netname);
	  n = net_get_n (rbn_get_net (syn));
	  dyn = (point*) xmalloc (n_iter * sizeof (point));
	  for (k = 0; k < n_iter; k++)
	    dyn [k] = point_alloc (dyn [k], n);
	  lab_run_syn (self, x0 [j], dyn, cycles [j]);
	  lab_run_asyn (self, x0 [j], dyn, looses [j]);
/* 	  /\* Syn *\/ */
/* 	  rbn_set_x0 (syn, x0 [j]); */
/* 	  rbn_dynamics (syn, dyn, n_iter); */
/* 	  attractor_start_cycle (cycles [j], syn, dyn, n_iter); */
/* 	  if (attractor_get_period (cycles [j]) != 0) */
/* 	    attractorSet_add_if_new (cycle_set, cycles [j]); */
/* 	  /\* Asyn *\/ */
/* 	  rbn_set_x0 (asyn, x0 [j]); */
/* 	  rbn_dynamics (asyn, dyn, n_iter); */
/* 	  attractor_start_loose (looses [j], asyn, dyn, n_iter); */
/* 	  if (attractor_get_floating_nodes (looses [j]) < threshold) */
/* 	    { */
/* 	      attractor_complete_loose (looses [j], asyn); */
/* 	      attractorSet_add_if_new (loose_set, looses [j]); */
/* 	    } */
	  attractorSet_evaluate (loose_set);
	  attractorSet_evaluate (cycle_set);
	  print_scm_results (self, netname, outname);
	}
      return 0;
}

int
lab_attractors_search_trimmed (lab self, char *path)
{
  int n = self->n;
  int n_nets = self->n_nets;
  int n_init = self->n_init;
  rbn syn = self->syn;
  rbn asyn = self->asyn;
  int n_iter = self->n_iter;
  /*   int threshold = self->threshold; */
  attractor *looses = self->looses;
  attractor *cycles = self->cycles;
  attractorSet loose_set = self->loose_set;
  attractorSet cycle_set = self->cycle_set;
	
  char prefix [SIZE];
  char netname [SIZE];
  char outname [SIZE];
  point *x0;
  point *dyn;
  rbn flag1;
  rbn flag2;
  int i;
  int j;
  int k;
	
  x0 = (point*) xmalloc (n_init *sizeof (point));
  /*	x0 = load_x0 (x0, n_init, initpath);
	for (i = 0; i < n_init; i++)
	point_print (x0[i]); */
	
  for (i = 0; i < SIZE -1; i++)
    netname [i] = ' ';
  netname [SIZE - 1] = '\0';
	
  build_prefix (prefix, path, self->n, self->k);
	
  dyn = (point*) xmalloc (n_iter * sizeof (point));
	
  for (i = 0; i < n_nets; i++)
    {
      build_name (netname, prefix, i);
      strcpy (outname, netname);
      strcat (outname, ".scm");			
      attractorSet_init (loose_set);
      attractorSet_init (cycle_set);
      flag1 = init_reduced_net (syn, netname);
      flag2 = init_reduced_net (asyn, netname);
      if (!flag1 || !flag2) /* Net has no nodes. */
	{
	  attractorSet_set_fixed_point (loose_set, n_init); /* Make a dummy */
	  attractorSet_set_fixed_point (cycle_set, n_init); /* fixed point. */
	}
      else /* Build tools for attractors searching. */
	{
	  n = net_get_n (rbn_get_net (syn));
	  for (k = 0; k < n_init; k++)
	    x0 [k] = point_alloc (x0 [k], n);
	  x0 = load_x0 (x0, n_init, netname);
	  for (k = 0; k < n_iter; k++)
	    dyn [k] = point_alloc (dyn [k], n);
	  /* Search attractor.*/
	  for (j = 0; j < n_init; j++)
	    {
	      lab_run_syn (self, x0 [j], dyn, cycles [j]);
	      lab_run_asyn (self, x0 [j], dyn, looses [j]);	  
	    }				
	  for (k = 0; k < n_iter; k++)
	    point_free (dyn [k]);
	  for (k = 0; k < n_init; k++)
	    point_free (x0 [k]);
	}
      attractorSet_evaluate (loose_set);
      attractorSet_evaluate (cycle_set);
      print_scm_results (self, netname, outname);
/*       attractorSet_println (cycle_set, stdout); */
    }

  free (dyn);
  free (x0);
  return 0;
}

int
lab_attractors_search_trimmed_random_inits (lab self, char *path)
{
  int n = self->n;
  int n_nets = self->n_nets;
  int n_init = self->n_init;
  rbn syn = self->syn;
  rbn asyn = self->asyn;
  int n_iter = self->n_iter;
  /*   int threshold = self->threshold; */
  attractor *looses = self->looses;
  attractor *cycles = self->cycles;
  attractorSet loose_set = self->loose_set;
  attractorSet cycle_set = self->cycle_set;
	
  char prefix [SIZE];
  char netname [SIZE];
  char outname [SIZE];
  point x0 = 0;
  point *dyn = 0;
  rbn flag1;
  rbn flag2;
  int i;
  int j;
  int k;
	
	
  for (i = 0; i < SIZE -1; i++)
    netname [i] = ' ';
  netname [SIZE - 1] = '\0';
	
  build_prefix (prefix, path, self->n, self->k);

  dyn = (point*) xmalloc (n_iter * sizeof (point));

  for (i = 0; i < n_nets; i++) {
    /* 		x0 = NULL; */
    /*printf ("i= %d, ", i);*/
    build_name (netname, prefix, i);
    strcpy (outname, netname);
    strcat (outname, ".scm");
    /*printf ("Evaluating %s...\n", netname);*/
    flag1 = init_reduced_net (syn, netname);
    flag2 = init_reduced_net (asyn, netname);
    if (!flag1 || !flag2) { /* the net has no nodes */
      attractorSet_init (loose_set);
      attractorSet_init (cycle_set);
      attractorSet_set_fixed_point (loose_set, n_init);
      attractorSet_set_fixed_point (cycle_set, n_init);
      goto skip;
    }
    n = net_get_n (rbn_get_net (syn));
    x0 = point_alloc (x0, n);
    for (k = 0; k < n_iter; k++)
      dyn [k] = point_alloc (dyn [k], n);
    attractorSet_init (loose_set);
    attractorSet_init (cycle_set);
    /* Initial condition loop.*/
    for (j = 0; j < n_init; j++) {
      point_init (x0);
      /*printf ("j= %d\n", j);*/
      lab_run_syn (self, x0, dyn, cycles [j]);
      lab_run_asyn (self, x0, dyn, looses [j]);	  
    }
    attractorSet_evaluate (loose_set);
    attractorSet_evaluate (cycle_set);
    /*attractorSet_println (cycle_set, stdout);*/
    for (k = 0; k < n_iter; k++)
      point_free (dyn [k]);
    point_free (x0);
  skip:
    print_scm_results (self, netname, outname);
  }
  free (x0);
  return 0;
}

void
lab_free (lab self)
{
  int j;

  rbn_free (self->syn);
  rbn_free (self->asyn);
  for (j = 0; j < self->n_init; j++)
    attractor_free (self->looses [j]);
  free (self->looses);
  for (j = 0; j < self->n_init; j++)
    attractor_free (self->cycles [j]);
  free (self->cycles);
  attractorSet_free (self->loose_set);
  attractorSet_free (self->cycle_set);
  free (self);
}
