/* syn_test.c - test some functions  */

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

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "header.h"
#include "rbn.h"

#define PREFIX_LEN 50
#define FILENAME_LEN 60

int
main (int argc, char **argv)
{
  int i, j;

  /* Filename strings.  */
  char *netsdir = "rbn/nets/";
  char n_k_str [10];
  char prefix [PREFIX_LEN];
  char id_str [5];
  char tmp_str [FILENAME_LEN];

  /* Dynamical parameters. */
  int n = 32;
  int k = 2;
  double p = 0.5;
  int autoref = ALLOWED;
  int update = SYN;
  char u; 
  int n_iter = 10000;
  int seed = 1;

  /* Statistical parameters. */
  int n_init = 10;
  int sample = 10;

  /* Data structures. */
  rbn sys = NULL;
  point x0 = NULL;
  point *dyn;
    
  int dyn_length = n_iter;

  /* Time variables.  */
  time_t time0, time2;
  int eval_time;
  int hours;
  int min;
  int sec;
  int display_evaluation_time = 1;

  /* Command line parsing. */

  if (argc > 1)
    {
      for (i = 1; i < argc; i++)
	{
	  if (strcmp (argv [i], "-seed") == 0)
	    sscanf (argv [i + 1], "%d", &seed);
	  if (strcmp (argv [i], "-n") == 0)
	    sscanf (argv [i + 1], "%d", &n);
	  if (strcmp (argv [i], "-k") == 0)
	    sscanf (argv [i + 1], "%d", &k);
	  if (strcmp (argv [i], "-p") == 0)
	    sscanf (argv [i + 1], "%lf", &p);
	  if (strcmp (argv [i], "-ar") == 0)
            sscanf (argv [i + 1], "%d", &autoref); 
	  if (strcmp (argv [i], "-i") == 0)
	    sscanf (argv [i + 1], "%d", &n_init);
	  if (strcmp (argv [i], "-s") == 0)
	    sscanf (argv [i + 1], "%d", &sample);
	  if (strcmp (argv [i], "-n_iter") == 0)
	    sscanf (argv [i + 1], "%d", &n_iter);
	  if (strcmp (argv [i], "-u") == 0)
	    {
	      sscanf (argv [i + 1], "%c", &u);
	      if (u == 's')
		update = SYN;
	      else if (u == 'a')
		update = ASYN;
	      else
		{
		  printf ("Wrong argument for -u: must be 's' or 'a'\n");
		  exit (0);
		}
	    }
	}
    }

#ifdef USE_GSL
  GSL_RBN_INIT;
#endif /* USE_GSL */

  SRAND (seed);
  time0 = time (NULL);

  dyn_length = n_iter;

  /* Memory allocations. */
  sys = rbn_alloc (sys, n, k, p, autoref, update, n_iter);
  x0 = point_alloc (x0, n);
  dyn = (point *) xmalloc (n_iter * sizeof (point));
  for (i = 0; i < n_iter; i++)
    dyn [i] = point_alloc (dyn [i], n);

  /* File prefix. */
  sprintf (n_k_str, "%d_%d", n, k);
  strcpy (prefix, netsdir);
  strcat (prefix, n_k_str);
  strcat (prefix, "/");
  strcat (prefix, n_k_str);
  strcat (prefix, "_");

  /* Print header.  */
  printf ("# N = %d, K = %d, P = %.2f, STEPS = %d, NETS = %d, INIT = %d\n", 
	  n, k, p, n_iter, sample, n_init);

  /* Main loop.  */
  for (i = 0; i < sample; i++)
    {
      sprintf (id_str, "%03d", i);
      strcpy (tmp_str, prefix);
      strcat (tmp_str, id_str);
      printf ("%s\n", tmp_str);
      rbn_init (sys);
      net_init_from_files (rbn_get_net (sys), tmp_str);
      for (j = 0; j < n_init; j++)
	{
	  point_init (x0);
	  printf ("# x0 = ");
	  point_print (x0);
	  printf ("\n");
	  rbn_set_x0 (sys, x0);
	  rbn_dynamics (sys, dyn, n_iter);
	  dyn_print (dyn, n_iter);
	}
    }


  /* Memory delocations. */
  for (i = 0; i < n_iter; i++)
    point_free (dyn [i]);
  free (dyn);
  rbn_free (sys);
  point_free (x0);

  /* Display elapsed time. */
  if (display_evaluation_time)
    {
      time2 = time (NULL);
      eval_time = (int) difftime (time2, time0);
      hours = eval_time / 3600;
      min = (eval_time % 3600) / 60;
      sec = eval_time % 60;
      printf ("# Evaluation time: %d h %d m %d s \n\n", hours, min, sec);
    }
  return 0;
}
