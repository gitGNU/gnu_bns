/* main.c - parse command line options and call appropriate LAB's method  */

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "lab.h"

int
main (int argc, char **argv)
{
  int i;
  /* Time variables used to calculate computation time.  */
  time_t time0, time2;
  int eval_time;
  int hours;
  int min;
  int sec;
  int display_evaluation_time = 1;

  /* Paths. */
  char *path = getenv ("BNS_PATH");
  char *initpath = getenv ("BNS_INIT_PATH");
  if (!path)
    path="./";
  if (!initpath)
    initpath = "./init.ini";

  lab rbn_lab = 0;
   
  time0 = time (NULL);
  
  rbn_lab = lab_alloc (rbn_lab, argc, argv);

  if (argc > 1)
    for (i = 1; i < argc; i++)
      {
	if (strcmp (argv [i], "-c") == 0)
	  lab_build_net_sample (rbn_lab, path);
	if (strcmp (argv [i], "-ct") == 0)
	  lab_build_net_sample_from_topology (rbn_lab, path);
	if (strcmp (argv [i], "-rs") == 0)
	  lab_single_net_syn (rbn_lab);
	if (strcmp (argv [i], "-pa") == 0)
	  lab_fixed_point_count (rbn_lab);
	if (strcmp (argv [i], "-r") == 0)
	  lab_attractors_search (rbn_lab, path, initpath);
	if (strcmp (argv [i], "-rr") == 0)
	  lab_attractors_search_reduced (rbn_lab, path);
	if (strcmp (argv [i], "-rd") == 0)
	  lab_attractors_search_trimmed (rbn_lab, path);
	if (strcmp (argv [i], "-rdr") == 0)
	  lab_attractors_search_trimmed_random_inits (rbn_lab, path);
    }
  else
    {
      printf ("Usage: %s OPTION1 [OPTION2]\n", argv [0]);
      printf ("OPTION1:\n");
      printf ("\t-c\tbuild net sample\n");
      printf ("\t-ct\tbuild nets sample with given topology\n");
      printf ("\t-rs\trun a single net\n");
      printf ("\t-pa\tsearch point attractors\n");
      printf ("\t-r\tsearch cycles and looses attractors\n");
      printf ("\t-rd\tsearch cycles and looses on decimated nets\n");
      printf ("\t-rdr\tsearch on decimated nets (random inits)\n");
      printf ("OPTION2:\n");
      printf ("\t-seed [int]\trandom seed\n");
      printf ("\t-n [int]\tnet's size\n");
      printf ("\t-k [int]\tconnectivity\n");
      printf ("\t-p [float]\tmagnetization bias\n");
      printf ("\t-ar [0|1|2]\tautoref: forbidden(0),allowed(1),forced(2)\n");
      printf ("\t-i [int]\tnumber of initial condition\n");
      printf ("\t-s [int]\tnumber of nets\n");
      printf ("\t-n_iter [int]\tnumber of iteration\n");
      printf ("\t-fn [int]\tfloating node treshold\n");
      printf ("\t-w [int]\tmax cycle size\n");
      printf ("\t-net [char*]\tnet name\n");
      printf ("\t-init [char]\tinitial condition\n");
      printf ("\t-u [a|s]\tsyncrhonous(s) or asynchronous(a) updating\n\n");
      printf ("You should set the enviroment variables BNS_PATH and BNS_INIT_PATH.\n");
      printf ("For -c option BNS_PATH must be a directory with subdirectories named <N>_<K>");
      printf ("See README for more details.");
      display_evaluation_time = 0;
    }

  lab_free (rbn_lab);
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
