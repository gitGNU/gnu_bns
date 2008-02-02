/* lab.h : interface to all things */

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

#ifndef LAB_H
#define LAB_H

#include "rbn.h"
#include "attractor.h"
#include "attractorSet.h"

typedef struct lab_str * lab;

lab
lab_alloc (lab self, int argc, char **argv);

void
lab_print_net_prms (lab self, FILE *fd);

void
lab_print_stats_prms (lab self, FILE *fd);

void
lab_print_prms (lab self, FILE *fd);

int
lab_build_net_sample (lab self, char*path);

/* Read N_SAMPLE topology files and write a rule file for each of them.  */

int
lab_build_net_sample_from_topology (lab self, char *path);

/* Count fixed points (normalized to 1) for connettivity from 1 to K.  */

int
lab_fixed_point_count (lab self);

int
lab_single_net_syn (lab self);

void
lab_run_syn (lab self, point x0, point *dyn, attractor cycle);

void
lab_run_asyn (lab self, point x0, point *dyn, attractor loose);

int
lab_attractors_search (lab self, char *prefix, char *initpath);

int
lab_attractors_search_reduced (lab self, char *path);

int
lab_attractors_search_trimmed (lab self, char *path);

int
lab_attractors_search_trimmed_random_inits (lab self, char *path);

void
lab_free (lab self);

#endif /* LAB_H*/
