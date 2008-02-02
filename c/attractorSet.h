/* attractorSet.h - set of attractor founded in one net  */

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

#ifndef ATTRACTOR_SET_H
#define ATTRACTOR_SET_H

#include <gsl/gsl_statistics_int.h>
#include "attractor.h"

typedef struct attractorSet_str * attractorSet;

attractorSet
attractorSet_alloc (attractorSet self);

attractorSet
attractorSet_init (attractorSet self);

void
attractorSet_set_fixed_point (attractorSet self, int basin);

void
attractorSet_add_if_new (attractorSet self, attractor x);

void
attractorSet_evaluate (attractorSet self);

int
attractorSet_get_n (attractorSet self);

attractor
attractorSet_get_item (attractorSet self, int i);

double
attractorSet_get_period (attractorSet self);

double
attractorSet_get_period_sd (attractorSet self);

double
attractorSet_get_fixed_nodes (attractorSet self);

double
attractorSet_get_fixed_nodes_sd (attractorSet self);

double
attractorSet_get_basin (attractorSet self);

double
attractorSet_get_basin_sd (attractorSet self);

int
attractorSet_get_basin_item (attractorSet self, int i);

int
attractorSet_get_tot_points (attractorSet self);

int
attractorSet_get_common_points (attractorSet self);

void
attractorSet_println (attractorSet self, FILE*fd);

void
attractorSet_print_scm (attractorSet self, FILE*fd,
			char *prefix);

void
attractorSet_free (attractorSet self);

#endif /* ATTRACTOR_SET_H */
