/* rbn.h - dynamical system  */

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

#ifndef RBN_H
#define RBN_H

#include "point.h"
#include "net.h"
#include "ev.h"

typedef struct rbn_str * rbn;

/*
 * Constructor.
 *
 */

rbn
rbn_alloc (rbn self, int n, int k, double bias, int autoref, int update, 
	   int n_iter);

rbn
rbn_alloc_from_net (rbn self, char*netname, int update, int n_iter);

/*
 * Random init net, rules and x0.
 *
 */

rbn
rbn_init (rbn self);

/*
 * Put SELF in initial condition.
 *
 */

rbn
rbn_reset (rbn self);

/*
 * Accessors.
 *
 */

point
rbn_get_x0 (rbn self);

int
rbn_get_n_iter (rbn self);

net
rbn_get_net (rbn self);

int
rbn_get_fp_transient (rbn self);

ev
rbn_get_evolver (rbn self);

/*
 * Mutators.
 *
 */

rbn
rbn_set_x0 (rbn self, point x0);

rbn
rbn_set_net (rbn self, net nt);

/*
 * Make a step, (N  random update for ASYN).
 * Update internal state X. 
 *
 */

point
rbn_next_point (rbn self);

/*
 * Search fixed points checkin each CHKSTP steps.
 *
 */

point
rbn_search_fp (rbn self, int chkstp);

/*
 * Check if X is a fixed point.
 *
 */

bool
rbn_check_fp (rbn self, point x);

/*
 * Evolve SELF for DYN_LEN steps, put each state in DYN.
 * DYN must be a vector of DYN_LEN points already allocated.
 *
 */

point *
rbn_dynamics (rbn self, point *dyn, int dyn_len);

/*
 * Print DYN.
 *
 */

void
dyn_print (point *dyn, int length);

/*
 * Destructor.
 *
 */

void
rbn_free (rbn self);

#endif /* RBN_H */
