/* attractor.h - attractor: same struct is used to cycles and looses  */

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

#ifndef ATTRACTOR_H
#define ATTRACTOR_H

#include "rbn.h"

typedef struct attractor_str * attractor;

/*
 * Constructor.
 *
 */

attractor
attractor_alloc (attractor self);

/*
 * Reinit
 *
 */

attractor
attractor_init (attractor self);

attractor
attractor_create_dummy (attractor self);

/*
 * Mutator: set the last RANGE points in which search for attractors.
 *
 */

void
attractor_set_range (attractor self, int range);

/*
 * Accessor: return RANGE
 *
 */

int
attractor_get_range (attractor self);

/*
 * Return length of attractor.
 *
 */

int
attractor_get_period (attractor self);

/*
 * Return attraction basin's size.
 *
 */

int
attractor_get_basin (attractor self);

/*
 * Set attraction basin's size.
 *
 */

void
attractor_set_basin (attractor self, int basin_size);

/*
 * Return number of fixed nodes.
 *
 */

int
attractor_get_fixed_nodes (attractor self);

/*
 * Return number of foating nodes.
 * (complementary to attractor_get_fixed_nodes() ).
 *
 */

int
attractor_get_floating_nodes (attractor self);

/*
 * True if fixed nodes are really fixed.
 * It's a weak test, it only tries to update each fixed node, return true if
 * it doesn't change its own state.
 * There can be nodes that are not fixed but pass this test.
 *
 */

bool
attractor_is_fixed_nodes_really_fixed (attractor self);

/*
 * True if attractor is complete.
 * This means that each reachable point is already contained.
 *
 */

bool
attractor_get_is_attractor (attractor self);

/*
 * Force a set of point to bbe consider attractor (FLAG == 1) 
 * or not (FLAG == 0)
 *
 */

void
attractor_set_is_attractor (attractor self, bool flag);

/*
 * Return Ith point in attractor.
 * There isn't any particular order.
 *
 */

point
attractor_get_item (attractor self, int i);

/*
 * Return frequency of Ith point in attractor.
 * There isn't any particular order.
 *
 */

int
attractor_get_item_freq (attractor self, int i);

/*
 * Return the greatest (as binary representation of integer) point.
 * Useful to identify attractors.
 *
 */

point
attractor_get_name (attractor self);

/*
 * Find loose attractor in DYN.
 *
 */

attractor
attractor_search_loose (attractor self, point *dyn, int n);

/*
 * Find attractor in synchronous net.
 *
 */

attractor
attractor_search_cycle2 (attractor self, rbn rbn, int win_size);

attractor
attractor_search_cycle (attractor self, point *dyn, int n);

/*
 * Check the attractor.
 * This function sets IS_ATTRACTOR attribute.
 * It checks also fixed point (as seen in IS_FIXED_NODES_REALLY_FIXED)
 *
 */

void
attractor_check_loose (attractor self, rbn rbn);

/*
 * Interface to attractor_init(), attractor_search() and attractor_check().
 *
 */

void
attractor_start_loose (attractor self, rbn rbn, point *dyn, int n_iter);

/*
 * Interface to attractor_init(), attractor_search_syn().
 *
 */

void
attractor_start_cycle (attractor self, rbn rbn, point *dyn, int n_iter);

/*
 * Fill attractor with all point reacheble.
 *
 */

void
attractor_complete_loose (attractor self, rbn rbn);

/*
 * Print all attractor's points.
 *
 */

void
attractor_print (attractor self);

/*
 * Print fixed nodes.
 *
 */

void
attractor_print_fixed_nodes (attractor self);

/*
 * Print floating nodes.
 *
 */

void
attractor_print_floating_nodes (attractor self);

/*
 * Print all values found.
 *
 */

void
attractor_print_results (attractor self);

/*
 * Print all values in a scheme source file.
 *
 */

void
attractor_print_scm (attractor self, FILE *fd);

/*
 * Destructor.
 *
 */

void
attractor_free (attractor self);

#endif /* ATTRACTOR_H */
