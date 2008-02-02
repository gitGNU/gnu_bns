/* ev.h - evolver for a rbn: may be SYN or ASYN  */

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

#ifndef EV_H
#define EV_H

#include "header.h"
#include "point.h"
#include "net.h"

enum type {SYN, ASYN};

typedef struct ev_str * ev;

/*
 * Allocation & initialization, type must be SYN or ASYN.
 * NET must be already allocated and initialized.
 *
 */

ev
ev_alloc (ev self, net nt, int type);

/*
 * Return the size of the net (SELF->NET->N).
 *
 */

int
ev_get_net_size (ev self);

int
ev_get_type (ev self);

/*
 * Evolve X to NEXT.
 * if SYN: update each node
 * if ASYN: choose a node at random and update it
 *          (repeating N times (where N is # nodes)).
 *
 */

point
ev_run (ev self, point x, point next);

/*
 * Take input values form nodes that are linked to Ith node of
 * SRC, apply them I's boolean function, and write result in
 * DST's Ith nodes.
 * Used internaly by ev_run() and ev_check_fp() [see "ev.c"],
 * it may be useful by itself.
 *
 */

void
ev_update_point_item (ev self, point src, point dst, int i);

/*
 * Return X if X is a fixed point, NULL elsewhere.
 * It should works also on a SYN net but it seems a little
 * useless.
 *
 */

point
ev_check_fp (ev self, point x);

/*
 * Destructor
 *
 */

void
ev_free (ev self);

#endif /* EV_H */
