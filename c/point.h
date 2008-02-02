/* point.h - point represent the net state as an array of bools  */

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

#ifndef POINT_H
#define POINT_H

#include "header.h"

typedef struct point_str * point;

/*
 * Allocate mem for a point of length N.
 *
 */

point
point_alloc (point self, int n);

/*
 * Initialize SELF at random.
 *
 */

point
point_init (point self);

/*
 * Initialize SELF from STRING.
 *
 */

point
point_init_from_string (point self, char*str, int n);

/*
 * Allocate a new point and copy SELF in it.
 *
 */

point
point_clone (point self);

/*
 * Return SELF length (number of nodes).
 *
 */

int
point_get_n (point self);

/*
 * Return item at INDEX.
 *
 */

bool
point_get_item (point self, int index);

/* 
 * Set item at INDEX to value VALUE.
 *
 */

void
point_set_item (point self, int index, bool value);

/* Change value of ITEM.
 *
 */

void
point_flip_item (point self, int index);


/********************* 
 * Bynary operators. *
 *********************/

/*
 * Copy SRC in DST.
 *
 */

void
point_copy (point src, point dst);

/*
 * Return 1 if X and Y have same value or if they are the same pointer.
 *
 */

int
point_is_equal (point x, point y);

/*
 * Consider X and Y as binary sequence and find max.
 *
 */

point
point_find_max (point x, point y);

/*
 * Return correlation (number of equal items normalized between 0 and 1).
 *
 */

double
point_correlation (point x, point y);

/*
 * Print SELF on stdin followed by newline.
 *
 */

void
point_print (point self);

/*
 * Print SELF to FD followed by newline.
 *
 */

void
point_fprint (point self, FILE *fd);

/*
 * Deallocate mem.
 *
 */

void
point_free (point self);

#endif /* POINT_H */
