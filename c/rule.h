/* rule.h - boolean function for a node  */

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

#ifndef RULE_H
#define RULE_H

#include "header.h"

typedef struct rule_str * rule;

/*
 *  Alloc (and realloc) SELF that is a boolean function that 
 *  takes INPUTS inputs
 */

rule
rule_alloc (rule self, int inputs);

rule
rule_realloc (rule self, int inputs);

/* Initialize SELF: P(1) = bias, P(0) = 1 - bias.  */

rule
rule_init (rule self, double bias);

/* Set rule from an array VALUE of SIZE elements. */

void
rule_set (rule self, bool *value, int size);

/* Get value of item I.  */

bool
rule_get_value (rule self, int i);

/* Length (= 2^INPUTS) is size of boolean array.  */

int
rule_get_length (rule self);

void
rule_save (rule self, FILE *fd);

void
rule_print (rule self);

void
rule_free (rule self);

#endif /* RULE_H */
