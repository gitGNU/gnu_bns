/* node.h - node in a net : it contains a RULE and some other things
 * It _NOT_ contain any sort of state: that's POINT's work.
 */

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

#ifndef NODE_H
#define NODE_H

#include "header.h"
#include "rule.h"

typedef struct node_str * node;

/* 
 * Alloc memory for a node that takes K_IN inputs.
 * (output's things are allocated when needed.)
 *
 */

node
node_alloc (node self, int id, int k_in);

/*
 * Init an already allocated node.
 *
 */


node
node_init (node self, double bias);

node
node_init_from_file (node self, FILE *fd);

/*
 * Alloc and init input and output nodes.
 * Boolean rules aren't initialized.
 *
 */

node
node_build_from_file (node self, int id, FILE *fd);

/*
 * Accessors.
 *
 */

int
node_get_id (node self);

int
node_get_k_in (node self);

int
node_get_k_inputs (node self);

int *
node_get_inputs (node self);

rule
node_get_rule (node self);

/*
 * Mutators.
 *
 */

void
node_reset_inputs (node self);

void
node_set_rule (node self, rule f);

/* 
 * Make linking: its not allowed to have more than one input
 * from the same node (ID).
 *
 */

int
node_set_input (node self, int id);

void
node_add_output (node self, int id);

bool
node_get_next_value (node self, bool *in_value);

/*
 * Input and output.
 *
 */

void
node_print (node self);

void
node_save (node self, FILE *fd);

/* 
 * Destructor.
 *
 */

void
node_free (node self);

#endif /* NODE_H */
