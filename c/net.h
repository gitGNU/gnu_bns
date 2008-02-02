/* net.h - net is a set of N nodes: BIAS is P(1) for rules init,
 * AUTOREF represent possibilty for a node to be linked with
 * itself.
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

#ifndef NET_H
#define NET_H

#include "header.h"
#include "rule.h"
#include "node.h"

enum autoref {FORBIDDEN, ALLOWED, FORCED, UNKNOWN};

typedef struct net_str * net;

/*
 * Memory allocation.
 *
 */

net
net_alloc (net self, int n, int k);
/*
net
net_alloc_unhomogeneus (net self, int n, int *k); */

net
net_alloc_unhomogeneus (net self, int n);


/*
 * Initializations.
 *
 */

net
net_init (net self, double bias, int autoref);

net
net_reinit (net self);

net
net_init_topology_from_file (net self, FILE *fd);

net
net_build_topology_from_file (net self, FILE *fd);

net
net_init_rules_from_file (net self, FILE *fd);

net
net_init_from_files (net self, char *prefix);

void
net_rules_init (net self, double bias);

void
net_rewire (net self);

net
net_build_from_file (net self, char *filename);

/*
 * Mutators.
 *
 */

net
net_set_n (net self, int n);

net
net_set_k (net self, int k);

/*
 * Accessors.
 *
 */

int
net_get_n (net self);

int
net_get_k (net self);

/*
 * Return a pointer to Ith node.
 *
 */

node
net_get_node (net self, int i);

/*
 * Return a pointer to Ith node's boolean rule.
 *
 */

rule
net_get_rule (net self, int i);

/*
 * Input and output.
 *
 */

void
net_print (net self);

void
net_save_topology (net self, FILE *fd);

void
net_save_rules (net self, FILE *fd);

void
net_save (net self, char *prefix);

/*
 * Destructor.
 *
 */

void
net_free (net self);

#endif /* NET_H */
