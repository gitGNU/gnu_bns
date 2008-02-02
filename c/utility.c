/* utility.c - miscellaneous utilities  */

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

#include "header.h"

/*
 * Reading lines from a file, appending '\0'.
 * Return number of character read (including '\0') or 0 if EOF.
 *
 */

int
get_next_line (FILE *fd, char *s)
{
  int i, c;

  for (i = 0; (c = fgetc(fd)) != EOF &&  c != '\n'; i++)
    s [i] = c;
  if (c == '\n')
    {
      s [i] = '\0';
      i++;
    }
  if (c == EOF)
    return 0;
  return i;
}

/*
 * Read a sequence of int value from BUFFER and put them in VALUE.
 * Note: BUFFER and VALUE must be already allocated.
 *
 */

int
get_int_values (char *buffer, int *value)
{
  int i = 0;
  char *endptr;

  while (buffer [0] != '\0')
    {
      value [i++] = strtol (buffer, &endptr, 10);
      if (*endptr == '\0')
				return i;
      buffer = endptr + 1;
    }

  return i;
}

/*
 * Cat PREFIX, ID, EXT > FILENAME
 *
 */

char *
create_filename (char *filename, char *prefix, int id, char *ext)
{
  char id_str [10];
  
  sprintf (id_str, "%d", id);
  strcpy (filename, prefix);
  strcat (filename, id_str);
  strcat (filename, ext);
  return filename;
}
