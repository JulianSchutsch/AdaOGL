/*
-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--  20.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implemenation
--  There is no guarantee constants in mpi.h stay the same from implementation
--  to implementation.
--  This file generates a mpiconstants.ads.
*/

#include "mpiconstants.h"
#include <stdio.h>
#include <stdlib.h>

#define OUTPUTFILE "mpiconstants.ads"

int main(int argc,char ** args)
{
  FILE *file;

  file=fopen(OUTPUTFILE,"w");
  if (file==NULL) {
    perror("Failed to open"OUTPUTFILE);
	return EXIT_FAILURE;
  }
  fputs("-------------------------------------------------------------------------------\n",file);
  fputs("--   Copyright 2012 Julian Schutsch\n",file);
  fputs("--\n",file);
  fputs("--   This file is part of ParallelSim\n",file);
  fputs("--\n",file);
  fputs("--   ParallelSim is free software: you can redistribute it and/or modify\n",file);
  fputs("--   it under the terms of the GNU Affero General Public License as published\n",file);
  fputs("--   by the Free Software Foundation, either version 3 of the License, or\n",file);
  fputs("--   (at your option) any later version.\n",file);
  fputs("--\n",file);
  fputs("--   ParallelSim is distributed in the hope that it will be useful,\n",file);
  fputs("--   but WITHOUT ANY WARRANTY; without even the implied warranty of\n",file);
  fputs("--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n",file);
  fputs("--   GNU Affero General Public License for more details.\n",file);
  fputs("--\n",file);
  fputs("--   You should have received a copy of the GNU Affero General Public License\n",file);
  fputs("--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.\n",file);
  fputs("-------------------------------------------------------------------------------\n",file);
  fputs("\n",file);
  fputs("-- This file is created automatically by /constantgenerators/mpi\n",file);
  fputs("\n",file);
  fputs("pragma Ada_2005\n;",file);
  fputs("package MPIConstants is\n",file);
  fputs("  -- Default Communicators\n",file);
  fprintf(file,"  MPI_COMM_WORLD : constant:=16#%x#;\n",MPI_COMM_WORLD);
  fputs("\n",file);
  fputs("  -- Error codes\n",file);
  fprintf(file,"  MPI_SUCCESS  : constant:=16#%x#;\n",MPI_SUCCESS);
  fprintf(file,"  MPI_ERR_COMM : constant:=16#%x#;\n",MPI_ERR_COMM);
  fprintf(file,"  MPI_ERR_ARG  : constant:=16#%x#;\n",MPI_ERR_ARG);
  fputs("\n",file);
  fputs("end MPIConstants;",file);
  fclose(file);
  
  return 0;

}