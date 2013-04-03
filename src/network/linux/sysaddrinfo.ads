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
--   13.Feb 2012 Julian Schutsch
--     - Original version
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package SysAddrInfo is

   type AddrInfo is
      record
         ai_flags     : Interfaces.C.int;
         ai_family    : Interfaces.C.int;
         ai_socktype  : Interfaces.C.int;
         ai_protocol  : Interfaces.C.int;
         ai_addrlen   : Interfaces.C.size_t;
         ai_addr      : System.Address;
         ai_canonname : Interfaces.C.Strings.chars_ptr;
         ai_next      : System.Address;
      end record;
   pragma Convention(C,AddrInfo);

end SysAddrInfo;
