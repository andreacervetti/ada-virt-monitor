-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2017 Andrea Cervetti                      --
--               Copyright (C) 2017 Homebrew Internet s.r.l.               --
--                                                                         --
-- This program is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                                                         --
-- This program is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
with Interfaces.C;
with Virtada; use Virtada;

package Formats is

   subtype Unsigned_Long is Interfaces.C.unsigned_long;

   function Memory_Size (Size : Unsigned_Long) return String;
   -- return a string representing the memory size
   -- in a significant unit (MiB, GiB etc.)
   -- Size : memory in KBytes

   function Memory_Size_Simple (Size : unsigned_long_long) return String;
   -- return a string representing the memory size
   -- in a simpli representation (XXXXM, XXXXG ecc.)
   -- Size : memory in bytes

   function Format_Time (Time : Long_Long_Integer) return String;
   -- return a string representing the time elasped (nanoseconds)
   -- in format hhh:mm:ss
end Formats;
