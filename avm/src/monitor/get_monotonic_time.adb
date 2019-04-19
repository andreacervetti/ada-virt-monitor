-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
--                                                                         --
-- This library is free software: you can redistribute it and/or modify    --
-- it under the terms of the GNU General Public License as published by    --
-- the Free Software Foundation, either version 3 of the License, or       --
-- (at your option) any later version.                                     --
--                                                                         --
-- As a special exception, if other files instantiate generics from        --
-- this unit, or you link this unit with other files to produce an         --
-- executable, this unit does not by itself cause the resulting executable --
-- to be covered by the GNU General Public License. This exception does    --
-- not however invalidate any other reasons why the executable file might  --
-- be covered by the GNU Public License.                                   --
--                                                                         --
-- This library is distributed in the hope that it will be useful,         --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of          --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           --
-- GNU General Public License for more details.                            --
--                                                                         --
-- You should have received a copy of the GNU General Public License       --
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.   --
--                                                                         --
-----------------------------------------------------------------------------
pragma Ada_2005;

with Interfaces.C; use Interfaces.C;

function Get_Monotonic_Time return Long_Long_Integer is

   -- I am not sure if every C lib support clock ids other than realtime
   CLOCK_REALTIME      : constant := 0;
   CLOCK_MONOTONIC     : constant := 1;
   CLOCK_MONOTONIC_RAW : constant := 4;
   pragma Unreferenced (CLOCK_REALTIME, CLOCK_MONOTONIC);
   -- These subtype definitions are a little verbose but they are here to
   -- maintain omogeneity with the correspondig C definitions.
   subtype uu_clockid_t is int;
   subtype uu_time_t is long;
   subtype uu_syscall_slong_t is long;
   subtype clockid_t is uu_clockid_t;

   type timespec is record
      tv_sec  : aliased uu_time_t;
      tv_nsec : aliased uu_syscall_slong_t;
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);

   function clock_gettime
     (uu_clock_id : clockid_t;
      uu_tp       : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   RC             : int;
   Returned_Clock : aliased timespec;

begin
   RC := clock_gettime (CLOCK_MONOTONIC_RAW, Returned_Clock'Access);
   if RC /= 0 then
      return 0;
   else
      return Long_Long_Integer
        (Returned_Clock.Tv_Sec * 1_000_000_000 + Returned_Clock.Tv_Nsec);
   end if;
end Get_Monotonic_Time;
