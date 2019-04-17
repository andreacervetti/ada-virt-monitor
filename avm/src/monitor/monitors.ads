-----------------------------------------------------------------------------
--                                                                         --
--                 Copyright (C) 2018 Andrea Cervetti                      --
--               Copyright (C) 2018 Homebrew Internet s.r.l.               --
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
with Ada.Containers.Indefinite_Ordered_Maps;

package Monitors is

   Url_Error        : exception;
   Host_Not_Found   : exception;
   Host_Unreachable : exception;
   Duplicate_Name   : exception;
   Group_Name_Error : exception;
   Group_Not_Found  : exception;
   Cannot_Delete    : exception;
   Domain_Not_Found : exception;

   package Strings_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String,
      String);

   package Sys_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String,
      Strings_Maps.Map,
      "=" => Strings_Maps."=");

end Monitors;
