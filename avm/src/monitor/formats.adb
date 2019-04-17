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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;
with Ada.Long_Long_Float_Text_IO; use Ada.Long_Long_Float_Text_IO;

package body Formats is
   use type Interfaces.C.unsigned_long;

   type Modular_60 is mod 60;

   function Memory_Size (Size : Unsigned_Long)
                         return String
   is
      Symbols : constant array (Natural range 0 .. 6) of String (1 .. 3) :=
        (0 => "KiB",
         1 => "MiB",
         2 => "GiB",
         3 => "TiB",
         4 => "PiB",
         5 => "EiB",
         6 => "ZiB"); -- Really? Wow!!
      Scale : Unsigned_Long;
      S_Pad : String (1 .. 16);
   begin
      for I in reverse Symbols'Range loop
         Scale := 2**(I * 10);
         if Size > Scale - 1 then
            Put
              (To   => S_Pad,
               Item => Long_Long_Float (Size) / Long_Long_Float (Scale),
               Aft  => 2,
               Exp  => 0);
            return Trim (S_Pad, Both) &
              " " &
              Symbols (I) &
              " (" &
              Trim (Unsigned_Long'Image (Size), Left) &
              " KiB)";
         end if;
      end loop;
      return "0 KiB";
   end Memory_Size;


   function Memory_Size_Simple (Size : Unsigned_Long_Long)
                                return String
   is
      Symbols : constant array (Natural range 0 .. 4) of Character :=
        ('B', 'K', 'M', 'G', 'T');
      Scale : Unsigned_Long_Long;
   begin
      for I in reverse Symbols'Range loop
         Scale := 2**(I * 10);
         if Size > Scale - 1 then
            return Trim
              (Unsigned_Long_Long'Image (Size / Scale), Left) & Symbols (I);
         end if;
      end loop;
      return "";
   end Memory_Size_Simple;


   function Format_Time (Time : Long_Long_Integer)
                         return String
   is
      Total_Seconds : Unsigned_Long := Unsigned_Long (Time / 1_000_000_000);
      Seconds       : Modular_60    := Modular_60    (Total_Seconds rem 60);
      Total_Minutes : Unsigned_Long :=                Total_Seconds / 60;
      Minutes       : Modular_60    := Modular_60 (Total_Minutes rem 60);
      Hours         : Unsigned_Long := Total_Minutes / 60;
   begin
      return
        (if Hours > 0 then
            Trim (Unsigned_Long'Image (Hours), Left) & ":" else "") &
        (if Minutes > 0 or else Hours > 0 then
            Trim (Modular_60'Image (Minutes), Left) & ":" else "") &
        Trim (Modular_60'Image (Seconds), Left);
   end Format_Time;

end Formats;
