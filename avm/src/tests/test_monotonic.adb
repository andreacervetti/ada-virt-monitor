with Ada.Text_IO; use Ada.Text_IO;
with Get_Monotonic_Time;

procedure Test_Monotonic is
begin
   Put_Line (Long_Long_Integer'Image(Get_Monotonic_Time));
end Test_Monotonic;
