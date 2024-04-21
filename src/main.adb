with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Main is
   type Rand_Range is new Integer range -1000..1000;
   package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
   use Rand_Int;
   Gen : Generator;
   Length : Integer := 100000;
   Arr : array(1..Length) of Rand_Range;

   thread_amount : constant Integer := 8;
   min_idx : Integer;

   procedure array_init is
   begin
      reset(Gen);
      for I in Arr'Range loop
         Arr(I) := Random(Gen);
      end loop;
   end array_init;

   function part_min(start_idx, end_idx: in Integer) return Integer is
      Min : Integer := start_idx;
   begin
      for I in start_idx..end_idx loop
         if Arr(I) < Arr(Min) then
            Min := I;
         end if;
      end loop;

      return Min;
   end part_min;

   task type Working_Thread is
      entry Start(start_idx, end_idx: in Integer);
   end Working_Thread;

   protected Monitor is
      procedure Set_Part_Min(Min: in Integer);
      entry Get_Min(Min: out Integer);
   private
      Task_Count : Integer := 0;
      Current_Minimum : Integer := 1;
   end Monitor;

   protected body Monitor is
      procedure Set_Part_Min(Min: in Integer) is
      begin
         if Arr(Current_Minimum) > Arr(Min) then
            Current_Minimum := Min;
         end if;
         Task_Count := Task_Count + 1;
      end Set_Part_Min;

      entry Get_Min(Min: out Integer) when Task_Count = Thread_Amount is
      begin
         Min := Current_Minimum;
      end Get_Min;
   end Monitor;

   task body Working_Thread is
      Min : Integer;
      Start_Idx, End_Idx : Integer;
   begin
      accept Start(Start_Idx : in Integer; End_Idx : in Integer) do
         Working_Thread.Start_Idx := Start_Idx;
         Working_Thread.End_Idx := End_Idx;
      end Start;
      Min := Part_Min(Start_Idx, End_Idx);
      Monitor.Set_Part_Min(Min);
   end Working_Thread;

   function Get_Min_Parallel return Integer is
      Min : Integer;
      Threads : array(1..Thread_Amount) of Working_Thread;
   begin
      for I in 1..Thread_Amount loop
         Threads(I).Start(Start_Idx => (I - 1) * Length / Thread_Amount + 1,
                          End_Idx => I * Length / Thread_Amount);
      end loop;

      Monitor.Get_Min(Min);
      return Min;
   end Get_Min_Parallel;

begin
   Array_Init;
   Min_Idx := Get_Min_Parallel;
   Put_Line("minimal " & Rand_Range'Image(Arr(Min_Idx)) & ": index " & Min_Idx'Image);
end Main;
