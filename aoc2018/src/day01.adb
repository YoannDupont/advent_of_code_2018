--------------------------------------------------------------------------------
--              An Ada implementation of the Advent Of Code 2018              --
--                                                                            --
--                         Day 1: Chronal Calibration                         --
--                                                                            --
-- The following is an MCW example (outside of data) for day 1 of AOC 2018.   --
-- See: <https://adventofcode.com/2018> for the whole event.                  --
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Unchecked_Conversion;

procedure Day01 is
   type Integer_Array is array(Positive range <>) of Integer;
   -- The data structure to store the input.

   function Hash is new Ada.Unchecked_Conversion
   (
      Source => Integer,
      Target => Ada.Containers.Hash_Type
   );
   -- Creates a hash value from an integer.
   -- It is not guaranteed that Integer and Hash_Type types have the same size,
   -- as those are implementation defined.

   package Integer_Sets is new Ada.Containers.Hashed_Sets
   (
      Element_Type        => Integer,
      Hash                => Hash,
      Equivalent_Elements => "="
   );
   -- For part 2: using a set to store frequencies.

   -- Creates the integer array from the name of the input file.
   function Read_Input(file_name : in String) return Integer_Array is
      -- Tail-call recursion to create the array of Integers.
      -- Using an accumulator allows to generate arbitrary length arrays.
      function Read_Input_Rec(input : in Ada.Text_IO.File_Type; acc : in Integer_Array) return Integer_Array is
      begin
         if Ada.Text_IO.End_Of_File(input) then
            return acc;
         else
            return Read_Input_Rec
            (
               input,
               acc & (1 => Integer'Value(Ada.Text_IO.Get_Line(input)))
            );
         end if;
      end Read_Input_Rec;

      F : Ada.Text_IO.File_Type;
      acc : Integer_Array(1 .. 0) := (others => 0);
   begin
      Ada.Text_IO.Open
      (
         File => F,
         Mode => Ada.Text_IO.In_File,
         Name => file_name
      );
      declare
         result : Integer_Array := Read_Input_Rec(F, acc);
      begin
         Ada.Text_IO.Close(F);
         return result;
      end;
   end Read_Input;
   
   function Part_1(input : in Integer_Array) return Integer is
      resulting_frequency : Integer := 0;
   begin
      for frequency of input loop
         resulting_frequency := resulting_frequency + frequency;
      end loop;
      return resulting_frequency;
   end Part_1;
   
   function Part_2(input : in Integer_Array) return Integer is
      set : Integer_Sets.Set;
      position : Integer_Sets.Cursor;
      inserted : Boolean;
      current_frequency : Integer := 0;
   begin
      set.Insert(current_frequency, position, inserted);
      loop
         for frequency of input loop
            current_frequency := current_frequency + frequency;
            set.Insert(current_frequency, position, inserted);
            if not inserted then
               return current_frequency;
            end if;
         end loop;
      end loop;
   end Part_2;

   input : constant Integer_Array := Read_Input("input/day01.txt");
begin
   Ada.Text_IO.Put_Line("Starting with a frequency of zero, what is the resulting frequency after all of the changes in frequency have been applied?");
   Ada.Text_IO.Put_Line(Integer'Image(Part_1(input)));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("What is the first frequency your device reaches twice?");
   Ada.Text_IO.Put_Line(Integer'Image(Part_2(input)));
end Day01;
