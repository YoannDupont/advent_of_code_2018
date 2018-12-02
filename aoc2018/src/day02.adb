--------------------------------------------------------------------------------
--              An Ada implementation of the Advent Of Code 2018              --
--                                                                            --
--                     Day 2: Inventory Management System                     --
--                                                                            --
-- The following is an MCW example (outside of data) for day 2 of AOC 2018.   --
-- See: <https://adventofcode.com/2018> for the whole event.                  --
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Day02 is
   package ASU renames Ada.Strings.Unbounded;
   -- for convenience purpose

   type US_Array is array(Positive range <>) of ASU.Unbounded_String;
   -- The IDs in the input data are all the same length, so we could use a
   -- fixed-length String array, but it would not be very generic, and it would
   -- not be suited for examples either.

   type Natural_Couple is array(1..2) of Natural;
   -- This data structure will be used to store two things:
   --   * IDs that have exactly 2 of any letter and exactly 3 of any letter;
   --   * the absolute count of IDs for the previous item.
   -- I could/should use a record for that, but meh, later maybe.

   type Character_Count is array(Character range 'a' .. 'z') of Natural;
   -- An array indexed on characters that will be used to count occurrences.
   -- This is not very generic (cough), but it will do for the input.

   -- Creates the "String" array from the file name
   function Read_Input(file_name : in String) return US_Array is
      -- Tail-call recursion to create the final array.
      function Read_Input_Rec(input : in Ada.Text_IO.File_Type; acc : in US_Array) return US_Array is
      begin
         if Ada.Text_IO.End_Of_File(input) then
            return acc;
         else
            return Read_Input_Rec
            (
               input,
               acc & (1 => ASU.To_Unbounded_String(Ada.Text_IO.Get_Line(input)))
            );
         end if;
      end Read_Input_Rec;

      F : Ada.Text_IO.File_Type;
      acc : US_Array(1 .. 0);
   begin
      Ada.Text_IO.Open
      (
         File => F,
         Mode => Ada.Text_IO.In_File,
         Name => file_name
      );
      declare
         result : US_Array := Read_Input_Rec(F, acc);
      begin
         Ada.Text_IO.Close(F);
         return result;
      end;
   end Read_Input;

   -- the number that have an ID containing exactly two of any letter and then
   -- separately counting those with exactly three of any letter.
   -- You can multiply those two counts together to get a rudimentary checksum.
   function Checksum(nc : Natural_Couple) return Natural is
   begin
      return nc(1) * nc(2);
   end Checksum;

   function Common_Part(left, right : in String) return String is
      function Common_Part_Rec(li, ri : in Positive; acc : in String) return String is
      begin
         if li > left'Last then
            return acc;
         else
            return Common_Part_Rec
            (
               li+1,
               ri+1,
               acc & (if left(li) = right(ri) then (1 => left(li)) else "")
            );
         end if;
      end Common_Part_Rec;
   begin
      return Common_Part_Rec(left'First, right'First, "");
   end Common_Part;

   function Part_1(input : in US_Array) return Natural is
      total_count : Natural_Couple := (0, 0);
   begin
      for ID of input loop
         declare
            counts : Character_Count := (others => 0);
            current_count : Natural_Couple := (0, 0);
         begin
            for C of ASU.To_String(ID) loop
               counts(C) := counts(C) + 1;
            end loop;
            for count of counts loop
               if count = 2 then
                  current_count(1) := 1;
               elsif count = 3 then
                  current_count(2) := 1;
               end if;
               exit when current_count = (1, 1);
            end loop;
            total_count(1) := total_count(1) + current_count(1);
            total_count(2) := total_count(2) + current_count(2);
         end;
      end loop;

      return Checksum(total_count);
   end Part_1;

   function Part_2(input : in US_Array) return String is
   begin
      for I in input'Range loop
         for J in I+1 .. input'Last loop
            declare
               common : constant String := Common_Part
               (
                  ASU.To_String(input(I)),
                  ASU.To_String(input(J))
               );
            begin
               if common'Length = ASU.Length(input(I)) - 1 then
                  return common;
               end if;
            end;
         end loop;
      end loop;
      return "";
   end Part_2;

   input : constant US_Array := Read_Input("input/day02.txt");
begin
   Ada.Text_IO.Put_Line("What is the checksum for your list of box IDs?");
   Ada.Text_IO.Put_Line(Natural'Image(Part_1(input)));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("What letters are common between the two correct box IDs?");
   Ada.Text_IO.Put_Line(Part_2(input));
end Day02;
