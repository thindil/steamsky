--    Copyright 2017-2023 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Goals is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Goal_Data is record
      Index: chars_ptr;
      G_Type: Integer;
      Amount: Natural;
      Target_Index: chars_ptr;
      Multiplier: Positive;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   procedure Load_Goals is
      use Interfaces.C;

      --## rule off IMPROPER_INITIALIZATION
      Nim_Goal: Nim_Goal_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Goal(Index: Natural; Goal: out Nim_Goal_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGoal";
   begin
      Load_Goals_Loop :
      for I in 1 .. 256 loop
         Get_Ada_Goal(Index => I, Goal => Nim_Goal);
         if Strlen(Item => Nim_Goal.Index) > 0 then
            Goals_List.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String
                      (Source => Value(Item => Nim_Goal.Index)),
                  G_Type => Goal_Types'Val(Nim_Goal.G_Type),
                  Amount => Nim_Goal.Amount,
                  Target_Index =>
                    To_Unbounded_String
                      (Source => Value(Item => Nim_Goal.Target_Index)),
                  Multiplier => Nim_Goal.Multiplier));
         end if;
      end loop Load_Goals_Loop;
   end Load_Goals;

   function Goal_Text(Index: Goals_Container.Extended_Index) return String is
      function Goal_Ada_Text(I: Natural) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "goalAdaText";
   begin
      return Value(Item => Goal_Ada_Text(I => Index));
   end Goal_Text;

   procedure Get_Current_Goal(Index: Goals_Container.Extended_Index := 0) is
      Goal: constant Goal_Data :=
        (if Index = 0 then Current_Goal else Goals_List(Index));
      Nim_Goal: constant Nim_Goal_Data :=
        (Index => New_String(Str => To_String(Source => Goal.Index)),
         G_Type => Goal_Types'Pos(Goal.G_Type), Amount => Goal.Amount,
         Target_Index =>
           New_String(Str => To_String(Source => Goal.Target_Index)),
         Multiplier => Goal.Multiplier);
      procedure Get_Ada_Current_Goal(Goal: Nim_Goal_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCurrentGoal";
   begin
      Get_Ada_Current_Goal(Goal => Nim_Goal);
   end Get_Current_Goal;

   procedure Set_Ada_Current_Goal(Goal: out Nim_Goal_Data) with
      Import => True,
      Convention => C,
      External_Name => "setAdaCurrentGoal";

   procedure Clear_Current_Goal is
      Nim_Goal: Nim_Goal_Data;
      procedure Clear_Ada_Current_Goal with
         Import => True,
         Convention => C,
         External_Name => "clearAdaCurrentGoal";
   begin
      Clear_Ada_Current_Goal;
      Set_Ada_Current_Goal(Goal => Nim_Goal);
      Current_Goal :=
        (Index => To_Unbounded_String(Source => Value(Item => Nim_Goal.Index)),
         G_Type => Goal_Types'Val(Nim_Goal.G_Type), Amount => Nim_Goal.Amount,
         Target_Index =>
           To_Unbounded_String(Source => Value(Item => Nim_Goal.Target_Index)),
         Multiplier => Nim_Goal.Multiplier);
   end Clear_Current_Goal;

   procedure Update_Goal
     (G_Type: Goal_Types; Target_Index: Unbounded_String;
      Amount: Positive := 1) is
      Nim_Goal: Nim_Goal_Data;
      procedure Update_Ada_Goal
        (Goal_Type: Integer; Target: chars_ptr; A: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaGoal";
   begin
      Get_Current_Goal;
      Update_Ada_Goal
        (Goal_Type => Goal_Types'Pos(G_Type),
         Target => New_String(Str => To_String(Source => Target_Index)),
         A => Amount);
      Set_Ada_Current_Goal(Goal => Nim_Goal);
      Current_Goal :=
        (Index => To_Unbounded_String(Source => Value(Item => Nim_Goal.Index)),
         G_Type => Goal_Types'Val(Nim_Goal.G_Type), Amount => Nim_Goal.Amount,
         Target_Index =>
           To_Unbounded_String(Source => Value(Item => Nim_Goal.Target_Index)),
         Multiplier => Nim_Goal.Multiplier);
   end Update_Goal;

   function Get_Current_Goal return Goal_Data is
      Nim_Goal: Nim_Goal_Data;
   begin
      Set_Ada_Current_Goal(Goal => Nim_Goal);
      return
        (Index => To_Unbounded_String(Source => Value(Item => Nim_Goal.Index)),
         G_Type => Goal_Types'Val(Nim_Goal.G_Type), Amount => Nim_Goal.Amount,
         Target_Index =>
           To_Unbounded_String(Source => Value(Item => Nim_Goal.Target_Index)),
         Multiplier => Nim_Goal.Multiplier);
   end Get_Current_Goal;

end Goals;
