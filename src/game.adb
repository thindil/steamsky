--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Exceptions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Bases; use Bases;
with BasesTypes;
with Config;
with Events;
with Goals;
with Log;
with Maps; use Maps;
with Ships; use Ships;
with Statistics; use Statistics;

package body Game is

   procedure New_Game is
      use Config;
      procedure New_Ada_Game with
         Import => True,
         Convention => C,
         External_Name => "newAdaGame";
   begin
      -- Save game configuration
      Save_Config;
      -- Set game statistics
      Clear_Game_Stats;
      -- Start the new game
      New_Ada_Game;
      -- Get data from Nim
      Get_Ship_From_Nim(Ship => Player_Ship);
      Get_Bases_Loop :
      for I in Sky_Bases'Range loop
         Get_Base_From_Nim(Base_Index => I);
      end loop Get_Bases_Loop;
      Get_Map_Y_Loop :
      for Y in 1 .. 1_024 loop
         Get_Map_X_Loop :
         for X in 1 .. 1_024 loop
            Set_Map_Cell(X => X, Y => Y);
         end loop Get_Map_X_Loop;
      end loop Get_Map_Y_Loop;
      Set_Game_Date;
   end New_Game;

   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) is
      use Events;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Update_Ada_Game(M, In_C: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaGame";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Update_Ada_Game(M => Minutes, In_C => (if In_Combat then 1 else 0));
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
   end Update_Game;

   procedure End_Game(Save: Boolean) is
      use Goals;

      procedure End_Ada_Game(S: Integer) with
         Import => True,
         Convention => C,
         External_Name => "endAdaGame";
   begin
      if Save then
         Set_Ship_In_Nim;
      end if;
      End_Ada_Game(S => (if Save then 1 else 0));
      Clear_Game_Stats;
      Clear_Current_Goal;
   end End_Game;

   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index is
      function Find_Ada_Skill_Index
        (S_Name: chars_ptr) return SkillsData_Container.Extended_Index with
         Import => True,
         Convention => C,
         External_Name => "findAdaSkillIndex";
   begin
      return Find_Ada_Skill_Index(S_Name => New_String(Str => Skill_Name));
   end Find_Skill_Index;

   function Load_Game_Data return String is
      use Ada.Exceptions;
      use BasesTypes;
      use Log;

      Result: chars_ptr;
      function Load_Ada_Game_Data return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaGameData";
      procedure Load_Data is
         use Interfaces.C;
         use Tiny_String;

         Item_Index: Natural;
         --## rule off TYPE_INITIAL_VALUES
         type Nim_Strings_Array is array(0 .. 12) of chars_ptr;
         type Nim_Integers_Array is array(0 .. 10) of Integer;
         --## rule on TYPE_INITIAL_VALUES
         Nim_Strings: Nim_Strings_Array;
         Nim_Integers: Nim_Integers_Array := (others => 0);
         procedure Get_Ada_Game_Strings(Values: out Nim_Strings_Array) with
            Import => True,
            Convention => C,
            External_Name => "getAdaGameStrings";
         procedure Get_Ada_Game_Integers(Values: out Nim_Integers_Array) with
            Import => True,
            Convention => C,
            External_Name => "getAdaGameIntegers";
      begin
         Item_Index := 0;
         Fill_Attributes_Block :
         declare
            --## rule off TYPE_INITIAL_VALUES
            type Attribute_Nim_Array is array(0 .. 1) of chars_ptr;
            --## rule on TYPE_INITIAL_VALUES
            Attribute_Array: Attribute_Nim_Array;
            procedure Get_Ada_Attribute
              (I_Index: Natural; Attribute: out Attribute_Nim_Array) with
               Import => True,
               Convention => C,
               External_Name => "getAdaAttribute";
         begin
            Fill_Attributes_Loop :
            loop
               Get_Ada_Attribute
                 (I_Index => Attributes_Amount, Attribute => Attribute_Array);
               exit Fill_Attributes_Loop when Strlen
                   (Item => Attribute_Array(0)) =
                 0;
               AttributesData_Container.Append
                 (Container => Attributes_List,
                  New_Item =>
                    (Name =>
                       Tiny_String.To_Bounded_String
                         (Source => Value(Item => Attribute_Array(0))),
                     Description =>
                       Short_String.To_Bounded_String
                         (Source => Value(Item => Attribute_Array(1)))));
               Attributes_Amount := Attributes_Amount + 1;
            end loop Fill_Attributes_Loop;
         end Fill_Attributes_Block;
         Fill_Skills_Block :
         declare
            --## rule off TYPE_INITIAL_VALUES
            type Nim_Skill_Record is record
               Name: chars_ptr;
               Attribute: Integer;
               Description: chars_ptr;
               Tool: chars_ptr;
            end record;
            --## rule on TYPE_INITIAL_VALUES
            --## rule off IMPROPER_INITIALIZATION
            Skill: Nim_Skill_Record;
            --## rule on IMPROPER_INITIALIZATION
            procedure Get_Ada_Skill
              (S_Index: Natural; Nim_Skill: out Nim_Skill_Record) with
               Import => True,
               Convention => C,
               External_Name => "getAdaSkill";
         begin
            Item_Index := 1;
            Fill_Skills_Loop :
            loop
               Get_Ada_Skill(S_Index => Item_Index, Nim_Skill => Skill);
               exit Fill_Skills_Loop when Strlen(Item => Skill.Name) = 0;
               Load_Skill_Block :
               declare
                  --## rule off TYPE_INITIAL_VALUES
                  type Nim_Tools_Array is array(0 .. 15, 0 .. 1) of Integer;
                  --## rule on TYPE_INITIAL_VALUES
                  function Get_Ada_Skill_Tools_Amount
                    (S_Index: Natural) return Integer with
                     Import => True,
                     Convention => C,
                     External_Name => "getAdaSkillToolsAmount";
                  procedure Get_Ada_Skill_Tools
                    (S_Index: Natural; Nim_Tools: out Nim_Tools_Array) with
                     Import => True,
                     Convention => C,
                     External_Name => "getAdaSkillTools";
                  Tools: Nim_Tools_Array;
                  Tools_Quality: Tool_Quality_Array
                    (1 ..
                         (if
                            Get_Ada_Skill_Tools_Amount(S_Index => Item_Index) >
                            0
                          then
                            Get_Ada_Skill_Tools_Amount(S_Index => Item_Index)
                          else 1)) :=
                    (others => <>);
                  Tmp_Skill: Skill_Record
                    (Quality_Amount => Tools_Quality'Length) :=
                    (Quality_Amount => Tools_Quality'Length, others => <>);
               begin
                  Get_Ada_Skill_Tools
                    (S_Index => Item_Index, Nim_Tools => Tools);
                  Load_Skills_Loop :
                  for J in Tools_Quality'Range loop
                     Tools_Quality(J) :=
                       (Level => Tools(J - 1, 0), Quality => Tools(J - 1, 1));
                  end loop Load_Skills_Loop;
                  if Get_Ada_Skill_Tools_Amount(S_Index => Item_Index) = 0 then
                     Tools_Quality := Empty_Tool_Quality_Array;
                  end if;
                  Tmp_Skill :=
                    (Quality_Amount => Tools_Quality'Length,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Skill.Name)),
                     Attribute => Skill.Attribute + 1,
                     Description =>
                       Short_String.To_Bounded_String
                         (Source => Value(Item => Skill.Description)),
                     Tool =>
                       Tiny_String.To_Bounded_String
                         (Source => Value(Item => Skill.Tool)),
                     Tools_Quality => Tools_Quality);
                  SkillsData_Container.Append
                    (Container => Skills_List, New_Item => Tmp_Skill);
                  Skills_Amount := Skills_Amount + 1;
               end Load_Skill_Block;
               Item_Index := Item_Index + 1;
            end loop Fill_Skills_Loop;
         end Fill_Skills_Block;
         Get_Ada_Game_Strings(Values => Nim_Strings);
         Money_Name :=
           To_Unbounded_String(Source => Value(Item => Nim_Strings(12)));
         Get_Ada_Game_Integers(Values => Nim_Integers);
         Money_Index := Nim_Integers(1);
         Talking_Skill := Count_Type(Nim_Integers(7));
      end Load_Data;
   begin
      if Get_Proto_Ship(Proto_Index => 1) /= Empty_Proto_Ship then
         return "";
      end if;
      Result := Load_Ada_Game_Data;
      Load_Data;
      Load_Bases_Types;
      return Value(Item => Result);
   exception
      when An_Exception : others =>
         Log_Message
           (Message => Exception_Message(X => An_Exception),
            Message_Type => EVERYTHING);
         return Exception_Message(X => An_Exception);
   end Load_Game_Data;

   procedure Get_Game_Date is
      procedure Get_Ada_Game_Date
        (Year, Month, Day, Hour, Minutes: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGameDate";
   begin
      Get_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour,
         Minutes => Game_Date.Minutes);
   end Get_Game_Date;

   procedure Set_Game_Date is
      procedure Set_Ada_Game_Date(Year, Month, Day, Hour, M: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameDate";
   begin
      Set_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour, M => Game_Date.Minutes);
   end Set_Game_Date;

end Game;
