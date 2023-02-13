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

with Ada.Characters.Handling;
with Interfaces.C.Strings;
with Ships;
with Crafts;
with Items;
with Utils;
with Statistics;
with Messages;
with Missions;
with Factions;
with Game;

package body Goals is

   procedure Load_Goals(File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Goal_Data is record
         Index: chars_ptr;
         G_Type: Integer;
         Amount: Natural;
         Target_Index: chars_ptr;
         Multiplier: Positive;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_Goal: Nim_Goal_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Load_Ada_Goals(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaGoals";
      procedure Get_Ada_Goal(Index: Natural; Goal: out Nim_Goal_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGoal";
   begin
      Load_Ada_Goals(Name => New_String(Str => File_Name));
      Load_Goals_Loop :
      for I in 1 .. 256 loop
         Get_Ada_Goal(Index => I, Goal => Nim_Goal);
         if Strlen(Item => Nim_Goal.Index) > 0 then
            Goals_List.Append
              (New_Item =>
                 (Index =>
                    To_Unbounded_String(Source => Value(Item => Nim_Goal.Index)),
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
      use Crafts;
      use Missions;
      use Game.Tiny_String;

      Text: Unbounded_String := Null_Unbounded_String;
      Goal: Goal_Data;
      Insert_Position: Positive := 1;
      Added: Boolean := False;
      --## rule off TYPE_INITIAL_VALUES
      type Faction_Name_Type is (NAME, MEMBERNAME, PLURALMEMBERNAME);
      --## rule on TYPE_INITIAL_VALUES
      function Get_Faction_Name
        (Faction_Index: Bounded_String; F_Type: Faction_Name_Type)
         return String is
         use Factions;

         Faction: constant Faction_Record :=
           Get_Faction(Index => Faction_Index);
      begin
         case F_Type is
            when NAME =>
               return To_String(Source => Faction.Name);
            when MEMBERNAME =>
               return To_String(Source => Faction.Member_Name);
            when PLURALMEMBERNAME =>
               return To_String(Source => Faction.Plural_Member_Name);
         end case;
      end Get_Faction_Name;
   begin
      Goal := (if Index > 0 then Goals_List(Index) else Current_Goal);
      case Goal.G_Type is
         when REPUTATION =>
            Text := To_Unbounded_String(Source => "Gain max reputation in");
         when DESTROY =>
            Text := To_Unbounded_String(Source => "Destroy");
         when DISCOVER =>
            Text := To_Unbounded_String(Source => "Discover");
         when VISIT =>
            Text := To_Unbounded_String(Source => "Visit");
         when CRAFT =>
            Text := To_Unbounded_String(Source => "Craft");
         when MISSION =>
            Text := To_Unbounded_String(Source => "Finish");
         when KILL =>
            Text := To_Unbounded_String(Source => "Kill");
         when RANDOM =>
            null;
      end case;
      Append(Source => Text, New_Item => Positive'Image(Goal.Amount));
      case Goal.G_Type is
         when REPUTATION | VISIT =>
            Append(Source => Text, New_Item => " base");
         when DESTROY =>
            Append(Source => Text, New_Item => " ship");
         when DISCOVER =>
            Append(Source => Text, New_Item => " field");
         when CRAFT =>
            Append(Source => Text, New_Item => " item");
         when MISSION =>
            Append(Source => Text, New_Item => " mission");
         when KILL =>
            Append(Source => Text, New_Item => " enem");
         when RANDOM =>
            null;
      end case;
      if Goal.G_Type not in RANDOM | KILL and Goal.Amount > 1 then
         Append(Source => Text, New_Item => "s");
      end if;
      case Goal.G_Type is
         when DISCOVER =>
            Append(Source => Text, New_Item => " of map");
         when KILL =>
            if Goal.Amount > 1 then
               Append(Source => Text, New_Item => "ies in melee combat");
            else
               Append(Source => Text, New_Item => "y in melee combat");
            end if;
         when others =>
            null;
      end case;
      if Goal.Target_Index /= Null_Unbounded_String then
         case Goal.G_Type is
            when REPUTATION | VISIT =>
               Insert_Position := Length(Source => Text) - 3;
               if Goal.Amount > 1 then
                  Insert_Position := Insert_Position - 1;
               end if;
               Insert
                 (Source => Text, Before => Insert_Position,
                  New_Item =>
                    Get_Faction_Name
                      (Faction_Index =>
                         To_Bounded_String
                           (Source => To_String(Source => Goal.Target_Index)),
                       F_Type => NAME) &
                    " ");
            when DESTROY =>
               Destroy_Ship_Block :
               declare
                  use Ships;
               begin
                  Destroy_Ship_Loop :
                  for I in Proto_Ships_List.Iterate loop
                     if Proto_Ships_Container.To_Index(Position => I) =
                       Positive'Value
                         (To_String(Source => Goal.Target_Index)) then
                        Append
                          (Source => Text,
                           New_Item =>
                             ": " &
                             To_String(Source => Proto_Ships_List(I).Name));
                        Added := True;
                        exit Destroy_Ship_Loop;
                     end if;
                  end loop Destroy_Ship_Loop;
               exception
                  when Constraint_Error =>
                     null;
               end Destroy_Ship_Block;
               if not Added then
                  Insert_Position := Length(Source => Text) - 3;
                  if Goal.Amount > 1 then
                     Insert_Position := Insert_Position - 1;
                  end if;
                  Insert
                    (Source => Text, Before => Insert_Position,
                     New_Item =>
                       Get_Faction_Name
                         (Faction_Index =>
                            To_Bounded_String
                              (Source =>
                                 To_String(Source => Goal.Target_Index)),
                          F_Type => NAME) &
                       " ");
               end if;
            when CRAFT =>
               if Recipes_Container.Contains
                   (Container => Recipes_List,
                    Key =>
                      To_Bounded_String
                        (Source =>
                           To_String(Source => Goal.Target_Index))) then
                  Get_Item_Name_Block :
                  declare
                     use Items;

                     Item_Index: constant Natural :=
                       Recipes_List
                         (To_Bounded_String
                            (Source => To_String(Source => Goal.Target_Index)))
                         .Result_Index;
                  begin
                     Append
                       (Source => Text,
                        New_Item =>
                          ": " &
                          To_String
                            (Source =>
                               Get_Proto_Item(Index => Item_Index).Name));
                  end Get_Item_Name_Block;
               else
                  Append
                    (Source => Text,
                     New_Item =>
                       ": " & To_String(Source => Goal.Target_Index));
               end if;
            when MISSION =>
               case Missions_Types'Value
                 (To_String(Source => Goal.Target_Index)) is
                  when DELIVER =>
                     Append
                       (Source => Text,
                        New_Item => ": Deliver items to bases");
                  when PATROL =>
                     Append(Source => Text, New_Item => ": Patrol areas");
                  when DESTROY =>
                     Append(Source => Text, New_Item => ": Destroy ships");
                  when EXPLORE =>
                     Append(Source => Text, New_Item => ": Explore areas");
                  when PASSENGER =>
                     Append
                       (Source => Text,
                        New_Item => ": Transport passengers to bases");
               end case;
            when KILL =>
               Insert_Position := Length(Source => Text) - 20;
               if Goal.Amount > 1 then
                  Insert_Position := Insert_Position - 2;
               end if;
               Get_Faction_Name_Block :
               declare
                  Stop_Position: Natural := Insert_Position + 4;
               begin
                  if Goal.Amount > 1 then
                     Stop_Position := Stop_Position + 2;
                     Replace_Slice
                       (Source => Text, Low => Insert_Position,
                        High => Stop_Position,
                        By =>
                          Get_Faction_Name
                            (Faction_Index =>
                               To_Bounded_String
                                 (Source =>
                                    To_String(Source => Goal.Target_Index)),
                             F_Type => PLURALMEMBERNAME));
                  else
                     Replace_Slice
                       (Source => Text, Low => Insert_Position,
                        High => Stop_Position,
                        By =>
                          Get_Faction_Name
                            (Faction_Index =>
                               To_Bounded_String
                                 (Source =>
                                    To_String(Source => Goal.Target_Index)),
                             F_Type => MEMBERNAME));
                  end if;
               end Get_Faction_Name_Block;
            when RANDOM | DISCOVER =>
               null;
         end case;
      end if;
      return To_String(Source => Text);
   end Goal_Text;

   procedure Clear_Current_Goal is
   begin
      Current_Goal :=
        (Index => Null_Unbounded_String, G_Type => RANDOM, Amount => 0,
         Target_Index => Null_Unbounded_String, Multiplier => 1);
   end Clear_Current_Goal;

   procedure Update_Goal
     (G_Type: Goal_Types; Target_Index: Unbounded_String;
      Amount: Positive := 1) is
      use Ada.Characters.Handling;
      use Messages;
      use Statistics;
      use Utils;

   begin
      if G_Type /= Current_Goal.G_Type then
         return;
      end if;
      if To_Lower(Item => To_String(Source => Target_Index)) /=
        To_Lower(Item => To_String(Source => Current_Goal.Target_Index)) and
        Current_Goal.Target_Index /= Null_Unbounded_String then
         return;
      end if;
      Current_Goal.Amount :=
        (if Amount >= Current_Goal.Amount then 0
         else Current_Goal.Amount - Amount);
      if Current_Goal.Amount = 0 then
         Update_Finished_Goals(Index => Current_Goal.Index);
         Add_Message
           (Message => "You finished your goal. New goal is set.",
            M_Type => OTHERMESSAGE, Color => BLUE);
         Current_Goal :=
           Goals_List
             (Get_Random
                (Min => Goals_List.First_Index, Max => Goals_List.Last_Index));
      end if;
   end Update_Goal;

end Goals;
