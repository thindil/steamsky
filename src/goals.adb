--    Copyright 2017-2021 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Ships; use Ships;
with Crafts; use Crafts;
with Items; use Items;
with Utils; use Utils;
with Statistics; use Statistics;
with Messages; use Messages;
with Missions; use Missions;
with Factions; use Factions;
with Game; use Game;

package body Goals is

   procedure Load_Goals(Reader: Tree_Reader) is
      Temp_Record: Goal_Data;
      Nodes_List: Node_List;
      Goals_Data: Document;
      Action: Data_Action;
      Goal_Index: Natural;
      Goal_Node: Node;
   begin
      Goals_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Goals_Data, Tag_Name => "goal");
      Load_Goals_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Index => Null_Unbounded_String, G_Type => RANDOM, Amount => 0,
            Target_Index => Null_Unbounded_String, Multiplier => 1);
         Goal_Node := Item(List => Nodes_List, Index => I);
         Temp_Record.Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Goal_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Goal_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Goal_Node, Name => "action"))
            else ADD);
         Goal_Index := 0;
         Get_Goal_Index_Loop :
         for J in Goals_List.Iterate loop
            if Goals_List(J).Index = Temp_Record.Index then
               Goal_Index := Goals_Container.To_Index(Position => J);
               exit Get_Goal_Index_Loop;
            end if;
         end loop Get_Goal_Index_Loop;
         if Action in UPDATE | REMOVE then
            if Goal_Index = 0 then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " goal '" & To_String(Source => Temp_Record.Index) &
                 "', there is no goal with that index.";
            end if;
         elsif Goal_Index > 0 then
            raise Data_Loading_Error
              with "Can't add goal '" &
              To_String(Source => Temp_Record.Index) &
              "', there is already a goal with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Goals_List(Goal_Index);
            end if;
            if Get_Attribute(Elem => Goal_Node, Name => "type") /= "" then
               Temp_Record.G_Type :=
                 Goal_Types'Value
                   (Get_Attribute(Elem => Goal_Node, Name => "type"));
            end if;
            if Get_Attribute(Elem => Goal_Node, Name => "amount") /= "" then
               Temp_Record.Amount :=
                 Natural'Value
                   (Get_Attribute(Elem => Goal_Node, Name => "amount"));
            end if;
            if Get_Attribute(Elem => Goal_Node, Name => "target") /= "" then
               Temp_Record.Target_Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Goal_Node, Name => "target"));
            end if;
            if Get_Attribute(Elem => Goal_Node, Name => "multiplier") /=
              "" then
               Temp_Record.Multiplier :=
                 Natural'Value
                   (Get_Attribute(Elem => Goal_Node, Name => "multiplier"));
            end if;
            if Action /= UPDATE then
               Goals_List.Append(New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Goal added: " & To_String(Source => Temp_Record.Index),
                  Message_Type => EVERYTHING);
            else
               Goals_List(Goal_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Goal updated: " & To_String(Source => Temp_Record.Index),
                  Message_Type => EVERYTHING);
            end if;
         else
            Goals_List.Delete(Index => Goal_Index);
            Log_Message
              (Message =>
                 "Goal removed: " & To_String(Source => Temp_Record.Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Goals_Loop;
   end Load_Goals;

   function Goal_Text(Index: Goals_Container.Extended_Index) return String is
      Text: Unbounded_String;
      Goal: Goal_Data;
      Insert_Position: Positive;
      Added: Boolean := False;
      type Faction_Name_Type is (NAME, MEMBERNAME, PLURALMEMBERNAME);
      function Get_Faction_Name
        (Faction_Index: Unbounded_String; F_Type: Faction_Name_Type)
         return String is
      begin
         case F_Type is
            when NAME =>
               return To_String(Source => Factions_List(Faction_Index).Name);
            when MEMBERNAME =>
               return
                 To_String(Source => Factions_List(Faction_Index).Member_Name);
            when PLURALMEMBERNAME =>
               return
                 To_String
                   (Source => Factions_List(Faction_Index).Plural_Member_Name);
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
      if (Goal.G_Type not in RANDOM | KILL) and Goal.Amount > 1 then
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
                      (Faction_Index => Goal.Target_Index, F_Type => NAME) &
                    " ");
            when DESTROY =>
               Destroy_Ship_Loop :
               for I in Proto_Ships_List.Iterate loop
                  if Proto_Ships_Container.Key(Position => I) =
                    Goal.Target_Index then
                     Append
                       (Source => Text,
                        New_Item =>
                          ": " &
                          To_String(Source => Proto_Ships_List(I).Name));
                     Added := True;
                     exit Destroy_Ship_Loop;
                  end if;
               end loop Destroy_Ship_Loop;
               if not Added then
                  Insert_Position := Length(Source => Text) - 3;
                  if Goal.Amount > 1 then
                     Insert_Position := Insert_Position - 1;
                  end if;
                  Insert
                    (Source => Text, Before => Insert_Position,
                     New_Item =>
                       Get_Faction_Name
                         (Faction_Index => Goal.Target_Index, F_Type => NAME) &
                       " ");
               end if;
            when CRAFT =>
               if Recipes_Container.Contains
                   (Container => Recipes_List, Key => Goal.Target_Index) then
                  Get_Item_Name_Block :
                  declare
                     Item_Index: constant Tiny_String.Bounded_String :=
                       Recipes_List(Goal.Target_Index).Result_Index;
                  begin
                     Append
                       (Source => Text,
                        New_Item =>
                          ": " &
                          To_String(Source => Items_List(Item_Index).Name));
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
                  when Deliver =>
                     Append
                       (Source => Text,
                        New_Item => ": Deliver items to bases");
                  when Patrol =>
                     Append(Source => Text, New_Item => ": Patrol areas");
                  when Destroy =>
                     Append(Source => Text, New_Item => ": Destroy ships");
                  when Explore =>
                     Append(Source => Text, New_Item => ": Explore areas");
                  when Passenger =>
                     Append(Source => Text, New_Item => ": Transport passengers to bases");
               end case;
            when KILL =>
               Insert_Position := Length(Source => Text) - 20;
               if Goal.Amount > 1 then
                  Insert_Position := Insert_Position - 2;
               end if;
               Get_Faction_Name_Block:
               declare
                  Stop_Position: Natural := Insert_Position + 4;
               begin
                  if Goal.Amount > 1 then
                     Stop_Position := Stop_Position + 2;
                     Replace_Slice
                       (Source => Text, Low => Insert_Position, High => Stop_Position,
                        By => Get_Faction_Name(Faction_Index => Goal.Target_Index, F_Type => PLURALMEMBERNAME));
                  else
                     Replace_Slice
                       (Source => Text, Low => Insert_Position, High => Stop_Position,
                        By => Get_Faction_Name(Faction_Index => Goal.Target_Index, F_Type => MEMBERNAME));
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
   begin
      if G_Type /= Current_Goal.G_Type then
         return;
      end if;
      if To_Lower(To_String(Target_Index)) /=
        To_Lower(To_String(Current_Goal.Target_Index)) and
        Current_Goal.Target_Index /= Null_Unbounded_String then
         return;
      end if;
      Current_Goal.Amount :=
        (if Amount >= Current_Goal.Amount then 0
         else Current_Goal.Amount - Amount);
      if Current_Goal.Amount = 0 then
         UpdateFinishedGoals(Current_Goal.Index);
         AddMessage
           ("You finished your goal. New goal is set.", OtherMessage, BLUE);
         Current_Goal :=
           Goals_List
             (Get_Random(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
   end Update_Goal;

end Goals;
