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
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Doc => Goals_Data, Tag_Name => "goal");
      Load_Goals_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Index => Null_Unbounded_String, G_Type => RANDOM, Amount => 0,
            Target_Index => Null_Unbounded_String, Multiplier => 1);
         Goal_Node := Item(List => Nodes_List, Index => I);
         Temp_Record.Index :=
           To_Unbounded_String(Source => Get_Attribute(Elem => Goal_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Goal_Node, Name => "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(Elem => Goal_Node, Name => "action"))
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
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " goal '" & To_String(Temp_Record.Index) &
                 "', there is no goal with that index.";
            end if;
         elsif Goal_Index > 0 then
            raise Data_Loading_Error
              with "Can't add goal '" & To_String(Temp_Record.Index) &
              "', there is already a goal with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Goals_List(Goal_Index);
            end if;
            if Get_Attribute(Goal_Node, "type") /= "" then
               Temp_Record.G_Type :=
                 Goal_Types'Value(Get_Attribute(Goal_Node, "type"));
            end if;
            if Get_Attribute(Goal_Node, "amount") /= "" then
               Temp_Record.Amount :=
                 Natural'Value(Get_Attribute(Goal_Node, "amount"));
            end if;
            if Get_Attribute(Goal_Node, "target") /= "" then
               Temp_Record.Target_Index :=
                 To_Unbounded_String(Get_Attribute(Goal_Node, "target"));
            end if;
            if Get_Attribute(Goal_Node, "multiplier") /= "" then
               Temp_Record.Multiplier :=
                 Natural'Value(Get_Attribute(Goal_Node, "multiplier"));
            end if;
            if Action /= UPDATE then
               Goals_List.Append(New_Item => Temp_Record);
               Log_Message
                 ("Goal added: " & To_String(Temp_Record.Index), EVERYTHING);
            else
               Goals_List(Goal_Index) := Temp_Record;
               Log_Message
                 ("Goal updated: " & To_String(Temp_Record.Index), EVERYTHING);
            end if;
         else
            Goals_List.Delete(Index => Goal_Index);
            Log_Message
              ("Goal removed: " & To_String(Temp_Record.Index), EVERYTHING);
         end if;
      end loop Load_Goals_Loop;
   end Load_Goals;

   function Goal_Text(Index: Goals_Container.Extended_Index) return String is
      Text: Unbounded_String;
      Goal: Goal_Data;
      InsertPosition: Positive;
      Added: Boolean := False;
      type FactionNameType is (NAME, MEMBERNAME, PLURALMEMBERNAME);
      function GetFactionName
        (FactionIndex: Unbounded_String; FType: FactionNameType)
         return String is
      begin
         case FType is
            when NAME =>
               return To_String(Factions_List(FactionIndex).Name);
            when MEMBERNAME =>
               return To_String(Factions_List(FactionIndex).Member_Name);
            when PLURALMEMBERNAME =>
               return
                 To_String(Factions_List(FactionIndex).Plural_Member_Name);
         end case;
      end GetFactionName;
   begin
      Goal := (if Index > 0 then Goals_List(Index) else Current_Goal);
      case Goal.G_Type is
         when REPUTATION =>
            Text := To_Unbounded_String("Gain max reputation in");
         when DESTROY =>
            Text := To_Unbounded_String("Destroy");
         when DISCOVER =>
            Text := To_Unbounded_String("Discover");
         when VISIT =>
            Text := To_Unbounded_String("Visit");
         when CRAFT =>
            Text := To_Unbounded_String("Craft");
         when MISSION =>
            Text := To_Unbounded_String("Finish");
         when KILL =>
            Text := To_Unbounded_String("Kill");
         when RANDOM =>
            null;
      end case;
      Append(Text, Positive'Image(Goal.Amount));
      case Goal.G_Type is
         when REPUTATION | VISIT =>
            Append(Text, " base");
         when DESTROY =>
            Append(Text, " ship");
         when DISCOVER =>
            Append(Text, " field");
         when CRAFT =>
            Append(Text, " item");
         when MISSION =>
            Append(Text, " mission");
         when KILL =>
            Append(Text, " enem");
         when RANDOM =>
            null;
      end case;
      if (Goal.G_Type not in RANDOM | KILL) and Goal.Amount > 1 then
         Append(Text, "s");
      end if;
      case Goal.G_Type is
         when DISCOVER =>
            Append(Text, " of map");
         when KILL =>
            if Goal.Amount > 1 then
               Append(Text, "ies in melee combat");
            else
               Append(Text, "y in melee combat");
            end if;
         when others =>
            null;
      end case;
      if Goal.Target_Index /= Null_Unbounded_String then
         case Goal.G_Type is
            when REPUTATION | VISIT =>
               InsertPosition := Length(Text) - 3;
               if Goal.Amount > 1 then
                  InsertPosition := InsertPosition - 1;
               end if;
               Insert
                 (Text, InsertPosition,
                  GetFactionName(Goal.Target_Index, NAME) & " ");
            when DESTROY =>
               Destroy_Ship_Loop :
               for I in Proto_Ships_List.Iterate loop
                  if Proto_Ships_Container.Key(I) = Goal.Target_Index then
                     Append(Text, ": " & To_String(Proto_Ships_List(I).Name));
                     Added := True;
                     exit Destroy_Ship_Loop;
                  end if;
               end loop Destroy_Ship_Loop;
               if not Added then
                  InsertPosition := Length(Text) - 3;
                  if Goal.Amount > 1 then
                     InsertPosition := InsertPosition - 1;
                  end if;
                  Insert
                    (Text, InsertPosition,
                     GetFactionName(Goal.Target_Index, NAME) & " ");
               end if;
            when CRAFT =>
               if Recipes_Container.Contains
                   (Recipes_List, Goal.Target_Index) then
                  declare
                     ItemIndex: constant Unbounded_String :=
                       Recipes_List(Goal.Target_Index).Result_Index;
                  begin
                     Append
                       (Text, ": " & To_String(Items_List(ItemIndex).Name));
                  end;
               else
                  Append(Text, ": " & To_String(Goal.Target_Index));
               end if;
            when MISSION =>
               case Missions_Types'Value(To_String(Goal.Target_Index)) is
                  when Deliver =>
                     Append(Text, ": Deliver items to bases");
                  when Patrol =>
                     Append(Text, ": Patrol areas");
                  when Destroy =>
                     Append(Text, ": Destroy ships");
                  when Explore =>
                     Append(Text, ": Explore areas");
                  when Passenger =>
                     Append(Text, ": Transport passengers to bases");
               end case;
            when KILL =>
               InsertPosition := Length(Text) - 20;
               if Goal.Amount > 1 then
                  InsertPosition := InsertPosition - 2;
               end if;
               declare
                  StopPosition: Natural := InsertPosition + 4;
               begin
                  if Goal.Amount > 1 then
                     StopPosition := StopPosition + 2;
                     Replace_Slice
                       (Text, InsertPosition, StopPosition,
                        GetFactionName(Goal.Target_Index, PLURALMEMBERNAME));
                  else
                     Replace_Slice
                       (Text, InsertPosition, StopPosition,
                        GetFactionName(Goal.Target_Index, MEMBERNAME));
                  end if;
               end;
            when RANDOM | DISCOVER =>
               null;
         end case;
      end if;
      return To_String(Text);
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
