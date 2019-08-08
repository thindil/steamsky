--    Copyright 2017-2019 Bartek thindil Jasicki
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

   procedure LoadGoals(Reader: Tree_Reader) is
      TempRecord: Goal_Data;
      NodesList: Node_List;
      GoalsData: Document;
      Action: DataAction;
      GoalIndex: Natural;
      GoalNode: Node;
   begin
      GoalsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(GoalsData, "goal");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Index => Null_Unbounded_String, GType => RANDOM, Amount => 0,
            TargetIndex => Null_Unbounded_String, Multiplier => 1);
         GoalNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(GoalNode, "index"));
         if Get_Attribute(GoalNode, "action")'Length > 0 then
            Action := DataAction'Value(Get_Attribute(GoalNode, "action"));
         else
            Action := ADD;
         end if;
         GoalIndex := 0;
         for J in Goals_List.Iterate loop
            if Goals_List(J).Index = TempRecord.Index then
               GoalIndex := Goals_Container.To_Index(J);
               exit;
            end if;
         end loop;
         if (Action = UPDATE or Action = REMOVE) then
            if GoalIndex = 0 then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " goal '" & To_String(TempRecord.Index) &
                 "', there no goal with that index.";
            end if;
         elsif GoalIndex > 0 then
            raise Data_Loading_Error
              with "Can't add goal '" & To_String(TempRecord.Index) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Goals_List(GoalIndex);
            end if;
            if Get_Attribute(GoalNode, "type") /= "" then
               TempRecord.GType :=
                 GoalTypes'Value(Get_Attribute(GoalNode, "type"));
            end if;
            if Get_Attribute(GoalNode, "amount") /= "" then
               TempRecord.Amount :=
                 Natural'Value(Get_Attribute(GoalNode, "amount"));
            end if;
            if Get_Attribute(GoalNode, "target") /= "" then
               TempRecord.TargetIndex :=
                 To_Unbounded_String(Get_Attribute(GoalNode, "target"));
            end if;
            if Get_Attribute(GoalNode, "multiplier") /= "" then
               TempRecord.Multiplier :=
                 Natural'Value(Get_Attribute(GoalNode, "multiplier"));
            end if;
            if Action /= UPDATE then
               Goals_List.Append(New_Item => TempRecord);
               LogMessage
                 ("Goal added: " & To_String(TempRecord.Index), Everything);
            else
               Goals_List(GoalIndex) := TempRecord;
               LogMessage
                 ("Goal updated: " & To_String(TempRecord.Index), Everything);
            end if;
         else
            Goals_List.Delete(Index => GoalIndex);
            LogMessage
              ("Goal removed: " & To_String(TempRecord.Index), Everything);
         end if;
      end loop;
   end LoadGoals;

   function GoalText(Index: Goals_Container.Extended_Index) return String is
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
               return To_String(Factions_List(FactionIndex).MemberName);
            when PLURALMEMBERNAME =>
               return To_String(Factions_List(FactionIndex).PluralMemberName);
         end case;
      end GetFactionName;
   begin
      if Index > 0 then
         Goal := Goals_List(Index);
      else
         Goal := CurrentGoal;
      end if;
      case Goal.GType is
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
      case Goal.GType is
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
      if (Goal.GType /= RANDOM and Goal.GType /= KILL) and Goal.Amount > 1 then
         Append(Text, "s");
      end if;
      case Goal.GType is
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
      if Goal.TargetIndex /= Null_Unbounded_String then
         case Goal.GType is
            when REPUTATION | VISIT =>
               InsertPosition := Length(Text) - 3;
               if Goal.Amount > 1 then
                  InsertPosition := InsertPosition - 1;
               end if;
               Insert
                 (Text, InsertPosition,
                  GetFactionName(Goal.TargetIndex, NAME) & " ");
            when DESTROY =>
               for I in ProtoShips_List.Iterate loop
                  if ProtoShips_Container.Key(I) = Goal.TargetIndex then
                     Append(Text, ": " & To_String(ProtoShips_List(I).Name));
                     Added := True;
                     exit;
                  end if;
               end loop;
               if not Added then
                  InsertPosition := Length(Text) - 3;
                  if Goal.Amount > 1 then
                     InsertPosition := InsertPosition - 1;
                  end if;
                  Insert
                    (Text, InsertPosition,
                     GetFactionName(Goal.TargetIndex, NAME) & " ");
               end if;
            when CRAFT =>
               if Recipes_Container.Contains
                   (Recipes_List, Goal.TargetIndex) then
                  declare
                     ItemIndex: constant Unbounded_String :=
                       Recipes_List(Goal.TargetIndex).ResultIndex;
                  begin
                     Append
                       (Text, ": " & To_String(Items_List(ItemIndex).Name));
                  end;
               else
                  Append(Text, ": " & To_String(Goal.TargetIndex));
               end if;
            when MISSION =>
               case Missions_Types'Value(To_String(Goal.TargetIndex)) is
                  when Deliver =>
                     Append(Text, ": Deliver item to base");
                  when Patrol =>
                     Append(Text, ": Patrol area");
                  when Destroy =>
                     Append(Text, ": Destroy ship");
                  when Explore =>
                     Append(Text, ": Explore area");
                  when Passenger =>
                     Append(Text, ": Transport passenger to base");
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
                        GetFactionName(Goal.TargetIndex, PLURALMEMBERNAME));
                  else
                     Replace_Slice
                       (Text, InsertPosition, StopPosition,
                        GetFactionName(Goal.TargetIndex, MEMBERNAME));
                  end if;
               end;
            when RANDOM | DISCOVER =>
               null;
         end case;
      end if;
      return To_String(Text);
   end GoalText;

   procedure ClearCurrentGoal is
   begin
      CurrentGoal :=
        (Index => Null_Unbounded_String, GType => RANDOM, Amount => 0,
         TargetIndex => Null_Unbounded_String, Multiplier => 1);
   end ClearCurrentGoal;

   procedure UpdateGoal
     (GType: GoalTypes; TargetIndex: Unbounded_String;
      Amount: Positive := 1) is
   begin
      if GType /= CurrentGoal.GType then
         return;
      end if;
      if To_Lower(To_String(TargetIndex)) /=
        To_Lower(To_String(CurrentGoal.TargetIndex)) and
        CurrentGoal.TargetIndex /= Null_Unbounded_String then
         return;
      end if;
      if Amount >= CurrentGoal.Amount then
         CurrentGoal.Amount := 0;
      else
         CurrentGoal.Amount := CurrentGoal.Amount - Amount;
      end if;
      if CurrentGoal.Amount = 0 then
         UpdateFinishedGoals(CurrentGoal.Index);
         AddMessage
           ("You finished your goal. New goal is set.", OtherMessage, BLUE);
         CurrentGoal :=
           Goals_List
             (GetRandom(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
   end UpdateGoal;

end Goals;
