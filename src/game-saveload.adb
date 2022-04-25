--    Copyright 2017-2022 Bartek thindil Jasicki
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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with Bases;
with Bases.SaveLoad; use Bases.SaveLoad;
with Careers;
with Config; use Config;
with Crafts; use Crafts;
with Events; use Events;
with Goals; use Goals;
with Log; use Log;
with Maps; use Maps;
with Messages; use Messages;
with Missions; use Missions;
with Ships; use Ships;
with Ships.SaveLoad; use Ships.SaveLoad;
with Statistics; use Statistics;
with Stories; use Stories;
with Utils;

package body Game.SaveLoad is

   -- ****iv* GSaveLoad/GSaveLoad.Save_Version
   -- FUNCTION
   -- Current version of the save game
   -- SOURCE
   Save_Version: constant Positive := 5;
   -- ****

   procedure Save_Game(Pretty_Print: Boolean := False) is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Save: DOM_Implementation;
      --## rule on IMPROPER_INITIALIZATION
      Category_Node, Main_Node: DOM.Core.Element;
      Raw_Value: Unbounded_String := Null_Unbounded_String;
      Save_File: File_Type;
      Save_Data: Document;
      procedure Save_Statistics
        (Statistics_Vector: in out Statistics_Container.Vector;
         Stat_Name: String) is
         Stat_Node: DOM.Core.Element;
      begin
         Save_Statistics_Loop :
         for Statistic of Statistics_Vector loop
            Stat_Node :=
              Append_Child
                (N => Category_Node,
                 New_Child =>
                   Create_Element(Doc => Save_Data, Tag_Name => Stat_Name));
            Set_Attribute
              (Elem => Stat_Node, Name => "index",
               Value => To_String(Source => Statistic.Index));
            Raw_Value :=
              To_Unbounded_String(Source => Integer'Image(Statistic.Amount));
            Set_Attribute
              (Elem => Stat_Node, Name => "amount",
               Value =>
                 To_String
                   (Source =>
                      Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         end loop Save_Statistics_Loop;
      end Save_Statistics;
      procedure Save_Number
        (Value: Integer; Name: String;
         Node: DOM.Core.Element := Category_Node) is
         Number_String: constant String :=
           Trim(Source => Integer'Image(Value), Side => Ada.Strings.Left);
      begin
         Set_Attribute(Elem => Node, Name => Name, Value => Number_String);
      end Save_Number;
      --## rule off TYPE_INITIAL_VALUES
      type Difficulty_Data is record
         Name: Unbounded_String;
         Value: Bonus_Type;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Difficulties: constant array(1 .. 8) of Difficulty_Data :=
        (1 =>
           (Name => To_Unbounded_String(Source => "enemydamagebonus"),
            Value => New_Game_Settings.Enemy_Damage_Bonus),
         2 =>
           (Name => To_Unbounded_String(Source => "playerdamagebonus"),
            Value => New_Game_Settings.Player_Damage_Bonus),
         3 =>
           (Name => To_Unbounded_String(Source => "enemymeleedamagebonus"),
            Value => New_Game_Settings.Enemy_Melee_Damage_Bonus),
         4 =>
           (Name => To_Unbounded_String(Source => "playermeleedamagebonus"),
            Value => New_Game_Settings.Player_Melee_Damage_Bonus),
         5 =>
           (Name => To_Unbounded_String(Source => "experiencebonus"),
            Value => New_Game_Settings.Experience_Bonus),
         6 =>
           (Name => To_Unbounded_String(Source => "reputationbonus"),
            Value => New_Game_Settings.Reputation_Bonus),
         7 =>
           (Name => To_Unbounded_String(Source => "upgradecostbonus"),
            Value => New_Game_Settings.Upgrade_Cost_Bonus),
         8 =>
           (Name => To_Unbounded_String(Source => "pricesbonus"),
            Value => New_Game_Settings.Prices_Bonus));
   begin
      Log_Message
        (Message =>
           "Start saving game in file " & To_String(Source => Save_Name) & ".",
         Message_Type => EVERYTHING);
      --## rule off IMPROPER_INITIALIZATION
      Save_Data := Create_Document(Implementation => Save);
      --## rule on IMPROPER_INITIALIZATION
      Main_Node :=
        Append_Child
          (N => Save_Data,
           New_Child => Create_Element(Doc => Save_Data, Tag_Name => "save"));
      -- Write save game version
      Set_Attribute
        (Elem => Main_Node, Name => "version",
         Value =>
           Trim
             (Source => Positive'Image(Save_Version),
              Side => Ada.Strings.Left));
      -- Save game difficulty settings
      Log_Message
        (Message => "Saving game difficulty settings...",
         Message_Type => EVERYTHING, New_Line => False);
      Category_Node :=
        Append_Child
          (N => Main_Node,
           New_Child =>
             Create_Element(Doc => Save_Data, Tag_Name => "difficulty"));
      Save_Difficulty_Loop :
      for Difficulty of Difficulties loop
         Raw_Value :=
           To_Unbounded_String(Source => Bonus_Type'Image(Difficulty.Value));
         Set_Attribute
           (Elem => Category_Node,
            Name => To_String(Source => Difficulty.Name),
            Value =>
              To_String
                (Source =>
                   Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
      end loop Save_Difficulty_Loop;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save game date
      Log_Message
        (Message => "Saving game time...", Message_Type => EVERYTHING,
         New_Line => False);
      Category_Node :=
        Append_Child
          (N => Main_Node,
           New_Child =>
             Create_Element(Doc => Save_Data, Tag_Name => "gamedate"));
      Save_Number(Value => Game_Date.Year, Name => "year");
      Save_Number(Value => Game_Date.Month, Name => "month");
      Save_Number(Value => Game_Date.Day, Name => "day");
      Save_Number(Value => Game_Date.Hour, Name => "hour");
      Save_Number(Value => Game_Date.Minutes, Name => "minutes");
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save map
      Log_Message
        (Message => "Saving map...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Map_Block :
      declare
         Field_Node: DOM.Core.Element;
      begin
         Save_Map_X_Loop :
         for X in Sky_Map'Range(1) loop
            Save_Map_Y_Loop :
            for Y in Sky_Map'Range(2) loop
               if Sky_Map(X, Y).Visited then
                  Field_Node :=
                    Append_Child
                      (N => Main_Node,
                       New_Child =>
                         Create_Element
                           (Doc => Save_Data, Tag_Name => "field"));
                  Save_Number(Value => X, Name => "x", Node => Field_Node);
                  Save_Number(Value => Y, Name => "y", Node => Field_Node);
               end if;
            end loop Save_Map_Y_Loop;
         end loop Save_Map_X_Loop;
      end Save_Map_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save bases
      Log_Message
        (Message => "Saving bases...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Bases(Save_Data => Save_Data, Main_Node => Main_Node);
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save player ship
      Log_Message
        (Message => "Saving player ship...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Player_Ship(Save_Data => Save_Data, Main_Node => Main_Node);
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save known recipes
      Log_Message
        (Message => "Saving known recipes...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Known_Recipes_Block :
      declare
         Recipe_Node: DOM.Core.Element;
      begin
         Save_Known_Recipes_Loop :
         for Recipe of Known_Recipes loop
            Recipe_Node :=
              Append_Child
                (N => Main_Node,
                 New_Child =>
                   Create_Element(Doc => Save_Data, Tag_Name => "recipe"));
            Set_Attribute
              (Elem => Recipe_Node, Name => "index",
               Value => To_String(Source => Recipe));
         end loop Save_Known_Recipes_Loop;
      end Save_Known_Recipes_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save messages
      Log_Message
        (Message => "Saving messages...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Messages_Block :
      declare
         Messages_To_Save: constant Natural :=
           (if Game_Settings.Saved_Messages > Messages_Amount then
              Messages_Amount
            else Game_Settings.Saved_Messages);
         Start_Loop: Positive := 1;
         Message_Node: DOM.Core.Element;
         Message: Message_Data :=
           (Message => Null_Unbounded_String, M_Type => DEFAULT,
            Color => WHITE);
         Message_Text: Text;
      begin
         if Messages_To_Save > 0 then
            Start_Loop := Messages_Amount - Messages_To_Save + 1;
            Save_Messages_Loop :
            for I in Start_Loop .. Messages_Amount loop
               Message := Get_Message(Message_Index => I);
               Message_Node :=
                 Append_Child
                   (N => Main_Node,
                    New_Child =>
                      Create_Element(Doc => Save_Data, Tag_Name => "message"));
               Save_Number
                 (Value => Message_Type'Pos(Message.M_Type), Name => "type",
                  Node => Message_Node);
               Save_Number
                 (Value => Message_Color'Pos(Message.Color), Name => "color",
                  Node => Message_Node);
               --## rule off ASSIGNMENTS
               Message_Text :=
                 Create_Text_Node
                   (Doc => Save_Data,
                    Data => To_String(Source => Message.Message));
               Message_Text :=
                 Append_Child(N => Message_Node, New_Child => Message_Text);
               --## rule on ASSIGNMENTS
            end loop Save_Messages_Loop;
         end if;
      end Save_Messages_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save events
      Log_Message
        (Message => "Saving events...", Message_Type => EVERYTHING,
         New_Line => False);
      Save_Known_Events_Block :
      declare
         Event_Node: DOM.Core.Element;
      begin
         Save_Events_Loop :
         for Event of Events_List loop
            Event_Node :=
              Append_Child
                (N => Main_Node,
                 New_Child =>
                   Create_Element(Doc => Save_Data, Tag_Name => "event"));
            Save_Number
              (Value => Events_Types'Pos(Event.E_Type), Name => "type",
               Node => Event_Node);
            Save_Number(Value => Event.Sky_X, Name => "x", Node => Event_Node);
            Save_Number(Value => Event.Sky_Y, Name => "y", Node => Event_Node);
            Save_Number
              (Value => Event.Time, Name => "time", Node => Event_Node);
            case Event.E_Type is
               when DOUBLEPRICE =>
                  Raw_Value :=
                    To_Unbounded_String
                      (Source =>
                         Tiny_String.To_String(Source => Event.Item_Index));
               when ATTACKONBASE | ENEMYSHIP | ENEMYPATROL | TRADER |
                 FRIENDLYSHIP =>
                  Raw_Value := To_Unbounded_String(Source => Event.Ship_Index'Img);
               when others =>
                  Raw_Value :=
                    To_Unbounded_String(Source => Integer'Image(Event.Data));
            end case;
            Set_Attribute
              (Elem => Event_Node, Name => "data",
               Value =>
                 To_String
                   (Source =>
                      Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         end loop Save_Events_Loop;
      end Save_Known_Events_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save game statistics
      Log_Message
        (Message => "Saving game statistics...", Message_Type => EVERYTHING,
         New_Line => False);
      Category_Node :=
        Append_Child
          (N => Main_Node,
           New_Child =>
             Create_Element(Doc => Save_Data, Tag_Name => "statistics"));
      Save_Statistics
        (Statistics_Vector => Game_Stats.Destroyed_Ships,
         Stat_Name => "destroyedships");
      Save_Number(Value => Game_Stats.Bases_Visited, Name => "visitedbases");
      Save_Number(Value => Game_Stats.Map_Visited, Name => "mapdiscovered");
      Save_Number
        (Value => Game_Stats.Distance_Traveled, Name => "distancetraveled");
      Save_Statistics
        (Statistics_Vector => Game_Stats.Crafting_Orders,
         Stat_Name => "finishedcrafts");
      Save_Number
        (Value => Game_Stats.Accepted_Missions, Name => "acceptedmissions");
      Save_Statistics
        (Statistics_Vector => Game_Stats.Finished_Missions,
         Stat_Name => "finishedmissions");
      Save_Statistics
        (Statistics_Vector => Game_Stats.Finished_Goals,
         Stat_Name => "finishedgoals");
      Save_Statistics
        (Statistics_Vector => Game_Stats.Killed_Mobs,
         Stat_Name => "killedmobs");
      Save_Number(Value => Game_Stats.Points, Name => "points");
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save current goal
      Log_Message
        (Message => "Saving current goal...", Message_Type => EVERYTHING,
         New_Line => False);
      Category_Node :=
        Append_Child
          (N => Main_Node,
           New_Child =>
             Create_Element(Doc => Save_Data, Tag_Name => "currentgoal"));
      Set_Attribute
        (Elem => Category_Node, Name => "index",
         Value => To_String(Source => Current_Goal.Index));
      Save_Number
        (Value => Goal_Types'Pos(Current_Goal.G_Type), Name => "type");
      Save_Number(Value => Current_Goal.Amount, Name => "amount");
      Set_Attribute
        (Elem => Category_Node, Name => "target",
         Value => To_String(Source => Current_Goal.Target_Index));
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Save current story
      if Current_Story.Index /= Null_Unbounded_String then
         Log_Message
           (Message => "Saving current story...", Message_Type => EVERYTHING,
            New_Line => False);
         Category_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element(Doc => Save_Data, Tag_Name => "Current_Story"));
         Set_Attribute
           (Elem => Category_Node, Name => "index",
            Value => To_String(Source => Current_Story.Index));
         Raw_Value :=
           To_Unbounded_String(Source => Positive'Image(Current_Story.Step));
         Set_Attribute
           (Elem => Category_Node, Name => "step",
            Value =>
              To_String
                (Source =>
                   Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         case Current_Story.Current_Step is
            when 0 =>
               Set_Attribute
                 (Elem => Category_Node, Name => "currentstep",
                  Value => "start");
            when -1 =>
               Set_Attribute
                 (Elem => Category_Node, Name => "currentstep",
                  Value => "finish");
            when others =>
               Set_Attribute
                 (Elem => Category_Node, Name => "currentstep",
                  Value =>
                    To_String
                      (Source =>
                         Stories_List(Current_Story.Index).Steps
                           (Current_Story.Current_Step)
                           .Index));
         end case;
         Save_Number(Value => Current_Story.Max_Steps, Name => "maxsteps");
         if Current_Story.Show_Text then
            Set_Attribute
              (Elem => Category_Node, Name => "showtext", Value => "Y");
         else
            Set_Attribute
              (Elem => Category_Node, Name => "showtext", Value => "N");
         end if;
         if Current_Story.Data /= Null_Unbounded_String then
            Set_Attribute
              (Elem => Category_Node, Name => "data",
               Value => To_String(Source => Current_Story.Data));
         end if;
         Save_Number
           (Value => Step_Condition_Type'Pos(Current_Story.Finished_Step),
            Name => "finishedstep");
         Log_Message
           (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
            Time_Stamp => False);
      end if;
      -- Save finished stories data
      Save_Finished_Stories_Block :
      declare
         Step_Node: DOM.Core.Element;
         Step_Text: Text;
      begin
         Log_Message
           (Message => "Saving finished stories...",
            Message_Type => EVERYTHING, New_Line => False);
         Save_Finished_Stories_Loop :
         for FinishedStory of Finished_Stories loop
            Category_Node :=
              Append_Child
                (N => Main_Node,
                 New_Child =>
                   Create_Element
                     (Doc => Save_Data, Tag_Name => "finishedstory"));
            Set_Attribute
              (Elem => Category_Node, Name => "index",
               Value => To_String(Source => FinishedStory.Index));
            Save_Number
              (Value => FinishedStory.Steps_Amount, Name => "stepsamount");
            Save_Story_Steps_Loop :
            for Step of FinishedStory.Steps_Texts loop
               Step_Node :=
                 Append_Child
                   (N => Category_Node,
                    New_Child =>
                      Create_Element
                        (Doc => Save_Data, Tag_Name => "steptext"));
               --## rule off ASSIGNMENTS
               Step_Text :=
                 Create_Text_Node
                   (Doc => Save_Data, Data => To_String(Source => Step));
               Step_Text :=
                 Append_Child(N => Step_Node, New_Child => Step_Text);
               --## rule on ASSIGNMENTS
            end loop Save_Story_Steps_Loop;
         end loop Save_Finished_Stories_Loop;
         Log_Message
           (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
            Time_Stamp => False);
      end Save_Finished_Stories_Block;
      -- Save missions accepted by player
      Save_Missions_Loop :
      for Mission of Accepted_Missions loop
         Category_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element
                  (Doc => Save_Data, Tag_Name => "acceptedmission"));
         Save_Number
           (Value => Missions_Types'Pos(Mission.M_Type), Name => "type");
         Raw_Value :=
           (if Mission.M_Type = DELIVER then
              To_Unbounded_String
                (Source => Tiny_String.To_String(Source => Mission.Item_Index))
            elsif Mission.M_Type = PASSENGER then
              To_Unbounded_String(Source => Integer'Image(Mission.Data))
            elsif Mission.M_Type = DESTROY then To_Unbounded_String(Source => Mission.Ship_Index'Img)
            else To_Unbounded_String(Source => Integer'Image(Mission.Target)));
         Set_Attribute
           (Elem => Category_Node, Name => "target",
            Value =>
              To_String
                (Source =>
                   Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         Save_Number(Value => Mission.Time, Name => "time");
         Save_Number(Value => Mission.Target_X, Name => "targetx");
         Save_Number(Value => Mission.Target_Y, Name => "targety");
         Save_Number(Value => Mission.Reward, Name => "reward");
         Save_Number(Value => Mission.Start_Base, Name => "startbase");
         if Mission.Finished then
            Set_Attribute
              (Elem => Category_Node, Name => "finished", Value => "Y");
         else
            Set_Attribute
              (Elem => Category_Node, Name => "finished", Value => "N");
         end if;
         if Mission.Multiplier /= 1.0 then
            Raw_Value :=
              To_Unbounded_String
                (Source => Reward_Multiplier'Image(Mission.Multiplier));
            Set_Attribute
              (Elem => Category_Node, Name => "multiplier",
               Value =>
                 To_String
                   (Source =>
                      Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
         end if;
      end loop Save_Missions_Loop;
      -- Save player career
      Log_Message
        (Message => "Saving player career...", Message_Type => EVERYTHING,
         New_Line => False);
      Category_Node :=
        Append_Child
          (N => Main_Node,
           New_Child =>
             Create_Element(Doc => Save_Data, Tag_Name => "playercareer"));
      Set_Attribute
        (Elem => Category_Node, Name => "index",
         Value => To_String(Source => Player_Career));
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      Create
        (File => Save_File, Mode => Out_File,
         Name => To_String(Source => Save_Name));
      Write
        (Stream => Stream(File => Save_File), N => Save_Data,
         Pretty_Print => Pretty_Print);
      Close(File => Save_File);
      Log_Message
        (Message => "Finished saving game.", Message_Type => EVERYTHING);
   end Save_Game;

   procedure Load_Game is
      use Ada.Exceptions;
      use DOM.Readers;
      use Input_Sources.File;
      use Careers;
      use Tiny_String;

      Save_File: File_Input;
      --## rule off IMPROPER_INITIALIZATION
      Reader: Tree_Reader;
      Child_Nodes_List: Node_List;
      --## rule on IMPROPER_INITIALIZATION
      Nodes_List: Node_List;
      Saved_Node: Node;
      Save_Data: Document;
   begin
      Log_Message
        (Message =>
           "Start loading game from file " & To_String(Source => Save_Name) &
           ".",
         Message_Type => EVERYTHING);
      Open(Filename => To_String(Source => Save_Name), Input => Save_File);
      --## rule off IMPROPER_INITIALIZATION
      Parse(Parser => Reader, Input => Save_File);
      Close(Input => Save_File);
      Save_Data := Get_Tree(Read => Reader);
      --## rule off IMPROPER_INITIALIZATION
      -- Check save game compatybility
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "save");
      Saved_Node := Item(List => Nodes_List, Index => 0);
      if Get_Attribute(Elem => Saved_Node, Name => "version") /= "" then
         if Positive'Value
             (Get_Attribute(Elem => Saved_Node, Name => "version")) >
           Save_Version then
            raise Save_Game_Invalid_Data
              with "This save is incompatible with this version of the game";
         end if;
      end if;
      -- Load game difficulty settings
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "difficulty");
      if Length(List => Nodes_List) > 0 then
         Log_Message
           (Message => "Loading game difficulty settings...",
            Message_Type => EVERYTHING, New_Line => False);
         Saved_Node := Item(List => Nodes_List, Index => 0);
         New_Game_Settings.Enemy_Damage_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(Elem => Saved_Node, Name => "enemydamagebonus"));
         New_Game_Settings.Player_Damage_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(Elem => Saved_Node, Name => "playerdamagebonus"));
         New_Game_Settings.Enemy_Melee_Damage_Bonus :=
           Bonus_Type'Value
             (Get_Attribute
                (Elem => Saved_Node, Name => "enemymeleedamagebonus"));
         New_Game_Settings.Player_Melee_Damage_Bonus :=
           Bonus_Type'Value
             (Get_Attribute
                (Elem => Saved_Node, Name => "playermeleedamagebonus"));
         New_Game_Settings.Experience_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(Elem => Saved_Node, Name => "experiencebonus"));
         New_Game_Settings.Reputation_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(Elem => Saved_Node, Name => "reputationbonus"));
         New_Game_Settings.Upgrade_Cost_Bonus :=
           Bonus_Type'Value
             (Get_Attribute(Elem => Saved_Node, Name => "upgradecostbonus"));
         if Get_Attribute(Elem => Saved_Node, Name => "pricesbonus") /= "" then
            New_Game_Settings.Prices_Bonus :=
              Bonus_Type'Value
                (Get_Attribute(Elem => Saved_Node, Name => "pricesbonus"));
         end if;
         Log_Message
           (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
            Time_Stamp => False);
      end if;
      -- Load game date
      Log_Message
        (Message => "Loading game time...", Message_Type => EVERYTHING,
         New_Line => False);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "gamedate");
      Saved_Node := Item(List => Nodes_List, Index => 0);
      Game_Date.Year :=
        Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "year"));
      Game_Date.Month :=
        Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "month"));
      Game_Date.Day :=
        Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "day"));
      Game_Date.Hour :=
        Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "hour"));
      Game_Date.Minutes :=
        Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "minutes"));
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load sky map
      Log_Message
        (Message => "Loading map...", Message_Type => EVERYTHING,
         New_Line => False);
      Sky_Map :=
        (others =>
           (others =>
              (Base_Index => 0, Visited => False, Event_Index => 0,
               Mission_Index => 0)));
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "field");
      Load_Map_Block :
      declare
         X, Y: Positive;
      begin
         Load_Map_Loop :
         for I in 0 .. Length(List => Nodes_List) - 1 loop
            Saved_Node := Item(List => Nodes_List, Index => I);
            X := Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "x"));
            Y := Natural'Value(Get_Attribute(Elem => Saved_Node, Name => "y"));
            Sky_Map(X, Y).Visited := True;
         end loop Load_Map_Loop;
      end Load_Map_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load sky bases
      Log_Message
        (Message => "Loading bases...", Message_Type => EVERYTHING,
         New_Line => False);
      Load_Bases(Save_Data => Save_Data);
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load player ship
      Log_Message
        (Message => "Loading player ship...", Message_Type => EVERYTHING,
         New_Line => False);
      Load_Player_Ship(Save_Data => Save_Data);
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load known recipes
      Log_Message
        (Message => "Loading known recipes...", Message_Type => EVERYTHING,
         New_Line => False);
      Known_Recipes.Clear;
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "recipe");
      Load_Known_Recipes_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Known_Recipes.Append
           (New_Item =>
              To_Bounded_String
                (Source =>
                   Get_Attribute
                     (Elem => Item(List => Nodes_List, Index => I),
                      Name => "index")));
      end loop Load_Known_Recipes_Loop;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load messages
      Log_Message
        (Message => "Loading messages...", Message_Type => EVERYTHING,
         New_Line => False);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "message");
      Clear_Messages;
      Load_Messages_Block :
      declare
         Text: Unbounded_String;
         M_Type: Message_Type;
         Color: Message_Color;
      begin
         Load_Messages_Loop :
         for I in 0 .. Length(List => Nodes_List) - 1 loop
            Saved_Node := Item(List => Nodes_List, Index => I);
            Text :=
              To_Unbounded_String
                (Source => Node_Value(N => First_Child(N => Saved_Node)));
            M_Type :=
              Message_Type'Val
                (Integer'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "type")));
            Color :=
              Message_Color'Val
                (Integer'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "color")));
            Restore_Message(Message => Text, M_Type => M_Type, Color => Color);
         end loop Load_Messages_Loop;
      end Load_Messages_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load events
      Log_Message
        (Message => "Loading events...", Message_Type => EVERYTHING,
         New_Line => False);
      Events_List.Clear;
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "event");
      Load_Events_Block :
      declare
         E_Type: Events_Types;
         X, Y, Time: Integer;
         Data: Unbounded_String;
      begin
         Load_Events_Loop :
         for I in 0 .. Length(List => Nodes_List) - 1 loop
            Saved_Node := Item(List => Nodes_List, Index => I);
            E_Type :=
              Events_Types'Val
                (Integer'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "type")));
            X := Integer'Value(Get_Attribute(Elem => Saved_Node, Name => "x"));
            Y := Integer'Value(Get_Attribute(Elem => Saved_Node, Name => "y"));
            Time :=
              Integer'Value(Get_Attribute(Elem => Saved_Node, Name => "time"));
            Data :=
              To_Unbounded_String
                (Source => Get_Attribute(Elem => Saved_Node, Name => "data"));
            case E_Type is
               when ENEMYSHIP =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => ENEMYSHIP, Sky_X => X, Sky_Y => Y,
                        Time => Time, Ship_Index => Positive'Value(To_String(Source => Data))));
               when ATTACKONBASE =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => ATTACKONBASE, Sky_X => X, Sky_Y => Y,
                        Time => Time, Ship_Index => Positive'Value(To_String(Source => Data))));
               when DISEASE =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => DISEASE, Sky_X => X, Sky_Y => Y,
                        Time => Time,
                        Data => Integer'Value(To_String(Source => Data))));
               when DOUBLEPRICE =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => DOUBLEPRICE, Sky_X => X, Sky_Y => Y,
                        Time => Time,
                        Item_Index =>
                          Tiny_String.To_Bounded_String
                            (Source => To_String(Source => Data))));
               when FULLDOCKS =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => FULLDOCKS, Sky_X => X, Sky_Y => Y,
                        Time => Time,
                        Data => Integer'Value(To_String(Source => Data))));
               when ENEMYPATROL =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => ENEMYPATROL, Sky_X => X, Sky_Y => Y,
                        Time => Time, Ship_Index => Positive'Value(To_String(Source => Data))));
               when TRADER =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => TRADER, Sky_X => X, Sky_Y => Y, Time => Time,
                        Ship_Index => Positive'Value(To_String(Source => Data))));
               when FRIENDLYSHIP =>
                  Events_List.Append
                    (New_Item =>
                       (E_Type => FRIENDLYSHIP, Sky_X => X, Sky_Y => Y,
                        Time => Time, Ship_Index => Positive'Value(To_String(Source => Data))));
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Sky_Map(Events_List(I + 1).Sky_X, Events_List(I + 1).Sky_Y)
              .Event_Index :=
              I + 1;
         end loop Load_Events_Loop;
      end Load_Events_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load game statistics
      Log_Message
        (Message => "Loading game statistics...", Message_Type => EVERYTHING,
         New_Line => False);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "statistics");
      Load_Statistics_Block :
      declare
         Stat_Index, Nodename: Unbounded_String;
         Stat_Amount: Positive;
      begin
         Saved_Node := Item(List => Nodes_List, Index => 0);
         Game_Stats.Bases_Visited :=
           Positive'Value
             (Get_Attribute(Elem => Saved_Node, Name => "visitedbases"));
         Game_Stats.Map_Visited :=
           Positive'Value
             (Get_Attribute(Elem => Saved_Node, Name => "mapdiscovered"));
         Game_Stats.Distance_Traveled :=
           Positive'Value
             (Get_Attribute(Elem => Saved_Node, Name => "distancetraveled"));
         Game_Stats.Accepted_Missions :=
           Natural'Value
             (Get_Attribute(Elem => Saved_Node, Name => "acceptedmissions"));
         Game_Stats.Points :=
           Positive'Value(Get_Attribute(Elem => Saved_Node, Name => "points"));
         Child_Nodes_List := Child_Nodes(N => Saved_Node);
         Load_Statistics_Loop :
         for I in 0 .. Length(List => Child_Nodes_List) - 1 loop
            Nodename :=
              To_Unbounded_String
                (Source =>
                   Node_Name(N => Item(List => Child_Nodes_List, Index => I)));
            if To_String(Source => Nodename) /= "#text" then
               Stat_Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Item(List => Child_Nodes_List, Index => I),
                         Name => "index"));
               Stat_Amount :=
                 Positive'Value
                   (Get_Attribute
                      (Elem => Item(List => Child_Nodes_List, Index => I),
                       Name => "amount"));
            end if;
            if To_String(Source => Nodename) = "destroyedships" then
               Game_Stats.Destroyed_Ships.Append
                 (New_Item => (Index => Stat_Index, Amount => Stat_Amount));
            elsif To_String(Source => Nodename) = "finishedcrafts" then
               Game_Stats.Crafting_Orders.Append
                 (New_Item => (Index => Stat_Index, Amount => Stat_Amount));
            elsif To_String(Source => Nodename) = "finishedmissions" then
               Game_Stats.Finished_Missions.Append
                 (New_Item => (Index => Stat_Index, Amount => Stat_Amount));
            elsif To_String(Source => Nodename) = "finishedgoals" then
               Game_Stats.Finished_Goals.Append
                 (New_Item => (Index => Stat_Index, Amount => Stat_Amount));
            elsif To_String(Source => Nodename) = "killedmobs" then
               Game_Stats.Killed_Mobs.Append
                 (New_Item => (Index => Stat_Index, Amount => Stat_Amount));
            end if;
         end loop Load_Statistics_Loop;
      end Load_Statistics_Block;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load current goal
      Log_Message
        (Message => "Loading current goal...", Message_Type => EVERYTHING,
         New_Line => False);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "currentgoal");
      Current_Goal.Index :=
        To_Unbounded_String
          (Source =>
             Get_Attribute
               (Elem => Item(List => Nodes_List, Index => 0),
                Name => "index"));
      Current_Goal.G_Type :=
        Goal_Types'Val
          (Integer'Value
             (Get_Attribute
                (Elem => Item(List => Nodes_List, Index => 0),
                 Name => "type")));
      Current_Goal.Amount :=
        Integer'Value
          (Get_Attribute
             (Elem => Item(List => Nodes_List, Index => 0), Name => "amount"));
      Current_Goal.Target_Index :=
        To_Unbounded_String
          (Source =>
             Get_Attribute
               (Elem => Item(List => Nodes_List, Index => 0),
                Name => "target"));
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      -- Load current story
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "Current_Story");
      if Length(List => Nodes_List) > 0 then
         Log_Message
           (Message => "Loading current story...", Message_Type => EVERYTHING,
            New_Line => False);
         Saved_Node := Item(List => Nodes_List, Index => 0);
         Current_Story.Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Saved_Node, Name => "index"));
         Current_Story.Step :=
           Positive'Value(Get_Attribute(Elem => Saved_Node, Name => "step"));
         if Get_Attribute(Elem => Saved_Node, Name => "currentstep") =
           "start" then
            Current_Story.Current_Step := 0;
         elsif Get_Attribute(Elem => Saved_Node, Name => "currentstep") =
           "finish" then
            Current_Story.Current_Step := -1;
         else
            Load_Story_Steps_Loop :
            for I in Stories_List(Current_Story.Index).Steps.Iterate loop
               if Stories_List(Current_Story.Index).Steps(I).Index =
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Saved_Node, Name => "currentstep")) then
                  Current_Story.Current_Step :=
                    Steps_Container.To_Index(Position => I);
                  exit Load_Story_Steps_Loop;
               end if;
            end loop Load_Story_Steps_Loop;
         end if;
         Current_Story.Max_Steps :=
           Positive'Value
             (Get_Attribute(Elem => Saved_Node, Name => "maxsteps"));
         Current_Story.Show_Text :=
           (if Get_Attribute(Elem => Saved_Node, Name => "showtext") = "Y" then
              True
            else False);
         if Get_Attribute(Elem => Saved_Node, Name => "data") /= "" then
            Current_Story.Data :=
              To_Unbounded_String
                (Source => Get_Attribute(Elem => Saved_Node, Name => "data"));
         end if;
         Current_Story.Finished_Step :=
           Step_Condition_Type'Val
             (Integer'Value
                (Get_Attribute(Elem => Saved_Node, Name => "finishedstep")));
         Log_Message
           (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
            Time_Stamp => False);
      end if;
      -- Load finished stories data
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "finishedstory");
      Load_Finished_Stories_Block :
      declare
         Steps_Amount: Positive;
         Temp_Texts: UnboundedString_Container.Vector;
         Story_Index: Unbounded_String;
      begin
         Log_Message
           (Message => "Loading finished stories...",
            Message_Type => EVERYTHING, New_Line => False);
         Load_Finished_Stories_Loop :
         for I in 0 .. Length(List => Nodes_List) - 1 loop
            Saved_Node := Item(List => Nodes_List, Index => I);
            Story_Index :=
              To_Unbounded_String
                (Source => Get_Attribute(Elem => Saved_Node, Name => "index"));
            Steps_Amount :=
              Positive'Value
                (Get_Attribute(Elem => Saved_Node, Name => "stepsamount"));
            Temp_Texts.Clear;
            Child_Nodes_List := Child_Nodes(N => Saved_Node);
            Load_Stories_Text_Loop :
            for J in 0 .. Length(List => Child_Nodes_List) - 1 loop
               Temp_Texts.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Source =>
                         Node_Value
                           (N =>
                              First_Child
                                (N =>
                                   Item
                                     (List => Child_Nodes_List,
                                      Index => J)))));
            end loop Load_Stories_Text_Loop;
            Finished_Stories.Append
              (New_Item =>
                 (Index => Story_Index, Steps_Amount => Steps_Amount,
                  Steps_Texts => Temp_Texts));
         end loop Load_Finished_Stories_Loop;
         Log_Message
           (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
            Time_Stamp => False);
      end Load_Finished_Stories_Block;
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "acceptedmission");
      Load_Accepted_Missions_Block :
      declare
         use Bases;

         M_Type: Missions_Types;
         Target_X, Target_Y, Start_Base: Natural;
         Time, Reward, M_Index: Positive;
         Finished: Boolean;
         Target: Natural;
         Index: Unbounded_String;
         Multiplier: Reward_Multiplier;
      begin
         Log_Message
           (Message => "Loading accepted missions...",
            Message_Type => EVERYTHING, New_Line => False);
         Load_Missions_Loop :
         for I in 0 .. Length(List => Nodes_List) - 1 loop
            Saved_Node := Item(List => Nodes_List, Index => I);
            M_Type :=
              Missions_Types'Val
                (Integer'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "type")));
            if M_Type in DELIVER | DESTROY then
               Index :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Saved_Node, Name => "target"));
            else
               Target :=
                 Integer'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "target"));
            end if;
            Time :=
              Positive'Value
                (Get_Attribute(Elem => Saved_Node, Name => "time"));
            Target_X :=
              Natural'Value
                (Get_Attribute(Elem => Saved_Node, Name => "targetx"));
            Target_Y :=
              Natural'Value
                (Get_Attribute(Elem => Saved_Node, Name => "targety"));
            Reward :=
              Positive'Value
                (Get_Attribute(Elem => Saved_Node, Name => "reward"));
            Start_Base :=
              Natural'Value
                (Get_Attribute(Elem => Saved_Node, Name => "startbase"));
            Multiplier :=
              (if Get_Attribute(Elem => Saved_Node, Name => "multiplier") /= ""
               then
                 Reward_Multiplier'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "multiplier"))
               else 1.0);
            Finished :=
              (if
                 Get_Attribute
                   (Elem => Item(List => Nodes_List, Index => I),
                    Name => "finished") =
                 "Y"
               then True
               else False);
            case M_Type is
               when DELIVER =>
                  Accepted_Missions.Append
                    (New_Item =>
                       (M_Type => DELIVER,
                        Item_Index =>
                          Tiny_String.To_Bounded_String
                            (Source => To_String(Source => Index)),
                        Time => Time, Target_X => Target_X,
                        Target_Y => Target_Y, Reward => Reward,
                        Start_Base => Start_Base, Finished => Finished,
                        Multiplier => Multiplier));
               when DESTROY =>
                  Accepted_Missions.Append
                    (New_Item =>
                       (M_Type => DESTROY, Ship_Index => Positive'Value(To_String(Source => Index)), Time => Time,
                        Target_X => Target_X, Target_Y => Target_Y,
                        Reward => Reward, Start_Base => Start_Base,
                        Finished => Finished, Multiplier => Multiplier));
               when PATROL =>
                  Accepted_Missions.Append
                    (New_Item =>
                       (M_Type => PATROL, Target => Target, Time => Time,
                        Target_X => Target_X, Target_Y => Target_Y,
                        Reward => Reward, Start_Base => Start_Base,
                        Finished => Finished, Multiplier => Multiplier));
               when EXPLORE =>
                  Accepted_Missions.Append
                    (New_Item =>
                       (M_Type => EXPLORE, Target => Target, Time => Time,
                        Target_X => Target_X, Target_Y => Target_Y,
                        Reward => Reward, Start_Base => Start_Base,
                        Finished => Finished, Multiplier => Multiplier));
               when PASSENGER =>
                  if Target > 91 then
                     Target := 91;
                  end if;
                  Accepted_Missions.Append
                    (New_Item =>
                       (M_Type => PASSENGER, Data => Target, Time => Time,
                        Target_X => Target_X, Target_Y => Target_Y,
                        Reward => Reward, Start_Base => Start_Base,
                        Finished => Finished, Multiplier => Multiplier));
            end case;
            M_Index := Accepted_Missions.Last_Index;
            if Finished then
               Sky_Map
                 (Sky_Bases(Accepted_Missions(M_Index).Start_Base).Sky_X,
                  Sky_Bases(Accepted_Missions(M_Index).Start_Base).Sky_Y)
                 .Mission_Index :=
                 M_Index;
            else
               Sky_Map
                 (Accepted_Missions(M_Index).Target_X,
                  Accepted_Missions(M_Index).Target_Y)
                 .Mission_Index :=
                 M_Index;
            end if;
         end loop Load_Missions_Loop;
      end Load_Accepted_Missions_Block;
      -- Load player career
      Log_Message
        (Message => "Loading player career...", Message_Type => EVERYTHING,
         New_Line => False);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "playercareer");
      if Length(List => Nodes_List) > 0 then
         Saved_Node := Item(List => Nodes_List, Index => 0);
         Player_Career :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Saved_Node, Name => "index"));
      else
         Player_Career :=
           Careers_Container.Key(Position => Careers_List.First);
      end if;
      Log_Message
        (Message => "done.", Message_Type => EVERYTHING, New_Line => True,
         Time_Stamp => False);
      Free(Read => Reader);
      Log_Message
        (Message => "Finished loading game.", Message_Type => EVERYTHING);
   exception
      when An_Exception : others =>
         Free(Read => Reader);
         Player_Ship.Crew.Clear;
         raise Save_Game_Invalid_Data
           with Exception_Message(X => An_Exception);
   end Load_Game;

   procedure Generate_Save_Name(Rename_Save: Boolean := False) is
      use Ada.Directories;
      use Tiny_String;
      use Utils;

      Old_Save_Name: constant String := To_String(Source => Save_Name);
   begin
      Generate_Save_Name_Loop :
      loop
         Save_Name :=
           Save_Directory & To_String(Source => Player_Ship.Crew(1).Name) &
           "_" & Tiny_String.To_String(Source => Player_Ship.Name) & "_" &
           Positive'Image(Get_Random(Min => 100, Max => 999))(2 .. 4) & ".sav";
         exit Generate_Save_Name_Loop when not Exists
             (Name => To_String(Source => Save_Name)) and
           Save_Name /= Old_Save_Name;
      end loop Generate_Save_Name_Loop;
      if Rename_Save then
         if Exists(Name => Old_Save_Name) then
            Rename
              (Old_Name => Old_Save_Name,
               New_Name => To_String(Source => Save_Name));
         end if;
      end if;
   end Generate_Save_Name;

end Game.SaveLoad;
