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

with Ada.Exceptions;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with Bases; use Bases;
with Bases.SaveLoad;
with Config;
with Crafts;
with Events;
with Log;
with Maps; use Maps;
with Messages;
with Ships; use Ships;
with Ships.SaveLoad;
with Stories; use Stories;

package body Game.SaveLoad is

   -- ****iv* GSaveLoad/GSaveLoad.Save_Version
   -- FUNCTION
   -- Current version of the save game
   -- SOURCE
   Save_Version: constant Positive := 5;
   -- ****

   procedure Save_Game(Pretty_Print: Boolean := False) is
      procedure Save_Ada_Game(P_Print: Integer) with
         Import => True,
         Convention => C,
         External_Name => "saveAdaGame";
   begin
      Set_Ship_In_Nim;
      Get_Bases_Loop :
      for I in Bases_Range loop
         Set_Base_In_Nim(Base_Index => I);
      end loop Get_Bases_Loop;
      Get_Map_Y_Loop :
      for Y in Map_Y_Range loop
         Get_Map_X_Loop :
         for X in Map_X_Range loop
            Get_Ada_Map_Cell
              (X => X, Y => Y, Base_Index => Sky_Map(X, Y).Base_Index,
               Visited => (if Sky_Map(X, Y).Visited then 1 else 0),
               Event_Index => Sky_Map(X, Y).Event_Index,
               Mission_Index => Sky_Map(X, Y).Mission_Index);
         end loop Get_Map_X_Loop;
      end loop Get_Map_Y_Loop;
      Get_Current_Story;
      Get_Ada_Game_String
        (Name => New_String(Str => "playerCareer"),
         Value => New_String(Str => To_String(Source => Player_Career)));
      Save_Ada_Game(P_Print => (if Pretty_Print then 1 else 0));
   end Save_Game;

   procedure Load_Game is
      use Ada.Exceptions;
      use DOM.Core;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use DOM.Readers;
      use Input_Sources.File;
      use Bases.SaveLoad;
      use Config;
      use Crafts;
      use Log;
      use Messages;
      use Ships.SaveLoad;
      use Tiny_String;

      Save_File: File_Input;
      --## rule off IMPROPER_INITIALIZATION
      Reader: Tree_Reader;
      --## rule on IMPROPER_INITIALIZATION
      Nodes_List: Node_List;
      Saved_Node: Node;
      Save_Data: Document;
      procedure Load_Ada_Game with
         Import => True,
         Convention => C,
         External_Name => "loadAdaGame";
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
         Set_Float_Setting
           (Name => "enemyDamageBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "enemydamagebonus")));
         Set_Float_Setting
           (Name => "playerDamageBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "playerdamagebonus")));
         Set_Float_Setting
           (Name => "enemyMeleeDamageBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "enemymeleedamagebonus")));
         Set_Float_Setting
           (Name => "playerMeleeDamageBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "playermeleedamagebonus")));
         Set_Float_Setting
           (Name => "experienceBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "experiencebonus")));
         Set_Float_Setting
           (Name => "reputationBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "reputationbonus")));
         Set_Float_Setting
           (Name => "upgradeCostBonus",
            Value =>
              Bonus_Type'Value
                (Get_Attribute
                   (Elem => Saved_Node, Name => "upgradecostbonus")));
         if Get_Attribute(Elem => Saved_Node, Name => "pricesbonus") /= "" then
            Set_Float_Setting
              (Name => "pricesBonus",
               Value =>
                 Bonus_Type'Value
                   (Get_Attribute(Elem => Saved_Node, Name => "pricesbonus")));
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
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "recipe");
      Load_Known_Recipes_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Add_Known_Recipe
           (Recipe_Index =>
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
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "event");
      Load_Events_Block :
      declare
         use Events;

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
            Get_Ada_Event
              (Index => Get_Events_Amount + 1,
               E_Type => Events_Types'Pos(E_Type), X => X, Y => Y,
               Time => Time,
               Data => Positive'Value(To_String(Source => Data)));
            Sky_Map(X, Y).Event_Index := I + 1;
         end loop Load_Events_Loop;
      end Load_Events_Block;
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
      Free(Read => Reader);
      Log_Message
        (Message => "Finished loading game.", Message_Type => EVERYTHING);
      Get_Ada_Save_Name
        (Name => New_String(Str => To_String(Source => Save_Name)));
      Load_Ada_Game;
      Player_Career := To_Unbounded_String(Source => Set_Game_String(Name => "playerCareer"));
   exception
      when An_Exception : others =>
         Free(Read => Reader);
         Player_Ship.Crew.Clear;
         raise Save_Game_Invalid_Data
           with Exception_Message(X => An_Exception);
   end Load_Game;

   procedure Generate_Save_Name(Rename_Save: Boolean := False) is
      New_Name: chars_ptr;
      procedure Generate_Ada_Save_Name(R_Save: Integer) with
         Import => True,
         Convention => C,
         External_Name => "generateAdaSaveName";
      procedure Set_Ada_Save_Name(Name: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaSaveName";
   begin
      Generate_Ada_Save_Name(R_Save => (if Rename_Save then 1 else 0));
      Set_Ada_Save_Name(Name => New_Name);
      Save_Name := To_Unbounded_String(Source => Value(Item => New_Name));
   end Generate_Save_Name;

end Game.SaveLoad;
