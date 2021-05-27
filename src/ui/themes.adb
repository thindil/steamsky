-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Config; use Config;
with CoreUI; use CoreUI;
with Game; use Game;

package body Themes is

   procedure Load_Themes is
      Themes_Directories, Files: Search_Type;
      Found_Directory, Found_File: Directory_Entry_Type;
      Config_File: File_Type;
      Raw_Data, Field_Name, Value: Unbounded_String := Null_Unbounded_String;
      Equal_Index: Natural := 0;
      Temp_Record: Theme_Record := Default_Theme;
   begin
      Temp_Record.Name := To_Unbounded_String(Source => "Default theme");
      Temp_Record.File_Name :=
        Data_Directory &
        To_Unbounded_String(Source => "ui" & Dir_Separator & "theme.tcl");
      Themes_Container.Include
        (Container => Themes_List, Key => "steamsky", New_Item => Temp_Record);
      Temp_Record := Default_Theme;
      Start_Search
        (Search => Themes_Directories,
         Directory => To_String(Source => Themes_Directory), Pattern => "",
         Filter => (Directory => True, others => False));
      Load_Themes_Loop :
      while More_Entries(Search => Themes_Directories) loop
         Get_Next_Entry
           (Search => Themes_Directories, Directory_Entry => Found_Directory);
         if Simple_Name(Directory_Entry => Found_Directory) in "." | ".." then
            goto End_Of_Load_Themes_Loop;
         end if;
         Start_Search
           (Search => Files,
            Directory => Full_Name(Directory_Entry => Found_Directory),
            Pattern => "*.cfg");
         Load_Config_Loop :
         while More_Entries(Search => Files) loop
            Get_Next_Entry(Search => Files, Directory_Entry => Found_File);
            Open
              (File => Config_File, Mode => In_File,
               Name => Full_Name(Directory_Entry => Found_File));
            Load_Config_Data_Loop :
            while not End_Of_File(File => Config_File) loop
               Raw_Data :=
                 To_Unbounded_String(Source => Get_Line(File => Config_File));
               if Length(Source => Raw_Data) = 0 then
                  goto End_Of_Load_Config_Loop;
               end if;
               Equal_Index := Index(Source => Raw_Data, Pattern => "=");
               Field_Name :=
                 Head(Source => Raw_Data, Count => Equal_Index - 2);
               Value :=
                 Tail
                   (Source => Raw_Data,
                    Count => Length(Source => Raw_Data) - Equal_Index - 1);
               if Field_Name = To_Unbounded_String(Source => "Name") then
                  Temp_Record.Name := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "FileName") then
                  Temp_Record.File_Name :=
                    To_Unbounded_String
                      (Source =>
                         Full_Name(Directory_Entry => Found_Directory) &
                         Dir_Separator) &
                    Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "EnemyShipIcon") then
                  Temp_Record.Enemy_Ship_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "AttackOnBaseIcon") then
                  Temp_Record.Attack_On_Base_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "DiseaseIcon") then
                  Temp_Record.Disease_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "DoublePriceIcon") then
                  Temp_Record.Double_Price_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullDocksIcon") then
                  Temp_Record.Full_Docks_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "EnemyPatrolIcon") then
                  Temp_Record.Enemy_Patrol_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "TraderIcon") then
                  Temp_Record.Trader_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FriendlyShipIcon") then
                  Temp_Record.Friendly_Ship_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "DeliverIcon") then
                  Temp_Record.Deliver_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "DestroyIcon") then
                  Temp_Record.Destroy_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "PatrolIcon") then
                  Temp_Record.Patrol_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ExploreIcon") then
                  Temp_Record.Explore_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "PassengerIcon") then
                  Temp_Record.Passenger_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "PilotIcon") then
                  Temp_Record.Pilot_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "EngineerIcon") then
                  Temp_Record.Engineer_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GunnerIcon") then
                  Temp_Record.Gunner_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CrewTraderIcon") then
                  Temp_Record.Crew_Trader_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "RepairIcon") then
                  Temp_Record.Repair_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "UpgradeIcon") then
                  Temp_Record.Upgrade_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CleanIcon") then
                  Temp_Record.Clean_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ManufactureIcon") then
                  Temp_Record.Manufacture_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpIcon") then
                  Temp_Record.Move_Map_Up_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownIcon") then
                  Temp_Record.Move_Map_Down_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapLeftIcon") then
                  Temp_Record.Move_Map_Left_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapRightIcon") then
                  Temp_Record.Move_Map_Right_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoFuelIcon") then
                  Temp_Record.No_Fuel_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoFoodIcon") then
                  Temp_Record.No_Food_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoDrinksIcon") then
                  Temp_Record.No_Drinks_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "NotVisitedBaseIcon") then
                  Temp_Record.Not_Visited_Base_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "EmptyMapIcon") then
                  Temp_Record.Empty_Map_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "TargetIcon") then
                  Temp_Record.Target_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "StoryIcon") then
                  Temp_Record.Story_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               elsif Field_Name =
                 To_Unbounded_String(Source => "OverloadedIcon") then
                  Temp_Record.Overloaded_Icon :=
                    Wide_Character'Val
                      (Natural'Value
                         ("16#" & To_String(Source => Value) & "#"));
               end if;
               <<End_Of_Load_Config_Loop>>
            end loop Load_Config_Data_Loop;
            Close(File => Config_File);
            Themes_Container.Include
              (Container => Themes_List,
               Key => Simple_Name(Directory_Entry => Found_Directory),
               New_Item => Temp_Record);
            Temp_Record := Default_Theme;
         end loop Load_Config_Loop;
         End_Search(Search => Files);
         <<End_Of_Load_Themes_Loop>>
      end loop Load_Themes_Loop;
      End_Search(Search => Themes_Directories);
      if not Themes_List.Contains
          (Key => To_String(Source => Game_Settings.Interface_Theme)) then
         Game_Settings.Interface_Theme :=
           To_Unbounded_String(Source => "steamsky");
      end if;
   end Load_Themes;

   procedure Set_Theme is
      Label: Ttk_Label := Get_Widget(pathName => Game_Header & ".nofuel");
      Button: Ttk_Button :=
        Get_Widget(pathName => Main_Paned & ".mapframe.buttons.show");
   begin
      Set_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_Container.Key(Position => I) /=
           Game_Settings.Interface_Theme then
            goto End_Of_Set_Theme_Loop;
         end if;
         Label.Name := New_String(Str => ".gameframe.header.nofuel");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).No_Fuel_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.nofood");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).No_Food_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.nodrink");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).No_Drinks_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.overloaded");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Overloaded_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.pilot");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Pilot_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.engineer");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Engineer_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.gunner");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Gunner_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.talk");
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Encode(Item => "" & Themes_List(I).Crew_Trader_Icon) & "}");
         Label.Name := New_String(Str => ".gameframe.header.repairs");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Repair_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.upgrade");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Upgrade_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.clean");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Encode(Item => "" & Themes_List(I).Clean_Icon) &
              "}");
         Label.Name := New_String(Str => ".gameframe.header.crafting");
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Encode(Item => "" & Themes_List(I).Manufacture_Icon) & "}");
         Button.Name :=
           New_String(Str => ".gameframe.paned.mapframe.buttons.show");
         configure
           (Widgt => Button,
            options =>
              "-text {" &
              Encode(Item => "" & Themes_List(I).Move_Map_Up_Icon) & "}");
         Button.Name :=
           New_String(Str => ".gameframe.paned.mapframe.buttons.hide");
         configure
           (Widgt => Button,
            options =>
              "-text {" &
              Encode(Item => "" & Themes_List(I).Move_Map_Down_Icon) & "}");
         Button.Name :=
           New_String(Str => ".gameframe.paned.mapframe.buttons.left");
         configure
           (Widgt => Button,
            options =>
              "-text {" &
              Encode(Item => "" & Themes_List(I).Move_Map_Left_Icon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.right");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).Move_Map_Right_Icon) & "}");
         <<End_Of_Set_Theme_Loop>>
      end loop Set_Theme_Loop;
   end Set_Theme;

end Themes;
