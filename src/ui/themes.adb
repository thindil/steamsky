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
with Game; use Game;

package body Themes is

   procedure Load_Themes is
      Directories, Files: Search_Type;
      FoundDirectory, FoundFile: Directory_Entry_Type;
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      TempRecord: Theme_Record;
      procedure SetDefaultValues is
      begin
         TempRecord :=
           (Name => Null_Unbounded_String, File_Name => Null_Unbounded_String,
            Enemy_Ship_Icon => Wide_Character'Val(16#f51c#),
            Attack_On_Base_Icon => Wide_Character'Val(16#f543#),
            Disease_Icon => Wide_Character'Val(16#f5a6#),
            Double_Price_Icon => Wide_Character'Val(16#f0d6#),
            Full_Docks_Icon => Wide_Character'Val(16#f057#),
            Enemy_Patrol_Icon => Wide_Character'Val(16#f51b#),
            Trader_Icon => Wide_Character'Val(16#f197#),
            Friendly_Ship_Icon => Wide_Character'Val(16#f197#),
            Deliver_Icon => Wide_Character'Val(16#f53b#),
            Destroy_Icon => Wide_Character'Val(16#fc6a#),
            Patrol_Icon => Wide_Character'Val(16#f540#),
            Explore_Icon => Wide_Character'Val(16#f707#),
            Passenger_Icon => Wide_Character'Val(16#f183#),
            Pilot_Icon => Wide_Character'Val(16#f655#),
            Engineer_Icon => Wide_Character'Val(16#f013#),
            Gunner_Icon => Wide_Character'Val(16#f4fb#),
            Crew_Trader_Icon => Wide_Character'Val(16#f651#),
            Repair_Icon => Wide_Character'Val(16#f54a#),
            Upgrade_Icon => Wide_Character'Val(16#f6e3#),
            Clean_Icon => Wide_Character'Val(16#f458#),
            Manufacture_Icon => Wide_Character'Val(16#f0e3#),
            Move_Map_Up_Icon => Wide_Character'Val(16#2191#),
            Move_Map_Down_Icon => Wide_Character'Val(16#2193#),
            Move_Map_Left_Icon => Wide_Character'Val(16#2190#),
            Move_Map_Right_Icon => Wide_Character'Val(16#2192#),
            No_Fuel_Icon => Wide_Character'Val(16#f2ca#),
            No_Food_Icon => Wide_Character'Val(16#f787#),
            No_Drinks_Icon => Wide_Character'Val(16#f72f#),
            Not_Visited_Base_Icon => Wide_Character'Val(16#229b#),
            Player_Ship_Icon => Wide_Character'Val(16#f135#),
            Empty_Map_Icon => Wide_Character'Val(16#f0c8#),
            Target_Icon => Wide_Character'Val(16#f05b#),
            Story_Icon => Wide_Character'Val(16#f059#),
            Overloaded_Icon => Wide_Character'Val(16#f55b#));
      end SetDefaultValues;
   begin
      SetDefaultValues;
      TempRecord.Name := To_Unbounded_String("Default theme");
      TempRecord.File_Name :=
        Data_Directory &
        To_Unbounded_String("ui" & Dir_Separator & "theme.tcl");
      Themes_Container.Include(Themes_List, "steamsky", TempRecord);
      SetDefaultValues;
      Start_Search
        (Directories, To_String(Themes_Directory), "",
         (Directory => True, others => False));
      Load_Themes_Loop :
      while More_Entries(Directories) loop
         Get_Next_Entry(Directories, FoundDirectory);
         if Simple_Name(FoundDirectory) in "." | ".." then
            goto End_Of_Load_Themes_Loop;
         end if;
         Start_Search(Files, Full_Name(FoundDirectory), "*.cfg");
         Load_Config_Loop :
         while More_Entries(Files) loop
            Get_Next_Entry(Files, FoundFile);
            Open(ConfigFile, In_File, Full_Name(FoundFile));
            while not End_Of_File(ConfigFile) loop
               RawData := To_Unbounded_String(Get_Line(ConfigFile));
               if Length(RawData) = 0 then
                  goto End_Of_Load_Config_Loop;
               end if;
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Name") then
                  TempRecord.Name := Value;
               elsif FieldName = To_Unbounded_String("FileName") then
                  TempRecord.File_Name :=
                    To_Unbounded_String
                      (Full_Name(FoundDirectory) & Dir_Separator) &
                    Value;
               elsif FieldName = To_Unbounded_String("EnemyShipIcon") then
                  TempRecord.Enemy_Ship_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("AttackOnBaseIcon") then
                  TempRecord.Attack_On_Base_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DiseaseIcon") then
                  TempRecord.Disease_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DoublePriceIcon") then
                  TempRecord.Double_Price_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("FullDocksIcon") then
                  TempRecord.Full_Docks_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EnemyPatrolIcon") then
                  TempRecord.Enemy_Patrol_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("TraderIcon") then
                  TempRecord.Trader_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("FriendlyShipIcon") then
                  TempRecord.Friendly_Ship_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DeliverIcon") then
                  TempRecord.Deliver_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DestroyIcon") then
                  TempRecord.Destroy_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PatrolIcon") then
                  TempRecord.Patrol_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("ExploreIcon") then
                  TempRecord.Explore_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PassengerIcon") then
                  TempRecord.Passenger_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PilotIcon") then
                  TempRecord.Pilot_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EngineerIcon") then
                  TempRecord.Engineer_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("GunnerIcon") then
                  TempRecord.Gunner_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("CrewTraderIcon") then
                  TempRecord.Crew_Trader_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("RepairIcon") then
                  TempRecord.Repair_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("UpgradeIcon") then
                  TempRecord.Upgrade_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("CleanIcon") then
                  TempRecord.Clean_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("ManufactureIcon") then
                  TempRecord.Manufacture_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapUpIcon") then
                  TempRecord.Move_Map_Up_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapDownIcon") then
                  TempRecord.Move_Map_Down_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapLeftIcon") then
                  TempRecord.Move_Map_Left_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapRightIcon") then
                  TempRecord.Move_Map_Right_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoFuelIcon") then
                  TempRecord.No_Fuel_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoFoodIcon") then
                  TempRecord.No_Food_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoDrinksIcon") then
                  TempRecord.No_Drinks_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NotVisitedBaseIcon") then
                  TempRecord.Not_Visited_Base_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EmptyMapIcon") then
                  TempRecord.Empty_Map_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("TargetIcon") then
                  TempRecord.Target_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("StoryIcon") then
                  TempRecord.Story_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("OverloadedIcon") then
                  TempRecord.Overloaded_Icon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               end if;
               <<End_Of_Load_Config_Loop>>
            end loop;
            Close(ConfigFile);
            Themes_Container.Include
              (Themes_List, Simple_Name(FoundDirectory), TempRecord);
            SetDefaultValues;
         end loop Load_Config_Loop;
         End_Search(Files);
         <<End_Of_Load_Themes_Loop>>
      end loop Load_Themes_Loop;
      End_Search(Directories);
      if not Themes_List.Contains
          (To_String(Game_Settings.Interface_Theme)) then
         Game_Settings.Interface_Theme := To_Unbounded_String("steamsky");
      end if;
   end Load_Themes;

   procedure Set_Theme is
      Label: Ttk_Label;
      Button: Ttk_Button;
   begin
      Label.Interp := Get_Context;
      Button.Interp := Get_Context;
      Set_Theme_Loop :
      for I in Themes_List.Iterate loop
         if Themes_Container.Key(I) /= Game_Settings.Interface_Theme then
            goto End_Of_Set_Theme_Loop;
         end if;
         Label.Name := New_String(".gameframe.header.nofuel");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).No_Fuel_Icon) & "}");
         Label.Name := New_String(".gameframe.header.nofood");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).No_Food_Icon) & "}");
         Label.Name := New_String(".gameframe.header.nodrink");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).No_Drinks_Icon) & "}");
         Label.Name := New_String(".gameframe.header.overloaded");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).Overloaded_Icon) & "}");
         Label.Name := New_String(".gameframe.header.pilot");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Pilot_Icon) & "}");
         Label.Name := New_String(".gameframe.header.engineer");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Engineer_Icon) & "}");
         Label.Name := New_String(".gameframe.header.gunner");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Gunner_Icon) & "}");
         Label.Name := New_String(".gameframe.header.talk");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).Crew_Trader_Icon) & "}");
         Label.Name := New_String(".gameframe.header.repairs");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Repair_Icon) & "}");
         Label.Name := New_String(".gameframe.header.upgrade");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Upgrade_Icon) & "}");
         Label.Name := New_String(".gameframe.header.clean");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).Clean_Icon) & "}");
         Label.Name := New_String(".gameframe.header.crafting");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).Manufacture_Icon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.show");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).Move_Map_Up_Icon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.hide");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).Move_Map_Down_Icon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.left");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).Move_Map_Left_Icon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.right");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).Move_Map_Right_Icon) & "}");
         <<End_Of_Set_Theme_Loop>>
      end loop Set_Theme_Loop;
   end Set_Theme;

end Themes;
