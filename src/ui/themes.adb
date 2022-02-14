-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Config; use Config;
with CoreUI;

package body Themes is

   procedure Load_Themes is
      use Ada.Text_IO;
      use Ada.Directories;

      Themes_Directories, Files: Search_Type;
      Found_Directory, Found_File: Directory_Entry_Type;
      Config_File: File_Type;
      Raw_Data, Field_Name, Value: Unbounded_String := Null_Unbounded_String;
      Equal_Index: Natural := 0;
      Temp_Record: Theme_Record := Default_Theme;
      function Convert_Path(Value: Unbounded_String) return Unbounded_String is
         use Ada.Strings.Maps;
      begin
         if Dir_Separator = '/' then
            return Value;
         end if;
         return
           Translate
             (Source => Value, Mapping => To_Mapping(From => "\", To => "/"));
      end Convert_Path;
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
                  Temp_Record.Pilot_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "EngineerIcon") then
                  Temp_Record.Engineer_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "GunnerIcon") then
                  Temp_Record.Gunner_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CrewTraderIcon") then
                  Temp_Record.Crew_Trader_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RepairIcon") then
                  Temp_Record.Repair_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoRepairIcon") then
                  Temp_Record.No_Repair_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RepairOrderIcon") then
                  Temp_Record.Repair_Order_Icon :=
                    Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "UpgradeIcon") then
                  Temp_Record.Upgrade_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoUpgradeIcon") then
                  Temp_Record.No_Upgrade_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CleanIcon") then
                  Temp_Record.Clean_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoCleanIcon") then
                  Temp_Record.No_Clean_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CleanOrderIcon") then
                  Temp_Record.Clean_Order_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ManufactureIcon") then
                  Temp_Record.Manufacture_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoManufactureIcon") then
                  Temp_Record.No_Manufacture_Icon :=
                    Convert_Path(Value => Value);
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
                  Temp_Record.No_Fuel_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowFuelIcon") then
                  Temp_Record.Low_Fuel_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoFoodIcon") then
                  Temp_Record.No_Food_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowFoodIcon") then
                  Temp_Record.Low_Food_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoDrinksIcon") then
                  Temp_Record.No_Drinks_Icon := Convert_Path(Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowDrinksIcon") then
                  Temp_Record.Low_Drinks_Icon := Convert_Path(Value => Value);
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
      use Ada.Strings.UTF_Encoding.Wide_Strings;
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use CoreUI;

      Label: Ttk_Label := Get_Widget(pathName => Game_Header & ".nofuel");
      Button: Ttk_Button :=
        Get_Widget(pathName => Main_Paned & ".mapframe.buttons.show");
      Theme: constant Theme_Record :=
        Themes_List(To_String(Source => Game_Settings.Interface_Theme));
   begin
      Load_Theme_Images;
      Label.Name := New_String(Str => Game_Header & ".nofuel");
      configure(Widgt => Label, options => "-image nofuelicon");
      Label.Name := New_String(Str => Game_Header & ".nofood");
      configure(Widgt => Label, options => "-image nofoodicon");
      Label.Name := New_String(Str => Game_Header & ".nodrink");
      configure(Widgt => Label, options => "-image nodrinksicon");
      Label.Name := New_String(Str => Game_Header & ".overloaded");
      configure
        (Widgt => Label,
         options =>
           "-text {" & Encode(Item => "" & Theme.Overloaded_Icon) & "}");
      Label.Name := New_String(Str => Game_Header & ".pilot");
      configure(Widgt => Label, options => "-image piloticon");
      Label.Name := New_String(Str => Game_Header & ".engineer");
      configure(Widgt => Label, options => "-image engineericon");
      Label.Name := New_String(Str => Game_Header & ".gunner");
      configure(Widgt => Label, options => "-image gunnericon");
      Label.Name := New_String(Str => Game_Header & ".talk");
      configure(Widgt => Label, options => "-image crewtradericon");
      Label.Name := New_String(Str => Game_Header & ".repairs");
      configure(Widgt => Label, options => "-image repairicon");
      Label.Name := New_String(Str => Game_Header & ".upgrade");
      configure(Widgt => Label, options => "-image upgradeicon");
      Label.Name := New_String(Str => Game_Header & ".clean");
      configure(Widgt => Label, options => "-image cleanicon");
      Label.Name := New_String(Str => Game_Header & ".crafting");
      configure(Widgt => Label, options => "-image crafticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.show");
      configure
        (Widgt => Button,
         options =>
           "-text {" & Encode(Item => "" & Theme.Move_Map_Up_Icon) & "}");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.hide");
      configure
        (Widgt => Button,
         options =>
           "-text {" & Encode(Item => "" & Theme.Move_Map_Down_Icon) & "}");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.left");
      configure
        (Widgt => Button,
         options =>
           "-text {" & Encode(Item => "" & Theme.Move_Map_Left_Icon) & "}");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.right");
      configure
        (Widgt => Button,
         options =>
           "-text {" & Encode(Item => "" & Theme.Move_Map_Right_Icon) & "}");
   end Set_Theme;

   procedure Load_Theme_Images is
      use Tcl.Tk.Ada.Image.Photo;

      Images_Names: constant array(Positive range <>) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "piloticon"),
         2 => To_Unbounded_String(Source => "engineericon"),
         3 => To_Unbounded_String(Source => "gunnericon"),
         4 => To_Unbounded_String(Source => "crewtradericon"),
         5 => To_Unbounded_String(Source => "repairicon"),
         6 => To_Unbounded_String(Source => "norepairicon"),
         7 => To_Unbounded_String(Source => "repairordericon"),
         8 => To_Unbounded_String(Source => "upgradeicon"),
         9 => To_Unbounded_String(Source => "noupgradeicon"),
         10 => To_Unbounded_String(Source => "cleanicon"),
         11 => To_Unbounded_String(Source => "nocleanicon"),
         12 => To_Unbounded_String(Source => "cleanordericon"),
         13 => To_Unbounded_String(Source => "crafticon"),
         14 => To_Unbounded_String(Source => "nocrafticon"),
         15 => To_Unbounded_String(Source => "nofuelicon"),
         16 => To_Unbounded_String(Source => "nofoodicon"),
         17 => To_Unbounded_String(Source => "lowfuelicon"),
         18 => To_Unbounded_String(Source => "lowfoodicon"),
         19 => To_Unbounded_String(Source => "nodrinksicon"),
         20 => To_Unbounded_String(Source => "lowdrinksicon"));
      Tmp_Image: Tk_Photo;
      pragma Unreferenced(Tmp_Image);
      Theme: constant Theme_Record :=
        Themes_List(To_String(Source => Game_Settings.Interface_Theme));
      Images_Files: constant array(Positive range <>) of Unbounded_String :=
        (1 => Theme.Pilot_Icon, 2 => Theme.Engineer_Icon,
         3 => Theme.Gunner_Icon, 4 => Theme.Crew_Trader_Icon,
         5 => Theme.Repair_Icon, 6 => Theme.No_Repair_Icon,
         7 => Theme.Repair_Order_Icon, 8 => Theme.Upgrade_Icon,
         9 => Theme.No_Upgrade_Icon, 10 => Theme.Clean_Icon,
         11 => Theme.No_Clean_Icon, 12 => Theme.Clean_Order_Icon,
         13 => Theme.Manufacture_Icon, 14 => Theme.No_Manufacture_Icon,
         15 => Theme.No_Fuel_Icon, 16 => Theme.No_Food_Icon,
         17 => Theme.Low_Fuel_Icon, 18 => Theme.Low_Food_Icon,
         19 => Theme.No_Drinks_Icon, 20 => Theme.Low_Drinks_Icon);
   begin
      Load_Images_Loop :
      for I in Images_Names'Range loop
         Tmp_Image :=
           Create
             (pathName => To_String(Source => Images_Names(I)),
              options =>
                "-file {" & To_String(Source => Images_Files(I)) &
                "} -format {svg -scaletoheight" &
                Positive'Image(Game_Settings.Interface_Font_Size + 7) & "}");
      end loop Load_Images_Loop;
   end Load_Theme_Images;

end Themes;
