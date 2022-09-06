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
with Ada.Directories; use Ada.Directories;
with Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Config; use Config;
with CoreUI;

package body Themes is

   procedure Load_Themes is
      use Ada.Text_IO;

      Themes_Directories, Files: Search_Type;
      Found_Directory, Found_File: Directory_Entry_Type;
      Config_File: File_Type;
      Raw_Data, Field_Name, Value: Unbounded_String := Null_Unbounded_String;
      Equal_Index: Natural := 0;
      Temp_Record: Theme_Record := Default_Theme;
      function Convert_Path
        (Old_Value: Unbounded_String) return Unbounded_String is
         use Ada.Strings.Maps;
      begin
         if Dir_Separator = '/' then
            return Old_Value;
         end if;
         return
           Translate
             (Source => Old_Value,
              Mapping => To_Mapping(From => "\", To => "/"));
      end Convert_Path;
   begin
      Themes_Container.Include
        (Container => Themes_List, Key => "steamsky", New_Item => Temp_Record);
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
                  Temp_Record.Pilot_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "EngineerIcon") then
                  Temp_Record.Engineer_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "GunnerIcon") then
                  Temp_Record.Gunner_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CrewTraderIcon") then
                  Temp_Record.Crew_Trader_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RepairIcon") then
                  Temp_Record.Repair_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoRepairIcon") then
                  Temp_Record.No_Repair_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RepairOrderIcon") then
                  Temp_Record.Repair_Order_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "UpgradeIcon") then
                  Temp_Record.Upgrade_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoUpgradeIcon") then
                  Temp_Record.No_Upgrade_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CleanIcon") then
                  Temp_Record.Clean_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoCleanIcon") then
                  Temp_Record.No_Clean_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CleanOrderIcon") then
                  Temp_Record.Clean_Order_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ManufactureIcon") then
                  Temp_Record.Manufacture_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoManufactureIcon") then
                  Temp_Record.No_Manufacture_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpIcon") then
                  Temp_Record.Move_Map_Up_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownIcon") then
                  Temp_Record.Move_Map_Down_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapLeftIcon") then
                  Temp_Record.Move_Map_Left_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapRightIcon") then
                  Temp_Record.Move_Map_Right_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoFuelIcon") then
                  Temp_Record.No_Fuel_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowFuelIcon") then
                  Temp_Record.Low_Fuel_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoFoodIcon") then
                  Temp_Record.No_Food_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowFoodIcon") then
                  Temp_Record.Low_Food_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NoDrinksIcon") then
                  Temp_Record.No_Drinks_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "LowDrinksIcon") then
                  Temp_Record.Low_Drinks_Icon :=
                    Convert_Path(Old_Value => Value);
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
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowUpIcon") then
                  Temp_Record.Arrow_Up_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowDownIcon") then
                  Temp_Record.Arrow_Down_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowLeftIcon") then
                  Temp_Record.Arrow_Left_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowRightIcon") then
                  Temp_Record.Arrow_Right_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowUpLeftIcon") then
                  Temp_Record.Arrow_Up_Left_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowUpRightIcon") then
                  Temp_Record.Arrow_Up_Right_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowDownRightIcon") then
                  Temp_Record.Arrow_Down_Right_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ArrowDownLeftIcon") then
                  Temp_Record.Arrow_Down_Left_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitIcon") then
                  Temp_Record.Wait_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveStepIcon") then
                  Temp_Record.Move_Step_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveToIcon") then
                  Temp_Record.Move_To_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MemuIcon") then
                  Temp_Record.Menu_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ExitIcon") then
                  Temp_Record.Exit_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RandomIcon") then
                  Temp_Record.Random_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "MaleIcon") then
                  Temp_Record.Male_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "FemaleIcon") then
                  Temp_Record.Female_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "EditIcon") then
                  Temp_Record.Edit_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "ShowIcon") then
                  Temp_Record.Show_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CancelIcon") then
                  Temp_Record.Cancel_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "RemoveIcon") then
                  Temp_Record.Remove_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "HelpIcon") then
                  Temp_Record.Help_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "SpecialHelpColor") then
                  Temp_Record.Special_Help_Color := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "UnderlineHelpColor") then
                  Temp_Record.Underline_Help_Color := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "BoldHelpColor") then
                  Temp_Record.Bold_Help_Color := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "ItalicHelpColor") then
                  Temp_Record.Italic_Help_Color := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "GiveIcon") then
                  Temp_Record.Give_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "DropIcon") then
                  Temp_Record.Drop_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name = To_Unbounded_String(Source => "BuyIcon") then
                  Temp_Record.Buy_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "SellIcon") then
                  Temp_Record.Sell_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CraftIcon") then
                  Temp_Record.Craft_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "StudyIcon") then
                  Temp_Record.Study_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "DeconstructIcon") then
                  Temp_Record.Deconstruct_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "NegotiateIcon") then
                  Temp_Record.Negotiate_Icon :=
                    Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "CargoIcon") then
                  Temp_Record.Cargo_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "EquipIcon") then
                  Temp_Record.Equip_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "UnequipIcon") then
                  Temp_Record.Unequip_Icon := Convert_Path(Old_Value => Value);
               elsif Field_Name =
                 To_Unbounded_String(Source => "SelectAllIcon") then
                  Temp_Record.Select_All_Icon :=
                    Convert_Path(Old_Value => Value);
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
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use CoreUI;

      Label: Ttk_Label := Get_Widget(pathName => Game_Header & ".nofuel");
      Button: Ttk_Button :=
        Get_Widget(pathName => Main_Paned & ".mapframe.buttons.show");
   begin
      Load_Theme_Images;
      Label.Name := New_String(Str => Game_Header & ".nofuel");
      configure(Widgt => Label, options => "-image nofuelicon");
      Label.Name := New_String(Str => Game_Header & ".nofood");
      configure(Widgt => Label, options => "-image nofoodicon");
      Label.Name := New_String(Str => Game_Header & ".nodrink");
      configure(Widgt => Label, options => "-image nodrinksicon");
      Label.Name := New_String(Str => Game_Header & ".overloaded");
      configure(Widgt => Label, options => "-image overloadedicon");
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
      configure(Widgt => Button, options => "-image movemapupicon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.hide");
      configure(Widgt => Button, options => "-image movemapdownicon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.left");
      configure(Widgt => Button, options => "-image movemaplefticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.right");
      configure(Widgt => Button, options => "-image movemaprighticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.n");
      configure(Widgt => Button, options => "-image arrowupicon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.n");
      configure(Widgt => Button, options => "-image arrowupicon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.s");
      configure(Widgt => Button, options => "-image arrowdownicon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.s");
      configure(Widgt => Button, options => "-image arrowdownicon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.w");
      configure(Widgt => Button, options => "-image arrowlefticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.w");
      configure(Widgt => Button, options => "-image arrowlefticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.e");
      configure(Widgt => Button, options => "-image arrowrighticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.e");
      configure(Widgt => Button, options => "-image arrowrighticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.nw");
      configure(Widgt => Button, options => "-image arrowuplefticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.nw");
      configure(Widgt => Button, options => "-image arrowuplefticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.ne");
      configure(Widgt => Button, options => "-image arrowuprighticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.ne");
      configure(Widgt => Button, options => "-image arrowuprighticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.se");
      configure(Widgt => Button, options => "-image arrowdownrighticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.se");
      configure(Widgt => Button, options => "-image arrowdownrighticon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.sw");
      configure(Widgt => Button, options => "-image arrowdownlefticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.sw");
      configure(Widgt => Button, options => "-image arrowdownlefticon");
      Button.Name := New_String(Str => Main_Paned & ".controls.buttons.wait");
      configure(Widgt => Button, options => "-image waiticon");
      Button.Name :=
        New_String(Str => Main_Paned & ".controls.buttons.box.moveto");
      configure(Widgt => Button, options => "-image movetoicon");
      Button.Name := New_String(Str => Main_Paned & ".mapframe.buttons.wait");
      configure(Widgt => Button, options => "-image menuicon");
      Button.Name := New_String(Str => Game_Header & ".menubutton");
      configure(Widgt => Button, options => "-image menuicon");
      Button.Name := New_String(Str => Game_Header & ".closebutton");
      configure(Widgt => Button, options => "-image exiticon");
   end Set_Theme;

   procedure Load_Theme_Images is
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Image.Photo;
      use Tcl.Tk.Ada.TtkStyle;

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
         20 => To_Unbounded_String(Source => "lowdrinksicon"),
         21 => To_Unbounded_String(Source => "movemapupicon"),
         22 => To_Unbounded_String(Source => "movemapdownicon"),
         23 => To_Unbounded_String(Source => "movemaplefticon"),
         24 => To_Unbounded_String(Source => "movemaprighticon"),
         25 => To_Unbounded_String(Source => "overloadedicon"),
         26 => To_Unbounded_String(Source => "arrowupicon"),
         27 => To_Unbounded_String(Source => "arrowdownicon"),
         28 => To_Unbounded_String(Source => "arrowlefticon"),
         29 => To_Unbounded_String(Source => "arrowrighticon"),
         30 => To_Unbounded_String(Source => "arrowuplefticon"),
         31 => To_Unbounded_String(Source => "arrowuprighticon"),
         32 => To_Unbounded_String(Source => "arrowdownrighticon"),
         33 => To_Unbounded_String(Source => "arrowdownlefticon"),
         34 => To_Unbounded_String(Source => "waiticon"),
         35 => To_Unbounded_String(Source => "movestepicon"),
         36 => To_Unbounded_String(Source => "movetoicon"),
         37 => To_Unbounded_String(Source => "menuicon"),
         38 => To_Unbounded_String(Source => "exiticon"),
         39 => To_Unbounded_String(Source => "randomicon"),
         40 => To_Unbounded_String(Source => "maleicon"),
         41 => To_Unbounded_String(Source => "femaleicon"),
         42 => To_Unbounded_String(Source => "editicon"),
         43 => To_Unbounded_String(Source => "showicon"),
         44 => To_Unbounded_String(Source => "cancelicon"),
         45 => To_Unbounded_String(Source => "removeicon"),
         46 => To_Unbounded_String(Source => "helpicon"),
         47 => To_Unbounded_String(Source => "giveicon"),
         48 => To_Unbounded_String(Source => "dropicon"),
         49 => To_Unbounded_String(Source => "buyicon"),
         50 => To_Unbounded_String(Source => "sellicon"),
         51 => To_Unbounded_String(Source => "crafticon"),
         52 => To_Unbounded_String(Source => "studyicon"),
         53 => To_Unbounded_String(Source => "deconstructicon"),
         54 => To_Unbounded_String(Source => "negotiateicon"),
         55 => To_Unbounded_String(Source => "cargoicon"),
         56 => To_Unbounded_String(Source => "equipicon"),
         57 => To_Unbounded_String(Source => "unequipicon"),
         58 => To_Unbounded_String(Source => "selectall"));
      Tmp_Image: Tk_Photo; --## rule line off IMPROPER_INITIALIZATION
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
         19 => Theme.No_Drinks_Icon, 20 => Theme.Low_Drinks_Icon,
         21 => Theme.Move_Map_Up_Icon, 22 => Theme.Move_Map_Down_Icon,
         23 => Theme.Move_Map_Left_Icon, 24 => Theme.Move_Map_Right_Icon,
         25 => Theme.Overloaded_Icon, 26 => Theme.Arrow_Up_Icon,
         27 => Theme.Arrow_Down_Icon, 28 => Theme.Arrow_Left_Icon,
         29 => Theme.Arrow_Right_Icon, 30 => Theme.Arrow_Up_Left_Icon,
         31 => Theme.Arrow_Up_Right_Icon, 32 => Theme.Arrow_Down_Right_Icon,
         33 => Theme.Arrow_Down_Left_Icon, 34 => Theme.Wait_Icon,
         35 => Theme.Move_Step_Icon, 36 => Theme.Move_To_Icon,
         37 => Theme.Menu_Icon, 38 => Theme.Exit_Icon, 39 => Theme.Random_Icon,
         40 => Theme.Male_Icon, 41 => Theme.Female_Icon, 42 => Theme.Edit_Icon,
         43 => Theme.Show_Icon, 44 => Theme.Cancel_Icon,
         45 => Theme.Remove_Icon, 46 => Theme.Help_Icon, 47 => Theme.Give_Icon,
         48 => Theme.Drop_Icon, 49 => Theme.Buy_Icon, 50 => Theme.Sell_Icon,
         51 => Theme.Craft_Icon, 52 => Theme.Study_Icon,
         53 => Theme.Deconstruct_Icon, 54 => Theme.Negotiate_Icon,
         55 => Theme.Cargo_Icon, 56 => Theme.Equip_Icon,
         57 => Theme.Unequip_Icon, 58 => Theme.Select_All_Icon);
   begin
      Load_Images_Loop :
      for I in Images_Names'Range loop
         Tmp_Image :=
           Create
             (pathName => To_String(Source => Images_Names(I)),
              options =>
                "-file {" & To_String(Source => Images_Files(I)) &
                "} -format {svg -scaletoheight" &
                Positive'Image(Game_Settings.Interface_Font_Size + 8) & "}");
      end loop Load_Images_Loop;
      Tcl_Eval
        (interp => Get_Context,
         strng =>
           "ttk::theme::" & Theme_Use & "::LoadImages " &
           Containing_Directory(Name => To_String(Source => Theme.File_Name)) &
           Positive'Image(Game_Settings.Interface_Font_Size + 8));
   end Load_Theme_Images;

end Themes;
