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

with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Utils.UI; use Utils.UI;

package body DebugUI is

   -- ****o* DebugUI/DebugUI.Refresh_Module_Command
   -- FUNCTION
   -- Refresh the information about selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshModule
   -- SOURCE
   function Refresh_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.ship";
      Proto_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".proto", Interp => Interp);
      Module_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".module", Interp => Interp);
      Module_Index: Positive;
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".weight", Interp => Interp);
   begin
      Module_Index := Natural'Value(Current(ComboBox => Module_Combo)) + 1;
      Set
        (ComboBox => Proto_Combo,
         Value =>
           "{" &
           To_String
             (Source =>
                BaseModules_Container.Element
                  (Container => Modules_List,
                   Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Name) &
           "}");
      Set
        (SpinBox => Spin_Box,
         Value => Positive'Image(Player_Ship.Modules(Module_Index).Weight));
      Spin_Box.Name := New_String(Str => Frame_Name & ".dur");
      Set
        (SpinBox => Spin_Box,
         Value => Integer'Image(Player_Ship.Modules(Module_Index).Durability));
      Spin_Box.Name := New_String(Str => Frame_Name & ".maxdur");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Positive'Image(Player_Ship.Modules(Module_Index).Max_Durability));
      Spin_Box.Name := New_String(Str => Frame_Name & ".upgrade");
      Set
        (SpinBox => Spin_Box,
         Value =>
           Natural'Image(Player_Ship.Modules(Module_Index).Upgrade_Progress));
      return TCL_OK;
   end Refresh_Module_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Member_Command
   -- FUNCTION
   -- Refresh the information about selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshMember
   -- SOURCE
   function Refresh_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.crew";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".member", Interp => Interp);
      Spin_Box: Ttk_SpinBox :=
        Get_Widget
          (pathName => Frame_Name & ".stats2.health", Interp => Interp);
      Member_Frame: Ttk_Frame :=
        Get_Widget(pathName => Frame_Name & ".stats", Interp => Interp);
      Rows: Natural := 0;
      Tokens: Slice_Set;
      Label: Ttk_Label;
      Member: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Skills_Indexes: Positive_Container.Vector;
      Skills_List_Values: Unbounded_String;
   begin
      Member :=
        Player_Ship.Crew(Natural'Value(Current(ComboBox => Combo_Box)) + 1);
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Health));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.thirst");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Thirst));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.hunger");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Hunger));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.tired");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Tired));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.morale");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Morale(1)));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.loyalty");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Member.Loyalty));
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Member_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets
        (Start_Index => 1, End_Index => Rows - 1, Frame => Member_Frame);
      Show_Stats_Loop :
      for I in Member.Attributes'Range loop
         Label :=
           Create
             (pathName =>
                Member_Frame & ".label" &
                Trim(Source => Positive'Image(I), Side => Left),
              options =>
                "-text {" &
                To_String
                  (Source =>
                     AttributesData_Container.Element
                       (Container => Attributes_List, Index => I)
                       .Name) &
                "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Spin_Box :=
           Create
             (pathName =>
                Member_Frame & ".value" &
                Trim(Source => Positive'Image(I), Side => Left),
              options =>
                "-from 1 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5");
         Set
           (SpinBox => Spin_Box,
            Value => Positive'Image(Member.Attributes(I).Level));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Spin_Box,
            Options => "-column 1 -row" & Positive'Image(I));
      end loop Show_Stats_Loop;
      Member_Frame.Name := New_String(Str => Frame_Name & ".skills");
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Member_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets
        (Start_Index => 1, End_Index => Rows - 1, Frame => Member_Frame);
      Show_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Member.Skills) ..
          Skills_Container.Last_Index(Container => Member.Skills) loop
         Label :=
           Create
             (pathName =>
                Member_Frame & ".label" &
                Trim(Source => Skills_Amount_Range'Image(I), Side => Left),
              options =>
                "-text {" &
                To_String
                  (Source =>
                     SkillsData_Container.Element
                       (Container => Skills_List,
                        Index =>
                          Skills_Container.Element
                            (Container => Member.Skills, Index => I)
                            .Index)
                       .Name) &
                "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Spin_Box :=
           Create
             (pathName =>
                Member_Frame & ".value" &
                Trim(Source => Skills_Amount_Range'Image(I), Side => Left),
              options =>
                "-from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5");
         Set
           (SpinBox => Spin_Box,
            Value =>
              Positive'Image
                (Skills_Container.Element
                   (Container => Member.Skills, Index => I)
                   .Level));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Spin_Box,
            Options => "-column 1 -row" & Skills_Amount_Range'Image(I));
         Skills_Indexes.Append
           (New_Item =>
              Natural
                (Skills_Container.Element
                   (Container => Member.Skills, Index => I)
                   .Index));
      end loop Show_Skills_Loop;
      Show_Add_Skills_Loop :
      for I in 1 .. Skills_Amount loop
         if not Skills_Indexes.Contains(Item => Natural(I)) then
            Append
              (Source => Skills_List_Values,
               New_Item =>
                 " " &
                 To_String
                   (Source =>
                      SkillsData_Container.Element
                        (Container => Skills_List, Index => I)
                        .Name));
         end if;
      end loop Show_Add_Skills_Loop;
      Combo_Box.Name := New_String(Str => Frame_Name & ".addskill.skills");
      configure
        (Widgt => Combo_Box,
         options =>
           "-values [list" & To_String(Source => Skills_List_Values) & "]");
      Current(ComboBox => Combo_Box, NewIndex => "0");
      return TCL_OK;
   end Refresh_Member_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Cargo_Command
   -- FUNCTION
   -- Refresh the information about the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshCargo
   -- SOURCE
   function Refresh_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Frame_Name: constant String := ".debugdialog.main.cargo";
      Cargo_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".update", Interp => Interp);
      Item_Index: Positive;
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".updateamount", Interp => Interp);
   begin
      Item_Index := Natural'Value(Current(ComboBox => Cargo_Combo)) + 1;
      Set
        (SpinBox => Amount_Box,
         Value =>
           Positive'Image
             (Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Amount));
      return TCL_OK;
   end Refresh_Cargo_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Events_Command
   -- FUNCTION
   -- Refresh the list of events
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshEvents
   -- SOURCE
   function Refresh_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.world.deleteevent";
      Events_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".delete", Interp => Interp);
      Values_List: Unbounded_String;
      Events_Button: constant Ttk_Button :=
        Get_Widget(pathName => Frame_Name & ".deleteevent", Interp => Interp);
   begin
      if Events_List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Events_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Events_Box);
         return TCL_OK;
      else
         Tcl.Tk.Ada.Grid.Grid(Slave => Events_Button);
         Tcl.Tk.Ada.Grid.Grid(Slave => Events_Box);
      end if;
      Update_Events_Loop :
      for Event of Events_List loop
         case Event.E_Type is
            when ENEMYSHIP =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Enemy ship: " &
                    To_String
                      (Source => Proto_Ships_List(Event.Ship_Index).Name) &
                    "}");
            when ATTACKONBASE =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Attack on base: " &
                    To_String
                      (Source => Proto_Ships_List(Event.Ship_Index).Name) &
                    "}");
            when DISEASE =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Disease in base: " &
                    To_String
                      (Source =>
                         Sky_Bases
                           (Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                           .Name) &
                    "}");
            when DOUBLEPRICE =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Double price in base: " &
                    To_String
                      (Source =>
                         Sky_Bases
                           (Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                           .Name) &
                    "}");
            when FULLDOCKS =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Full docks in base: " &
                    To_String
                      (Source =>
                         Sky_Bases
                           (Sky_Map(Event.Sky_X, Event.Sky_Y).Base_Index)
                           .Name) &
                    "}");
            when ENEMYPATROL =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Enemy patrol: " &
                    To_String
                      (Source => Proto_Ships_List(Event.Ship_Index).Name) &
                    "}");
            when TRADER =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Trader: " &
                    To_String
                      (Source => Proto_Ships_List(Event.Ship_Index).Name) &
                    "}");
            when FRIENDLYSHIP =>
               Append
                 (Source => Values_List,
                  New_Item =>
                    " {Friendly ship: " &
                    To_String
                      (Source => Proto_Ships_List(Event.Ship_Index).Name) &
                    "}");
            when others =>
               null;
         end case;
      end loop Update_Events_Loop;
      configure
        (Widgt => Events_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Current(ComboBox => Events_Box, NewIndex => "0");
      return TCL_OK;
   end Refresh_Events_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Command
   -- FUNCTION
   -- Refresh the whole game information
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Refresh
   -- SOURCE
   function Refresh_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Interfaces.C;
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main";
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".ship.x", Interp => Interp);
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".ship.module", Interp => Interp);
      Values_List: Unbounded_String;
   begin
      Set(SpinBox => Spin_Box, Value => Positive'Image(Player_Ship.Sky_X));
      Spin_Box.Name := New_String(Str => Frame_Name & ".ship.y");
      Set(SpinBox => Spin_Box, Value => Positive'Image(Player_Ship.Sky_Y));
      Update_Modules_Loop :
      for Module of Player_Ship.Modules loop
         Append
           (Source => Values_List,
            New_Item => " {" & To_String(Source => Module.Name) & "}");
      end loop Update_Modules_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Current(ComboBox => Combo_Box, NewIndex => "0");
      if Refresh_Module_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      Combo_Box.Name := New_String(Str => Frame_Name & ".crew.member");
      Values_List := Null_Unbounded_String;
      Update_Members_Loop :
      for Member of Player_Ship.Crew loop
         Append
           (Source => Values_List,
            New_Item => " {" & To_String(Source => Member.Name) & "}");
      end loop Update_Members_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Current(ComboBox => Combo_Box, NewIndex => "0");
      if Refresh_Member_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      Combo_Box.Name := New_String(Str => Frame_Name & ".cargo.update");
      Values_List := Null_Unbounded_String;
      Update_Cargo_Loop :
      for Item of Player_Ship.Cargo loop
         Append
           (Source => Values_List,
            New_Item =>
              " {" &
              Get_Item_Name
                (Item => Item, Damage_Info => False, To_Lower => False) &
              "}");
      end loop Update_Cargo_Loop;
      configure
        (Widgt => Combo_Box,
         options => "-values [list" & To_String(Source => Values_List) & "]");
      Current(ComboBox => Combo_Box, NewIndex => "0");
      if Refresh_Cargo_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      if Refresh_Events_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Refresh_Command;

   -- ****o* DebugUI/DebugUI.Refresh_Base_Command
   -- FUNCTION
   -- Refresh the information about the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RefreshBase
   -- SOURCE
   function Refresh_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Refresh_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.bases";
      Name_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".name", Interp => Interp);
      Base_Index: Natural := 0;
      Base_Name: constant Bounded_String :=
        To_Bounded_String(Source => Get(Widgt => Name_Entry));
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".type", Interp => Interp);
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".population", Interp => Interp);
   begin
      Find_Base_Index_Loop :
      for I in Sky_Bases'Range loop
         if Sky_Bases(I).Name = Base_Name then
            Base_Index := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
      if Base_Index = 0 then
         return TCL_OK;
      end if;
      Set
        (ComboBox => Combo_Box,
         Value =>
           To_String
             (Source =>
                Bases_Types_List(Sky_Bases(Base_Index).Base_Type).Name));
      Combo_Box.Name := New_String(Str => Frame_Name & ".owner");
      Set
        (ComboBox => Combo_Box,
         Value =>
           To_String
             (Source => Factions_List(Sky_Bases(Base_Index).Owner).Name));
      Combo_Box.Name := New_String(Str => Frame_Name & ".size");
      Current
        (ComboBox => Combo_Box,
         NewIndex =>
           Natural'Image(Bases_Size'Pos(Sky_Bases(Base_Index).Size)));
      Set
        (SpinBox => Spin_Box,
         Value => Natural'Image(Sky_Bases(Base_Index).Population));
      Spin_Box.Name := New_String(Str => Frame_Name & ".reputation");
      Set
        (SpinBox => Spin_Box,
         Value => Integer'Image(Sky_Bases(Base_Index).Reputation.Level));
      Spin_Box.Name := New_String(Str => Frame_Name & ".money");
      if BaseCargo_Container.Length(Container => Sky_Bases(Base_Index).Cargo) >
        0 then
         Set
           (SpinBox => Spin_Box,
            Value =>
              Natural'Image
                (BaseCargo_Container.Element
                   (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
                   .Amount));
      else
         Set(SpinBox => Spin_Box, Value => "0");
      end if;
      return TCL_OK;
   end Refresh_Base_Command;

   -- ****o* DebugUI/DebugUI.Save_Game_Command
   -- FUNCTION
   -- Save the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugSaveGame
   -- SOURCE
   function Save_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Save_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Save_Game(Pretty_Print => True);
      return TCL_OK;
   end Save_Game_Command;

   -- ****o* DebugUI/DebugUI.Move_Ship_Command
   -- FUNCTION
   -- Move the player ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugMoveShip
   -- SOURCE
   function Move_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Frame_Name: constant String := ".debugdialog.main.ship";
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".x", Interp => Interp);
   begin
      Player_Ship.Sky_X := Positive'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".y");
      Player_Ship.Sky_Y := Positive'Value(Get(Widgt => Spin_Box));
      Show_Sky_Map(Clear => True);
      return TCL_OK;
   end Move_Ship_Command;

   -- ****o* DebugUI/DebugUI.Update_Module_Command
   -- FUNCTION
   -- Update the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateModule
   -- SOURCE
   function Update_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.ship";
      Module_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".module", Interp => Interp);
      Module_Index: constant Positive :=
        Natural'Value(Current(ComboBox => Module_Box)) + 1;
      Proto_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".proto", Interp => Interp);
      Value: Unbounded_String :=
        To_Unbounded_String(Source => Get(Widgt => Proto_Combo));
      Spin_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".weight", Interp => Interp);
   begin
      Update_Proto_Index_Loop :
      for I in
        BaseModules_Container.First_Index(Container => Modules_List) ..
          BaseModules_Container.Last_Index(Container => Modules_List) loop
         if To_String
             (Source =>
                BaseModules_Container.Element
                  (Container => Modules_List, Index => I)
                  .Name) =
           To_String(Source => Value) then
            Value := Null_Unbounded_String;
            Player_Ship.Modules(Module_Index).Proto_Index := I;
            exit Update_Proto_Index_Loop;
         end if;
      end loop Update_Proto_Index_Loop;
      Player_Ship.Modules(Module_Index).Weight :=
        Natural'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".dur");
      Player_Ship.Modules(Module_Index).Durability :=
        Natural'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".maxdur");
      Player_Ship.Modules(Module_Index).Max_Durability :=
        Natural'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".upgrade");
      Player_Ship.Modules(Module_Index).Upgrade_Progress :=
        Natural'Value(Get(Widgt => Spin_Box));
      return TCL_OK;
   end Update_Module_Command;

   -- ****o* DebugUI/DebugUI.Add_Skill_Command
   -- FUNCTION
   -- Add a new skill to the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddSkill
   -- SOURCE
   function Add_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.crew";
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".member", Interp => Interp);
      Member_Index: constant Positive :=
        Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      Skill_Name: Unbounded_String;
   begin
      Combo_Box.Name := New_String(Str => Frame_Name & ".addskill.skills");
      Skill_Name := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Add_Skill_Loop :
      for I in 1 .. Skills_Amount loop
         if To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => I)
                       .Name)) =
           Skill_Name then
            Skills_Container.Append
              (Container => Player_Ship.Crew(Member_Index).Skills,
               New_Item => (Index => I, Level => 1, Experience => 0));
            return
              Refresh_Member_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         end if;
      end loop Add_Skill_Loop;
      return TCL_OK;
   end Add_Skill_Command;

   -- ****o* DebugUI/DebugUI.Update_Member_Command
   -- FUNCTION
   -- Update the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateMember
   -- SOURCE
   function Update_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Member_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Frame_Name: constant String := ".debugdialog.main.crew";
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".member", Interp => Interp);
      Member_Index: Positive;
      Spin_Box: Ttk_SpinBox :=
        Get_Widget
          (pathName => Frame_Name & ".stats2.health", Interp => Interp);
      Local_Attribute: Mob_Attribute_Record;
   begin
      Member_Index := Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      Player_Ship.Crew(Member_Index).Health :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.thirst");
      Player_Ship.Crew(Member_Index).Thirst :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.hunger");
      Player_Ship.Crew(Member_Index).Hunger :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.tired");
      Player_Ship.Crew(Member_Index).Tired :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.morale");
      Player_Ship.Crew(Member_Index).Morale(1) :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Spin_Box.Name := New_String(Str => Frame_Name & ".stats2.loyalty");
      Player_Ship.Crew(Member_Index).Loyalty :=
        Skill_Range'Value(Get(Widgt => Spin_Box));
      Update_Stats_Loop :
      for I in Player_Ship.Crew(Member_Index).Attributes'Range loop
         Spin_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".stats.value" &
                Trim(Source => Positive'Image(I), Side => Left));
         Local_Attribute :=
           (Level => Positive'Value(Get(Widgt => Spin_Box)),
            Experience =>
              Player_Ship.Crew(Member_Index).Attributes(I).Experience);
         Player_Ship.Crew(Member_Index).Attributes(I) := Local_Attribute;
      end loop Update_Stats_Loop;
      Update_Skills_Loop :
      for I in
        Skills_Container.First_Index
          (Container => Player_Ship.Crew(Member_Index).Skills) ..
          Skills_Container.Last_Index
            (Container => Player_Ship.Crew(Member_Index).Skills) loop
         Spin_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".skills.value" &
                Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
         Update_Skill_Block :
         declare
            New_Skill: Skill_Info :=
              Skills_Container.Element
                (Container => Player_Ship.Crew(Member_Index).Skills,
                 Index => I);
         begin
            New_Skill.Level := Positive'Value(Get(Widgt => Spin_Box));
            Skills_Container.Replace_Element
              (Container => Player_Ship.Crew(Member_Index).Skills, Index => I,
               New_Item => New_Skill);
         end Update_Skill_Block;
      end loop Update_Skills_Loop;
      return TCL_OK;
   end Update_Member_Command;

   -- ****o* DebugUI/DebugUI.Add_Item_Command
   -- FUNCTION
   -- Add a new item to the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddItem
   -- SOURCE
   function Add_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.cargo";
      Item_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".add", Interp => Interp);
      Item_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".amount", Interp => Interp);
      Item_Name: Bounded_String;
      Item_Index: Objects_Container.Extended_Index;
   begin
      Item_Name := To_Bounded_String(Source => Get(Widgt => Item_Entry));
      Find_Index_Loop :
      for I in
        Objects_Container.First_Index(Container => Items_List) ..
          Objects_Container.Last_Index(Container => Items_List) loop
         if Objects_Container.Element(Container => Items_List, Index => I)
             .Name =
           Item_Name then
            Item_Index := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if Item_Index = 0 then
         return TCL_OK;
      end if;
      Update_Cargo
        (Ship => Player_Ship, Proto_Index => Item_Index,
         Amount => Positive'Value(Get(Widgt => Item_Box)));
      return
        Refresh_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Add_Item_Command;

   -- ****o* DebugUI/DebugUI.Update_Item_Command
   -- FUNCTION
   -- Update the amount of an item in the player ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateItem
   -- SOURCE
   function Update_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Frame_Name: constant String := ".debugdialog.main.cargo";
      Item_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".update", Interp => Interp);
      Item_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".updateamount", Interp => Interp);
      Item_Index: Positive;
   begin
      Item_Index := Natural'Value(Current(ComboBox => Item_Combo)) + 1;
      Update_Cargo
        (Ship => Player_Ship, Amount => Positive'Value(Get(Widgt => Item_Box)),
         Cargo_Index => Item_Index);
      return
        Refresh_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Update_Item_Command;

   -- ****o* DebugUI/DebugUI.Update_Base_Command
   -- FUNCTION
   -- Update the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugUpdateBase
   -- SOURCE
   function Update_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Base_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.bases";
      Base_Index: Natural := 0;
      Base_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".name", Interp => Interp);
      Base_Name: Bounded_String;
      Base_Combo: Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".type", Interp => Interp);
      Base_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".population", Interp => Interp);
      Item: Base_Cargo;
   begin
      Base_Name := To_Bounded_String(Source => Get(Widgt => Base_Entry));
      Find_Index_Loop :
      for I in Sky_Bases'Range loop
         if Sky_Bases(I).Name = Base_Name then
            Base_Index := I;
            exit Find_Index_Loop;
         end if;
      end loop Find_Index_Loop;
      if Base_Index = 0 then
         return TCL_OK;
      end if;
      Update_Base_Type_Loop :
      for I in Bases_Types_List.Iterate loop
         if Bases_Types_List(I).Name =
           To_Unbounded_String(Source => Get(Widgt => Base_Combo)) then
            Sky_Bases(Base_Index).Base_Type :=
              BasesTypes_Container.Key(Position => I);
            exit Update_Base_Type_Loop;
         end if;
      end loop Update_Base_Type_Loop;
      Base_Combo.Name := New_String(Str => Frame_Name & ".owner");
      Update_Base_Owner_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name =
           To_Bounded_String(Source => Get(Widgt => Base_Combo)) then
            Sky_Bases(Base_Index).Owner :=
              Factions_Container.Key(Position => I);
            exit Update_Base_Owner_Loop;
         end if;
      end loop Update_Base_Owner_Loop;
      Base_Combo.Name := New_String(Str => Frame_Name & ".size");
      Sky_Bases(Base_Index).Size := Bases_Size'Value(Get(Widgt => Base_Combo));
      Sky_Bases(Base_Index).Population :=
        Natural'Value(Get(Widgt => Base_Box));
      Base_Box.Name := New_String(Str => Frame_Name & ".reputation");
      Sky_Bases(Base_Index).Reputation.Level :=
        Integer'Value(Get(Widgt => Base_Box));
      Base_Box.Name := New_String(Str => Frame_Name & ".money");
      Item :=
        BaseCargo_Container.Element
          (Container => Sky_Bases(Base_Index).Cargo, Index => 1);
      Item.Amount := Natural'Value(Get(Widgt => Base_Box));
      BaseCargo_Container.Replace_Element
        (Container => Sky_Bases(Base_Index).Cargo, Index => 1,
         New_Item => Item);
      return TCL_OK;
   end Update_Base_Command;

   -- ****o* DebugUI/DebugUI.Add_Ship_Command
   -- FUNCTION
   -- Add a new ship based event to the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddShip
   -- SOURCE
   function Add_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.world";
      Ship_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".ship", Interp => Interp);
      Ship_Name: Bounded_String;
      Npc_Ship_X, Npc_Ship_Y, Duration: Positive;
      Ship_Box: Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".x", Interp => Interp);
   begin
      Ship_Name := To_Bounded_String(Source => Get(Widgt => Ship_Entry));
      Npc_Ship_X := Positive'Value(Get(Widgt => Ship_Box));
      Ship_Box.Name := New_String(Str => Frame_Name & ".y");
      Npc_Ship_Y := Positive'Value(Get(Widgt => Ship_Box));
      Ship_Box.Name := New_String(Str => Frame_Name & ".duration");
      Duration := Positive'Value(Get(Widgt => Ship_Box));
      Add_Ship_Event_Loop :
      for I in Proto_Ships_List.Iterate loop
         if Proto_Ships_List(I).Name = Ship_Name then
            if Traders.Contains
                (Item => Proto_Ships_Container.To_Index(Position => I)) then
               Events_List.Append
                 (New_Item =>
                    (E_Type => TRADER, Sky_X => Npc_Ship_X,
                     Sky_Y => Npc_Ship_Y, Time => Duration,
                     Ship_Index =>
                       Proto_Ships_Container.To_Index(Position => I)));
            elsif Friendly_Ships.Contains
                (Item => Proto_Ships_Container.To_Index(Position => I)) then
               Events_List.Append
                 (New_Item =>
                    (E_Type => FRIENDLYSHIP, Sky_X => Npc_Ship_X,
                     Sky_Y => Npc_Ship_Y, Time => Duration,
                     Ship_Index =>
                       Proto_Ships_Container.To_Index(Position => I)));
            else
               Events_List.Append
                 (New_Item =>
                    (E_Type => ENEMYSHIP, Sky_X => Npc_Ship_X,
                     Sky_Y => Npc_Ship_Y, Time => Duration,
                     Ship_Index =>
                       Proto_Ships_Container.To_Index(Position => I)));
            end if;
            Sky_Map(Npc_Ship_X, Npc_Ship_Y).Event_Index :=
              Events_List.Last_Index;
            return Refresh_Events_Command(Client_Data => Client_Data, Interp => Interp, Argc => Argc, Argv => Argv);
         end if;
      end loop Add_Ship_Event_Loop;
      return TCL_OK;
   end Add_Ship_Command;

   -- ****o* DebugUI/DebugUI.Toggle_Item_Entry_Command
   -- FUNCTION
   -- Show or hide item entry for bases events
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleItemEntry
   -- SOURCE
   function Toggle_Item_Entry_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Item_Entry_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Frame_Name: constant String := ".debugdialog.main.world";
      Event_Combo: constant Ttk_ComboBox :=
        Get_Widget(pathName => Frame_Name & ".event", Interp => Interp);
      Item_Entry: constant Ttk_Entry := Get_Widget(pathName => Frame_Name & ".item", Interp => Interp);
      Item_Label: constant Ttk_Label :=
        Get_Widget(pathName => Frame_Name & ".itemlbl", Interp => Interp);
   begin
      if Current(ComboBox => Event_Combo) = "1" then
         Tcl.Tk.Ada.Grid.Grid(Slave => Item_Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Item_Entry);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Item_Label);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Item_Entry);
      end if;
      return TCL_OK;
   end Toggle_Item_Entry_Command;

   -- ****o* DebugUI/DebugUI.Add_Event_Command
   -- FUNCTION
   -- Add a new base event to the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugAddEvent
   -- SOURCE
   function Add_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;
      Frame_Name: constant String := ".debugdialog.main.world";
      Event_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame_Name & ".base", Interp => Interp);
      Event_Name: Unbounded_String;
      Base_Index, Event_Type: Natural := 0;
      Event_Box: Ttk_ComboBox := Get_Widget(pathName => Frame_Name & ".event", Interp => Interp);
      Duration_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Frame_Name & ".baseduration", Interp => Interp);
      Added: Boolean := True;
   begin
      Event_Name := To_Unbounded_String(Source => Get(Widgt => Event_Entry));
      Find_Base_Index_Loop :
      for I in Sky_Bases'Range loop
         if To_String(Source => Sky_Bases(I).Name) = To_String(Source => Event_Name) then
            Base_Index := I;
            exit Find_Base_Index_Loop;
         end if;
      end loop Find_Base_Index_Loop;
      if Base_Index = 0 then
         return TCL_OK;
      end if;
      Event_Type := Natural'Value(Current(ComboBox => Event_Box));
      case Event_Type is
         when 0 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => DISEASE, Sky_X => Sky_Bases(Base_Index).Sky_X,
                  Sky_Y => Sky_Bases(Base_Index).Sky_Y, Time => Positive'Value(Get(Widgt => Duration_Box)),
                  Data => 1));
         when 1 =>
            Event_Box.Name := New_String(Frame_Name & ".item");
            Event_Name := To_Unbounded_String(Source => Get(Widgt => Event_Box));
            Added := False;
            Find_Item_Loop :
            for I in
              Objects_Container.First_Index(Container => Items_List) ..
                Objects_Container.Last_Index(Container => Items_List) loop
               if To_String
                   (Source =>
                      Objects_Container.Element
                        (Container => Items_List, Index => I)
                        .Name) =
                 To_String(Source => Event_Name) then
                  Events_List.Append
                    (New_Item =>
                       (E_Type => DOUBLEPRICE, Sky_X => Sky_Bases(Base_Index).Sky_X,
                        Sky_Y => Sky_Bases(Base_Index).Sky_Y,
                        Time => Positive'Value(Get(Widgt => Duration_Box)), Item_Index => I));
                  Added := True;
                  exit Find_Item_Loop;
               end if;
            end loop Find_Item_Loop;
         when 2 =>
            Events_List.Append
              (New_Item =>
                 (E_Type => DISEASE, Sky_X => Sky_Bases(Base_Index).Sky_X,
                  Sky_Y => Sky_Bases(Base_Index).Sky_Y, Time => Positive'Value(Get(Widgt => Duration_Box)),
                  Data => 1));
         when others =>
            null;
      end case;
      if not Added then
         return TCL_OK;
      end if;
      Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
        .Event_Index :=
        Events_List.Last_Index;
      return Refresh_Events_Command(Client_Data => Client_Data, Interp => Interp, Argc => Argc, Argv => Argv);
   end Add_Event_Command;

   -- ****o* DebugUI/DebugUI.Delete_Event_Command
   -- FUNCTION
   -- Remove the selected event from the game
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DebugDeleteEvent
   -- SOURCE
   function Delete_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Event_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Event_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".debugdialog.main.world.deleteevent.delete", Interp => Interp);
   begin
      Delete_Event(Event_Index => Natural'Value(Current(ComboBox => Event_Box)) + 1);
      return Refresh_Events_Command(Client_Data => Client_Data, Interp => Interp, Argc => Argc, Argv => Argv);
   end Delete_Event_Command;

   procedure Show_Debug_Ui is
      use Tiny_String;

      Frame_Name: constant String := ".debugdialog.main.bases";
      Combo_Box: Ttk_ComboBox := Get_Widget(pathName => Frame_Name & ".type");
      Values_List: Unbounded_String;
   begin
      Tcl_EvalFile
        (interp => Get_Context,
         fileName => To_String(Source => Data_Directory) & "ui" & Dir_Separator & "debug.tcl");
      Add_Command(Name => "Refresh", Ada_Command => Refresh_Command'Access);
      Add_Command(Name => "RefreshModule", Ada_Command => Refresh_Module_Command'Access);
      Add_Command(Name => "RefreshMember", Ada_Command => Refresh_Member_Command'Access);
      Add_Command(Name => "RefreshCargo", Ada_Command => Refresh_Cargo_Command'Access);
      Add_Command(Name => "RefreshBase", Ada_Command => Refresh_Base_Command'Access);
      Add_Command(Name => "RefreshEvents", Ada_Command => Refresh_Events_Command'Access);
      Add_Command(Name => "DebugSaveGame", Ada_Command => Save_Game_Command'Access);
      Add_Command(Name => "DebugMoveShip", Ada_Command => Move_Ship_Command'Access);
      Add_Command(Name => "DebugUpdateModule", Ada_Command => Update_Module_Command'Access);
      Add_Command(Name => "DebugAddSkill", Ada_Command => Add_Skill_Command'Access);
      Add_Command(Name => "DebugUpdateMember", Ada_Command => Update_Member_Command'Access);
      Add_Command(Name => "DebugAddItem", Ada_Command => Add_Item_Command'Access);
      Add_Command(Name => "DebugUpdateItem", Ada_Command => Update_Item_Command'Access);
      Add_Command(Name => "DebugUpdateBase", Ada_Command => Update_Base_Command'Access);
      Add_Command(Name => "DebugAddShip", Ada_Command => Add_Ship_Command'Access);
      Add_Command(Name => "ToggleItemEntry", Ada_Command => Toggle_Item_Entry_Command'Access);
      Add_Command("DebugAddEvent", Add_Event_Command'Access);
      Add_Command("DebugDeleteEvent", Delete_Event_Command'Access);
      Load_Bases_Types_Loop :
      for BaseType of Bases_Types_List loop
         Append(Values_List, " {" & BaseType.Name & "}");
      end loop Load_Bases_Types_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Frame_Name & ".owner");
      Load_Factions_Loop :
      for Faction of Factions_List loop
         Append(Values_List, " {" & To_String(Source => Faction.Name) & "}");
      end loop Load_Factions_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(Frame_Name & ".name");
      Load_Bases_Loop :
      for Base of Sky_Bases loop
         Append(Values_List, " {" & To_String(Source => Base.Name) & "}");
      end loop Load_Bases_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Combo_Box.Name := New_String(".debugdialog.main.world.base");
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(".debugdialog.main.ship.proto");
      Load_Modules_Prototypes_Loop :
      for I in
        BaseModules_Container.First_Index(Container => Modules_List) ..
          BaseModules_Container.Last_Index(Container => Modules_List) loop
         Append
           (Values_List,
            " {" &
            To_String
              (Source =>
                 BaseModules_Container.Element
                   (Container => Modules_List, Index => I)
                   .Name) &
            "}");
      end loop Load_Modules_Prototypes_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(".debugdialog.main.cargo.add");
      Load_Items_Loop :
      for I in
        Objects_Container.First_Index(Container => Items_List) ..
          Objects_Container.Last_Index(Container => Items_List) loop
         Append
           (Values_List,
            " {" &
            To_String
              (Source =>
                 Objects_Container.Element(Container => Items_List, Index => I)
                   .Name) &
            "}");
      end loop Load_Items_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Combo_Box.Name := New_String(".debugdialog.main.world.item");
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Values_List := Null_Unbounded_String;
      Combo_Box.Name := New_String(".debugdialog.main.world.ship");
      Load_Ships_Loop :
      for Ship of Proto_Ships_List loop
         Append(Values_List, " {" & To_String(Source => Ship.Name) & "}");
      end loop Load_Ships_Loop;
      configure(Combo_Box, "-values [list" & To_String(Values_List) & "]");
      Tcl_Eval(Get_Context, "Refresh");
   end Show_Debug_Ui;

end DebugUI;
