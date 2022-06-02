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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Font; use Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI; use CoreUI;
with Crafts; use Crafts;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.UI.Crew; use Ships.UI.Crew;
with Ships.Upgrade; use Ships.Upgrade;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Ships.UI.Modules is

   -- ****iv* SUModules/SUModules.Modules_Table
   -- FUNCTION
   -- Table with info about the installed modules on the player ship
   -- SOURCE
   Modules_Table: Table_Widget (Amount => 2);
   -- ****

   -- ****iv* SUModules/SUModules.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Modules_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* SUModules/SUModules.Show_Module_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected module options
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModuleMenu moduleindex
   -- ModuleIndex is the index of the module's menu to show
   -- SOURCE
   function Show_Module_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Module_Max_Value: Positive;
      Is_Passenger: Boolean := False;
      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".modulemmenu",
           Title =>
             To_String(Source => Player_Ship.Modules(Module_Index).Name) &
             " actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Module_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Module_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Module_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Module_Menu & ".newname;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".newname", Label => "Rename module",
         Command =>
           "GetString {Enter a new name for the " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) &
           ":} modulename" & CArgv.Arg(Argv => Argv, N => 1) &
           " {Renaming the module}");
      if Player_Ship.Repair_Module /= Module_Index then
         Add_Button
           (Name => ".repair",
            Label => "Repair selected module as first when damaged",
            Command => "SetRepair assign " & CArgv.Arg(Argv => Argv, N => 1));
      end if;
      Module_Max_Value :=
        Natural
          (Float
             (BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Player_Ship.Modules(Module_Index).Proto_Index)
                .Durability) *
           1.5);
      if Player_Ship.Modules(Module_Index).Upgrade_Action = DURABILITY and
        Player_Ship.Upgrade_Module = Module_Index then
         Module_Max_Value := 1;
      end if;
      if Player_Ship.Modules(Module_Index).Max_Durability <
        Module_Max_Value then
         Add_Button
           (Name => ".upgrade", Label => "Start upgrading module durability",
            Command => "SetUpgrade 1 " & CArgv.Arg(Argv => Argv, N => 1));
      end if;
      case Player_Ship.Modules(Module_Index).M_Type is
         when ENGINE =>
            Module_Max_Value :=
              Natural
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Upgrade_Action = MAX_VALUE and
              Player_Ship.Upgrade_Module = Module_Index then
               Module_Max_Value := 1;
            end if;
            if Player_Ship.Modules(Module_Index).Power < Module_Max_Value then
               Add_Button
                 (Name => ".upgrade2", Label => "Start upgrading engine power",
                  Command =>
                    "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
            Module_Max_Value :=
              Natural
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Value) /
                 2.0);
            if Player_Ship.Modules(Module_Index).Upgrade_Action = VALUE and
              Player_Ship.Upgrade_Module = Module_Index then
               Module_Max_Value :=
                 Player_Ship.Modules(Module_Index).Fuel_Usage + 1;
            end if;
            if Player_Ship.Modules(Module_Index).Fuel_Usage >
              Module_Max_Value then
               Add_Button
                 (Name => ".fuel",
                  Label => "Start working on reduce fuel usage of this engine",
                  Command =>
                    "SetUpgrade 3 " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
            if not Player_Ship.Modules(Module_Index).Disabled then
               Add_Button
                 (Name => ".switch",
                  Label => "Turn off engine so it stop using fuel",
                  Command =>
                    "DisableEngine " & CArgv.Arg(Argv => Argv, N => 1));
            else
               Add_Button
                 (Name => ".switch",
                  Label => "Turn on engine so ship will be fly faster",
                  Command =>
                    "DisableEngine " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
         when CABIN =>
            Module_Max_Value :=
              Natural
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Upgrade_Action = MAX_VALUE and
              Player_Ship.Upgrade_Module = Module_Index then
               Module_Max_Value := 1;
            end if;
            if Player_Ship.Modules(Module_Index).Quality <
              Module_Max_Value then
               Add_Button
                 (Name => ".upgrade2",
                  Label => "Start upgrading cabin quality",
                  Command =>
                    "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
            Missions_Loop :
            for Mission of Accepted_Missions loop
               if Mission.M_Type = PASSENGER then
                  Check_Passenger_Loop :
                  for Owner of Player_Ship.Modules(Module_Index).Owner loop
                     if Mission.Data = Owner then
                        Is_Passenger := True;
                        exit Missions_Loop;
                     end if;
                  end loop Check_Passenger_Loop;
               end if;
            end loop Missions_Loop;
            if not Is_Passenger then
               Add_Button
                 (Name => ".assigncrew",
                  Label => "Assign a crew member as owner of cabin...",
                  Command =>
                    "ShowAssignCrew " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
         when GUN | HARPOON_GUN =>
            Set_Gun_Upgrade_Block :
            declare
               Current_Value: constant Positive :=
                 (if Player_Ship.Modules(Module_Index).M_Type = GUN then
                    Player_Ship.Modules(Module_Index).Damage
                  else Player_Ship.Modules(Module_Index).Duration);
            begin
               Module_Max_Value :=
                 Natural
                   (Float
                      (BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Module_Index).Proto_Index)
                         .Max_Value) *
                    1.5);
               if Player_Ship.Modules(Module_Index).Upgrade_Action =
                 MAX_VALUE and
                 Player_Ship.Upgrade_Module = Module_Index then
                  Module_Max_Value := 1;
               end if;
               if Current_Value < Module_Max_Value then
                  if Player_Ship.Modules(Module_Index).M_Type = GUN then
                     Add_Button
                       (Name => ".upgrade2",
                        Label => "Start upgrading damage of gun",
                        Command =>
                          "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
                  else
                     Add_Button
                       (Name => ".upgrade2",
                        Label => "Start upgrading strength of gun",
                        Command =>
                          "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
                  end if;
               end if;
            end Set_Gun_Upgrade_Block;
            Add_Button
              (Name => ".assigncrew",
               Label => "Assign a crew member as gunner...",
               Command => "ShowAssignCrew " & CArgv.Arg(Argv => Argv, N => 1));
            Set_Ammo_Block :
            declare
               Ammo_Index: constant Natural :=
                 (if Player_Ship.Modules(Module_Index).M_Type = GUN then
                    Player_Ship.Modules(Module_Index).Ammo_Index
                  else Player_Ship.Modules(Module_Index).Harpoon_Index);
            begin
               Find_Ammo_Loop :
               for I in
                 Inventory_Container.First_Index
                   (Container => Player_Ship.Cargo) ..
                   Inventory_Container.Last_Index
                     (Container => Player_Ship.Cargo) loop
                  if Objects_Container.Element
                      (Container => Items_List,
                       Index =>
                         Inventory_Container.Element
                           (Container => Player_Ship.Cargo, Index => I)
                           .Proto_Index)
                      .I_Type =
                    TinyString_Formal_Container.Element
                      (Container => Items_Types,
                       Index =>
                         BaseModules_Container.Element
                           (Container => Modules_List,
                            Index =>
                              Player_Ship.Modules(Module_Index).Proto_Index)
                           .Value) and
                    I /= Ammo_Index then
                     Add_Button
                       (Name => ".assignammo",
                        Label => "Assign an ammo to gun...",
                        Command =>
                          "ShowAssignAmmo " & CArgv.Arg(Argv => Argv, N => 1));
                     exit Find_Ammo_Loop;
                  end if;
               end loop Find_Ammo_Loop;
            end Set_Ammo_Block;
         when BATTERING_RAM =>
            Module_Max_Value :=
              Natural
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Upgrade_Action = MAX_VALUE and
              Player_Ship.Upgrade_Module = Module_Index then
               Module_Max_Value := 1;
            end if;
            if Player_Ship.Modules(Module_Index).Damage2 <
              Module_Max_Value then
               Add_Button
                 (Name => ".upgrade2",
                  Label => "Start upgrading damage of battering ram",
                  Command =>
                    "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
         when HULL =>
            Module_Max_Value :=
              Natural
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Upgrade_Action = MAX_VALUE and
              Player_Ship.Upgrade_Module = Module_Index then
               Module_Max_Value := 1;
            end if;
            if Player_Ship.Modules(Module_Index).Max_Modules <
              Module_Max_Value then
               Add_Button
                 (Name => ".upgrade2",
                  Label =>
                    "Start enlarging hull so it can have more modules installed",
                  Command =>
                    "SetUpgrade 2 " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
         when WORKSHOP =>
            if Player_Ship.Modules(Module_Index).Crafting_Index /=
              Null_Bounded_String then
               Add_Button
                 (Name => ".assigncrew",
                  Label => "Assign a crew member as worker...",
                  Command =>
                    "ShowAssignCrew " & CArgv.Arg(Argv => Argv, N => 1));
               Add_Button
                 (Name => ".cancelorder",
                  Label => "Cancel current crafting order",
                  Command => "CancelOrder " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
         when MEDICAL_ROOM =>
            Find_Healing_Tool_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Health < 100 and
                 Find_Item
                     (Inventory => Player_Ship.Cargo,
                      Item_Type =>
                        Factions_List(Player_Ship.Crew(1).Faction)
                          .Healing_Tools) >
                   0 then
                  Add_Button
                    (Name => ".assigncrew",
                     Label => "Assign a crew member as medic...",
                     Command =>
                       "ShowAssignCrew " & CArgv.Arg(Argv => Argv, N => 1));
                  exit Find_Healing_Tool_Loop;
               end if;
            end loop Find_Healing_Tool_Loop;
         when TRAINING_ROOM =>
            if Player_Ship.Modules(Module_Index).Trained_Skill > 0 then
               Add_Button
                 (Name => ".assigncrew",
                  Label => "Assign a crew member as worker...",
                  Command =>
                    "ShowAssignCrew " & CArgv.Arg(Argv => Argv, N => 1));
            end if;
            Add_Button
              (Name => ".assignskill",
               Label =>
                 "Assign a skill which will be trained in the training room...",
               Command =>
                 "ShowAssignSkill " & CArgv.Arg(Argv => Argv, N => 1));
         when others =>
            null;
      end case;
      Add_Button
        (Name => ".info", Label => "Show more info about the module",
         Command => "ShowModuleInfo " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Module_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Module_Menu_Command;

   -- ****o* SUModules/SUModules.Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModuleInfo moduleindex
   -- ModuleIndex is the index of the module to show
   -- SOURCE
   function Show_Module_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Short_String;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module: constant Module_Data := Player_Ship.Modules(Module_Index);
      Module_Max_Value: Positive;
      Have_Ammo: Boolean;
      M_Amount, Max_Upgrade: Natural := 0;
      Damage_Percent, Upgrade_Percent: Float;
      Progress_Bar: Ttk_ProgressBar;
      Label: Ttk_Label;
      Module_Info, Progress_Bar_Style: Unbounded_String;
      Module_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".moduledialog",
           Title =>
             To_String(Source => Player_Ship.Modules(Module_Index).Name),
           Columns => 2);
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Module_Dialog & ".yscroll",
           options =>
             "-orient vertical -command [list .moduledialog.canvas yview]");
      Module_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Module_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Module_Frame: constant Ttk_Frame :=
        Create(pathName => Module_Canvas & ".frame");
      Module_Text: constant Tk_Text :=
        Create
          (pathName => Module_Frame & ".info",
           options => "-wrap char -height 15 -width 40");
      Height: Positive := 10;
      procedure Add_Owners_Info(Owners_Name: String) is
         Have_Owner: Boolean := False;
      begin
         Insert
           (TextWidget => Module_Text, Index => "end",
            Text => "{" & LF & Owners_Name & "}");
         if Module.Owner.Length > 1 then
            Insert(TextWidget => Module_Text, Index => "end", Text => "s");
         end if;
         Insert
           (TextWidget => Module_Text, Index => "end",
            Text => "{ (max" & Count_Type'Image(Module.Owner.Length) & "): }");
         Add_Owners_Info_Loop :
         for I in Module.Owner.First_Index .. Module.Owner.Last_Index loop
            if Module.Owner(I) > 0 then
               if Have_Owner then
                  Insert
                    (TextWidget => Module_Text, Index => "end",
                     Text => "{, }");
               end if;
               Have_Owner := True;
               Insert
                 (TextWidget => Module_Text, Index => "end",
                  Text =>
                    To_String
                      (Source => Player_Ship.Crew(Module.Owner(I)).Name));
            end if;
         end loop Add_Owners_Info_Loop;
         if not Have_Owner then
            Insert
              (TextWidget => Module_Text, Index => "end", Text => "{none}");
         end if;
      end Add_Owners_Info;
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Module_Canvas, Options => "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Y_Scroll,
         Options => "-sticky ns -column 1 -row 1 -padx {0 5} -pady 5");
      Tcl.Tk.Ada.Grid.Grid_Propagate(Master => Module_Dialog, Value => "off");
      Tcl.Tk.Ada.Grid.Column_Configure
        (Master => Module_Dialog, Slave => Module_Canvas,
         Options => "-weight 1");
      Tcl.Tk.Ada.Grid.Row_Configure
        (Master => Module_Dialog, Slave => Module_Canvas,
         Options => "-weight 1");
      Autoscroll(Scroll => Y_Scroll);
      if Module.Durability < Module.Max_Durability then
         Label := Create(pathName => Module_Frame & ".damagelbl");
         Damage_Percent :=
           (Float(Module.Durability) / Float(Module.Max_Durability));
         if Damage_Percent < 1.0 and Damage_Percent > 0.79 then
            configure
              (Widgt => Label, options => "-text {Status: Slightly damaged}");
         elsif Damage_Percent < 0.8 and Damage_Percent > 0.49 then
            configure(Widgt => Label, options => "-text {Status: Damaged}");
         elsif Damage_Percent < 0.5 and Damage_Percent > 0.19 then
            configure
              (Widgt => Label, options => "-text {Status: Heavily damaged}");
         elsif Damage_Percent < 0.2 and Damage_Percent > 0.0 then
            configure
              (Widgt => Label, options => "-text {Status: Almost destroyed}");
         elsif Damage_Percent = 0.0 then
            configure(Widgt => Label, options => "-text {Status: Destroyed}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-sticky w");
         Height :=
           Height +
           Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         Module_Max_Value :=
           Positive
             (Float
                (BaseModules_Container.Element
                   (Container => Modules_List, Index => Module.Proto_Index)
                   .Durability) *
              1.5);
         if Module.Max_Durability = Module_Max_Value then
            configure
              (Widgt => Label,
               options =>
                 "-text {" & cget(Widgt => Label, option => "-text") &
                 " (max upgrade)}");
         end if;
      end if;
      Tag_Configure
        (TextWidget => Module_Text, TagName => "red",
         Options => "-foreground red");
      Insert
        (TextWidget => Module_Text, Index => "end",
         Text =>
           "{Weight: " & Integer'Image(Module.Weight) & " kg" & LF &
           "Repair/Upgrade material: }");
      Find_Repair_Material_Loop :
      for I in
        Objects_Container.First_Index(Container => Items_List) ..
          Objects_Container.Last_Index(Container => Items_List) loop
         if To_String
             (Source =>
                Objects_Container.Element(Container => Items_List, Index => I)
                  .I_Type) =
           To_String
             (Source =>
                BaseModules_Container.Element
                  (Container => Modules_List, Index => Module.Proto_Index)
                  .Repair_Material) then
            if M_Amount > 0 then
               Insert
                 (TextWidget => Module_Text, Index => "end", Text => "{ or }");
            end if;
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" &
                 To_String
                   (Source =>
                      Objects_Container.Element
                        (Container => Items_List, Index => I)
                        .Name) &
                 "}" &
                 (if
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Item_Type =>
                         Objects_Container.Element
                           (Container => Items_List, Index => I)
                           .I_Type) =
                    0
                  then " [list red]"
                  else ""));
            M_Amount := M_Amount + 1;
         end if;
      end loop Find_Repair_Material_Loop;
      Insert
        (TextWidget => Module_Text, Index => "end",
         Text =>
           "{" & LF & "Repair/Upgrade skill: " &
           To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List,
                   Index =>
                     BaseModules_Container.Element
                       (Container => Modules_List, Index => Module.Proto_Index)
                       .Repair_Skill)
                  .Name) &
           "/" &
           To_String
             (Source =>
                AttributesData_Container.Element
                  (Container => Attributes_List,
                   Index =>
                     SkillsData_Container.Element
                       (Container => Skills_List,
                        Index =>
                          BaseModules_Container.Element
                            (Container => Modules_List,
                             Index => Module.Proto_Index)
                            .Repair_Skill)
                       .Attribute)
                  .Name) &
           "}");
      case Module.M_Type is
         when ENGINE =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF & "Max power:" & Integer'Image(Module.Power) & "}");
            Module_Max_Value :=
              Positive
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Max_Value) *
                 1.5);
            if Module.Power = Module_Max_Value then
               Insert
                 (TextWidget => Module_Text, Index => "end",
                  Text => "{ (max upgrade)}");
            end if;
            if Module.Disabled then
               Insert
                 (TextWidget => Module_Text, Index => "end",
                  Text => "{ (disabled)}");
            end if;
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF & "Fuel usage:" & Integer'Image(Module.Fuel_Usage) &
                 "}");
            Module_Max_Value :=
              Positive
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Value) /
                 2.0);
            if Module.Fuel_Usage = Module_Max_Value then
               Insert(TextWidget => Module_Text, Index => "end", Text => "{ (max upgrade)}");
            end if;
         when CARGO_ROOM =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text => "{" & LF & "Max cargo:" &
               Integer'Image
                 (BaseModules_Container.Element
                    (Container => Modules_List, Index => Module.Proto_Index)
                    .Max_Value) &
               " kg}");
         when HULL =>
            Label :=
              Create
                (pathName => Module_Frame & ".modules",
                 options => "-text {Modules installed:" &
                 Integer'Image(Module.Installed_Modules) & " /" &
                 Integer'Image(Module.Max_Modules) & "}");
            Module_Max_Value :=
              Positive
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Max_Value) *
                 1.5);
            if Module.Max_Modules = Module_Max_Value then
               configure
                 (Widgt => Label, options => "-text {" & cget(Widgt => Label, option => "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-sticky w");
            Height := Height + Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         when CABIN =>
            Add_Owners_Info(Owners_Name => "Owner");
            if Module.Cleanliness /= Module.Quality then
               Label := Create(pathName => Module_Frame & ".cleanlbl");
               Damage_Percent :=
                 1.0 - (Float(Module.Cleanliness) / Float(Module.Quality));
               if Damage_Percent > 0.0 and Damage_Percent < 0.2 then
                  configure(Widgt => Label, options => "-text {Bit dusty}");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style green.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.19 and Damage_Percent < 0.5 then
                  configure(Widgt => Label, options => "-text {Dusty}");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style yellow.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.49 and Damage_Percent < 0.8 then
                  configure(Widgt => Label, options => "-text {Dirty}");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style yellow.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.79 and Damage_Percent < 1.0 then
                  configure(Widgt => Label, options => "-text {Very dirty}");
                  Progress_Bar_Style := Null_Unbounded_String;
               else
                  configure(Widgt => Label, options => "-text {Ruined}");
                  Progress_Bar_Style := Null_Unbounded_String;
               end if;
               Progress_Bar :=
                 Create
                   (pathName => Module_Frame & ".clean",
                    options => "-orient horizontal -maximum 1.0 -value {" &
                    Float'Image(Damage_Percent) & "}" &
                    To_String(Source => Progress_Bar_Style));
               Add(Widget => Progress_Bar, Message => "Cleanliness of the selected cabin");
               Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-row 1 -sticky w");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Progress_Bar, Options => "-row 1 -column 1 -sticky we");
               Height :=
                 Height + Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
            end if;
            Progress_Bar :=
              Create
                (pathName => Module_Frame & ".quality",
                 options => "-orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
                 Float'Image(Float(Module.Quality) / 100.0) & "}");
            Add(Widget => Progress_Bar, Message => "Quality of the selected cabin");
            Label :=
              Create
                (pathName => Module_Frame & ".qualitylbl",
                 options => "-text {" & Get_Cabin_Quality(Quality => Module.Quality) & "}");
            Module_Max_Value :=
              Positive
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Max_Value) *
                 1.5);
            if Module.Quality = Module_Max_Value then
               configure
                 (Widgt => Label, options => "-text {" & cget(Widgt => Label, option => "-text") & " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-row 2 -sticky w");
            Tcl.Tk.Ada.Grid.Grid(Slave => Progress_Bar, Options => "-row 2 -column 1 -sticky we");
            Height := Height + Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         when GUN | HARPOON_GUN =>
            Insert
              (Module_Text, "end",
               "{" & LF & "Strength:" &
               (if
                  BaseModules_Container.Element
                    (Container => Modules_List, Index => Module.Proto_Index)
                    .M_Type =
                  GUN
                then Positive'Image(Module.Damage)
                else Positive'Image(Module.Duration)) &
               LF & "Ammunition: }");
            Have_Ammo := False;
            declare
               AmmoIndex: constant Natural :=
                 (if Module.M_Type = GUN then Module.Ammo_Index
                  else Module.Harpoon_Index);
            begin
               if AmmoIndex in
                   Inventory_Container.First_Index
                         (Container => Player_Ship.Cargo) ..
                         Inventory_Container.Last_Index
                           (Container => Player_Ship.Cargo)
                 and then
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => AmmoIndex)
                          .Proto_Index)
                     .I_Type =
                   TinyString_Formal_Container.Element
                     (Container => Items_Types,
                      Index =>
                        BaseModules_Container.Element
                          (Container => Modules_List,
                           Index => Module.Proto_Index)
                          .Value) then
                  Insert
                    (Module_Text, "end",
                     "{" &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index =>
                             Inventory_Container.Element
                               (Container => Player_Ship.Cargo,
                                Index => AmmoIndex)
                               .Proto_Index)
                          .Name) &
                     " (assigned)}");
                  Have_Ammo := True;
               end if;
            end;
            if not Have_Ammo then
               M_Amount := 0;
               Find_Ammo_Info_Loop :
               for I in
                 Objects_Container.First_Index(Container => Items_List) ..
                   Objects_Container.Last_Index(Container => Items_List) loop
                  if Objects_Container.Element
                      (Container => Items_List, Index => I)
                      .I_Type =
                    TinyString_Formal_Container.Element
                      (Container => Items_Types,
                       Index =>
                         BaseModules_Container.Element
                           (Container => Modules_List,
                            Index => Module.Proto_Index)
                           .Value) then
                     if M_Amount > 0 then
                        Insert(Module_Text, "end", "{ or }");
                     end if;
                     Insert
                       (Module_Text, "end",
                        "{" &
                        To_String
                          (Objects_Container.Element
                             (Container => Items_List, Index => I)
                             .Name) &
                        "}" &
                        (if Find_Item(Player_Ship.Cargo, I) > 0 then ""
                         else " [list red]"));
                     M_Amount := M_Amount + 1;
                  end if;
               end loop Find_Ammo_Info_Loop;
            end if;
            Insert
              (Module_Text, "end",
               "{" & LF & "Gunner: " &
               (if Module.Owner(1) > 0 then
                  To_String(Player_Ship.Crew(Module.Owner(1)).Name)
                else "none") &
               "}");
            if Module.M_Type = GUN then
               Insert
                 (Module_Text, "end",
                  "{" & LF & "Max fire rate:" &
                  (if
                     BaseModules_Container.Element
                       (Container => Modules_List, Index => Module.Proto_Index)
                       .Speed >
                     0
                   then
                     Positive'Image
                       (BaseModules_Container.Element
                          (Container => Modules_List,
                           Index => Module.Proto_Index)
                          .Speed) &
                     "/round}"
                   else "1/" &
                     Trim
                       (Integer'Image
                          (abs
                           (BaseModules_Container.Element
                              (Container => Modules_List,
                               Index => Module.Proto_Index)
                              .Speed)),
                        Left) &
                     " rounds}"));
            end if;
         when TURRET =>
            Insert
              (Module_Text, "end",
               "{" & LF & "Weapon: " &
               (if Module.Gun_Index > 0 then
                  To_String(Player_Ship.Modules(Module.Gun_Index).Name)
                else "none") &
               "}");
         when WORKSHOP =>
            Add_Owners_Info("Worker");
            Insert(Module_Text, "end", "{" & LF & "}");
            if Module.Crafting_Index /= Tiny_String.Null_Bounded_String then
               if Length(Module.Crafting_Index) > 6
                 and then Slice(Module.Crafting_Index, 1, 5) = "Study" then
                  Insert
                    (Module_Text, "end",
                     "{Studying " &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index =>
                             Positive'Value
                               (Slice
                                  (Module.Crafting_Index, 7,
                                   Length(Module.Crafting_Index))))
                          .Name) &
                     "}");
               elsif Length(Module.Crafting_Index) > 12
                 and then Slice(Module.Crafting_Index, 1, 11) =
                   "Deconstruct" then
                  Insert
                    (Module_Text, "end",
                     "{Deconstructing " &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index =>
                             Positive'Value
                               (Slice
                                  (Module.Crafting_Index, 13,
                                   Length(Module.Crafting_Index))))
                          .Name) &
                     "}");
               else
                  Insert
                    (Module_Text, "end",
                     "{Manufacturing:" &
                     Positive'Image(Module.Crafting_Amount) & "x " &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index =>
                             Recipes_List
                               (To_Bounded_String
                                  (Source =>
                                     To_String
                                       (Source => Module.Crafting_Index)))
                               .Result_Index)
                          .Name) &
                     "}");
               end if;
               Insert
                 (Module_Text, "end",
                  "{" & LF & "Time to complete current:" &
                  Positive'Image(Module.Crafting_Time) & " mins}");
            else
               Insert(Module_Text, "end", "{Manufacturing: nothing}");
            end if;
         when MEDICAL_ROOM =>
            Add_Owners_Info("Medic");
         when TRAINING_ROOM =>
            Insert
              (Module_Text, "end",
               "{" & LF &
               (if Module.Trained_Skill > 0 then
                  "Set for training " &
                  To_String
                    (SkillsData_Container.Element
                       (Skills_List, Module.Trained_Skill)
                       .Name)
                else "Must be set for training") &
               ".}");
            Add_Owners_Info("Trainee");
         when BATTERING_RAM =>
            Insert
              (Module_Text, "end",
               "Strength:" & Positive'Image(Module.Damage2) & "}");
         when others =>
            null;
      end case;
      Insert
        (Module_Text, "end",
         "{" & LF & "Size:" &
         Natural'Image
           (BaseModules_Container.Element
              (Container => Modules_List, Index => Module.Proto_Index)
              .Size) &
         "}");
      if BaseModules_Container.Element
          (Container => Modules_List, Index => Module.Proto_Index)
          .Description /=
        Short_String.Null_Bounded_String then
         Insert
           (Module_Text, "end",
            "{" & LF & LF &
            To_String
              (BaseModules_Container.Element
                 (Container => Modules_List, Index => Module.Proto_Index)
                 .Description) &
            "}");
      end if;
      if Module.Upgrade_Action /= NONE then
         Module_Info := To_Unbounded_String("Upgrading: ");
         case Module.Upgrade_Action is
            when DURABILITY =>
               Append(Module_Info, "durability");
               Max_Upgrade :=
                 BaseModules_Container.Element
                   (Container => Modules_List, Index => Module.Proto_Index)
                   .Durability;
            when MAX_VALUE =>
               case BaseModules_Container.Element
                 (Container => Modules_List, Index => Module.Proto_Index)
                 .M_Type is
                  when ENGINE =>
                     Append(Module_Info, "power");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value /
                       20;
                  when CABIN =>
                     Append(Module_Info, "quality");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value;
                  when GUN | BATTERING_RAM =>
                     Append(Module_Info, "damage");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value *
                       2;
                  when HULL =>
                     Append(Module_Info, "enlarge");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value *
                       40;
                  when HARPOON_GUN =>
                     Append(Module_Info, "strength");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value *
                       10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case BaseModules_Container.Element
                 (Container => Modules_List, Index => Module.Proto_Index)
                 .M_Type is
                  when ENGINE =>
                     Append(Module_Info, "fuel usage");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Value *
                       20;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Max_Upgrade :=
           Integer
             (Float(Max_Upgrade) *
              Float(New_Game_Settings.Upgrade_Cost_Bonus));
         if Max_Upgrade = 0 then
            Max_Upgrade := 1;
         end if;
         Upgrade_Percent :=
           1.0 - (Float(Module.Upgrade_Progress) / Float(Max_Upgrade));
         Progress_Bar_Style :=
           (if Upgrade_Percent > 0.74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif Upgrade_Percent > 0.24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         Progress_Bar :=
           Create
             (Module_Frame & ".upgrade",
              "-orient horizontal -maximum 1.0 -value {" &
              Float'Image(Upgrade_Percent) & "}" &
              To_String(Progress_Bar_Style));
         Add
           (Progress_Bar, "The progress of the current upgrade of the module");
         Label :=
           Create
             (Module_Frame & ".upgradelbl",
              "-text {" & To_String(Module_Info) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 3 -sticky w");
         Tcl.Tk.Ada.Grid.Grid(Progress_Bar, "-row 3 -column 1 -sticky we");
         Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      end if;
      configure
        (Module_Text,
         "-state disabled -height" &
         Positive'Image
           (Positive'Value(Count(Module_Text, "-displaylines", "0.0", "end")) /
            Positive'Value(Metrics("InterfaceFont", "-linespace")) +
            1));
      Tcl.Tk.Ada.Grid.Grid(Module_Text, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Module_Text, "reqheight"));
      Add_Close_Button
        (Module_Frame & ".button", "Close", "CloseDialog " & Module_Dialog, 2);
      Height :=
        Height +
        Positive'Value
          (Winfo_Get
             (Ttk_Frame'(Get_Widget(Module_Frame & ".button")), "reqheight"));
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (Module_Frame,
         "-height" & Positive'Image(Height) & " -width " &
         Winfo_Get(Module_Text, "reqwidth"));
      Canvas_Create
        (Module_Canvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(Module_Frame));
      configure
        (Module_Canvas,
         "-scrollregion [list " & BBox(Module_Canvas, "all") & "]");
      Height :=
        Height + 15 +
        Positive'Value
          (Winfo_Get
             (Ttk_Frame'(Get_Widget(Module_Dialog & ".header")), "reqheight"));
      declare
         Width: Positive;
      begin
         Width :=
           Positive'Value(Winfo_Get(Module_Text, "reqwidth")) +
           Positive'Value(Winfo_Get(Y_Scroll, "reqwidth")) + 5;
         configure
           (Module_Dialog,
            "-height" & Positive'Image(Height) & " -width" &
            Positive'Image(Width));
      end;
      Show_Dialog
        (Dialog => Module_Dialog, Relative_X => 0.2, Relative_Y => 0.1);
      return TCL_OK;
   end Show_Module_Info_Command;

   -- ****o* SUModules/SUModules.Set_Upgrade_Command
   -- FUNCTION
   -- Set the selected upgrade for the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUpgrade upgradetype moduleindex
   -- upgradetype is type of upgrade to start: 1, 2 or 3. moduleindex is the
   -- index of the player ship module which will be upgraded
   -- SOURCE
   function Set_Upgrade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Upgrade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
   begin
      Start_Upgrading
        (Positive'Value(CArgv.Arg(Argv, 2)),
         Positive'Value(CArgv.Arg(Argv, 1)));
      Update_Orders(Player_Ship);
      Update_Messages;
      Update_Header;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Upgrade_Command;

   -- ****o* SUModules/SUModules.Assign_Module_Command
   -- FUNCTION
   -- Assign member, ammo or skill to module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AssignModule assigntype moduleindex assignindex
   -- assigntype is type of item to assing to module: crew, ammo, skills.
   -- moduleindex is the index of the Player_Ship module to which item will be
   -- assigned. assignindex is the index of the item which will be assigned
   -- to the module
   -- SOURCE
   function Assign_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Assign_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      AssignIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 3));
      Assigned: Boolean;
      procedure UpdateOrder(Order: Crew_Orders) is
      begin
         Give_Orders(Player_Ship, AssignIndex, Order, ModuleIndex);
         if Player_Ship.Crew(AssignIndex).Order /= Order then
            Tcl_SetVar
              (Interp,
               ".moduledialog.canvas.frame.crewbutton" & CArgv.Arg(Argv, 3),
               "0");
         end if;
      end UpdateOrder;
   begin
      if CArgv.Arg(Argv, 1) = "crew" then
         case BaseModules_Container.Element
           (Container => Modules_List,
            Index => Player_Ship.Modules(ModuleIndex).Proto_Index)
           .M_Type is
            when CABIN =>
               Modules_Loop :
               for Module of Player_Ship.Modules loop
                  if Module.M_Type = CABIN then
                     for Owner of Module.Owner loop
                        if Owner = AssignIndex then
                           Owner := 0;
                           exit Modules_Loop;
                        end if;
                     end loop;
                  end if;
               end loop Modules_Loop;
               Assigned := False;
               Check_Assigned_Loop :
               for Owner of Player_Ship.Modules(ModuleIndex).Owner loop
                  if Owner = 0 then
                     Owner := AssignIndex;
                     Assigned := True;
                     exit Check_Assigned_Loop;
                  end if;
               end loop Check_Assigned_Loop;
               if not Assigned then
                  Player_Ship.Modules(ModuleIndex).Owner(1) := AssignIndex;
               end if;
               Add_Message
                 ("You assigned " &
                  To_String(Player_Ship.Modules(ModuleIndex).Name) & " to " &
                  To_String(Player_Ship.Crew(AssignIndex).Name) & ".",
                  ORDERMESSAGE);
            when GUN | HARPOON_GUN =>
               UpdateOrder(GUNNER);
            when ALCHEMY_LAB .. GREENHOUSE =>
               UpdateOrder(CRAFT);
            when MEDICAL_ROOM =>
               UpdateOrder(HEAL);
            when TRAINING_ROOM =>
               UpdateOrder(TRAIN);
            when others =>
               null;
         end case;
         Update_Header;
      elsif CArgv.Arg(Argv, 1) = "ammo" then
         if Player_Ship.Modules(ModuleIndex).M_Type = GUN then
            Player_Ship.Modules(ModuleIndex).Ammo_Index := AssignIndex;
         else
            Player_Ship.Modules(ModuleIndex).Harpoon_Index := AssignIndex;
         end if;
         Add_Message
           ("You assigned " &
            To_String
              (Objects_Container.Element
                 (Container => Items_List,
                  Index =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => AssignIndex)
                      .Proto_Index)
                 .Name) &
            " to " & To_String(Player_Ship.Modules(ModuleIndex).Name) & ".",
            ORDERMESSAGE);
      elsif CArgv.Arg(Argv, 1) = "skill" then
         if Player_Ship.Modules(ModuleIndex).Trained_Skill =
           Skills_Amount_Range(AssignIndex) then
            return TCL_OK;
         end if;
         Player_Ship.Modules(ModuleIndex).Trained_Skill :=
           Skills_Amount_Range(AssignIndex);
         Add_Message
           ("You prepared " &
            To_String(Player_Ship.Modules(ModuleIndex).Name) &
            " for training " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List, Skills_Amount_Range(AssignIndex))
                 .Name) &
            ".",
            ORDERMESSAGE);
      end if;
      Update_Messages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   exception
      when An_Exception : Crew_Order_Error =>
         Show_Message
           (Text => Exception_Message(An_Exception),
            Title => "Can't assign crew");
         return TCL_OK;
   end Assign_Module_Command;

   -- ****o* SUModules/SUModules.Disable_Engine_Command
   -- FUNCTION
   -- Enable or disable selected engine
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DisableEngine engineindex
   -- engineindex is the index of the engine module in the player ship
   -- SOURCE
   function Disable_Engine_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Disable_Engine_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      CanDisable: Boolean := False;
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if not Player_Ship.Modules(ModuleIndex).Disabled then
         Check_Can_Disable_Loop :
         for I in Player_Ship.Modules.Iterate loop
            if Player_Ship.Modules(I).M_Type = ENGINE
              and then
              (not Player_Ship.Modules(I).Disabled and
               Modules_Container.To_Index(I) /= ModuleIndex) then
               CanDisable := True;
               exit Check_Can_Disable_Loop;
            end if;
         end loop Check_Can_Disable_Loop;
         if not CanDisable then
            Show_Message
              (Text =>
                 "You can't disable this engine because it is your last working engine.",
               Title => "Can't disable engine");
            return TCL_OK;
         end if;
         Player_Ship.Modules(ModuleIndex).Disabled := True;
         Add_Message
           ("You disabled " &
            To_String(Player_Ship.Modules(ModuleIndex).Name) & ".",
            ORDERMESSAGE);
      else
         Player_Ship.Modules(ModuleIndex).Disabled := False;
         Add_Message
           ("You enabled " & To_String(Player_Ship.Modules(ModuleIndex).Name) &
            ".",
            ORDERMESSAGE);
      end if;
      Update_Messages;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Disable_Engine_Command;

   -- ****o* SUModules/SUModules.Stop_Upgrading_Command
   -- FUNCTION
   -- Stop the current ship upgrade
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StopUpgrading
   -- SOURCE
   function Stop_Upgrading_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Stop_Upgrading_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      Player_Ship.Upgrade_Module := 0;
      Give_Orders_Loop :
      for I in Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index loop
         if Player_Ship.Crew(I).Order = UPGRADING then
            Give_Orders(Player_Ship, I, REST);
            exit Give_Orders_Loop;
         end if;
      end loop Give_Orders_Loop;
      Add_Message("You stopped current upgrade.", ORDERMESSAGE);
      Update_Messages;
      Update_Header;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Stop_Upgrading_Command;

   -- ****o* SUModules/SUModules.Set_Repair_Command
   -- FUNCTION
   -- Set or remove the repair priority from the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetRepair action
   -- Action can be assing or remove. If assing, then assing the currently
   -- selected module as the repair first, otherwise clear current priority
   -- setting
   -- SOURCE
   function Set_Repair_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Repair_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

   begin
      if CArgv.Arg(Argv, 1) = "assign" then
         Player_Ship.Repair_Module := Positive'Value(CArgv.Arg(Argv, 2));
         Add_Message
           ("You assigned " &
            To_String
              (Player_Ship.Modules(Positive'Value(CArgv.Arg(Argv, 2))).Name) &
            " as repair priority.",
            ORDERMESSAGE);
      else
         Player_Ship.Repair_Module := 0;
         Add_Message("You removed repair priority.", ORDERMESSAGE);
      end if;
      Update_Messages;
      return Show_Ship_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Repair_Command;

   -- ****o* SUModules/SUModules.Reset_Destination_Command
   -- FUNCTION
   -- Reset the current destination point for the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetDestination
   -- SOURCE
   function Reset_Destination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Destination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      Player_Ship.Destination_X := 0;
      Player_Ship.Destination_Y := 0;
      return Show_Ship_Info_Command(ClientData, Interp, 2, Argv);
   end Reset_Destination_Command;

   -- ****o* SUModules/SUModules.Update_Assign_Crew_Command
   -- FUNCTION
   -- Update assign the crew member UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateAssignCrew moduleindex ?crewindex?
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned. Crewindex is the index of the crew member which will be
   -- assigned or removed
   -- SOURCE
   function Update_Assign_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Assign_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Assigned: Natural := 0;
      FrameName: constant String := ".moduledialog.canvas.frame";
      CrewButton: Ttk_CheckButton;
      ButtonName: Unbounded_String;
      CrewIndex: constant Natural :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 0);
      InfoLabel: constant Ttk_Label :=
        Get_Widget(FrameName & ".infolabel", Interp);
   begin
      if Argc = 3 then
         if Tcl_GetVar
             (Interp, FrameName & ".crewbutton" & CArgv.Arg(Argv, 2)) =
           "0" then
            Remove_Owner_Loop :
            for Owner of Player_Ship.Modules(ModuleIndex).Owner loop
               if Owner = CrewIndex then
                  Owner := 0;
                  exit Remove_Owner_Loop;
               end if;
            end loop Remove_Owner_Loop;
            if BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Player_Ship.Modules(ModuleIndex).Proto_Index)
                .M_Type /=
              CABIN then
               Give_Orders(Player_Ship, CrewIndex, REST, 0, False);
            end if;
         elsif Assign_Module_Command
             (ClientData, Interp, 4,
              CArgv.Empty & "AssignModule" & "crew" & CArgv.Arg(Argv, 1) &
              CArgv.Arg(Argv, 2)) /=
           TCL_OK then
            return TCL_ERROR;
         end if;
      end if;
      CrewButton.Interp := Interp;
      Enable_Buttons_Loop :
      for I in Player_Ship.Crew.Iterate loop
         CrewButton.Name :=
           New_String
             (FrameName & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left));
         State(CrewButton, "!disabled");
         configure(CrewButton, "-takefocus 1");
      end loop Enable_Buttons_Loop;
      for Owner of Player_Ship.Modules(ModuleIndex).Owner loop
         if Owner /= 0 then
            Assigned := Assigned + 1;
         end if;
      end loop;
      if Assigned =
        Positive(Player_Ship.Modules(ModuleIndex).Owner.Length) then
         Disable_Buttons_Loop :
         for I in Player_Ship.Crew.Iterate loop
            ButtonName :=
              To_Unbounded_String
                (FrameName & ".crewbutton" &
                 Trim(Positive'Image(Crew_Container.To_Index(I)), Left));
            if Tcl_GetVar(Interp, To_String(ButtonName)) = "0" then
               CrewButton.Name := New_String(To_String(ButtonName));
               State(CrewButton, "disabled");
               configure(CrewButton, "-takefocus 0");
            end if;
         end loop Disable_Buttons_Loop;
      end if;
      if Winfo_Get(InfoLabel, "exists") = "1" then
         configure
           (InfoLabel,
            "-text {Available:" &
            Natural'Image
              (Positive(Player_Ship.Modules(ModuleIndex).Owner.Length) -
               Assigned) &
            "}");
         Update_Header;
         Update_Crew_Info;
      end if;
      return TCL_OK;
   end Update_Assign_Crew_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Crew_Command
   -- FUNCTION
   -- Show assign the crew member UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignCrew moduleindex
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Module: constant Module_Data := Player_Ship.Modules(ModuleIndex);
      ModuleDialog: constant Ttk_Frame :=
        Create_Dialog
          (".moduledialog",
           "Assign a crew member to " & To_String(Module.Name), 250);
      YScroll: constant Ttk_Scrollbar :=
        Create
          (ModuleDialog & ".yscroll",
           "-orient vertical -command [list .moduledialog.canvas yview]");
      CrewCanvas: constant Tk_Canvas :=
        Create
          (ModuleDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      CrewFrame: constant Ttk_Frame := Create(CrewCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (ModuleDialog & ".button",
           "-text Close -command {CloseDialog " & Widget_Image(ModuleDialog) &
           "}");
      Height: Positive := 10;
      Width: Positive := 250;
      CrewButton: Ttk_CheckButton;
      InfoLabel: Ttk_Label;
      Assigned: Natural := 0;
      Recipe: constant Craft_Data :=
        (if Module.M_Type = WORKSHOP then
           Set_Recipe_Data
             (To_Bounded_String
                (Source => To_String(Source => Module.Crafting_Index)))
         else Craft_Data'(others => <>));
   begin
      Tcl.Tk.Ada.Grid.Grid(CrewCanvas, "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, "-sticky ns -padx {0 5} -pady {5 0} -row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-pady {0 5} -columnspan 2");
      Focus(CloseButton);
      Autoscroll(YScroll);
      Load_Crew_List_Loop :
      for I in Player_Ship.Crew.Iterate loop
         CrewButton :=
           Create
             (CrewFrame & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(Player_Ship.Crew(I).Name) &
              (if Module.M_Type = WORKSHOP then
                 Get_Skill_Marks(Recipe.Skill, Crew_Container.To_Index(I))
               else "") &
              "} -command {UpdateAssignCrew" & Positive'Image(ModuleIndex) &
              Positive'Image(Crew_Container.To_Index(I)) & "}");
         Tcl_SetVar(Interp, Widget_Image(CrewButton), "0");
         Count_Assigned_Loop :
         for Owner of Module.Owner loop
            if Owner = Crew_Container.To_Index(I) then
               Tcl_SetVar(Interp, Widget_Image(CrewButton), "1");
               Assigned := Assigned + 1;
               exit Count_Assigned_Loop;
            end if;
         end loop Count_Assigned_Loop;
         Tcl.Tk.Ada.Pack.Pack(CrewButton, "-anchor w");
         Height := Height + Positive'Value(Winfo_Get(CrewButton, "reqheight"));
         if Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10 > Width then
            Width := Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10;
         end if;
         Bind(CrewButton, "<Escape>", "{" & CloseButton & " invoke;break}");
         Bind
           (CrewButton, "<Tab>",
            "{focus [GetActiveButton" &
            Positive'Image(Crew_Container.To_Index(I)) & "];break}");
      end loop Load_Crew_List_Loop;
      if Update_Assign_Crew_Command(ClientData, Interp, Argc, Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      InfoLabel :=
        Create
          (CrewFrame & ".infolabel",
           "-text {Available:" &
           Natural'Image(Positive(Module.Owner.Length) - Assigned) & "}");
      Tcl.Tk.Ada.Pack.Pack(InfoLabel);
      Height := Height + Positive'Value(Winfo_Get(InfoLabel, "reqheight"));
      if Positive'Value(Winfo_Get(InfoLabel, "reqwidth")) > Width then
         Width := Positive'Value(Winfo_Get(InfoLabel, "reqwidth"));
      end if;
      if Height > 500 then
         Height := 500;
      end if;
      Canvas_Create
        (CrewCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(CrewFrame));
      Tcl_Eval(Interp, "update");
      configure
        (CrewCanvas,
         "-scrollregion [list " & BBox(CrewCanvas, "all") & "] -height" &
         Positive'Image(Height) & " -width" & Positive'Image(Width));
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Tab>", "{focus [GetActiveButton 0];break}");
      Show_Dialog(Dialog => ModuleDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Assign_Crew_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Skill_Command
   -- FUNCTION
   -- Show assign the skill UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignSkill moduleindex
   -- Moduleindex is the index of the module to which a new skill will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ModuleDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".moduledialog",
           Title =>
             "Assign skill to " &
             To_String(Player_Ship.Modules(ModuleIndex).Name),
           Title_Width => 400);
      SkillsFrame: constant Ttk_Frame := Create(ModuleDialog & ".frame");
      SkillName, ToolColor: Unbounded_String;
      ProtoIndex: Objects_Container.Extended_Index;
      ToolName: Bounded_String;
      SkillsTable: Table_Widget (2) :=
        Create_Table
          (Widget_Image(SkillsFrame),
           (To_Unbounded_String("Skill"),
            To_Unbounded_String("Training tool")));
   begin
      Load_Skills_List_Loop :
      for I in 1 .. Skills_Amount loop
         if SkillsData_Container.Element(Skills_List, I).Tool /=
           Null_Bounded_String then
            ProtoIndex :=
              Find_Proto_Item
                (Item_Type =>
                   SkillsData_Container.Element(Skills_List, I).Tool);
            ToolName :=
              (if
                 Objects_Container.Element
                   (Container => Items_List, Index => ProtoIndex)
                   .Show_Type /=
                 Null_Bounded_String
               then
                 Objects_Container.Element
                   (Container => Items_List, Index => ProtoIndex)
                   .Show_Type
               else Objects_Container.Element
                   (Container => Items_List, Index => ProtoIndex)
                   .I_Type);
         end if;
         SkillName :=
           To_Unbounded_String
             (To_String(SkillsData_Container.Element(Skills_List, I).Name));
         ToolColor := To_Unbounded_String("green");
         if Get_Item_Amount
             (Objects_Container.Element
                (Container => Items_List, Index => ProtoIndex)
                .I_Type) =
           0 then
            Append(SkillName, " (no tool)");
            ToolColor := To_Unbounded_String("red");
         end if;
         Add_Button
           (SkillsTable, To_String(SkillName),
            "Press mouse " &
            (if Game_Settings.Right_Button then "right" else "left") &
            " button to set as trained skill",
            "AssignModule skill" & Positive'Image(ModuleIndex) &
            Skills_Amount_Range'Image(I),
            1);
         Add_Button
           (SkillsTable, To_String(ToolName),
            "Press mouse " &
            (if Game_Settings.Right_Button then "right" else "left") &
            " button to set as trained skill",
            "AssignModule skill" & Positive'Image(ModuleIndex) &
            Skills_Amount_Range'Image(I),
            2, True, To_String(ToolColor));
      end loop Load_Skills_List_Loop;
      Update_Table(SkillsTable);
      Tcl.Tk.Ada.Grid.Grid(SkillsFrame, "-padx 2");
      Tcl_Eval(Get_Context, "update");
      configure
        (SkillsTable.Canvas,
         "-scrollregion [list " & BBox(SkillsTable.Canvas, "all") & "]");
      Xview_Move_To(SkillsTable.Canvas, "0.0");
      Yview_Move_To(SkillsTable.Canvas, "0.0");
      Add_Close_Button
        (ModuleDialog & ".button", "Close", "CloseDialog " & ModuleDialog);
      Show_Dialog(Dialog => ModuleDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Assign_Skill_Command;

   -- ****o* SUModules/SUModules.Cancel_Order_Command
   -- FUNCTION
   -- Cancel the current crafting order
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Cancel moduleindex
   -- Moduleindex is the index of the module which the crafting order will
   -- be canceled
   -- SOURCE
   function Cancel_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      Player_Ship.Modules(ModuleIndex).Crafting_Index := Null_Bounded_String;
      Player_Ship.Modules(ModuleIndex).Crafting_Amount := 0;
      Player_Ship.Modules(ModuleIndex).Crafting_Time := 0;
      Give_Orders_Loop :
      for Owner of Player_Ship.Modules(ModuleIndex).Owner loop
         if Owner > 0 then
            Give_Orders(Player_Ship, Owner, REST);
         end if;
      end loop Give_Orders_Loop;
      Add_Message
        ("You cancelled crafting order in " &
         To_String(Player_Ship.Modules(ModuleIndex).Name) & ".",
         CRAFTMESSAGE, RED);
      Update_Messages;
      Update_Header;
      Update_Crew_Info;
      return TCL_OK;
   end Cancel_Order_Command;

   -- ****o* SUModules/SUModules.Get_Active_Button_Command
   -- FUNCTION
   -- Get the next active button in assing crew dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetActiveButton crewindex
   -- Crewindex is the index of the crew member which is currently selected
   -- or 0 for close button
   -- SOURCE
   function Get_Active_Button_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_Active_Button_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      CrewIndex: constant Natural := Natural'Value(CArgv.Arg(Argv, 1));
      ButtonName: Unbounded_String;
      Button: Ttk_CheckButton;
   begin
      Find_Active_Button_Loop :
      for I in Player_Ship.Crew.Iterate loop
         ButtonName :=
           To_Unbounded_String
             (".moduledialog.canvas.frame.crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left));
         Button := Get_Widget(To_String(ButtonName), Interp);
         exit Find_Active_Button_Loop when InState(Button, "disabled") =
           "0" and
           Crew_Container.To_Index(I) > CrewIndex;
         ButtonName := Null_Unbounded_String;
      end loop Find_Active_Button_Loop;
      if ButtonName = Null_Unbounded_String then
         ButtonName := To_Unbounded_String(".moduledialog.button");
      end if;
      Button := Get_Widget(To_String(ButtonName), Interp);
      Focus(Button);
      return TCL_OK;
   end Get_Active_Button_Command;

   procedure Update_Modules_Info(Page: Positive := 1) is
      use Tiny_String;

      ShipCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".shipinfoframe.modules.canvas");
      ShipInfoFrame: constant Ttk_Frame := Get_Widget(ShipCanvas & ".frame");
      Row: Positive := 2;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      if Modules_Table.Row_Height = 1 then
         Modules_Table :=
           Create_Table
             (Widget_Image(ShipInfoFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Durability")),
              Get_Widget(Main_Paned & ".shipinfoframe.modules.scrolly"),
              "SortShipModules", "Press mouse button to sort the modules.");
      end if;
      if Modules_Indexes.Length /= Player_Ship.Modules.Length then
         Modules_Indexes.Clear;
         for I in Player_Ship.Modules.Iterate loop
            Modules_Indexes.Append(Modules_Container.To_Index(I));
         end loop;
      end if;
      Clear_Table(Modules_Table);
      Show_Modules_Menu_Loop :
      for Module_Index of Modules_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Add_Button
           (Modules_Table, To_String(Player_Ship.Modules(Module_Index).Name),
            "Show available module's options",
            "ShowModuleMenu" & Positive'Image(Module_Index), 1);
         Add_Progress_Bar
           (Modules_Table, Player_Ship.Modules(Module_Index).Durability,
            Player_Ship.Modules(Module_Index).Max_Durability,
            "Show available module's options",
            "ShowModuleMenu" & Positive'Image(Module_Index), 2, True);
         Row := Row + 1;
         exit Show_Modules_Menu_Loop when Modules_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Show_Modules_Menu_Loop;
      if Page > 1 then
         if Modules_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Modules_Table, "ShowModules" & Positive'Image(Page - 1), "");
         else
            Add_Pagination
              (Modules_Table, "ShowModules" & Positive'Image(Page - 1),
               "ShowModules" & Positive'Image(Page + 1));
         end if;
      elsif Modules_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Modules_Table, "", "ShowModules" & Positive'Image(Page + 1));
      end if;
      Update_Table(Modules_Table);
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
   end Update_Modules_Info;

   -- ****o* SUModules/SUModules.Show_Modules_Command
   -- FUNCTION
   -- Show the list of the player's ship modules to a player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModules ?page?
   -- Page parameter is a index of page from which starts showing
   -- modules.
   -- SOURCE
   function Show_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      Update_Modules_Info(Positive'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end Show_Modules_Command;

   -- ****it* SUModules/SUModules.Modules_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the ship modules list
   -- OPTIONS
   -- NAMEASC    - Sort modules by name ascending
   -- NAMEDESC   - Sort modules by name descending
   -- DAMAGEASC  - Sort modules by damage ascending
   -- DAMAGEDESC - Sort modules by damage descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Modules_Sort_Orders is
     (NAMEASC, NAMEDESC, DAMAGEASC, DAMAGEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUModules/SUModules.Default_Modules_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's modules
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Modules_Sort_Order: constant Modules_Sort_Orders := NONE;
   -- ****

   -- ****iv* SUModules/SUModules.Modules_Sort_Order
   -- FUNCTION
   -- The current sorting order for modules list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****

   -- ****o* SUModules/SUModules.Sort_Modules_Command
   -- FUNCTION
   -- Sort the player's ship's modules list
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Modules_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Module_Data is record
         Name: Bounded_String;
         Damage: Float;
         Id: Positive;
      end record;
      type Modules_Array is array(Positive range <>) of Local_Module_Data;
      Local_Modules: Modules_Array(1 .. Positive(Player_Ship.Modules.Length));
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Modules_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Modules_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Modules_Sort_Order = DAMAGEASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Modules_Sort_Order = DAMAGEDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Modules is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Modules_Array);
   begin
      case Column is
         when 1 =>
            if Modules_Sort_Order = NAMEASC then
               Modules_Sort_Order := NAMEDESC;
            else
               Modules_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Modules_Sort_Order = DAMAGEASC then
               Modules_Sort_Order := DAMAGEDESC;
            else
               Modules_Sort_Order := DAMAGEASC;
            end if;
         when others =>
            null;
      end case;
      if Modules_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Player_Ship.Modules.Iterate loop
         Local_Modules(Modules_Container.To_Index(I)) :=
           (Name => Player_Ship.Modules(I).Name,
            Damage =>
              Float(Player_Ship.Modules(I).Durability) /
              Float(Player_Ship.Modules(I).Max_Durability),
            Id => Modules_Container.To_Index(I));
      end loop;
      Sort_Modules(Local_Modules);
      Modules_Indexes.Clear;
      for Module of Local_Modules loop
         Modules_Indexes.Append(Module.Id);
      end loop;
      Update_Modules_Info;
      return TCL_OK;
   end Sort_Modules_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Ammo_Command
   -- FUNCTION
   -- Show the list of available ammo for the selected gun
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssingAmmo index
   -- Index is the module index of the selected gun which will be have
   -- assigned a new ammo
   -- SOURCE
   function Show_Assign_Ammo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Ammo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      ModuleIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      AmmoIndex: constant Natural :=
        (if Player_Ship.Modules(ModuleIndex).M_Type = GUN then
           Player_Ship.Modules(ModuleIndex).Ammo_Index
         else Player_Ship.Modules(ModuleIndex).Harpoon_Index);
      Ammo_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".ammomenu", Title => "Available ammo", Parent_Name => ".");
      Row: Positive := 1;
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Ammo_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Ammo_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Ammo_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Ammo_Menu & ".ammo1;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Find_Ammo_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         if Objects_Container.Element
             (Container => Items_List,
              Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Proto_Index)
             .I_Type =
           TinyString_Formal_Container.Element
             (Container => Items_Types,
              Index =>
                BaseModules_Container.Element
                  (Container => Modules_List,
                   Index => Player_Ship.Modules(ModuleIndex).Proto_Index)
                  .Value) and
           I /= AmmoIndex then
            Add_Button
              (Name => ".ammo" & Trim(Positive'Image(Row), Left),
               Label =>
                 To_String
                   (Objects_Container.Element
                      (Container => Items_List,
                       Index =>
                         Inventory_Container.Element
                           (Container => Player_Ship.Cargo, Index => I)
                           .Proto_Index)
                      .Name),
               Command =>
                 "AssignModule ammo " & CArgv.Arg(Argv => Argv, N => 1) &
                 Positive'Image(I));
            Row := Row + 1;
         end if;
      end loop Find_Ammo_Loop;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Ammo_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Assign_Ammo_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowModuleMenu", Show_Module_Menu_Command'Access);
      Add_Command("ShowModuleInfo", Show_Module_Info_Command'Access);
      Add_Command("SetUpgrade", Set_Upgrade_Command'Access);
      Add_Command("AssignModule", Assign_Module_Command'Access);
      Add_Command("DisableEngine", Disable_Engine_Command'Access);
      Add_Command("StopUpgrading", Stop_Upgrading_Command'Access);
      Add_Command("SetRepair", Set_Repair_Command'Access);
      Add_Command("ResetDestination", Reset_Destination_Command'Access);
      Add_Command("ShowAssignCrew", Show_Assign_Crew_Command'Access);
      Add_Command("UpdateAssignCrew", Update_Assign_Crew_Command'Access);
      Add_Command("ShowAssignSkill", Show_Assign_Skill_Command'Access);
      Add_Command("CancelOrder", Cancel_Order_Command'Access);
      Add_Command("GetActiveButton", Get_Active_Button_Command'Access);
      Add_Command("ShowModules", Show_Modules_Command'Access);
      Add_Command("SortShipModules", Sort_Modules_Command'Access);
      Add_Command("ShowAssignAmmo", Show_Assign_Ammo_Command'Access);
   end Add_Commands;

end Ships.UI.Modules;
