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
           " {Renaming the module} {Rename}");
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
               Insert
                 (TextWidget => Module_Text, Index => "end",
                  Text => "{ (max upgrade)}");
            end if;
         when CARGO_ROOM =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF & "Max cargo:" &
                 Integer'Image
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Max_Value) &
                 " kg}");
         when HULL =>
            Label :=
              Create
                (pathName => Module_Frame & ".modules",
                 options =>
                   "-text {Modules installed:" &
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
                 (Widgt => Label,
                  options =>
                    "-text {" & cget(Widgt => Label, option => "-text") &
                    " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-sticky w");
            Height :=
              Height +
              Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
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
                    options =>
                      "-orient horizontal -maximum 1.0 -value {" &
                      Float'Image(Damage_Percent) & "}" &
                      To_String(Source => Progress_Bar_Style));
               Add
                 (Widget => Progress_Bar,
                  Message => "Cleanliness of the selected cabin");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Label, Options => "-row 1 -sticky w");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Progress_Bar,
                  Options => "-row 1 -column 1 -sticky we");
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Label, Info => "reqheight"));
            end if;
            Progress_Bar :=
              Create
                (pathName => Module_Frame & ".quality",
                 options =>
                   "-orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
                   Float'Image(Float(Module.Quality) / 100.0) & "}");
            Add
              (Widget => Progress_Bar,
               Message => "Quality of the selected cabin");
            Label :=
              Create
                (pathName => Module_Frame & ".qualitylbl",
                 options =>
                   "-text {" & Get_Cabin_Quality(Quality => Module.Quality) &
                   "}");
            Module_Max_Value :=
              Positive
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Max_Value) *
                 1.5);
            if Module.Quality = Module_Max_Value then
               configure
                 (Widgt => Label,
                  options =>
                    "-text {" & cget(Widgt => Label, option => "-text") &
                    " (max upgrade)}");
            end if;
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Label, Options => "-row 2 -sticky w");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Bar,
               Options => "-row 2 -column 1 -sticky we");
            Height :=
              Height +
              Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         when GUN | HARPOON_GUN =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
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
            Find_Ammo_Block :
            declare
               Ammo_Index: constant Natural :=
                 (if Module.M_Type = GUN then Module.Ammo_Index
                  else Module.Harpoon_Index);
            begin
               if Ammo_Index in
                   Inventory_Container.First_Index
                         (Container => Player_Ship.Cargo) ..
                         Inventory_Container.Last_Index
                           (Container => Player_Ship.Cargo)
                 and then
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => Ammo_Index)
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
                    (TextWidget => Module_Text, Index => "end",
                     Text =>
                       "{" &
                       To_String
                         (Source =>
                            Objects_Container.Element
                              (Container => Items_List,
                               Index =>
                                 Inventory_Container.Element
                                   (Container => Player_Ship.Cargo,
                                    Index => Ammo_Index)
                                   .Proto_Index)
                              .Name) &
                       " (assigned)}");
                  Have_Ammo := True;
               end if;
            end Find_Ammo_Block;
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
                        Insert
                          (TextWidget => Module_Text, Index => "end",
                           Text => "{ or }");
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
                                Proto_Index => I) >
                             0
                           then ""
                           else " [list red]"));
                     M_Amount := M_Amount + 1;
                  end if;
               end loop Find_Ammo_Info_Loop;
            end if;
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF & "Gunner: " &
                 (if Module.Owner(1) > 0 then
                    To_String(Source => Player_Ship.Crew(Module.Owner(1)).Name)
                  else "none") &
                 "}");
            if Module.M_Type = GUN then
               Insert
                 (TextWidget => Module_Text, Index => "end",
                  Text =>
                    "{" & LF & "Max fire rate:" &
                    (if
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
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
                         (Source =>
                            Integer'Image
                              (abs
                               (BaseModules_Container.Element
                                  (Container => Modules_List,
                                   Index => Module.Proto_Index)
                                  .Speed)),
                          Side => Left) &
                       " rounds}"));
            end if;
         when TURRET =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF & "Weapon: " &
                 (if Module.Gun_Index > 0 then
                    To_String
                      (Source => Player_Ship.Modules(Module.Gun_Index).Name)
                  else "none") &
                 "}");
         when WORKSHOP =>
            Add_Owners_Info(Owners_Name => "Worker");
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text => "{" & LF & "}");
            Show_Order_Info_Block :
            declare
               Recipe_Name: constant String :=
                 Get_Workshop_Recipe_Name(Workshop => Module_Index);
            begin
               if Recipe_Name'Length > 0 then
                  Insert
                    (TextWidget => Module_Text, Index => "end",
                     Text => "{" & Recipe_Name & "}");
                  Insert
                    (TextWidget => Module_Text, Index => "end",
                     Text =>
                       "{" & LF & "Time to complete current:" &
                       Positive'Image(Module.Crafting_Time) & " mins}");
               else
                  Insert
                    (TextWidget => Module_Text, Index => "end",
                     Text => "{Manufacturing: nothing}");
               end if;
            end Show_Order_Info_Block;
         when MEDICAL_ROOM =>
            Add_Owners_Info(Owners_Name => "Medic");
         when TRAINING_ROOM =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & LF &
                 (if Module.Trained_Skill > 0 then
                    "Set for training " &
                    To_String
                      (Source =>
                         SkillsData_Container.Element
                           (Container => Skills_List,
                            Index => Module.Trained_Skill)
                           .Name)
                  else "Must be set for training") &
                 ".}");
            Add_Owners_Info(Owners_Name => "Trainee");
         when BATTERING_RAM =>
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text => "Strength:" & Positive'Image(Module.Damage2) & "}");
         when others =>
            null;
      end case;
      Insert
        (TextWidget => Module_Text, Index => "end",
         Text =>
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
           (TextWidget => Module_Text, Index => "end",
            Text =>
              "{" & LF & LF &
              To_String
                (Source =>
                   BaseModules_Container.Element
                     (Container => Modules_List, Index => Module.Proto_Index)
                     .Description) &
              "}");
      end if;
      if Module.Upgrade_Action /= NONE then
         Module_Info := To_Unbounded_String(Source => "Upgrading: ");
         case Module.Upgrade_Action is
            when DURABILITY =>
               Append(Source => Module_Info, New_Item => "durability");
               Max_Upgrade :=
                 BaseModules_Container.Element
                   (Container => Modules_List, Index => Module.Proto_Index)
                   .Durability;
            when MAX_VALUE =>
               case BaseModules_Container.Element
                 (Container => Modules_List, Index => Module.Proto_Index)
                 .M_Type is
                  when ENGINE =>
                     Append(Source => Module_Info, New_Item => "power");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value /
                       20;
                  when CABIN =>
                     Append(Source => Module_Info, New_Item => "quality");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value;
                  when GUN | BATTERING_RAM =>
                     Append(Source => Module_Info, New_Item => "damage");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value *
                       2;
                  when HULL =>
                     Append(Source => Module_Info, New_Item => "enlarge");
                     Max_Upgrade :=
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index => Module.Proto_Index)
                         .Max_Value *
                       40;
                  when HARPOON_GUN =>
                     Append(Source => Module_Info, New_Item => "strength");
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
                     Append(Source => Module_Info, New_Item => "fuel usage");
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
              To_Unbounded_String
                (Source => " -style green.Horizontal.TProgressbar")
            elsif Upgrade_Percent > 0.24 then
              To_Unbounded_String
                (Source => " -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String
                (Source => " -style Horizontal.TProgressbar"));
         Progress_Bar :=
           Create
             (pathName => Module_Frame & ".upgrade",
              options =>
                "-orient horizontal -maximum 1.0 -value {" &
                Float'Image(Upgrade_Percent) & "}" &
                To_String(Source => Progress_Bar_Style));
         Add
           (Widget => Progress_Bar,
            Message => "The progress of the current upgrade of the module");
         Label :=
           Create
             (pathName => Module_Frame & ".upgradelbl",
              options => "-text {" & To_String(Source => Module_Info) & "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-row 3 -sticky w");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar, Options => "-row 3 -column 1 -sticky we");
         Height :=
           Height +
           Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
      end if;
      configure
        (Widgt => Module_Text,
         options =>
           "-state disabled -height" &
           Positive'Image
             (Positive'Value
                (Count
                   (TextWidget => Module_Text, Options => "-displaylines",
                    Index1 => "0.0", Index2 => "end")) /
              Positive'Value
                (Metrics(Font => "InterfaceFont", Option => "-linespace")) +
              1));
      Tcl.Tk.Ada.Grid.Grid(Slave => Module_Text, Options => "-columnspan 2");
      Height :=
        Height +
        Positive'Value(Winfo_Get(Widgt => Module_Text, Info => "reqheight"));
      Add_Close_Button
        (Name => Module_Frame & ".button", Text => "Close",
         Command => "CloseDialog " & Module_Dialog, Column_Span => 2,
         Row => 2);
      Height :=
        Height +
        Positive'Value
          (Winfo_Get
             (Widgt =>
                Ttk_Frame'(Get_Widget(pathName => Module_Frame & ".button")),
              Info => "reqheight"));
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (Widgt => Module_Frame,
         options =>
           "-height" & Positive'Image(Height) & " -width " &
           Winfo_Get(Widgt => Module_Text, Info => "reqwidth"));
      Canvas_Create
        (Parent => Module_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Module_Frame));
      configure
        (Widgt => Module_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Module_Canvas, TagOrId => "all") & "]");
      Height :=
        Height + 15 +
        Positive'Value
          (Winfo_Get
             (Widgt =>
                Ttk_Frame'(Get_Widget(pathName => Module_Dialog & ".header")),
              Info => "reqheight"));
      Set_Width_Block :
      declare
         Width: Positive;
      begin
         Width :=
           Positive'Value
             (Winfo_Get(Widgt => Module_Text, Info => "reqwidth")) +
           Positive'Value(Winfo_Get(Widgt => Y_Scroll, Info => "reqwidth")) +
           5;
         configure
           (Widgt => Module_Dialog,
            options =>
              "-height" & Positive'Image(Height) & " -width" &
              Positive'Image(Width));
      end Set_Width_Block;
      Show_Dialog
        (Dialog => Module_Dialog, Relative_X => 0.2, Relative_Y => 0.1);
      return TCL_OK;
   end Show_Module_Info_Command;

   -- ****o* SUModules/SUModules.Set_Upgrade_Command
   -- FUNCTION
   -- Set the selected upgrade for the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUpgrade upgradetype moduleindex
   -- upgradetype is type of upgrade to start: 1, 2 or 3. moduleindex is the
   -- index of the player ship module which will be upgraded
   -- SOURCE
   function Set_Upgrade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Upgrade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
   begin
      Start_Upgrading
        (Module_Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 2)),
         Upgrade_Type => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      Update_Orders(Ship => Player_Ship);
      Update_Messages;
      Update_Header;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Set_Upgrade_Command;

   -- ****o* SUModules/SUModules.Assign_Module_Command
   -- FUNCTION
   -- Assign member, ammo or skill to module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Assign_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Assign_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 3));
      Assigned: Boolean;
      procedure Update_Order(Order: Crew_Orders) is
      begin
         Give_Orders
           (Ship => Player_Ship, Member_Index => Assign_Index,
            Given_Order => Order, Module_Index => Module_Index);
         if Player_Ship.Crew(Assign_Index).Order /= Order then
            Tcl_SetVar
              (interp => Interp,
               varName =>
                 ".moduledialog.canvas.frame.crewbutton" &
                 CArgv.Arg(Argv => Argv, N => 3),
               newValue => "0");
         end if;
      end Update_Order;
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "crew" then
         case BaseModules_Container.Element
           (Container => Modules_List,
            Index => Player_Ship.Modules(Module_Index).Proto_Index)
           .M_Type is
            when CABIN =>
               Modules_Loop :
               for Module of Player_Ship.Modules loop
                  if Module.M_Type = CABIN then
                     Find_Owner_Loop :
                     for Owner of Module.Owner loop
                        if Owner = Assign_Index then
                           Owner := 0;
                           exit Modules_Loop;
                        end if;
                     end loop Find_Owner_Loop;
                  end if;
               end loop Modules_Loop;
               Assigned := False;
               Check_Assigned_Loop :
               for Owner of Player_Ship.Modules(Module_Index).Owner loop
                  if Owner = 0 then
                     Owner := Assign_Index;
                     Assigned := True;
                     exit Check_Assigned_Loop;
                  end if;
               end loop Check_Assigned_Loop;
               if not Assigned then
                  Player_Ship.Modules(Module_Index).Owner(1) := Assign_Index;
               end if;
               Add_Message
                 (Message =>
                    "You assigned " &
                    To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    " to " &
                    To_String(Source => Player_Ship.Crew(Assign_Index).Name) &
                    ".",
                  M_Type => ORDERMESSAGE);
            when GUN | HARPOON_GUN =>
               Update_Order(Order => GUNNER);
            when ALCHEMY_LAB .. GREENHOUSE =>
               Update_Order(Order => CRAFT);
            when MEDICAL_ROOM =>
               Update_Order(Order => HEAL);
            when TRAINING_ROOM =>
               Update_Order(Order => TRAIN);
            when others =>
               null;
         end case;
         Update_Header;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "ammo" then
         if Player_Ship.Modules(Module_Index).M_Type = GUN then
            Player_Ship.Modules(Module_Index).Ammo_Index := Assign_Index;
         else
            Player_Ship.Modules(Module_Index).Harpoon_Index := Assign_Index;
         end if;
         Add_Message
           (Message =>
              "You assigned " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo,
                           Index => Assign_Index)
                          .Proto_Index)
                     .Name) &
              " to " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              ".",
            M_Type => ORDERMESSAGE);
      elsif CArgv.Arg(Argv => Argv, N => 1) = "skill" then
         if Player_Ship.Modules(Module_Index).Trained_Skill =
           Skills_Amount_Range(Assign_Index) then
            return TCL_OK;
         end if;
         Player_Ship.Modules(Module_Index).Trained_Skill :=
           Skills_Amount_Range(Assign_Index);
         Add_Message
           (Message =>
              "You prepared " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              " for training " &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List,
                      Index => Skills_Amount_Range(Assign_Index))
                     .Name) &
              ".",
            M_Type => ORDERMESSAGE);
         Update_Messages;
         return TCL_OK;
      end if;
      Update_Messages;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   exception
      when An_Exception : Crew_Order_Error =>
         Show_Message
           (Text => Exception_Message(X => An_Exception),
            Title => "Can't assign crew");
         return TCL_OK;
   end Assign_Module_Command;

   -- ****o* SUModules/SUModules.Disable_Engine_Command
   -- FUNCTION
   -- Enable or disable selected engine
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DisableEngine engineindex
   -- engineindex is the index of the engine module in the player ship
   -- SOURCE
   function Disable_Engine_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Disable_Engine_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Can_Disable: Boolean := False;
      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      if not Player_Ship.Modules(Module_Index).Disabled then
         Check_Can_Disable_Loop :
         for I in Player_Ship.Modules.Iterate loop
            if Player_Ship.Modules(I).M_Type = ENGINE
              and then
              (not Player_Ship.Modules(I).Disabled and
               Modules_Container.To_Index(Position => I) /= Module_Index) then
               Can_Disable := True;
               exit Check_Can_Disable_Loop;
            end if;
         end loop Check_Can_Disable_Loop;
         if not Can_Disable then
            Show_Message
              (Text =>
                 "You can't disable this engine because it is your last working engine.",
               Title => "Can't disable engine");
            return TCL_OK;
         end if;
         Player_Ship.Modules(Module_Index).Disabled := True;
         Add_Message
           (Message =>
              "You disabled " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              ".",
            M_Type => ORDERMESSAGE);
      else
         Player_Ship.Modules(Module_Index).Disabled := False;
         Add_Message
           (Message =>
              "You enabled " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              ".",
            M_Type => ORDERMESSAGE);
      end if;
      Update_Messages;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => Argv);
   end Disable_Engine_Command;

   -- ****o* SUModules/SUModules.Stop_Upgrading_Command
   -- FUNCTION
   -- Stop the current ship upgrade
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StopUpgrading
   -- SOURCE
   function Stop_Upgrading_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Stop_Upgrading_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      Player_Ship.Upgrade_Module := 0;
      Give_Orders_Loop :
      for I in Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index loop
         if Player_Ship.Crew(I).Order = UPGRADING then
            Give_Orders
              (Ship => Player_Ship, Member_Index => I, Given_Order => REST);
            exit Give_Orders_Loop;
         end if;
      end loop Give_Orders_Loop;
      Add_Message
        (Message => "You stopped current upgrade.", M_Type => ORDERMESSAGE);
      Update_Messages;
      Update_Header;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => Argv);
   end Stop_Upgrading_Command;

   -- ****o* SUModules/SUModules.Set_Repair_Command
   -- FUNCTION
   -- Set or remove the repair priority from the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetRepair action
   -- Action can be assing or remove. If assing, then assing the currently
   -- selected module as the repair first, otherwise clear current priority
   -- setting
   -- SOURCE
   function Set_Repair_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Repair_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "assign" then
         Player_Ship.Repair_Module :=
           Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
         Add_Message
           (Message =>
              "You assigned " &
              To_String
                (Source =>
                   Player_Ship.Modules
                     (Positive'Value(CArgv.Arg(Argv => Argv, N => 2)))
                     .Name) &
              " as repair priority.",
            M_Type => ORDERMESSAGE);
      else
         Player_Ship.Repair_Module := 0;
         Add_Message
           (Message => "You removed repair priority.", M_Type => ORDERMESSAGE);
      end if;
      Update_Messages;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Set_Repair_Command;

   -- ****o* SUModules/SUModules.Reset_Destination_Command
   -- FUNCTION
   -- Reset the current destination point for the player's ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetDestination
   -- SOURCE
   function Reset_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      Player_Ship.Destination_X := 0;
      Player_Ship.Destination_Y := 0;
      return
        Show_Ship_Info_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => Argv);
   end Reset_Destination_Command;

   -- ****o* SUModules/SUModules.Update_Assign_Crew_Command
   -- FUNCTION
   -- Update assign the crew member UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateAssignCrew moduleindex ?crewindex?
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned. Crewindex is the index of the crew member which will be
   -- assigned or removed
   -- SOURCE
   function Update_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Assigned: Natural := 0;
      Frame_Name: constant String := ".moduledialog.canvas.frame";
      Crew_Button: Ttk_CheckButton;
      Button_Name: Unbounded_String;
      Crew_Index: constant Natural :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv => Argv, N => 2))
         else 0);
      Info_Label: constant Ttk_Label :=
        Get_Widget(pathName => Frame_Name & ".infolabel", Interp => Interp);
   begin
      if Argc = 3 then
         if Tcl_GetVar
             (interp => Interp,
              varName =>
                Frame_Name & ".crewbutton" & CArgv.Arg(Argv => Argv, N => 2)) =
           "0" then
            Remove_Owner_Loop :
            for Owner of Player_Ship.Modules(Module_Index).Owner loop
               if Owner = Crew_Index then
                  Owner := 0;
                  exit Remove_Owner_Loop;
               end if;
            end loop Remove_Owner_Loop;
            if BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Player_Ship.Modules(Module_Index).Proto_Index)
                .M_Type /=
              CABIN then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST, Module_Index => 0,
                  Check_Priorities => False);
            end if;
         elsif Assign_Module_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 4,
              Argv =>
                CArgv.Empty & "AssignModule" & "crew" &
                CArgv.Arg(Argv => Argv, N => 1) &
                CArgv.Arg(Argv => Argv, N => 2)) /=
           TCL_OK then
            return TCL_ERROR;
         end if;
      end if;
      Crew_Button.Interp := Interp;
      Enable_Buttons_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Crew_Button.Name :=
           New_String
             (Str =>
                Frame_Name & ".crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left));
         State(Widget => Crew_Button, StateSpec => "!disabled");
         configure(Widgt => Crew_Button, options => "-takefocus 1");
      end loop Enable_Buttons_Loop;
      Count_Owners_Loop :
      for Owner of Player_Ship.Modules(Module_Index).Owner loop
         if Owner /= 0 then
            Assigned := Assigned + 1;
         end if;
      end loop Count_Owners_Loop;
      if Assigned =
        Positive(Player_Ship.Modules(Module_Index).Owner.Length) then
         Disable_Buttons_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Button_Name :=
              To_Unbounded_String
                (Source =>
                   Frame_Name & ".crewbutton" &
                   Trim
                     (Source =>
                        Positive'Image(Crew_Container.To_Index(Position => I)),
                      Side => Left));
            if Tcl_GetVar
                (interp => Interp,
                 varName => To_String(Source => Button_Name)) =
              "0" then
               Crew_Button.Name :=
                 New_String(Str => To_String(Source => Button_Name));
               State(Widget => Crew_Button, StateSpec => "disabled");
               configure(Widgt => Crew_Button, options => "-takefocus 0");
            end if;
         end loop Disable_Buttons_Loop;
      end if;
      if Winfo_Get(Widgt => Info_Label, Info => "exists") = "1" then
         configure
           (Widgt => Info_Label,
            options =>
              "-text {Available:" &
              Natural'Image
                (Positive(Player_Ship.Modules(Module_Index).Owner.Length) -
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
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignCrew moduleindex
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module: constant Module_Data := Player_Ship.Modules(Module_Index);
      Module_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".moduledialog",
           Title =>
             "Assign a crew member to " & To_String(Source => Module.Name),
           Title_Width => 250);
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Module_Dialog & ".yscroll",
           options =>
             "-orient vertical -command [list .moduledialog.canvas yview]");
      Crew_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Module_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Crew_Frame: constant Ttk_Frame :=
        Create(pathName => Crew_Canvas & ".frame");
      Close_Button: constant Ttk_Button :=
        Create
          (pathName => Module_Dialog & ".button",
           options =>
             "-text Close -command {CloseDialog " &
             Widget_Image(Win => Module_Dialog) & "}");
      Height: Positive := 10;
      Width: Positive := 250;
      Crew_Button: Ttk_CheckButton;
      Info_Label: Ttk_Label;
      Assigned: Natural := 0;
      Recipe: constant Craft_Data :=
        (if Module.M_Type = WORKSHOP then
           Set_Recipe_Data
             (Recipe_Index =>
                To_Bounded_String
                  (Source => To_String(Source => Module.Crafting_Index)))
         else Craft_Data'(others => <>));
   begin
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crew_Canvas, Options => "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Y_Scroll,
         Options => "-sticky ns -padx {0 5} -pady {5 0} -row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-pady {0 5} -columnspan 2");
      Focus(Widgt => Close_Button);
      Autoscroll(Scroll => Y_Scroll);
      Load_Crew_List_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Crew_Button :=
           Create
             (pathName =>
                Crew_Frame & ".crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
                (if Module.M_Type = WORKSHOP then
                   Get_Skill_Marks
                     (Skill_Index => Recipe.Skill,
                      Member_Index => Crew_Container.To_Index(Position => I))
                 else "") &
                "} -command {UpdateAssignCrew" & Positive'Image(Module_Index) &
                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
         Tcl_SetVar
           (interp => Interp, varName => Widget_Image(Win => Crew_Button),
            newValue => "0");
         Count_Assigned_Loop :
         for Owner of Module.Owner loop
            if Owner = Crew_Container.To_Index(Position => I) then
               Tcl_SetVar
                 (interp => Interp,
                  varName => Widget_Image(Win => Crew_Button),
                  newValue => "1");
               Assigned := Assigned + 1;
               exit Count_Assigned_Loop;
            end if;
         end loop Count_Assigned_Loop;
         Tcl.Tk.Ada.Pack.Pack(Slave => Crew_Button, Options => "-anchor w");
         Height :=
           Height +
           Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqheight"));
         if Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
           10 >
           Width then
            Width :=
              Positive'Value
                (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
              10;
         end if;
         Bind
           (Widgt => Crew_Button, Sequence => "<Escape>",
            Script => "{" & Close_Button & " invoke;break}");
         Bind
           (Widgt => Crew_Button, Sequence => "<Tab>",
            Script =>
              "{focus [GetActiveButton" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              "];break}");
      end loop Load_Crew_List_Loop;
      if Update_Assign_Crew_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      Info_Label :=
        Create
          (pathName => Crew_Frame & ".infolabel",
           options =>
             "-text {Available:" &
             Natural'Image(Positive(Module.Owner.Length) - Assigned) & "}");
      Tcl.Tk.Ada.Pack.Pack(Slave => Info_Label);
      Height :=
        Height +
        Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqheight"));
      if Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqwidth")) >
        Width then
         Width :=
           Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqwidth"));
      end if;
      if Height > 500 then
         Height := 500;
      end if;
      Canvas_Create
        (Parent => Crew_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Crew_Frame));
      Tcl_Eval(interp => Interp, strng => "update");
      configure
        (Widgt => Crew_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Crew_Canvas, TagOrId => "all") & "] -height" &
           Positive'Image(Height) & " -width" & Positive'Image(Width));
      Bind
        (Widgt => Close_Button, Sequence => "<Escape>",
         Script => "{" & Close_Button & " invoke;break}");
      Bind
        (Widgt => Close_Button, Sequence => "<Tab>",
         Script => "{focus [GetActiveButton 0];break}");
      Show_Dialog(Dialog => Module_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Assign_Crew_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Skill_Command
   -- FUNCTION
   -- Show assign the skill UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignSkill moduleindex
   -- Moduleindex is the index of the module to which a new skill will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".moduledialog",
           Title =>
             "Assign skill to " &
             To_String(Source => Player_Ship.Modules(Module_Index).Name),
           Title_Width => 400);
      Skills_Frame: constant Ttk_Frame :=
        Create(pathName => Module_Dialog & ".frame");
      Skill_Name, Tool_Color: Unbounded_String;
      Proto_Index: Objects_Container.Extended_Index;
      Tool_Name: Bounded_String;
      Skills_Table: Table_Widget (Amount => 2) :=
        Create_Table
          (Parent => Widget_Image(Win => Skills_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Skill"),
              2 => To_Unbounded_String(Source => "Training tool")));
      Dialog_Close_Button: constant Ttk_Button :=
        Get_Widget(pathName => Module_Dialog & ".button");
   begin
      Load_Skills_List_Loop :
      for I in 1 .. Skills_Amount loop
         if SkillsData_Container.Element(Container => Skills_List, Index => I)
             .Tool /=
           Null_Bounded_String then
            Proto_Index :=
              Find_Proto_Item
                (Item_Type =>
                   SkillsData_Container.Element
                     (Container => Skills_List, Index => I)
                     .Tool);
            Tool_Name :=
              (if
                 Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .Show_Type /=
                 Null_Bounded_String
               then
                 Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .Show_Type
               else Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .I_Type);
         end if;
         Skill_Name :=
           To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => I)
                       .Name));
         Tool_Color := To_Unbounded_String(Source => "green");
         if Get_Item_Amount
             (Item_Type =>
                Objects_Container.Element
                  (Container => Items_List, Index => Proto_Index)
                  .I_Type) =
           0 then
            Append(Source => Skill_Name, New_Item => " (no tool)");
            Tool_Color := To_Unbounded_String(Source => "red");
         end if;
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Skill_Name),
            Tooltip =>
              "Press mouse " &
              (if Game_Settings.Right_Button then "right" else "left") &
              " button to set as trained skill",
            Command =>
              "AssignModule skill" & Positive'Image(Module_Index) &
              Skills_Amount_Range'Image(I),
            Column => 1);
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Tool_Name),
            Tooltip =>
              "Press mouse " &
              (if Game_Settings.Right_Button then "right" else "left") &
              " button to set as trained skill",
            Command =>
              "AssignModule skill" & Positive'Image(Module_Index) &
              Skills_Amount_Range'Image(I),
            Column => 2, New_Row => True,
            Color => To_String(Source => Tool_Color));
      end loop Load_Skills_List_Loop;
      Update_Table(Table => Skills_Table);
      Tcl.Tk.Ada.Grid.Grid(Slave => Skills_Frame, Options => "-padx 2");
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Skills_Table.Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Skills_Table.Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Skills_Table.Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Skills_Table.Canvas, Fraction => "0.0");
      Add_Close_Button
        (Name => Module_Dialog & ".button", Text => "Close",
         Command => "CloseDialog " & Module_Dialog, Row => 2);
      Bind
        (Widgt => Dialog_Close_Button, Sequence => "<Tab>",
         Script => "{focus " & Skills_Table.Canvas & ";break}");
      Bind
        (Widgt => Skills_Table.Canvas, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Show_Dialog(Dialog => Module_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Assign_Skill_Command;

   -- ****o* SUModules/SUModules.Cancel_Order_Command
   -- FUNCTION
   -- Cancel the current crafting order
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Cancel moduleindex
   -- Moduleindex is the index of the module which the crafting order will
   -- be canceled
   -- SOURCE
   function Cancel_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Player_Ship.Modules(Module_Index).Crafting_Index := Null_Bounded_String;
      Player_Ship.Modules(Module_Index).Crafting_Amount := 0;
      Player_Ship.Modules(Module_Index).Crafting_Time := 0;
      Give_Orders_Loop :
      for Owner of Player_Ship.Modules(Module_Index).Owner loop
         if Owner > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Owner,
               Given_Order => REST);
         end if;
      end loop Give_Orders_Loop;
      Add_Message
        (Message =>
           "You cancelled crafting order in " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) & ".",
         M_Type => CRAFTMESSAGE, Color => RED);
      Update_Messages;
      Update_Header;
      Update_Crew_Info;
      return TCL_OK;
   end Cancel_Order_Command;

   -- ****o* SUModules/SUModules.Get_Active_Button_Command
   -- FUNCTION
   -- Get the next active button in assing crew dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetActiveButton crewindex
   -- Crewindex is the index of the crew member which is currently selected
   -- or 0 for close button
   -- SOURCE
   function Get_Active_Button_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_Active_Button_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Crew_Index: constant Natural :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Button_Name: Unbounded_String;
      Button: Ttk_CheckButton;
   begin
      Find_Active_Button_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Button_Name :=
           To_Unbounded_String
             (Source =>
                ".moduledialog.canvas.frame.crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left));
         Button :=
           Get_Widget
             (pathName => To_String(Source => Button_Name), Interp => Interp);
         exit Find_Active_Button_Loop when InState
             (Widget => Button, StateSpec => "disabled") =
           "0" and
           Crew_Container.To_Index(Position => I) > Crew_Index;
         Button_Name := Null_Unbounded_String;
      end loop Find_Active_Button_Loop;
      if Button_Name = Null_Unbounded_String then
         Button_Name := To_Unbounded_String(Source => ".moduledialog.button");
      end if;
      Button :=
        Get_Widget
          (pathName => To_String(Source => Button_Name), Interp => Interp);
      Focus(Widgt => Button);
      return TCL_OK;
   end Get_Active_Button_Command;

   procedure Update_Modules_Info(Page: Positive := 1) is
      use Tiny_String;

      Ship_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".shipinfoframe.modules.canvas");
      Ship_Info_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Ship_Canvas & ".frame");
      Row: Positive := 2;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      if Modules_Table.Row_Height = 1 then
         Modules_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Ship_Info_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Durability")),
              Scrollbar =>
                Get_Widget
                  (pathName => Main_Paned & ".shipinfoframe.modules.scrolly"),
              Command => "SortShipModules",
              Tooltip => "Press mouse button to sort the modules.");
      end if;
      if Modules_Indexes.Length /= Player_Ship.Modules.Length then
         Modules_Indexes.Clear;
         Update_Modules_Indexes_Loop :
         for I in Player_Ship.Modules.Iterate loop
            Modules_Indexes.Append
              (New_Item => Modules_Container.To_Index(Position => I));
         end loop Update_Modules_Indexes_Loop;
      end if;
      Clear_Table(Table => Modules_Table);
      Show_Modules_Menu_Loop :
      for Module_Index of Modules_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Add_Button
           (Table => Modules_Table,
            Text =>
              To_String(Source => Player_Ship.Modules(Module_Index).Name),
            Tooltip => "Show available module's options",
            Command => "ShowModuleMenu" & Positive'Image(Module_Index),
            Column => 1);
         Add_Progress_Bar
           (Table => Modules_Table,
            Value => Player_Ship.Modules(Module_Index).Durability,
            Max_Value => Player_Ship.Modules(Module_Index).Max_Durability,
            Tooltip => "Show available module's options",
            Command => "ShowModuleMenu" & Positive'Image(Module_Index),
            Column => 2, New_Row => True);
         Row := Row + 1;
         exit Show_Modules_Menu_Loop when Modules_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Show_Modules_Menu_Loop;
      if Page > 1 then
         if Modules_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Table => Modules_Table,
               Previous_Command => "ShowModules" & Positive'Image(Page - 1));
         else
            Add_Pagination
              (Table => Modules_Table,
               Previous_Command => "ShowModules" & Positive'Image(Page - 1),
               Next_Command => "ShowModules" & Positive'Image(Page + 1));
         end if;
      elsif Modules_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Modules_Table,
            Next_Command => "ShowModules" & Positive'Image(Page + 1));
      end if;
      Update_Table(Table => Modules_Table);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Ship_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Ship_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
   end Update_Modules_Info;

   -- ****o* SUModules/SUModules.Show_Modules_Command
   -- FUNCTION
   -- Show the list of the player's ship modules to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModules ?page?
   -- Page parameter is a index of page from which starts showing
   -- modules.
   -- SOURCE
   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Update_Modules_Info
        (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
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
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          (Table => Modules_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
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
      Fill_Local_Modules_Loop :
      for I in Player_Ship.Modules.Iterate loop
         Local_Modules(Modules_Container.To_Index(Position => I)) :=
           (Name => Player_Ship.Modules(I).Name,
            Damage =>
              Float(Player_Ship.Modules(I).Durability) /
              Float(Player_Ship.Modules(I).Max_Durability),
            Id => Modules_Container.To_Index(Position => I));
      end loop Fill_Local_Modules_Loop;
      Sort_Modules(Container => Local_Modules);
      Modules_Indexes.Clear;
      Fill_Modules_Indexes_Loop :
      for Module of Local_Modules loop
         Modules_Indexes.Append(New_Item => Module.Id);
      end loop Fill_Modules_Indexes_Loop;
      Update_Modules_Info;
      return TCL_OK;
   end Sort_Modules_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Ammo_Command
   -- FUNCTION
   -- Show the list of available ammo for the selected gun
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssingAmmo index
   -- Index is the module index of the selected gun which will be have
   -- assigned a new ammo
   -- SOURCE
   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Ammo_Index: constant Natural :=
        (if Player_Ship.Modules(Module_Index).M_Type = GUN then
           Player_Ship.Modules(Module_Index).Ammo_Index
         else Player_Ship.Modules(Module_Index).Harpoon_Index);
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
                   Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Value) and
           I /= Ammo_Index then
            Add_Button
              (Name =>
                 ".ammo" & Trim(Source => Positive'Image(Row), Side => Left),
               Label =>
                 To_String
                   (Source =>
                      Objects_Container.Element
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
      Add_Command
        (Name => "ShowModuleMenu",
         Ada_Command => Show_Module_Menu_Command'Access);
      Add_Command
        (Name => "ShowModuleInfo",
         Ada_Command => Show_Module_Info_Command'Access);
      Add_Command
        (Name => "SetUpgrade", Ada_Command => Set_Upgrade_Command'Access);
      Add_Command
        (Name => "AssignModule", Ada_Command => Assign_Module_Command'Access);
      Add_Command
        (Name => "DisableEngine",
         Ada_Command => Disable_Engine_Command'Access);
      Add_Command
        (Name => "StopUpgrading",
         Ada_Command => Stop_Upgrading_Command'Access);
      Add_Command
        (Name => "SetRepair", Ada_Command => Set_Repair_Command'Access);
      Add_Command
        (Name => "ResetDestination",
         Ada_Command => Reset_Destination_Command'Access);
      Add_Command
        (Name => "ShowAssignCrew",
         Ada_Command => Show_Assign_Crew_Command'Access);
      Add_Command
        (Name => "UpdateAssignCrew",
         Ada_Command => Update_Assign_Crew_Command'Access);
      Add_Command
        (Name => "ShowAssignSkill",
         Ada_Command => Show_Assign_Skill_Command'Access);
      Add_Command
        (Name => "CancelOrder", Ada_Command => Cancel_Order_Command'Access);
      Add_Command
        (Name => "GetActiveButton",
         Ada_Command => Get_Active_Button_Command'Access);
      Add_Command
        (Name => "ShowModules", Ada_Command => Show_Modules_Command'Access);
      Add_Command
        (Name => "SortShipModules",
         Ada_Command => Sort_Modules_Command'Access);
      Add_Command
        (Name => "ShowAssignAmmo",
         Ada_Command => Show_Assign_Ammo_Command'Access);
   end Add_Commands;

end Ships.UI.Modules;
