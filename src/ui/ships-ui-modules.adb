-- Copyright (c) 2020-2023 Bartek thindil Jasicki
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

with Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI;
with Crafts; use Crafts;
with Dialogs; use Dialogs;
with Factions;
with Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions;
with Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.UI.Crew; use Ships.UI.Crew;
with Ships.Upgrade;
with Table; use Table;
with Utils.UI; use Utils.UI;
with ShipModules; use ShipModules;

package body Ships.UI.Modules is

   -- ****iv* SUModules/SUModules.Modules_Table
   -- FUNCTION
   -- Table with info about the installed modules on the player ship
   -- SOURCE
   Modules_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****iv* SUModules/SUModules.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Modules_Indexes: Positive_Container.Vector;
   -- ****

   -- ****o* SUModules/SUModules.Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
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
      pragma Unreferenced(Client_Data, Argc);
      use Ada.Characters.Latin_1;
      use Tcl.Tk.Ada.Font;
      use Tcl.Tk.Ada.Widgets.Text;
      use Tcl.Tk.Ada.Widgets.TtkProgressBar;
      use Tcl.Tklib.Ada.Tooltip;
      use Short_String;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module: constant Module_Data := Player_Ship.Modules(Module_Index);
      Module_Max_Value: Positive;
      Have_Ammo: Boolean := False;
      M_Amount, Max_Upgrade: Natural := 0;
      Damage_Percent: Float;
      Upgrade_Percent: Float := 0.0;
      Progress_Bar: Ttk_ProgressBar;
      Label: Ttk_Label; --## rule line off IMPROPER_INITIALIZATION
      Module_Info: Unbounded_String := Null_Unbounded_String;
      Progress_Bar_Style: Unbounded_String;
      Info_Button: Ttk_Button;
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
           options => "-wrap char -height 5 -width 15");
      Height: Positive := 10;
      Close_Dialog_Button: constant Ttk_Button :=
        Get_Widget(pathName => Module_Frame & ".button");
      Current_Row: Natural := 0;
      Status_Tooltip: Unbounded_String;
      procedure Add_Label
        (Name, Label_Text: String;
         Row, Column, Column_Span, Wrap_Length: Natural := 0;
         Count_Height: Boolean := False) is
      begin
         Label :=
           Create
             (pathName => Name,
              options =>
                "-text {" & Label_Text & "} -wraplength" &
                (if Wrap_Length > 0 then Wrap_Length'Img else " 380"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options =>
              "-sticky w -row" & Row'Img & " -column" & Column'Img &
              (if Column_Span > 0 then " -columnspan" & Column_Span'Img
               else ""));
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Label & " " & Y_Scroll);
         if Count_Height then
            Height :=
              Height +
              Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         end if;
      end Add_Label;
      procedure Add_Owners_Info
        (Owners_Name: String; Add_Button: Boolean := False;
         Row: Natural := 0) is
         Have_Owner: Boolean := False;
         Owners_Text: Unbounded_String := Null_Unbounded_String;
      begin
         Append(Source => Owners_Text, New_Item => Owners_Name);
         if Module.Owner.Length > 1 then
            Append(Source => Owners_Text, New_Item => "s");
         end if;
         Append
           (Source => Owners_Text,
            New_Item =>
              " (max" & Count_Type'Image(Module.Owner.Length) & "):");
         Add_Label
           (Name => Module_Frame & ".lblowners",
            Label_Text => To_String(Source => Owners_Text), Row => Row);
         Owners_Text := Null_Unbounded_String;
         Add_Owners_Info_Loop :
         for Owner of Module.Owner loop
            if Owner > 0 then
               if Have_Owner then
                  Append(Source => Owners_Text, New_Item => ", ");
               end if;
               Have_Owner := True;
               Append
                 (Source => Owners_Text,
                  New_Item =>
                    To_String(Source => Player_Ship.Crew(Owner).Name));
            end if;
         end loop Add_Owners_Info_Loop;
         if not Have_Owner then
            Append(Source => Owners_Text, New_Item => "none");
         end if;
         Add_Label
           (Name => Module_Frame & ".lblowners2",
            Label_Text => To_String(Source => Owners_Text), Row => Row,
            Column => 1);
         if Add_Button then
            Info_Button :=
              Create
                (pathName => Module_Frame & ".ownersbutton",
                 options =>
                   "-image assigncrewicon -command {" & Close_Dialog_Button &
                   " invoke;ShowAssignCrew " &
                   CArgv.Arg(Argv => Argv, N => 1) & "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message => "Assign crew members to the module.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options =>
                 "-row" & Row'Img & " -column 2 -sticky n -padx {5 0}");
            Bind
              (Widgt => Info_Button, Sequence => "<Escape>",
               Script => "{" & Close_Dialog_Button & " invoke;break}");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
         end if;
         Height :=
           Height +
           Positive'Value
             (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
      end Add_Owners_Info;
      procedure Add_Upgrade_Button
        (Upgrade_Type: Ship_Upgrade; Button_Tooltip: String; Box: Ttk_Frame;
         Ship_Module: Module_Data; Column: Positive := 1;
         Button_Name: String := "button"; Row: Natural := 0) is
         Upgrade_Number: constant String :=
           (case Upgrade_Type is when MAX_VALUE => "2", when VALUE => "3",
              when others => "1");
      begin
         if Ship_Module.Upgrade_Action = Upgrade_Type and
           Player_Ship.Upgrade_Module = Module_Index then
            Info_Button :=
              Create
                (pathName => Box & "." & Button_Name,
                 options =>
                   "-image cancelicon -command {" & Close_Dialog_Button &
                   " invoke;StopUpgrading " & CArgv.Arg(Argv => Argv, N => 1) &
                   "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message => "Stop upgrading the " & Button_Tooltip);
         else
            Info_Button :=
              Create
                (pathName => Box & "." & Button_Name,
                 options =>
                   "-image upgradebuttonicon -command {" &
                   Close_Dialog_Button & " invoke;SetUpgrade " &
                   Upgrade_Number & " " & CArgv.Arg(Argv => Argv, N => 1) &
                   "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message => "Start upgrading the " & Button_Tooltip);
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Info_Button,
            Options =>
              "-row" & Row'Img & " -column" & Column'Img &
              " -sticky n -padx {5 0}");
         Bind
           (Widgt => Info_Button, Sequence => "<Escape>",
            Script => "{" & Close_Dialog_Button & " invoke;break}");
      end Add_Upgrade_Button;
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
      -- Show the module's name
      Add_Label(Name => Module_Frame & ".nameinfo", Label_Text => "Name:");
      Add_Label
        (Name => Module_Frame & ".nameinfo2",
         Label_Text => To_String(Source => Module.Name), Row => Current_Row,
         Column => 1);
      Info_Button :=
        Create
          (pathName => Module_Frame & ".namebutton",
           options =>
             "-image editicon -command {" & Close_Dialog_Button &
             " invoke;GetString {Enter a new name for the " &
             To_String(Source => Player_Ship.Modules(Module_Index).Name) &
             ":} modulename" & CArgv.Arg(Argv => Argv, N => 1) &
             " {Renaming the module} {Rename}} -style Small.TButton");
      Add
        (Widget => Info_Button,
         Message => "Set a new name for the crew member");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Info_Button,
         Options =>
           "-row" & Current_Row'Img & " -column 2 -sticky n -padx {5 0}");
      Bind
        (Widgt => Info_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Tcl_Eval
        (interp => Interp,
         strng => "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
      Height :=
        Height +
        Positive'Value(Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
      -- Show the module's damage
      Current_Row := Current_Row + 1;
      Add_Label
        (Name => Module_Frame & ".damagelbl", Label_Text => "Status:",
         Row => Current_Row);
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Damage_Percent :=
        (Float(Module.Durability) / Float(Module.Max_Durability));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Damage_Percent < 1.0 and Damage_Percent > 0.79 then
         Progress_Bar_Style :=
           To_Unbounded_String
             (Source => " -style green.Horizontal.TProgressbar");
         Status_Tooltip := To_Unbounded_String(Source => "Slightly damaged");
      elsif Damage_Percent < 0.8 and Damage_Percent > 0.49 then
         Progress_Bar_Style :=
           To_Unbounded_String
             (Source => " -style yellow.Horizontal.TProgressbar");
         Status_Tooltip := To_Unbounded_String(Source => "Damaged");
      elsif Damage_Percent < 0.5 and Damage_Percent > 0.19 then
         Progress_Bar_Style :=
           To_Unbounded_String
             (Source => " -style yellow.Horizontal.TProgressbar");
         Status_Tooltip := To_Unbounded_String(Source => "Heavily damaged");
      elsif Damage_Percent < 0.2 and Damage_Percent > 0.0 then
         Progress_Bar_Style := Null_Unbounded_String;
         Status_Tooltip := To_Unbounded_String(Source => "Almost destroyed");
      elsif Damage_Percent = 0.0 then
         Progress_Bar_Style := Null_Unbounded_String;
         Status_Tooltip := To_Unbounded_String(Source => "Destroyed");
      else
         Progress_Bar_Style :=
           To_Unbounded_String
             (Source => " -style green.Horizontal.TProgressbar");
         Status_Tooltip := To_Unbounded_String(Source => "Not damaged");
      end if;
      Module_Max_Value :=
        Positive
          (Float(Get_Module(Index => Module.Proto_Index).Durability) * 1.5);
      if Module.Max_Durability = Module_Max_Value then
         Append(Source => Status_Tooltip, New_Item => " (max upgrade)");
      end if;
      Progress_Bar :=
        Create
          (pathName => Module_Frame & ".damagebar",
           options =>
             "-orient horizontal -maximum 1.0 -value {" &
             Float'Image(Damage_Percent) & "}" &
             To_String(Source => Progress_Bar_Style));
      Add
        (Widget => Progress_Bar,
         Message => To_String(Source => Status_Tooltip));
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Progress_Bar,
         Options => "-row" & Current_Row'Img & " -column 1 -sticky we");
      if Player_Ship.Repair_Module = Module_Index then
         Info_Button :=
           Create
             (pathName => Module_Frame & ".repairbutton",
              options =>
                "-image cancelicon -command {" & Close_Dialog_Button &
                " invoke;SetRepair remove} -style Small.TButton");
         Add(Widget => Info_Button, Message => "Remove the repair priority");
      else
         Info_Button :=
           Create
             (pathName => Module_Frame & ".repairbutton",
              options =>
                "-image repairpriorityicon -command {" & Close_Dialog_Button &
                " invoke;SetRepair assign" & Module_Index'Img &
                "} -style Small.TButton");
         Add
           (Widget => Info_Button,
            Message => "Repair selected module as first when damaged");
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Info_Button,
         Options =>
           "-row" & Current_Row'Img & " -column 2 -sticky n -padx {5 0}");
      Bind
        (Widgt => Info_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      if Module.Max_Durability < Module_Max_Value then
         Add_Upgrade_Button
           (Upgrade_Type => DURABILITY,
            Button_Tooltip => "module's durability", Box => Module_Frame,
            Ship_Module => Module, Column => 3,
            Button_Name => "durabilitybutton", Row => Current_Row);
      end if;
      Height :=
        Height +
        Positive'Value(Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
      -- Show the module's weight
      Current_Row := Current_Row + 1;
      Add_Label
        (Name => Module_Frame & ".weightlbl", Label_Text => "Weight:",
         Row => Current_Row);
      Add_Label
        (Name => Module_Frame & ".weightlbl2",
         Label_Text => Integer'Image(Module.Weight) & " kg",
         Row => Current_Row, Column => 1, Count_Height => True);
      -- Show the module's size
      Current_Row := Current_Row + 1;
      Add_Label
        (Name => Module_Frame & ".lblsize", Label_Text => "Size:",
         Row => Current_Row);
      Add_Label
        (Name => Module_Frame & ".lblsize2",
         Label_Text =>
           Natural'Image(Get_Module(Index => Module.Proto_Index).Size),
         Row => Current_Row, Column => 1, Count_Height => True);
      -- Show the modules' repair material
      Current_Row := Current_Row + 1;
      Add_Label
        (Name => Module_Frame & ".lblrepairmaterial",
         Label_Text => "Repair material:", Row => Current_Row,
         Wrap_Length => 200);
      Tag_Configure
        (TextWidget => Module_Text, TagName => "red",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" &
                To_String(Source => Get_Interface_Theme) &
                "::colors(-red)"));
      Find_Repair_Material_Loop :
      for I in 1 .. Get_Proto_Amount loop
         if To_String(Source => Get_Proto_Item(Index => I).I_Type) =
           To_String
             (Source =>
                Get_Module(Index => Module.Proto_Index).Repair_Material) then
            if M_Amount > 0 then
               Insert
                 (TextWidget => Module_Text, Index => "end", Text => "{ or }");
            end if;
            Insert
              (TextWidget => Module_Text, Index => "end",
               Text =>
                 "{" & To_String(Source => Get_Proto_Item(Index => I).Name) &
                 "}" &
                 (if
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Item_Type => Get_Proto_Item(Index => I).I_Type) =
                    0
                  then " [list red]"
                  else ""));
            M_Amount := M_Amount + 1;
         end if;
      end loop Find_Repair_Material_Loop;
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
                (Metrics(Font => "InterfaceFont", Option => "-linespace"))));
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Module_Text,
         Options => "-row" & Current_Row'Img & " -column 1 -sticky nw");
      Count_Height_Block :
      declare
         New_Height: Positive :=
           Positive'Value
             (Winfo_Get(Widgt => Module_Text, Info => "reqheight"));
      begin
         if New_Height <
           Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight")) then
            New_Height :=
              Positive'Value(Winfo_Get(Widgt => Label, Info => "reqheight"));
         end if;
         Height := Height + New_Height;
      end Count_Height_Block;
      -- Show module's upgrade skill
      Current_Row := Current_Row + 1;
      Add_Label
        (Name => Module_Frame & ".upgradeskill", Label_Text => "Repair skill:",
         Row => Current_Row, Wrap_Length => 200, Count_Height => True);
      Add_Label
        (Name => Module_Frame & ".upgradeskill2",
         Label_Text =>
           To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List,
                   Index =>
                     Get_Module(Index => Module.Proto_Index).Repair_Skill)
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
                          Get_Module(Index => Module.Proto_Index).Repair_Skill)
                       .Attribute)
                  .Name),
         Row => Current_Row, Column => 1);
      -- Show the module's upgrade action
      if Module.Upgrade_Action /= NONE then
         Current_Row := Current_Row + 1;
         Module_Info := To_Unbounded_String(Source => "");
         case Module.Upgrade_Action is
            when DURABILITY =>
               Append(Source => Module_Info, New_Item => "Durability");
               Max_Upgrade :=
                 Get_Module(Index => Module.Proto_Index).Durability;
            when MAX_VALUE =>
               case Get_Module(Index => Module.Proto_Index).M_Type is
                  when ENGINE =>
                     Append(Source => Module_Info, New_Item => "Power");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Max_Value / 20;
                  when CABIN =>
                     Append(Source => Module_Info, New_Item => "Quality");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Max_Value;
                  when GUN | BATTERING_RAM =>
                     Append(Source => Module_Info, New_Item => "Damage");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Max_Value * 2;
                  when HULL =>
                     Append(Source => Module_Info, New_Item => "Enlarge");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Max_Value * 40;
                  when HARPOON_GUN =>
                     Append(Source => Module_Info, New_Item => "Strength");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Max_Value * 10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Get_Module(Index => Module.Proto_Index).M_Type is
                  when ENGINE =>
                     Append(Source => Module_Info, New_Item => "Fuel usage");
                     Max_Upgrade :=
                       Get_Module(Index => Module.Proto_Index).Value * 20;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Max_Upgrade :=
           Integer(Float(Max_Upgrade) * New_Game_Settings.Upgrade_Cost_Bonus);
         if Max_Upgrade = 0 then
            Max_Upgrade := 1;
         end if;
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Upgrade_Percent :=
           1.0 - (Float(Module.Upgrade_Progress) / Float(Max_Upgrade));
         --## rule on SIMPLIFIABLE_EXPRESSIONS
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
             (pathName => Module_Frame & ".upgradebar",
              options =>
                "-orient horizontal -maximum 1.0 -value {" &
                Float'Image(Upgrade_Percent) & "}" &
                To_String(Source => Progress_Bar_Style));
         Add
           (Widget => Progress_Bar,
            Message => To_String(Source => Module_Info));
         Add_Label
           (Name => Module_Frame & ".upgradelbl",
            Label_Text => "Upgrade progress:", Row => Current_Row);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options =>
              "-row" & Current_Row'Img & " -column 1 -sticky we -padx {5 0}");
         if Player_Ship.Upgrade_Module = Module_Index then
            Info_Button :=
              Create
                (pathName => Module_Frame & ".upgradebutton",
                 options =>
                   "-image cancelicon -command {" & Close_Dialog_Button &
                   " invoke;StopUpgrading " & CArgv.Arg(Argv => Argv, N => 1) &
                   "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message => "Stop upgrading cabin quality");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options =>
                 "-row" & Current_Row'Img &
                 " -column 2 -sticky n -padx {5 0}");
            Bind
              (Widgt => Info_Button, Sequence => "<Escape>",
               Script => "{" & Close_Dialog_Button & " invoke;break}");
         end if;
         Height :=
           Height +
           Positive'Value
             (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
      end if;
      -- Show information specific to the module's type
      case Module.M_Type is
         -- Show information about engine
         when ENGINE =>
            -- Show the engine power
            Current_Row := Current_Row + 1;
            Module_Max_Value :=
              Positive
                (Float(Get_Module(Index => Module.Proto_Index).Max_Value) *
                 1.5);
            Add_Label
              (Name => Module_Frame & ".powerlbl", Label_Text => "Max power:",
               Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".powerlbl2",
               Label_Text =>
                 Integer'Image(Module.Power) &
                 (if Module.Power = Module_Max_Value then " (max upgrade)"
                  else ""),
               Row => Current_Row, Column => 1);
            if Module.Power < Module_Max_Value then
               Add_Upgrade_Button
                 (Upgrade_Type => MAX_VALUE,
                  Button_Tooltip => "engine's power", Box => Module_Frame,
                  Ship_Module => Module, Column => 2,
                  Button_Name => "powerbutton", Row => Current_Row);
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
            else
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Label, Info => "reqheight"));
            end if;
            -- Show the engine fuel usage
            Current_Row := Current_Row + 1;
            Module_Max_Value :=
              Positive
                (Float(Get_Module(Index => Module.Proto_Index).Value) / 2.0);
            Add_Label
              (Name => Module_Frame & ".fuellbl", Label_Text => "Fuel usage:",
               Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".fuellbl2",
               Label_Text =>
                 Integer'Image(Module.Fuel_Usage) &
                 (if Module_Max_Value = Module.Fuel_Usage then " (max upgrade)"
                  else ""),
               Row => Current_Row, Column => 1);
            if Module.Fuel_Usage > Module_Max_Value then
               Add_Upgrade_Button
                 (Upgrade_Type => VALUE,
                  Button_Tooltip => "engine's fuel usage", Box => Module_Frame,
                  Ship_Module => Module, Column => 2,
                  Button_Name => "fuelbutton", Row => Current_Row);
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
            else
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Label, Info => "reqheight"));
            end if;
            -- Show the engine state
            Current_Row := Current_Row + 1;
            Add_Label
              (Name => Module_Frame & ".statelbl", Label_Text => "State: ",
               Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".statelbl2",
               Label_Text =>
                 (if Module.Disabled then "Disabled" else "Enabled"),
               Row => Current_Row, Column => 1);
            Info_Button :=
              Create
                (pathName => Module_Frame & ".statebutton",
                 options =>
                   "-image powericon -command {" & Close_Dialog_Button &
                   " invoke;DisableEngine " & CArgv.Arg(Argv => Argv, N => 1) &
                   "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message =>
                 "Turn" & (if Module.Disabled then " on " else " off ") &
                 "the engine.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options =>
                 "-row" & Current_Row'Img &
                 " -column 2 -sticky n -padx {5 0}");
            Bind
              (Widgt => Info_Button, Sequence => "<Escape>",
               Script => "{" & Close_Dialog_Button & " invoke;break}");
            Height :=
              Height +
              Positive'Value
                (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
         -- Show information about cargo room
         when CARGO_ROOM =>
            Current_Row := Current_Row + 1;
            Add_Label
              (Name => Module_Frame & ".maxcargolbl",
               Label_Text => "Max cargo:", Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".maxcargolbl2",
               Label_Text =>
                 Integer'Image
                   (Get_Module(Index => Module.Proto_Index).Max_Value) &
                 " kg",
               Row => Current_Row, Column => 1, Count_Height => True);
         -- Show information about hull
         when HULL =>
            Current_Row := Current_Row + 1;
            Add_Label
              (Name => Module_Frame & ".modules",
               Label_Text => "Modules installed:", Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".modules2",
               Label_Text =>
                 Integer'Image(Module.Installed_Modules) & " /" &
                 Integer'Image(Module.Max_Modules),
               Row => Current_Row, Column => 1);
            Module_Max_Value :=
              Positive
                (Float(Get_Module(Index => Module.Proto_Index).Max_Value) *
                 1.5);
            if Module.Max_Modules = Module_Max_Value then
               configure
                 (Widgt => Label,
                  options =>
                    "-text {" & cget(Widgt => Label, option => "-text") &
                    " (max upgrade)}");
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Label, Info => "reqheight"));
            else
               Add_Upgrade_Button
                 (Upgrade_Type => MAX_VALUE,
                  Button_Tooltip =>
                    "hull's size so it can have more modules installed",
                  Box => Module_Frame, Ship_Module => Module, Column => 2,
                  Button_Name => "resizebutton", Row => Current_Row);
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
            end if;
         -- Show information about cabin
         when CABIN =>
            -- Show information about cabin's owners
            Current_Row := Current_Row + 1;
            Cabin_Owner_Info_Block :
            declare
               use Missions;

               Is_Passenger: Boolean := False;
               --## rule off IMPROPER_INITIALIZATION
               Mission: Mission_Data;
               --## rule on IMPROPER_INITIALIZATION
            begin
               Missions_Loop :
               for I in 1 .. Get_Accepted_Missions_Amount loop
                  Mission := Get_Accepted_Mission(Mission_Index => I);
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
               Add_Owners_Info
                 (Owners_Name => "Owner", Add_Button => not Is_Passenger,
                  Row => Current_Row);
            end Cabin_Owner_Info_Block;
            -- Show information about cabin's cleanliness
            Current_Row := Current_Row + 1;
            Add_Cleanliness_Info_Block :
            declare
               New_Status_Tooltip: Unbounded_String;
            begin
               Add_Label
                 (Name => Module_Frame & ".cleanlbl",
                  Label_Text => "Cleanliness:", Row => Current_Row,
                  Count_Height => True);
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Damage_Percent :=
                 1.0 - (Float(Module.Cleanliness) / Float(Module.Quality));
               --## rule on SIMPLIFIABLE_EXPRESSIONS
               if Damage_Percent = 0.0 then
                  New_Status_Tooltip := To_Unbounded_String(Source => "Clean");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style green.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.0 and Damage_Percent < 0.2 then
                  New_Status_Tooltip :=
                    To_Unbounded_String(Source => "Bit dusty");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style green.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.19 and Damage_Percent < 0.5 then
                  New_Status_Tooltip := To_Unbounded_String(Source => "Dusty");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style yellow.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.49 and Damage_Percent < 0.8 then
                  New_Status_Tooltip := To_Unbounded_String(Source => "Dirty");
                  Progress_Bar_Style :=
                    To_Unbounded_String
                      (Source => " -style yellow.Horizontal.TProgressbar");
               elsif Damage_Percent > 0.79 and Damage_Percent < 1.0 then
                  New_Status_Tooltip :=
                    To_Unbounded_String(Source => "Very dirty");
                  Progress_Bar_Style := Null_Unbounded_String;
               else
                  New_Status_Tooltip :=
                    To_Unbounded_String(Source => "Ruined");
                  Progress_Bar_Style := Null_Unbounded_String;
               end if;
               Progress_Bar :=
                 Create
                   (pathName => Module_Frame & ".cleanbar",
                    options =>
                      "-orient horizontal -maximum 1.0 -value {" &
                      Float'Image(1.0 - Damage_Percent) & "}" &
                      To_String(Source => Progress_Bar_Style));
               Add
                 (Widget => Progress_Bar,
                  Message => To_String(Source => New_Status_Tooltip));
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Progress_Bar,
                  Options =>
                    "-row" & Current_Row'Img & " -column 1 -sticky we");
            end Add_Cleanliness_Info_Block;
            -- Show information about cabin's quality
            Current_Row := Current_Row + 1;
            Progress_Bar :=
              Create
                (pathName => Module_Frame & ".qualitybar",
                 options =>
                   "-orient horizontal -style blue.Horizontal.TProgressbar -maximum 1.0 -value {" &
                   Float'Image(Float(Module.Quality) / 100.0) & "}");
            Add_Label
              (Name => Module_Frame & ".qualitylbl", Label_Text => "Quality:",
               Row => Current_Row);
            Module_Max_Value :=
              Positive
                (Float(Get_Module(Index => Module.Proto_Index).Max_Value) *
                 1.5);
            Add
              (Widget => Progress_Bar,
               Message =>
                 Get_Cabin_Quality(Quality => Module.Quality) &
                 (if Module.Quality = Module_Max_Value then " (max upgrade)"
                  else ""));
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Bar,
               Options => "-row" & Current_Row'Img & " -column 1 -sticky we");
            if Module.Quality < Module_Max_Value then
               Add_Upgrade_Button
                 (Upgrade_Type => MAX_VALUE,
                  Button_Tooltip => "cabin's quality", Box => Module_Frame,
                  Ship_Module => Module, Column => 2, Row => Current_Row,
                  Button_Name => "qualitybutton");
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
            else
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Label, Info => "reqheight"));
            end if;
         -- Show information about guns and harpoon guns
         when GUN | HARPOON_GUN =>
            -- Show information about gun's strength
            Current_Row := Current_Row + 1;
            Add_Strength_Info_Block :
            declare
               Module_Strength: constant Positive :=
                 (if Get_Module(Index => Module.Proto_Index).M_Type = GUN then
                    Module.Damage
                  else Module.Duration);
            begin
               Module_Max_Value :=
                 Positive
                   (Float(Get_Module(Index => Module.Proto_Index).Max_Value) *
                    1.5);
               Add_Label
                 (Name => Module_Frame & ".strengthlbl",
                  Label_Text => "Strength:", Row => Current_Row);
               Add_Label
                 (Name => Module_Frame & ".strengthlbl2",
                  Label_Text =>
                    Positive'Image(Module_Strength) &
                    (if Module_Strength = Module_Max_Value then
                       " (max upgrade)"
                     else ""),
                  Row => Current_Row, Column => 1);
               if Module_Strength < Module_Max_Value then
                  Add_Upgrade_Button
                    (Upgrade_Type => MAX_VALUE,
                     Button_Tooltip =>
                       (if Get_Module(Index => Module.Proto_Index).M_Type = GUN
                        then "damage"
                        else "strength") &
                       " of gun",
                     Box => Module_Frame, Ship_Module => Module,
                     Row => Current_Row, Button_Name => "strentghbutton",
                     Column => 2);
                  Height :=
                    Height +
                    Positive'Value
                      (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
               else
                  Height :=
                    Height +
                    Positive'Value
                      (Winfo_Get(Widgt => Label, Info => "reqheight"));
               end if;
            end Add_Strength_Info_Block;
            -- Show information about gun's owners
            Current_Row := Current_Row + 1;
            Add_Owners_Info
              (Owners_Name => "Gunner", Add_Button => True,
               Row => Current_Row);
            -- Show information about gun's ammunition
            Current_Row := Current_Row + 1;
            Show_Ammo_Block :
            declare
               Ammo_Index: constant Natural :=
                 (if Module.M_Type = GUN then Module.Ammo_Index
                  else Module.Harpoon_Index);
               Ammo_Text: constant Tk_Text :=
                 Create
                   (pathName => Module_Frame & ".ammoinfo",
                    options => "-wrap char -height 3 -width 15");
            begin
               Add_Label
                 (Name => Module_Frame & ".ammolbl",
                  Label_Text => "Ammunition:", Row => Current_Row);
               Tag_Configure
                 (TextWidget => Ammo_Text, TagName => "red",
                  Options =>
                    "-foreground " &
                    Tcl_GetVar
                      (interp => Interp,
                       varName =>
                         "ttk::theme::" &
                         To_String(Source => Get_Interface_Theme) &
                         "::colors(-red)"));
               Have_Ammo := False;
               if Ammo_Index in
                   Inventory_Container.First_Index
                         (Container => Player_Ship.Cargo) ..
                         Inventory_Container.Last_Index
                           (Container => Player_Ship.Cargo)
                 and then
                   Get_Proto_Item
                     (Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => Ammo_Index)
                          .Proto_Index)
                     .I_Type =
                   Get_Ada_Item_Type
                     (Item_Index =>
                        Get_Module(Index => Module.Proto_Index).Value - 1) then
                  Insert
                    (TextWidget => Ammo_Text, Index => "end",
                     Text =>
                       "{" &
                       To_String
                         (Source =>
                            Get_Proto_Item
                              (Index =>
                                 Inventory_Container.Element
                                   (Container => Player_Ship.Cargo,
                                    Index => Ammo_Index)
                                   .Proto_Index)
                              .Name) &
                       " (assigned)}");
                  Have_Ammo := True;
               end if;
               if not Have_Ammo then
                  M_Amount := 0;
                  Find_Ammo_Info_Loop :
                  for I in 1 .. Get_Proto_Amount loop
                     if Get_Proto_Item(Index => I).I_Type =
                       Get_Ada_Item_Type
                         (Item_Index =>
                            Get_Module(Index => Module.Proto_Index).Value -
                            1) then
                        if M_Amount > 0 then
                           Insert
                             (TextWidget => Ammo_Text, Index => "end",
                              Text => "{ or }");
                        end if;
                        Insert
                          (TextWidget => Ammo_Text, Index => "end",
                           Text =>
                             "{" &
                             To_String
                               (Source => Get_Proto_Item(Index => I).Name) &
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
               Find_Ammo_Loop :
               for I in
                 Inventory_Container.First_Index
                   (Container => Player_Ship.Cargo) ..
                   Inventory_Container.Last_Index
                     (Container => Player_Ship.Cargo) loop
                  if Get_Proto_Item
                      (Index =>
                         Inventory_Container.Element
                           (Container => Player_Ship.Cargo, Index => I)
                           .Proto_Index)
                      .I_Type =
                    Get_Ada_Item_Type
                      (Item_Index =>
                         Get_Module
                           (Index =>
                              Player_Ship.Modules(Module_Index).Proto_Index)
                           .Value -
                         1) and
                    I /= Ammo_Index then
                     Info_Button :=
                       Create
                         (pathName => Module_Frame & ".ammobutton",
                          options =>
                            "-image assignammoicon -command {" &
                            Close_Dialog_Button & " invoke;ShowAssignAmmo " &
                            CArgv.Arg(Argv => Argv, N => 1) &
                            "} -style Small.TButton");
                     Add
                       (Widget => Info_Button,
                        Message => "Assign an ammo to the gun.");
                     Tcl.Tk.Ada.Grid.Grid
                       (Slave => Info_Button,
                        Options =>
                          "-row" & Current_Row'Img &
                          " -column 2 -sticky w -padx {5 0}");
                     Bind
                       (Widgt => Info_Button, Sequence => "<Escape>",
                        Script =>
                          "{" & Close_Dialog_Button & " invoke;break}");
                     Tcl_Eval
                       (interp => Interp,
                        strng =>
                          "SetScrollbarBindings " & Info_Button & " " &
                          Y_Scroll);
                     exit Find_Ammo_Loop;
                  end if;
               end loop Find_Ammo_Loop;
               configure
                 (Widgt => Ammo_Text,
                  options =>
                    "-state disabled -height" &
                    Positive'Image
                      (Positive'Value
                         (Count
                            (TextWidget => Ammo_Text,
                             Options => "-displaylines", Index1 => "0.0",
                             Index2 => "end")) /
                       Positive'Value
                         (Metrics
                            (Font => "InterfaceFont",
                             Option => "-linespace")) -
                       2));
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Ammo_Text,
                  Options =>
                    "-sticky w -row" & Current_Row'Img & " -column 1");
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Ammo_Text, Info => "reqheight"));
            end Show_Ammo_Block;
            -- Show information about guns fire rate
            if Module.M_Type = GUN then
               Current_Row := Current_Row + 1;
               Add_Label
                 (Name => Module_Frame & ".lblfirerate",
                  Label_Text => "Max fire rate:", Row => Current_Row);
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Add_Label
                 (Name => Module_Frame & ".lblfirerate2",
                  Label_Text =>
                    (if Get_Module(Index => Module.Proto_Index).Speed > 0 then
                       Positive'Image
                         (Get_Module(Index => Module.Proto_Index).Speed) &
                       " each turn"
                     else "1 every " &
                       Integer'Image
                         (abs
                          (Get_Module(Index => Module.Proto_Index).Speed)) &
                       " turns"),
                  Row => Current_Row, Column => 1, Count_Height => True);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            end if;
         -- Show information about turrets
         when TURRET =>
            Current_Row := Current_Row + 1;
            Add_Label
              (Name => Module_Frame & ".lblturretgun", Label_Text => "Weapon:",
               Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".lblturretgun2",
               Label_Text =>
                 (if Module.Gun_Index > 0 then
                    To_String
                      (Source => Player_Ship.Modules(Module.Gun_Index).Name)
                  else "none"),
               Row => Current_Row, Column => 1, Count_Height => True);
         -- Show information about workshops
         when WORKSHOP =>
            -- Show information about workshop's owners
            Current_Row := Current_Row + 1;
            Add_Owners_Info
              (Owners_Name => "Worker",
               Add_Button =>
                 (if
                    Player_Ship.Modules(Module_Index).Crafting_Index /=
                    Tiny_String.Null_Bounded_String
                  then True
                  else False),
               Row => Current_Row);
            -- Show information about workshop's order
            Show_Order_Info_Block :
            declare
               Recipe_Name: constant String :=
                 Get_Workshop_Recipe_Name(Workshop => Module_Index);
            begin
               Current_Row := Current_Row + 1;
               if Recipe_Name'Length > 0 then
                  Add_Label
                    (Name => Module_Frame & ".orderlbl",
                     Label_Text => "Order:", Row => Current_Row);
                  Add_Label
                    (Name => Module_Frame & ".orderlbl2",
                     Label_Text => Recipe_Name, Row => Current_Row,
                     Column => 1, Count_Height => True);
                  Current_Row := Current_Row + 1;
                  Add_Label
                    (Name => Module_Frame & ".ordertimelbl",
                     Label_Text => "Finish order in:", Row => Current_Row);
                  Add_Label
                    (Name => Module_Frame & ".ordertimelbl2",
                     Label_Text =>
                       Positive'Image(Module.Crafting_Time) & " mins",
                     Row => Current_Row, Column => 1);
                  Info_Button :=
                    Create
                      (pathName => Module_Frame & ".orderbutton",
                       options =>
                         "-image cancelicon -command {" & Close_Dialog_Button &
                         " invoke;CancelOrder " &
                         CArgv.Arg(Argv => Argv, N => 1) &
                         "} -style Small.TButton");
                  Add
                    (Widget => Info_Button,
                     Message => "Cancel current crafting order");
                  Tcl.Tk.Ada.Grid.Grid
                    (Slave => Info_Button,
                     Options =>
                       "-row" & Current_Row'Img &
                       " -column 2 -sticky n -padx {5 0}");
                  Bind
                    (Widgt => Info_Button, Sequence => "<Escape>",
                     Script => "{" & Close_Dialog_Button & " invoke;break}");
                  Height :=
                    Height +
                    Positive'Value
                      (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
               else
                  Add_Label
                    (Name => Module_Frame & ".orderlbl",
                     Label_Text => "Order:", Row => Current_Row);
                  Add_Label
                    (Name => Module_Frame & ".orderlbl2",
                     Label_Text => "not set", Row => Current_Row, Column => 1,
                     Count_Height => True);
               end if;
            end Show_Order_Info_Block;
         -- Show information about medical rooms
         when MEDICAL_ROOM =>
            Find_Healing_Tool_Block :
            declare
               use Factions;

               Has_Healing_Tool: Boolean := False;
            begin
               Find_Healing_Tool_Loop :
               for Member of Player_Ship.Crew loop
                  if Member.Health < 100 and
                    Find_Item
                        (Inventory => Player_Ship.Cargo,
                         Item_Type =>
                           Get_Faction(Index => Player_Ship.Crew(1).Faction)
                             .Healing_Tools) >
                      0 then
                     Has_Healing_Tool := True;
                     exit Find_Healing_Tool_Loop;
                  end if;
               end loop Find_Healing_Tool_Loop;
               Current_Row := Current_Row + 1;
               Add_Owners_Info
                 (Owners_Name => "Medic", Add_Button => Has_Healing_Tool,
                  Row => Current_Row);
            end Find_Healing_Tool_Block;
         -- Show information about training rooms
         when TRAINING_ROOM =>
            -- Show information about trainees
            Current_Row := Current_Row + 1;
            Add_Owners_Info
              (Owners_Name => "Trainee",
               Add_Button =>
                 (if Module.Trained_Skill > 0 then True else False),
               Row => Current_Row);
            -- Show information about trained skill
            Show_Skill_Info_Block :
            declare
               Train_Text: Unbounded_String;
            begin
               if Module.Trained_Skill > 0 then
                  Train_Text :=
                    To_Unbounded_String
                      (Source =>
                         To_String
                           (Source =>
                              SkillsData_Container.Element
                                (Container => Skills_List,
                                 Index => Module.Trained_Skill)
                                .Name));
               else
                  Train_Text := To_Unbounded_String(Source => "not set");
               end if;
               Current_Row := Current_Row + 1;
               Add_Label
                 (Name => Module_Frame & ".trainlbl",
                  Label_Text => "Trained skill:", Row => Current_Row);
               Add_Label
                 (Name => Module_Frame & ".trainlbl2",
                  Label_Text => To_String(Source => Train_Text),
                  Row => Current_Row, Column => 1);
               Info_Button :=
                 Create
                   (pathName => Module_Frame & ".trainbutton",
                    options =>
                      "-image assigncrewicon -command {" &
                      Close_Dialog_Button & " invoke;ShowAssignSkill " &
                      CArgv.Arg(Argv => Argv, N => 1) &
                      "} -style Small.TButton");
               Add
                 (Widget => Info_Button,
                  Message =>
                    "Assign a skill which will be trained in the training room.");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Info_Button,
                  Options =>
                    "-row" & Current_Row'Img &
                    " -column 2 -sticky n -padx {5 0}");
               Bind
                 (Widgt => Info_Button, Sequence => "<Escape>",
                  Script => "{" & Close_Dialog_Button & " invoke;break}");
               Height :=
                 Height +
                 Positive'Value
                   (Winfo_Get(Widgt => Info_Button, Info => "reqheight"));
            end Show_Skill_Info_Block;
         -- Show information about battering rams
         when BATTERING_RAM =>
            Current_Row := Current_Row + 1;
            Module_Max_Value :=
              Positive
                (Float(Get_Module(Index => Module.Proto_Index).Max_Value) *
                 1.5);
            Add_Label
              (Name => Module_Frame & ".strengthlbl",
               Label_Text => "Strength:", Row => Current_Row);
            Add_Label
              (Name => Module_Frame & ".strengthlbl2",
               Label_Text =>
                 Positive'Image(Module.Damage2) &
                 (if Module.Damage2 = Module_Max_Value then " (max upgrade)"
                  else ""),
               Row => Current_Row, Column => 1, Count_Height => True);
            if Module.Damage2 < Module_Max_Value then
               Add_Upgrade_Button
                 (Upgrade_Type => MAX_VALUE,
                  Button_Tooltip => "damage of battering ram",
                  Box => Module_Frame, Ship_Module => Module,
                  Row => Current_Row, Column => 2,
                  Button_Name => "damagebutton");
            end if;
         when others =>
            null;
      end case;
      if Get_Module(Index => Module.Proto_Index).Description /=
        Short_String.Null_Bounded_String then
         Current_Row := Current_Row + 1;
         Tcl_Eval(interp => Interp, strng => "update");
         Add_Label
           (Name => Module_Frame & ".lbldescription",
            Label_Text =>
              LF &
              To_String
                (Source =>
                   Get_Module(Index => Module.Proto_Index).Description),
            Row => Current_Row, Count_Height => True, Column_Span => 4,
            Wrap_Length =>
              Positive'Value
                (Winfo_Get(Widgt => Module_Frame, Info => "reqwidth")));
      end if;
      Add_Close_Button
        (Name => Module_Frame & ".button", Text => "Close",
         Command => "CloseDialog " & Module_Dialog, Column_Span => 4,
         Row => Current_Row + 1);
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Tab>",
         Script => "{focus " & Module_Frame & ".nameinfo.button;break}");
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
         Tcl_Eval(interp => Interp, strng => "update");
         Width :=
           Positive'Value
             (Winfo_Get(Widgt => Module_Frame, Info => "reqwidth")) +
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

      use Ships.Upgrade;
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
      use Ada.Exceptions;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Assign_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 3));
      Assigned: Boolean := False;
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
         case Get_Module
           (Index => Player_Ship.Modules(Module_Index).Proto_Index)
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
                   Get_Proto_Item
                     (Index =>
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
      if Player_Ship.Modules(Module_Index).Disabled then
         Player_Ship.Modules(Module_Index).Disabled := False;
         Add_Message
           (Message =>
              "You enabled " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              ".",
            M_Type => ORDERMESSAGE);
      else
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
      use Interfaces.C.Strings;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Assigned: Natural := 0;
      Frame_Name: constant String := ".moduledialog.canvas.frame";
      --## rule off IMPROPER_INITIALIZATION
      Crew_Button: Ttk_CheckButton;
      Button_Name: Unbounded_String;
      --## rule on IMPROPER_INITIALIZATION
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
            if Get_Module
                (Index => Player_Ship.Modules(Module_Index).Proto_Index)
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
      --## rule off IMPROPER_INITIALIZATION
      Crew_Button: Ttk_CheckButton;
      Info_Label: Ttk_Label;
      --## rule on IMPROPER_INITIALIZATION
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
      use Ships.Cargo;
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
      Skill_Name, Tool_Color: Unbounded_String := Null_Unbounded_String;
      Proto_Index: Natural := 0;
      Tool_Name: Bounded_String := Null_Bounded_String;
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
                 Get_Proto_Item(Index => Proto_Index).Show_Type /=
                 Null_Bounded_String
               then Get_Proto_Item(Index => Proto_Index).Show_Type
               else Get_Proto_Item(Index => Proto_Index).I_Type);
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
             (Item_Type => Get_Proto_Item(Index => Proto_Index).I_Type) =
           0 then
            Append(Source => Skill_Name, New_Item => " (no tool)");
            Tool_Color := To_Unbounded_String(Source => "red");
         end if;
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Skill_Name),
            Tooltip =>
              "Press mouse " &
              (if Get_Boolean_Setting(Name => "rightButton") then "right"
               else "left") &
              " button to set as trained skill",
            Command =>
              "AssignModule skill" & Positive'Image(Module_Index) &
              Skills_Amount_Range'Image(I),
            Column => 1);
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Tool_Name),
            Tooltip =>
              "Press mouse " &
              (if Get_Boolean_Setting(Name => "rightButton") then "right"
               else "left") &
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
      Button_Name: Unbounded_String := Null_Unbounded_String;
      Button: Ttk_CheckButton; --## rule line off IMPROPER_INITIALIZATION
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

   -- ****if* SUModules/SUModules.Get_Module_Info
   -- FUNCTION
   -- Get the additional information about the module
   -- PARAMETERS
   -- Module_Index - the index of the module in the player's ship to show info
   -- RESULT
   -- The string with additional information about the module or empty string
   -- if no info is available.
   -- SOURCE
   function Get_Module_Info(Module_Index: Positive) return String is
      -- ****
      use Tiny_String;

      Info_Text: Unbounded_String := Null_Unbounded_String;
   begin
      case Player_Ship.Modules(Module_Index).M_Type is
         when GUN =>
            if Player_Ship.Modules(Module_Index).Ammo_Index in
                Inventory_Container.First_Index
                      (Container => Player_Ship.Cargo) ..
                      Inventory_Container.Last_Index
                        (Container => Player_Ship.Cargo)
              and then
                Get_Proto_Item
                  (Index =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo,
                        Index => Player_Ship.Modules(Module_Index).Ammo_Index)
                       .Proto_Index)
                  .I_Type =
                Get_Ada_Item_Type
                  (Item_Index =>
                     Get_Module
                       (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                       .Value -
                     1) then
               Info_Text :=
                 To_Unbounded_String
                   (Source =>
                      "Uses " &
                      To_String
                        (Source =>
                           Get_Proto_Item
                             (Index =>
                                Inventory_Container.Element
                                  (Container => Player_Ship.Cargo,
                                   Index =>
                                     Player_Ship.Modules(Module_Index)
                                       .Ammo_Index)
                                  .Proto_Index)
                             .Name) &
                      ", ");
            else
               Info_Text :=
                 To_Unbounded_String(Source => "No ammunition assigned, ");
            end if;
            if Player_Ship.Modules(Module_Index).Owner(1) = 0 then
               Append(Source => Info_Text, New_Item => " no gunner.");
            else
               Append
                 (Source => Info_Text,
                  New_Item =>
                    " " &
                    To_String
                      (Source =>
                         Player_Ship.Crew
                           (Player_Ship.Modules(Module_Index).Owner(1))
                           .Name) &
                    " is gunner.");
            end if;
         when WORKSHOP =>
            Show_Order_Info_Block :
            declare
               Recipe_Name: constant String :=
                 Get_Workshop_Recipe_Name(Workshop => Module_Index);
               Has_Workers: Boolean := False;
            begin
               if Recipe_Name'Length > 0 then
                  Info_Text := To_Unbounded_String(Source => Recipe_Name);
                  Show_Workers_Info_Loop :
                  for Owner of Player_Ship.Modules(Module_Index).Owner loop
                     if Owner > 0 then
                        if Has_Workers then
                           Append
                             (Source => Info_Text,
                              New_Item =>
                                ", " &
                                To_String
                                  (Source => Player_Ship.Crew(Owner).Name));
                        else
                           Append
                             (Source => Info_Text,
                              New_Item =>
                                ", workers: " &
                                To_String
                                  (Source => Player_Ship.Crew(Owner).Name));
                        end if;
                        Has_Workers := True;
                     end if;
                  end loop Show_Workers_Info_Loop;
                  if not Has_Workers then
                     Append
                       (Source => Info_Text,
                        New_Item => ", no workers assigned");
                  end if;
                  Append(Source => Info_Text, New_Item => ".");
               else
                  Info_Text :=
                    To_Unbounded_String(Source => "No crafting order.");
               end if;
            end Show_Order_Info_Block;
         when ENGINE =>
            if Player_Ship.Modules(Module_Index).Disabled then
               Info_Text := To_Unbounded_String(Source => "Engine disabled.");
            else
               Info_Text := Null_Unbounded_String;
            end if;
         when TRAINING_ROOM =>
            if Player_Ship.Modules(Module_Index).Trained_Skill > 0 then
               Info_Text :=
                 "Set for training " &
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           SkillsData_Container.Element
                             (Container => Skills_List,
                              Index =>
                                Player_Ship.Modules(Module_Index)
                                  .Trained_Skill)
                             .Name));
               Show_Trainee_Info_Block :
               declare
                  Has_Trainees: Boolean := False;
               begin
                  Show_Trainee_Info_Loop :
                  for Owner of Player_Ship.Modules(Module_Index).Owner loop
                     if Owner > 0 then
                        if Has_Trainees then
                           Append
                             (Source => Info_Text,
                              New_Item =>
                                ", " &
                                To_String
                                  (Source => Player_Ship.Crew(Owner).Name));
                        else
                           Append
                             (Source => Info_Text,
                              New_Item =>
                                ", trainees: " &
                                To_String
                                  (Source => Player_Ship.Crew(Owner).Name));
                        end if;
                        Has_Trainees := True;
                     end if;
                  end loop Show_Trainee_Info_Loop;
                  if not Has_Trainees then
                     Append
                       (Source => Info_Text,
                        New_Item => ", no trainees assigned");
                  end if;
                  Append(Source => Info_Text, New_Item => ".");
               end Show_Trainee_Info_Block;
            else
               Info_Text :=
                 To_Unbounded_String(Source => "Not set for training.");
            end if;
         when others =>
            Info_Text := Null_Unbounded_String;
      end case;
      return To_String(Source => Info_Text);
   end Get_Module_Info;

   procedure Update_Modules_Info(Page: Positive := 1) is
      use CoreUI;
      use Tiny_String;

      Ship_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".shipinfoframe.modules.canvas");
      Ship_Info_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Ship_Canvas & ".frame");
      Row: Positive := 2;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Start_Row: constant Positive :=
        ((Page - 1) * Get_Integer_Setting(Name => "listsLimit")) + 1;
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Current_Row: Positive := 1;
   begin
      if Modules_Table.Row_Height = 1 then
         Modules_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Ship_Info_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Durability"),
                 3 => To_Unbounded_String(Source => "Additional info")),
              Scrollbar =>
                Get_Widget
                  (pathName => Main_Paned & ".shipinfoframe.modules.scrolly"),
              Command => "SortShipModules",
              Tooltip_Text => "Press mouse button to sort the modules.");
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
            Tooltip => "Show the module's info",
            Command => "ShowModuleInfo" & Positive'Image(Module_Index),
            Column => 1);
         Add_Progress_Bar
           (Table => Modules_Table,
            Value => Player_Ship.Modules(Module_Index).Durability,
            Max_Value => Player_Ship.Modules(Module_Index).Max_Durability,
            Tooltip => "Show the module's info",
            Command => "ShowModuleInfo" & Positive'Image(Module_Index),
            Column => 2);
         Add_Button
           (Table => Modules_Table,
            Text => Get_Module_Info(Module_Index => Module_Index),
            Tooltip => "Show the module's info",
            Command => "ShowModuleInfo" & Positive'Image(Module_Index),
            Column => 3, New_Row => True);
         Row := Row + 1;
         exit Show_Modules_Menu_Loop when Modules_Table.Row =
           Get_Integer_Setting(Name => "listsLimit") + 1;
         <<End_Of_Loop>>
      end loop Show_Modules_Menu_Loop;
      if Page > 1 then
         if Modules_Table.Row <
           Get_Integer_Setting(Name => "listsLimit") + 1 then
            Add_Pagination
              (Table => Modules_Table,
               Previous_Command => "ShowModules" & Positive'Image(Page - 1));
         else
            Add_Pagination
              (Table => Modules_Table,
               Previous_Command => "ShowModules" & Positive'Image(Page - 1),
               Next_Command => "ShowModules" & Positive'Image(Page + 1));
         end if;
      elsif Modules_Table.Row =
        Get_Integer_Setting(Name => "listsLimit") + 1 then
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
   -- INFOASC    - Sort modules by info ascending
   -- INFODESC   - Sort modules by info descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.9 - Added sorting by info column
   -- SOURCE
   type Modules_Sort_Orders is
     (NAMEASC, NAMEDESC, DAMAGEASC, DAMAGEDESC, INFOASC, INFODESC, NONE) with
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

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUModules/SUModules.Modules_Sort_Order
   -- FUNCTION
   -- The current sorting order for modules list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

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

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Modules_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Module_Data is record
         Name: Bounded_String;
         Damage: Float;
         Id: Positive;
         Info: Unbounded_String;
      end record;
      type Modules_Array is array(Positive range <>) of Local_Module_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Modules: Modules_Array(1 .. Positive(Player_Ship.Modules.Length));
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function Get_Modules_Sort_Order return Modules_Sort_Orders is
      begin
         return Modules_Sort_Order;
      end Get_Modules_Sort_Order;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Get_Modules_Sort_Order = NAMEASC
           and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFOASC
           and then Left.Info < Right.Info then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFODESC
           and then Left.Info > Right.Info then
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
            if Get_Modules_Sort_Order = NAMEASC then
               Modules_Sort_Order := NAMEDESC;
            else
               Modules_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Get_Modules_Sort_Order = DAMAGEASC then
               Modules_Sort_Order := DAMAGEDESC;
            else
               Modules_Sort_Order := DAMAGEASC;
            end if;
         when 3 =>
            if Get_Modules_Sort_Order = INFOASC then
               Modules_Sort_Order := INFODESC;
            else
               Modules_Sort_Order := INFOASC;
            end if;
         when others =>
            null;
      end case;
      if Get_Modules_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Modules_Loop :
      for I in Player_Ship.Modules.Iterate loop
         Local_Modules(Modules_Container.To_Index(Position => I)) :=
           (Name => Player_Ship.Modules(I).Name,
            Damage =>
              Float(Player_Ship.Modules(I).Durability) /
              Float(Player_Ship.Modules(I).Max_Durability),
            Id => Modules_Container.To_Index(Position => I),
            Info =>
              To_Unbounded_String
                (Source =>
                   Get_Module_Info
                     (Module_Index =>
                        Modules_Container.To_Index(Position => I))));
      end loop Fill_Local_Modules_Loop;
      Sort_Modules(Container => Local_Modules);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Modules_Indexes.Clear;
      Fill_Modules_Indexes_Loop :
      for Module of Local_Modules loop
         Modules_Indexes.Append(New_Item => Module.Id);
      end loop Fill_Modules_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
         if Get_Proto_Item
             (Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Proto_Index)
             .I_Type =
           Get_Ada_Item_Type
             (Item_Index =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Value -
                1) and
           I /= Ammo_Index then
            Add_Button
              (Name =>
                 ".ammo" & Trim(Source => Positive'Image(Row), Side => Left),
               Label =>
                 To_String
                   (Source =>
                      Get_Proto_Item
                        (Index =>
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

   procedure Add_Modules_Commands is
   begin
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
   end Add_Modules_Commands;

end Ships.UI.Modules;
