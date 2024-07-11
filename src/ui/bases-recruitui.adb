-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip;
with Config;
with CoreUI;
with Dialogs;
with Maps; use Maps;
with Ships.Crew;
with Table; use Table;
with Utils.UI;

package body Bases.RecruitUI is

   -- ****iv* RecruitUI/RecruitUI.Recruit_Table
   -- FUNCTION
   -- Table with info about the available recruits
   -- SOURCE
   Recruit_Table: Table_Widget (Amount => 6);
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****iv* RecruitUI/RecruitUI.Modules_Indexes
   -- FUNCTION
   -- Indexes of the available recruits in base
   -- SOURCE
   Recruits_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* RecruitUI/RecruitUI.Get_Highest_Attribute
   -- FUNCTION
   -- Get the highest attribute's name of the selected recruit
   -- PARAMETERS
   -- Base_Index   - The index of the base in which the recruit's attributes
   --                will be check
   -- Member_Index - The index of the recruit which attributes will be check
   -- RESULT
   -- The name of the attribute with the highest level of the selected recruit
   -- HISTORY
   -- 6.5 - Added
   -- 7.6 - Renamed parameters to Base_Index and Member_Index
   -- SOURCE
   function Get_Highest_Attribute
     (Base_Index, Member_Index: Positive) return Unbounded_String is
     -- ****
      function Get_Ada_Highest_Attribute
        (B_Index, M_Index: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaHighestAttribute";
   begin
      return
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Get_Ada_Highest_Attribute
                    (B_Index => Base_Index, M_Index => Member_Index)));
   end Get_Highest_Attribute;

   -- ****if* RecruitUI/RecruitUI.Get_Highest_Skill
   -- FUNCTION
   -- Get the highest skill's name of the selected recruit
   -- PARAMETERS
   -- Base_Index   - The index of the base in which the recruit's skills will
   --                be check
   -- Member_Index - The index of the recruit which skills will be check
   -- RESULT
   -- The name of the skill with the highest level of the selected recruit
   -- HISTORY
   -- 6.5 - Added
   -- 7.6 - Renamed parameters to Base_Index and Member_Index
   -- SOURCE
   function Get_Highest_Skill
     (Base_Index, Member_Index: Positive) return Unbounded_String is
     -- ****
      function Get_Ada_Highest_Skill
        (B_Index, M_Index: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaHighestRecSkill";
   begin
      return
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Get_Ada_Highest_Skill
                    (B_Index => Base_Index, M_Index => Member_Index)));
   end Get_Highest_Skill;
   --## rule on REDUCEABLE_SCOPE

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Command
   -- FUNCTION
   -- Show the selected base available recruits
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruit
   -- SOURCE
   function Show_Recruit_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tcl.Tk.Ada.Winfo;
      use CoreUI;

      Recruit_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".recruitframe", Interp => Interp);
      function Show_Ada_Recruit_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showRecruitCommand";
   begin
      if Show_Ada_Recruit_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      if Winfo_Get(Widgt => Recruit_Frame, Info => "exists") = "0" then
         Recruit_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Recruit_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Gender"),
                 3 => To_Unbounded_String(Source => "Faction"),
                 4 => To_Unbounded_String(Source => "Base cost"),
                 5 => To_Unbounded_String(Source => "Highest stat"),
                 6 => To_Unbounded_String(Source => "Highest skill")),
              Command => "SortRecruits",
              Tooltip_Text => "Press mouse button to sort the recruits.");
      end if;
      return TCL_OK;
   end Show_Recruit_Command;

   -- ****iv* RecruitUI/RecruitUI.Recruit_Index
   -- FUNCTION
   -- The index of currently selected recruit
   -- SOURCE
   Recruit_Index: Positive;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* RecruitUI/RecruitUI.Get_Recruit_Index
   -- FUNCTION
   -- Get the index of the currently selected recruit
   -- RESULT
   -- The index of the currently selected recruit
   -- SOURCE
   function Get_Recruit_Index return Positive is
      -- ****
   begin
      return Recruit_Index;
   end Get_Recruit_Index;
   --## rule on REDUCEABLE_SCOPE

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Info_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruitInfo recruitindex
   -- RecruitIndex is a index of the recruit which menu will be shown
   -- SOURCE
   function Show_Recruit_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      function Show_Ada_Recruit_Info_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showRecruitInfoCommand";
      function Get_Ada_Recruit_Index return Positive with
         Convention => C,
         Import => True,
         External_Name => "getAdaRecruitIndex";
   begin
      Recruit_Index := Get_Ada_Recruit_Index;
      return Show_Ada_Recruit_Info_Command(C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
   end Show_Recruit_Info_Command;

   -- ****o* RecruitUI/RecruitUI.Negotiate_Hire_Command
   -- FUNCTION
   -- Update information about hiring of the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NegotiateHire
   -- SOURCE
   function Negotiate_Hire_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "negotiateHireCommand";
      -- ****

   -- ****o* RecruitUI/RecruitUI.Hire_Command
   -- FUNCTION
   -- Hire the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Hire
   -- SOURCE
   function Hire_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "hireCommand";
      -- ****

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Tab_Command
   -- FUNCTION
   -- Show the selected information about the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberTab
   -- SOURCE
   function Show_Recruit_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showRecruitTabCommand";
      -- ****

--   function Show_Recruit_Tab_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc, Argv);
--      use Tcl.Tk.Ada.Widgets.Canvas;
--
--      Recruit_Canvas: constant Tk_Canvas :=
--        Get_Widget(pathName => ".recruitdialog.canvas", Interp => Interp);
--      Frame: constant Ttk_Frame :=
--        Get_Widget
--          (pathName =>
--             Recruit_Canvas & "." &
--             Tcl_GetVar(interp => Interp, varName => "newtab"));
--   begin
--      Delete(CanvasWidget => Recruit_Canvas, TagOrId => "info");
--      Canvas_Create
--        (Parent => Recruit_Canvas, Child_Type => "window",
--         Options => "32 0 -anchor nw -window " & Frame & " -tag info");
--      Tcl_Eval(interp => Interp, strng => "update");
--      configure
--        (Widgt => Recruit_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Recruit_Canvas, TagOrId => "all") & "]");
--      return TCL_OK;
--   end Show_Recruit_Tab_Command;

   -- ****o* RecruitUI/RecruitUI.Negotiate_Command
   -- FUNCTION
   -- Show negotation UI to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Negotiate
   -- SOURCE
   function Negotiate_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Characters.Latin_1;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Tcl.Tk.Ada.Widgets.Text;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkScale;
      use Tcl.Tklib.Ada.Tooltip;
      use Config;
      use Dialogs;
      use Ships.Crew;

      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(Base_Index).Recruits,
           Index => Get_Recruit_Index);
      Negotiate_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".negotiatedialog",
           Title =>
             "Negotiate with " &
             Tiny_String.To_String(Source => Recruit.Name));
      Frame: constant Ttk_Frame :=
        Create(pathName => Negotiate_Dialog & ".buttonbox");
      Hire_Button: constant Ttk_Button :=
        Create
          (pathName => Frame & ".hirebutton",
           options =>
             "-text Hire -command {Hire} -image negotiate2icon -style Dialoggreen.TButton");
      Dialog_Close_Button: constant Ttk_Button :=
        Create
          (pathName => Negotiate_Dialog & ".buttonbox.button",
           options =>
             "-text Close -command {CloseDialog " & Negotiate_Dialog &
             "} -image cancelicon -style Dialogred.TButton");
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      Spinbox: Ttk_SpinBox;
      Label_Frame: Ttk_Frame;
      Contract_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Negotiate_Dialog & ".contract",
           options =>
             "-state readonly -values [list {Pernament} {100 days} {30 days} {20 days} {10 days}]");
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Cost: Positive;
      Money_Info: constant Tk_Text :=
        Create
          (pathName => Negotiate_Dialog & ".cost",
           options => "-height 2 -width 22 -wrap char");
   begin
      Label_Frame := Create(pathName => Negotiate_Dialog & ".dailylbl");
      Label :=
        Create
          (pathName => Label_Frame & ".label",
           options => "-text {Daily payment:}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-pady {5 0}");
      Tcl_SetVar
        (interp => Interp, varName => "daily",
         newValue =>
           Trim(Source => Natural'Image(Recruit.Payment), Side => Left));
      Spinbox :=
        Create
          (pathName => Label_Frame & ".field",
           options =>
             "-from 0 -to" & Natural'Image(Recruit.Payment * 2) &
             " -width 5 -textvariable daily -validate key -validatecommand {ValidateNegotiate %W %P} -command {ValidateNegotiate " &
             Label_Frame & ".field}");
      Bind
        (Widgt => Spinbox, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Spinbox, Options => "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label_Frame);
      Scale :=
        Create
          (pathName => Negotiate_Dialog & ".daily",
           options =>
             "-from 0 -command NegotiateHire -length 250 -to" &
             Natural'Image(Recruit.Payment * 2) & " -variable daily");
      Bind
        (Widgt => Scale, Sequence => "<Escape>",
         Script => "{" & Negotiate_Dialog & ".buttonbox.button invoke;break}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Scale);
      Label_Frame := Create(pathName => Negotiate_Dialog & ".percentlbl");
      Label :=
        Create
          (pathName => Label_Frame & ".label",
           options => "-text {Percent of profit from trades:}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx {5 0}");
      Tcl_SetVar(interp => Interp, varName => "percent", newValue => "0");
      Spinbox :=
        Create
          (pathName => Label_Frame & ".field",
           options =>
             "-from 0 -to 10 -width 2 -textvariable percent -validate key -validatecommand {ValidateNegotiate %W %P} -command {ValidateNegotiate " &
             Label_Frame & ".field}");
      Bind
        (Widgt => Spinbox, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Spinbox, Options => "-row 0 -column 1 -padx {0 5}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label_Frame, Options => "-padx 5");
      Scale :=
        Create
          (pathName => Negotiate_Dialog & ".percent",
           options =>
             "-from 0 -to 10 -command NegotiateHire -length 250 -variable percent");
      Tcl.Tk.Ada.Grid.Grid(Slave => Scale);
      Label :=
        Create
          (pathName => Negotiate_Dialog & ".contractlbl",
           options => "-text {Contract time:}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Tcl.Tk.Ada.Grid.Grid(Slave => Contract_Box);
      Bind
        (Widgt => Contract_Box, Sequence => "<<ComboboxSelected>>",
         Script => "{NegotiateHire}");
      Bind
        (Widgt => Scale, Sequence => "<Tab>",
         Script => "{focus " & Contract_Box & ";break}");
      Bind
        (Widgt => Scale, Sequence => "<Escape>",
         Script => "{" & Negotiate_Dialog & ".buttonbox.button invoke;break}");
      Current(ComboBox => Contract_Box, NewIndex => "0");
      Add(Widget => Hire_Button, Message => "Hire the selected recruit.");
      Bind
        (Widgt => Contract_Box, Sequence => "<Tab>",
         Script => "{focus " & Hire_Button & ";break}");
      Bind
        (Widgt => Contract_Box, Sequence => "<Escape>",
         Script => "{" & Negotiate_Dialog & ".buttonbox.button invoke;break}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Money_Info);
      Cost := Recruit.Price;
      Count_Price(Price => Cost, Trader_Index => Find_Member(Order => TALK));
      Tag_Configure
        (TextWidget => Money_Info, TagName => "red",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-red)"));
      Tag_Configure
        (TextWidget => Money_Info, TagName => "gold",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-goldenyellow)"));
      if Money_Index_2 > 0 then
         Insert
           (TextWidget => Money_Info, Index => "end", Text => "{You have}");
         Insert
           (TextWidget => Money_Info, Index => "end",
            Text =>
              "{" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
                   .Amount) &
              "} [list gold]");
         Insert
           (TextWidget => Money_Info, Index => "end",
            Text => "{ " & To_String(Source => Money_Name) & "}");
         if Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Money_Index_2)
             .Amount <
           Cost then
            configure(Widgt => Hire_Button, options => "-state disabled");
         else
            configure(Widgt => Hire_Button, options => "-state !disabled");
         end if;
      else
         Insert
           (TextWidget => Money_Info, Index => "end",
            Text =>
              "{You don't have enough money to recruit anyone} [list red]");
         configure(Widgt => Hire_Button, options => "-state disabled");
      end if;
      Insert
        (TextWidget => Money_Info, Index => "end",
         Text => "{" & LF & "Hire for}");
      Insert
        (TextWidget => Money_Info, Index => "end",
         Text => "{" & Positive'Image(Cost) & "} [list gold]");
      Insert
        (TextWidget => Money_Info, Index => "end",
         Text => "{ " & To_String(Source => Money_Name) & "}");
      configure(Widgt => Money_Info, options => "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(Slave => Hire_Button);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Dialog_Close_Button, Options => "-row 0 -column 1");
      Add
        (Widget => Dialog_Close_Button,
         Message => "Cancel negotiation \[Escape key\]");
      Tcl.Tk.Ada.Grid.Grid(Slave => Frame, Options => "-pady {0 5}");
      Focus(Widgt => Dialog_Close_Button);
      Bind
        (Widgt => Dialog_Close_Button, Sequence => "<Tab>",
         Script => "{focus " & Negotiate_Dialog & ".dailylbl.field;break}");
      Bind
        (Widgt => Hire_Button, Sequence => "<Tab>",
         Script => "{focus " & Dialog_Close_Button & ";break}");
      Bind
        (Widgt => Negotiate_Dialog, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Bind
        (Widgt => Dialog_Close_Button, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Show_Dialog(Dialog => Negotiate_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Negotiate_Command;

   -- ****it* RecruitUI/RecruitUI.Recruits_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the list of available recruits in base
   -- OPTIONS
   -- NAMEASC       - Sort recruits by name ascending
   -- NAMEDESC      - Sort recruits by name descending
   -- GENDERASC     - Sort recruits by gender ascending
   -- GENDERDESC    - Sort recruits by gender descending
   -- FACTIONASC    - Sort recruits by faction ascending
   -- FACTIONDESC   - Sort recruits by faction descending
   -- PRICEASC      - Sort recruits by price ascending
   -- PRICEDESC     - Sort recruits by price descending
   -- ATTRIBUTEASC  - Sort recruits by attribute ascending
   -- ATTRIBUTEDESC - Sort recruits by attribute descending
   -- SKILLASC      - Sort recruits by skill ascending
   -- SKILLDESC     - Sort recruits by skill descending
   -- NONE       - No sorting recruits (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Recruits_Sort_Orders is
     (NAMEASC, NAMEDESC, GENDERASC, GENDERDESC, FACTIONDESC, FACTIONASC,
      PRICEASC, PRICEDESC, ATTRIBUTEASC, ATTRIBUTEDESC, SKILLASC, SKILLDESC,
      NONE) with
      Default_Value => NONE;
      -- ****

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      -- ****id* RecruitUI/RecruitUI.Default_Recruits_Sort_Order
      -- FUNCTION
      -- Default sorting order for the available recruits in base
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Recruits_Sort_Order: constant Recruits_Sort_Orders := NONE;
   -- ****

   -- ****iv* RecruitUI/RecruitUI.Recruits_Sort_Order
   -- FUNCTION
   -- The current sorting order for the available recruits in base
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Recruits_Sort_Order: Recruits_Sort_Orders := Default_Recruits_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* RecruitUI/RecruitUI.Sort_Recruits_Command
   -- FUNCTION
   -- Sort the list of available recruits in base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortRecruits x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Recruits_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Recruits_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Recruit_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Module_Data is record
         Name: Bounded_String;
         Gender: Character;
         Faction: Bounded_String;
         Price: Positive;
         Attribute: Unbounded_String;
         Skill: Unbounded_String;
         Id: Positive;
      end record;
      type Recruits_Array is array(Positive range <>) of Local_Module_Data;
      --## rule on TYPE_INITIAL_VALUES
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      --## rule off IMPROPER_INITIALIZATION
      Local_Recruits: Recruits_Array
        (1 ..
             Positive
               (Recruit_Container.Length
                  (Container => Sky_Bases(Base_Index).Recruits)));
      --## rule off IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Recruits_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Recruits_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Recruits_Sort_Order = GENDERASC
           and then Left.Gender < Right.Gender then
            return True;
         end if;
         if Recruits_Sort_Order = GENDERDESC
           and then Left.Gender > Right.Gender then
            return True;
         end if;
         if Recruits_Sort_Order = FACTIONASC
           and then Left.Faction < Right.Faction then
            return True;
         end if;
         if Recruits_Sort_Order = FACTIONDESC
           and then Left.Faction > Right.Faction then
            return True;
         end if;
         if Recruits_Sort_Order = PRICEASC
           and then Left.Price < Right.Price then
            return True;
         end if;
         if Recruits_Sort_Order = PRICEDESC
           and then Left.Price > Right.Price then
            return True;
         end if;
         if Recruits_Sort_Order = ATTRIBUTEASC
           and then Left.Attribute < Right.Attribute then
            return True;
         end if;
         if Recruits_Sort_Order = ATTRIBUTEDESC
           and then Left.Attribute > Right.Attribute then
            return True;
         end if;
         if Recruits_Sort_Order = SKILLASC
           and then Left.Skill < Right.Skill then
            return True;
         end if;
         if Recruits_Sort_Order = SKILLDESC
           and then Left.Skill > Right.Skill then
            return True;
         end if;
         return False;
      end "<";
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      procedure Sort_Recruits is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Recruits_Array);
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      case Column is
         when 1 =>
            if Recruits_Sort_Order = NAMEASC then
               Recruits_Sort_Order := NAMEDESC;
            else
               Recruits_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Recruits_Sort_Order = GENDERASC then
               Recruits_Sort_Order := GENDERDESC;
            else
               Recruits_Sort_Order := GENDERASC;
            end if;
         when 3 =>
            if Recruits_Sort_Order = FACTIONASC then
               Recruits_Sort_Order := FACTIONDESC;
            else
               Recruits_Sort_Order := FACTIONASC;
            end if;
         when 4 =>
            if Recruits_Sort_Order = PRICEASC then
               Recruits_Sort_Order := PRICEDESC;
            else
               Recruits_Sort_Order := PRICEASC;
            end if;
         when 5 =>
            if Recruits_Sort_Order = ATTRIBUTEASC then
               Recruits_Sort_Order := ATTRIBUTEDESC;
            else
               Recruits_Sort_Order := ATTRIBUTEASC;
            end if;
         when 6 =>
            if Recruits_Sort_Order = SKILLASC then
               Recruits_Sort_Order := SKILLDESC;
            else
               Recruits_Sort_Order := SKILLASC;
            end if;
         when others =>
            null;
      end case;
      if Recruits_Sort_Order = NONE then
         return TCL_OK;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Fill_Local_Recruits_Loop :
      for I in
        Recruit_Container.First_Index
          (Container => Sky_Bases(Base_Index).Recruits) ..
          Recruit_Container.Last_Index
            (Container => Sky_Bases(Base_Index).Recruits) loop
         Local_Recruits(I) :=
           (Name =>
              Recruit_Container.Element
                (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                .Name,
            Gender =>
              Recruit_Container.Element
                (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                .Gender,
            Faction =>
              Recruit_Container.Element
                (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                .Faction,
            Price =>
              Recruit_Container.Element
                (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                .Price,
            Attribute =>
              Get_Highest_Attribute
                (Base_Index => Base_Index, Member_Index => I),
            Skill =>
              Get_Highest_Skill(Base_Index => Base_Index, Member_Index => I),
            Id => I);
      end loop Fill_Local_Recruits_Loop;
      Sort_Recruits(Container => Local_Recruits);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Recruits_Indexes.Clear;
      Fill_Recruit_Indexes_Loop :
      for Recruit of Local_Recruits loop
         Recruits_Indexes.Append(New_Item => Recruit.Id);
      end loop Fill_Recruit_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Recruit_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowRecruits" & "1");
   end Sort_Recruits_Command;

   -- ****o* RecruitUI/RecruitUI.Validate_Negotiate_Command
   -- FUNCTION
   -- Validate value of numeric fields in negotiate dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateNegotiate field value
   -- Field is Tcl path to the field which will be validated, value is
   -- the new value of the field to validate
   -- SOURCE
   function Validate_Negotiate_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Negotiate_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => CArgv.Arg(Argv => Argv, N => 1));
      Value: constant String :=
        (if Argc = 3 then CArgv.Arg(Argv => Argv, N => 2)
         else Get(Widgt => Spin_Box));
   begin
      if Value = "" then
         Tcl_SetResult(interp => Interp, str => "1");
         return TCL_OK;
      end if;
      Tcl_Eval
        (interp => Interp,
         strng =>
           "ValidateSpinbox " & CArgv.Arg(Argv => Argv, N => 1) & " " & Value &
           " {}");
      if Tcl_GetStringResult(interp => Interp) = "0" then
         return TCL_OK;
      end if;
      Tcl_Eval(interp => Interp, strng => "NegotiateHire");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Validate_Negotiate_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowRecruit", Ada_Command => Show_Recruit_Command'Access);
      Add_Command
        (Name => "ShowRecruitInfo",
         Ada_Command => Show_Recruit_Info_Command'Access);
      Add_Command
        (Name => "NegotiateHire",
         Ada_Command => Negotiate_Hire_Command'Access);
      Add_Command(Name => "Hire", Ada_Command => Hire_Command'Access);
      Add_Command
        (Name => "ShowRecruitTab",
         Ada_Command => Show_Recruit_Tab_Command'Access);
      Add_Command
        (Name => "Negotiate", Ada_Command => Negotiate_Command'Access);
      Add_Command
        (Name => "SortRecruits", Ada_Command => Sort_Recruits_Command'Access);
      Add_Command
        (Name => "ValidateNegotiate",
         Ada_Command => Validate_Negotiate_Command'Access);
   end Add_Commands;

end Bases.RecruitUI;
