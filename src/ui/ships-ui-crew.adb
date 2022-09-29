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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with CoreUI; use CoreUI;
with Crafts; use Crafts;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Game; use Game.Tiny_String;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Crew; use Ships.Crew;
with Ships.UI.Crew.Inventory;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew is

   -- ****iv* SUCrew/SUCrew.Crew_Table
   -- FUNCTION
   -- Table with info about the player's ship crew
   -- SOURCE
   Crew_Table: Table_Widget (Amount => 8);
   -- ****

   -- ****iv* SUCrew/SUCrew.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship crew
   -- SOURCE
   Crew_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* SUCrew/SUCrew.Get_Highest_Skill
   -- FUNCTION
   -- Get the name of the highest skill of the selected crew member
   -- PARAMETERS
   -- Member_Index - The index of the selected crew member which skill will
   --                be get
   -- RESULT
   -- The name of the highest skill of the selected crew member
   -- HISTORY
   -- 6.9 - Added
   -- SOURCE
   function Get_Highest_Skill(Member_Index: Positive) return String is
      -- ****
      Highest_Level: Skill_Range := 1;
      Highest_Index: Skills_Amount_Range := 1;
   begin
      Get_Highest_Skill_Level_Loop :
      for Skill of Player_Ship.Crew(Member_Index).Skills loop
         if Skill.Level > Highest_Level then
            Highest_Level := Skill.Level;
            Highest_Index := Skill.Index;
         end if;
      end loop Get_Highest_Skill_Level_Loop;
      return
        To_String
          (Source =>
             SkillsData_Container.Element
               (Container => Skills_List, Index => Highest_Index)
               .Name);
   end Get_Highest_Skill;

   procedure Update_Crew_Info(Page: Positive := 1; Skill: Natural := 0) is
      Buttons_Frame: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Ship_Canvas: Tk_Canvas;
      Need_Repair, Need_Clean: Boolean := False;
      Button: Ttk_Button;
      Tired_Level: Integer;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Crew_Info_Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe.crew.canvas.frame");
      Orders_Label: Ttk_Label;
      Skill_Box: Ttk_ComboBox;
   begin
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Crew_Info_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets
        (Start_Index => 1, End_Index => Rows - 1, Frame => Crew_Info_Frame);
      Buttons_Frame := Create(pathName => Crew_Info_Frame & ".ordersbuttons");
      Check_Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.Durability < Module.Max_Durability then
            Need_Repair := True;
         end if;
         if (Module.Durability > 0 and Module.M_Type = CABIN)
           and then Module.Cleanliness < Module.Quality then
            Need_Clean := True;
         end if;
         exit Check_Modules_Loop when Need_Clean and Need_Repair;
      end loop Check_Modules_Loop;
      if Need_Clean then
         Orders_Label :=
           Create
             (pathName => Buttons_Frame & ".label",
              options => "-text {Orders for all:}");
         Add
           (Widget => Orders_Label,
            Message => "Give the selected order to the whole crew.");
         Tcl.Tk.Ada.Grid.Grid(Slave => Orders_Label, Options => "-padx {5 2}");
         Button :=
           Create
             (pathName => Buttons_Frame & ".clean",
              options => "-image cleanordericon -command {OrderForAll Clean}");
         Add(Widget => Button, Message => "Clean ship everyone");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-row 0 -column 1 -padx {0 2}");
      end if;
      if Need_Repair then
         Button :=
           Create
             (pathName => Buttons_Frame & ".repair",
              options =>
                "-image repairordericon -command {OrderForAll Repair}");
         Add(Widget => Button, Message => "Repair ship everyone");
         if Need_Clean then
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-row 0 -column 2");
         else
            Orders_Label :=
              Create
                (pathName => Buttons_Frame & ".label",
                 options => "-text {Orders for all:}");
            Add
              (Widget => Orders_Label,
               Message => "Give the selected order to the whole crew.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Orders_Label, Options => "-padx {5 2}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-row 0 -column 1");
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid(Slave => Buttons_Frame, Options => "-sticky w");
      Buttons_Frame := Create(pathName => Crew_Info_Frame & ".selectskill");
      Orders_Label :=
        Create
          (pathName => Buttons_Frame & ".label", options => "-text {Skill:}");
      Add
        (Widget => Orders_Label,
         Message =>
           "Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.");
      Tcl.Tk.Ada.Grid.Grid(Slave => Orders_Label, Options => "-padx {5 2}");
      Show_Skill_Box_Block :
      declare
         Skills: Unbounded_String :=
           To_Unbounded_String(Source => " {Highest}");
      begin
         Load_Skills_Loop :
         for I in 1 .. Skills_Amount loop
            Load_Skills_Block :
            declare
               Skill: constant Skill_Record :=
                 SkillsData_Container.Element
                   (Container => Skills_List, Index => I);
            begin
               Append
                 (Source => Skills,
                  New_Item => " {" & To_String(Source => Skill.Name) & "}");
            end Load_Skills_Block;
         end loop Load_Skills_Loop;
         Skill_Box :=
           Create
             (pathName => Crew_Info_Frame & ".selectskill.combox",
              options =>
                "-state readonly -values [list" & To_String(Source => Skills) &
                "]");
         Bind
           (Widgt => Skill_Box, Sequence => "<<ComboboxSelected>>",
            Script => "SelectCrewSkill");
         Current(ComboBox => Skill_Box, NewIndex => Natural'Image(Skill));
         Add
           (Widget => Skill_Box,
            Message =>
              "Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Skill_Box, Options => "-row 0 -column 1");
      end Show_Skill_Box_Block;
      Tcl.Tk.Ada.Grid.Grid(Slave => Buttons_Frame, Options => "-sticky w");
      Crew_Table :=
        Create_Table
          (Parent => Widget_Image(Win => Crew_Info_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Name"),
              2 => To_Unbounded_String(Source => "Order"),
              3 => To_Unbounded_String(Source => "Skill"),
              4 => To_Unbounded_String(Source => "Health"),
              5 => To_Unbounded_String(Source => "Fatigue"),
              6 => To_Unbounded_String(Source => "Thirst"),
              7 => To_Unbounded_String(Source => "Hunger"),
              8 => To_Unbounded_String(Source => "Morale")),
           Scrollbar =>
             Get_Widget
               (pathName => ".gameframe.paned.shipinfoframe.crew.scrolly"),
           Command => "SortShipCrew",
           Tooltip => "Press mouse button to sort the crew.");
      if Crew_Indexes.Length /= Player_Ship.Crew.Length then
         Crew_Indexes.Clear;
         Update_Crew_Indexes_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Crew_Indexes.Append
              (New_Item => Crew_Container.To_Index(Position => I));
         end loop Update_Crew_Indexes_Loop;
      end if;
      Load_Crew_Loop :
      for I of Crew_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Add_Button
           (Table => Crew_Table,
            Text => To_String(Source => Player_Ship.Crew(I).Name),
            Tooltip => "Show available crew member's options",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 1);
         Add_Button
           (Table => Crew_Table,
            Text =>
              Crew_Orders'Image(Player_Ship.Crew(I).Order)(1) &
              To_Lower
                (Item =>
                   Crew_Orders'Image(Player_Ship.Crew(I).Order)
                     (2 .. Crew_Orders'Image(Player_Ship.Crew(I).Order)'Last)),
            Tooltip =>
              "The current order for the selected crew member.\nPress the mouse button to change it.",
            Command => "ShowCrewOrder" & Positive'Image(I), Column => 2);
         if Skill = 0 then
            Add_Button
              (Table => Crew_Table,
               Text => Get_Highest_Skill(Member_Index => I),
               Tooltip => "The highest skill of the selected crew member",
               Command => "ShowMemberMenu" & Positive'Image(I), Column => 3);
         else
            Add_Button
              (Table => Crew_Table,
               Text =>
                 Get_Skill_Level_Name
                   (Skill_Level =>
                      Get_Skill_Level
                        (Member => Player_Ship.Crew(I),
                         Skill_Index => Skills_Amount_Range(Skill))),
               Tooltip =>
                 "The level of the " & Get(Widgt => Skill_Box) &
                 " of the selected crew member",
               Command => "ShowMemberMenu" & Positive'Image(I), Column => 3);
         end if;
         Add_Progress_Bar
           (Table => Crew_Table, Value => Player_Ship.Crew(I).Health,
            Max_Value => Skill_Range'Last,
            Tooltip => "The current health level of the selected crew member",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 4);
         Tired_Level :=
           Player_Ship.Crew(I).Tired -
           Player_Ship.Crew(I).Attributes(Positive(Condition_Index)).Level;
         if Tired_Level < 0 then
            Tired_Level := 0;
         end if;
         Add_Progress_Bar
           (Table => Crew_Table, Value => Tired_Level,
            Max_Value => Skill_Range'Last,
            Tooltip => "The current tired level of the selected crew member",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 5,
            New_Row => False, Invert_Colors => True);
         Add_Progress_Bar
           (Table => Crew_Table, Value => Player_Ship.Crew(I).Thirst,
            Max_Value => Skill_Range'Last,
            Tooltip => "The current thirst level of the selected crew member",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 6,
            New_Row => False, Invert_Colors => True);
         Add_Progress_Bar
           (Table => Crew_Table, Value => Player_Ship.Crew(I).Hunger,
            Max_Value => Skill_Range'Last,
            Tooltip => "The current hunger level of the selected crew member",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 7,
            New_Row => False, Invert_Colors => True);
         Add_Progress_Bar
           (Table => Crew_Table, Value => Player_Ship.Crew(I).Morale(1),
            Max_Value => Skill_Range'Last,
            Tooltip => "The current morale level of the selected crew member",
            Command => "ShowMemberMenu" & Positive'Image(I), Column => 8,
            New_Row => True);
         exit Load_Crew_Loop when Crew_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Crew_Loop;
      if Page > 1 then
         Add_Pagination
           (Table => Crew_Table,
            Previous_Command =>
              "ShowCrew" & Positive'Image(Page - 1) & Natural'Image(Skill),
            Next_Command =>
              (if Crew_Table.Row < Game_Settings.Lists_Limit + 1 then ""
               else "ShowCrew" & Positive'Image(Page + 1)) &
              Natural'Image(Skill));
      elsif Crew_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Crew_Table,
            Next_Command =>
              "ShowCrew" & Positive'Image(Page + 1) & Natural'Image(Skill));
      end if;
      Update_Table(Table => Crew_Table);
      Tcl_Eval(interp => Get_Context, strng => "update");
      Ship_Canvas :=
        Get_Widget(pathName => Main_Paned & ".shipinfoframe.crew.canvas");
      configure
        (Widgt => Ship_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Ship_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
   end Update_Crew_Info;

   -- ****o* SUCrew/SUCrew.Order_For_All_Command
   -- FUNCTION
   -- Set the selected order for the whole crew
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- OrderForAll order
   -- Order is the name of the order which will be assigned to the whole
   -- player ship crew
   -- SOURCE
   function Order_For_All_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Order_For_All_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Give_Orders_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Give_Orders
           (Ship => Player_Ship,
            Member_Index => Crew_Container.To_Index(Position => I),
            Given_Order => Crew_Orders'Value(CArgv.Arg(Argv => Argv, N => 1)));
      end loop Give_Orders_Loop;
      Update_Header;
      Update_Messages;
      Update_Crew_Info;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error =>
         Add_Message
           (Message => Exception_Message(X => An_Exception),
            M_Type => ORDERMESSAGE);
         Update_Header;
         Update_Messages;
         return TCL_OK;
   end Order_For_All_Command;

   -- ****o* SUCrew/SUCrew.Dismiss_Command
   -- FUNCTION
   -- Dismiss the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Dismiss memberindex
   -- Memberindex is the index of the player ship crew member which will be
   -- dismissed
   -- SOURCE
   function Dismiss_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Dismiss_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Show_Question
        (Question =>
           "Are you sure want to dismiss " &
           To_String(Source => Player_Ship.Crew(Member_Index).Name) & "?",
         Result => CArgv.Arg(Argv => Argv, N => 1));
      return TCL_OK;
   end Dismiss_Command;

   -- ****o* SUCrew/SUCrew.Set_Crew_Order_Command
   -- FUNCTION
   -- Set order for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCrewOrder order memberindex ?moduleindex?
   -- Order is an index for the order which will be set, memberindex is an
   -- index of the member in the player ship crew which will be have order set
   -- and optional parameter moduleindex is index of module in player ship
   -- which will be assigned to the crew member
   -- SOURCE
   function Set_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp);
      Module_Index: Natural := 0;
   begin
      if Argc = 4 then
         Module_Index := Natural'Value(CArgv.Arg(Argv => Argv, N => 3));
      end if;
      Give_Orders
        (Ship => Player_Ship,
         Member_Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 2)),
         Given_Order => Crew_Orders'Value(CArgv.Arg(Argv => Argv, N => 1)),
         Module_Index => Module_Index);
      Update_Header;
      Update_Messages;
      Update_Crew_Info;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
         Add_Message
           (Message => Exception_Message(X => An_Exception),
            M_Type => ORDERMESSAGE, Color => RED);
         Update_Messages;
         return TCL_OK;
   end Set_Crew_Order_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Tab_Command
   -- FUNCTION
   -- Show the selected information about the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberTab
   -- SOURCE
   function Show_Member_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Member_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => ".memberdialog.canvas", Interp => Interp);
      Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName =>
             Member_Canvas & "." &
             Tcl_GetVar(interp => Interp, varName => "newtab"));
   begin
      Delete(CanvasWidget => Member_Canvas, TagOrId => "info");
      Canvas_Create
        (Parent => Member_Canvas, Child_Type => "window",
         Options => "32 0 -anchor nw -window " & Frame & " -tag info");
      Tcl_Eval(interp => Interp, strng => "update");
      configure
        (Widgt => Member_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Member_Canvas, TagOrId => "all") & "]");
      return TCL_OK;
   end Show_Member_Tab_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Info_Command
   -- FUNCTION
   -- Show information about the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberInfo memberindex
   -- MemberIndex is the index of the crew member to show
   -- SOURCE
   function Show_Member_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Member: constant Member_Data := Player_Ship.Crew(Member_Index);
      Member_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog", Title => "Crew member details",
           Columns => 2);
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Member_Dialog & ".yscroll",
           options =>
             "-orient vertical -command [list .memberdialog.canvas yview]");
      Member_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Member_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Close_Button: constant Ttk_Button :=
        Get_Widget(pathName => Member_Dialog & ".button", Interp => Interp);
      Progress_Frame: Ttk_Frame;
      Member_Info: Unbounded_String := Null_Unbounded_String;
      Member_Label: Ttk_Label;
      Tired_Points: Integer;
      Progress_Bar: Ttk_ProgressBar;
      Tab_Button: Ttk_RadioButton;
      Info_Button: Ttk_Button;
      Frame: Ttk_Frame;
   begin
      Frame := Create(pathName => Member_Dialog & ".buttonbox");
      Tcl_SetVar(interp => Interp, varName => "newtab", newValue => "general");
      Tab_Button :=
        Create
          (pathName => Frame & ".general",
           options =>
             " -text General -state selected -style Radio.Toolbutton -value general -variable newtab -command ShowMemberTab");
      Tcl.Tk.Ada.Grid.Grid(Slave => Tab_Button);
      Bind
        (Widgt => Tab_Button, Sequence => "<Escape>",
         Script => "{" & Close_Button & " invoke;break}");
      if Skills_Container.Length(Container => Member.Skills) > 0 and
        Member.Contract_Length /= 0 then
         Tab_Button :=
           Create
             (pathName => Frame & ".stats",
              options =>
                " -text Attributes -style Radio.Toolbutton -value stats -variable newtab -command ShowMemberTab");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Tab_Button, Options => "-column 1 -row 0");
         Bind
           (Widgt => Tab_Button, Sequence => "<Escape>",
            Script => "{" & Close_Button & " invoke;break}");
         Tab_Button :=
           Create
             (pathName => Frame & ".skills",
              options =>
                " -text Skills -style Radio.Toolbutton -value skills -variable newtab -command ShowMemberTab");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Tab_Button, Options => "-column 2 -row 0");
         Bind
           (Widgt => Tab_Button, Sequence => "<Escape>",
            Script => "{" & Close_Button & " invoke;break}");
      else
         Bind
           (Widgt => Tab_Button, Sequence => "<Tab>",
            Script => "{focus " & Close_Button & ";break}");
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Frame, Options => "-pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Member_Canvas, Options => "-sticky nwes -pady 5 -padx 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Y_Scroll,
         Options => " -sticky ns -pady 5 -padx {0 5} -row 2 -column 1");
      Add_Close_Button
        (Name => Member_Dialog & ".button", Text => "Close",
         Command => "CloseDialog " & Member_Dialog, Column_Span => 2,
         Row => 3);
      Autoscroll(Scroll => Y_Scroll);
      -- General info about the selected crew member
      Frame := Create(pathName => Member_Canvas & ".general");
      Add_Name_Info_Block :
      declare
         Name_Box: constant Ttk_Frame :=
           Create(pathName => Frame & ".nameinfo", options => "-width 360");
      begin
         Member_Label :=
           Create
             (pathName => Name_Box & ".info",
              options =>
                "-text {Name: " & To_String(Source => Member.Name) &
                " } -wraplength 325");
         Tcl.Tk.Ada.Grid.Grid(Slave => Member_Label, Options => "-sticky w");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
         Info_Button :=
           Create
             (pathName => Name_Box & ".button",
              options =>
                "-image giveordericon -command {" & Close_Button &
                " invoke;ShowCrewOrder " & Positive'Image(Member_Index) &
                "} -style Small.TButton");
         Add(Widget => Info_Button, Message => "Rename the crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Info_Button,
            Options => "-row 0 -column 1 -sticky n -padx {5 0}");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Name_Box, Options => "-sticky w -padx 5");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Name_Box & " " & Y_Scroll);
      end Add_Name_Info_Block;
      if Member.Health < 100 then
         if Game_Settings.Show_Numbers then
            Member_Label :=
              Create
                (pathName => Frame & ".health",
                 options =>
                   "-text {Health:" & Natural'Image(Member.Health) & "%}");
         else
            case Member.Health is
               when 81 .. 99 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".health",
                       options => "-text {Slightly wounded}");
               when 51 .. 80 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".health",
                       options => "-text {Wounded}");
               when 1 .. 50 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".health",
                       options => "-text {Heavily wounded}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Member_Label, Options => "-sticky w -padx 5");
      end if;
      Tired_Points :=
        Member.Tired - Member.Attributes(Positive(Condition_Index)).Level;
      if Tired_Points < 0 then
         Tired_Points := 0;
      end if;
      if Tired_Points > 0 then
         if Game_Settings.Show_Numbers then
            Member_Label :=
              Create
                (pathName => Frame & ".tired",
                 options =>
                   "-text {Tiredness:" & Natural'Image(Tired_Points) & "%}");
         else
            case Tired_Points is
               when 1 .. 40 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".tired",
                       options => "-text {Bit tired}");
               when 41 .. 80 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".tired",
                       options => "-text {Tired}");
               when 81 .. 99 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".tired",
                       options => "-text {Very tired}");
               when 100 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".tired",
                       options => "-text {Unconscious}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Member_Label, Options => "-sticky w -padx 5");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
      end if;
      if Member.Thirst > 0 then
         if Game_Settings.Show_Numbers then
            Member_Label :=
              Create
                (pathName => Frame & ".thirst",
                 options =>
                   "-text {Thirst:" & Natural'Image(Member.Thirst) & "%}");
         else
            case Member.Thirst is
               when 1 .. 40 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".thirst",
                       options => "-text {Bit thirsty}");
               when 41 .. 80 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".thirst",
                       options => "-text {Thirsty}");
               when 81 .. 99 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".thirst",
                       options => "-text {Very thirsty}");
               when 100 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".thirst",
                       options => "-text {Dehydrated}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Member_Label, Options => "-sticky w -padx 5");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
      end if;
      if Member.Hunger > 0 then
         if Game_Settings.Show_Numbers then
            Member_Label :=
              Create
                (pathName => Frame & ".hunger",
                 options =>
                   "-text {Hunger:" & Natural'Image(Member.Hunger) & "%}");
         else
            case Member.Hunger is
               when 1 .. 40 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".hunger",
                       options => "-text {Bit hungry}");
               when 41 .. 80 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".hunger",
                       options => "-text {Hungry}");
               when 81 .. 99 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".hunger",
                       options => "-text {Very hungry}");
               when 100 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".hunger",
                       options => "-text {Starving}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Member_Label, Options => "-sticky w -padx 5");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
      end if;
      if Member.Morale(1) /= 50 then
         if Game_Settings.Show_Numbers then
            Member_Label :=
              Create
                (pathName => Frame & ".morale",
                 options =>
                   "-text {Morale:" & Natural'Image(Member.Morale(1)) & "%}");
         else
            case Member.Morale(1) is
               when 0 .. 24 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".morale",
                       options => "-text {Upset}");
               when 25 .. 49 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".morale",
                       options => "-text {Unhappy}");
               when 51 .. 74 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".morale",
                       options => "-text {Happy}");
               when 75 .. 100 =>
                  Member_Label :=
                    Create
                      (pathName => Frame & ".morale",
                       options => "-text {Excited}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Member_Label, Options => "-sticky w -padx 5");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
      end if;
      if Skills_Container.Length(Container => Member.Skills) > 0 then
         Add_Order_Info_Block :
         declare
            Order_Box: constant Ttk_Frame :=
              Create
                (pathName => Frame & ".orderinfo", options => "-width 360");
         begin
            Member_Label :=
              Create
                (pathName => Order_Box & ".info",
                 options =>
                   "-text {Order: " &
                   To_String
                     (Source =>
                        Get_Current_Order(Member_Index => Member_Index)) &
                   " } -wraplength 325");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Member_Label, Options => "-sticky w");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
            Info_Button :=
              Create
                (pathName => Order_Box & ".button",
                 options =>
                   "-image giveordericon -command {" & Close_Button &
                   " invoke;ShowCrewOrder " & Positive'Image(Member_Index) &
                   "} -style Small.TButton");
            Add
              (Widget => Info_Button,
               Message => "Set the new order for the crew member.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options => "-row 0 -column 1 -sticky n -padx {5 0}");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Order_Box, Options => "-sticky w -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng => "SetScrollbarBindings " & Order_Box & " " & Y_Scroll);
         end Add_Order_Info_Block;
      end if;
      if Factions_List(Member.Faction).Flags.Find_Index
          (Item => To_Unbounded_String(Source => "nogender")) =
        UnboundedString_Container.No_Index then
         Append
           (Source => Member_Info,
            New_Item =>
              "Gender: " &
              (if Member.Gender = 'M' then
                 To_Unbounded_String(Source => "Male")
               else To_Unbounded_String(Source => "Female")) &
              LF);
      end if;
      Append
        (Source => Member_Info,
         New_Item =>
           "Faction: " &
           To_String(Source => Factions_List(Member.Faction).Name) & LF &
           "Home base: " &
           To_String(Source => Sky_Bases(Member.Home_Base).Name));
      if Skills_Container.Length(Container => Member.Skills) = 0 or
        Member.Contract_Length = 0 then
         Append(Source => Member_Info, New_Item => LF & "Passenger");
         if Member.Contract_Length > 0 then
            Append(Source => Member_Info, New_Item => LF & "Time limit:");
            Minutes_To_Date
              (Minutes => Member.Contract_Length, Info_Text => Member_Info);
         end if;
      else
         if Member_Index > 1 then
            Append(Source => Member_Info, New_Item => LF & "Contract length:");
            Append
              (Source => Member_Info,
               New_Item =>
                 (if Member.Contract_Length > 0 then
                    Integer'Image(Member.Contract_Length) & " days."
                  else " pernament.") &
                 LF & "Payment:" & Natural'Image(Member.Payment(1)) & " " &
                 To_String(Source => Money_Name) & " each day");
            if Member.Payment(2) > 0 then
               Append
                 (Source => Member_Info,
                  New_Item =>
                    " and " & Natural'Image(Member.Payment(2)) &
                    " percent of profit from each trade");
            end if;
            Append(Source => Member_Info, New_Item => ".");
         end if;
      end if;
      Member_Label :=
        Create
          (pathName => Frame & ".label",
           options =>
             "-text {" & To_String(Source => Member_Info) &
             "} -wraplength 360");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Member_Label, Options => "-sticky nw -padx 5");
      Tcl_Eval
        (interp => Interp,
         strng => "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
      Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
      Tcl_Eval
        (interp => Interp,
         strng => "SetScrollbarBindings " & Frame & " " & Y_Scroll);
      if Skills_Container.Length(Container => Member.Skills) > 0 and
        Member.Contract_Length /= 0 then
         -- Statistics of the selected crew member
         Frame := Create(pathName => Member_Canvas & ".stats");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Frame & " " & Y_Scroll);
         Load_Statistics_Loop :
         for I in Member.Attributes'Range loop
            Progress_Frame :=
              Create
                (pathName =>
                   Frame & ".statinfo" &
                   Trim(Source => Positive'Image(I), Side => Left));
            Member_Label :=
              Create
                (pathName => Progress_Frame & ".label",
                 options =>
                   "-text {" &
                   To_String
                     (Source =>
                        AttributesData_Container.Element
                          (Container => Attributes_List, Index => I)
                          .Name) &
                   ": " &
                   Get_Attribute_Level_Name
                     (Attribute_Level => Member.Attributes(I).Level) &
                   "}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Member_Label, Options => "-sticky we");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
            Tcl.Tk.Ada.Grid.Column_Configure
              (Master => Progress_Frame, Slave => Member_Label,
               Options => "-weight 1");
            Tcl.Tk.Ada.Grid.Row_Configure
              (Master => Progress_Frame, Slave => Member_Label,
               Options => "-weight 1");
            Info_Button :=
              Create
                (pathName => Progress_Frame & ".button",
                 options =>
                   "-image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo" &
                   Positive'Image(I) & " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Info_Button,
               Message =>
                 "Show detailed information about the selected attribute.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options => "-column 1 -row 0 -padx {5 0}");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Frame, Options => "-sticky we -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Frame & " " & Y_Scroll);
            Tcl_Eval(interp => Interp, strng => "update");
            Progress_Bar :=
              Create
                (pathName =>
                   Frame & ".level" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options =>
                   "-value" &
                   Positive'Image
                     ((if Member.Attributes(I).Level > 2 then
                         Member.Attributes(I).Level * 2
                       else 6)));
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Progress_Bar,
               Message => "The current level of the attribute.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Bar, Options => "-sticky w -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Bar & " " & Y_Scroll);
            Progress_Frame :=
              Create
                (pathName =>
                   Frame & ".experienceframe" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options => "-height 12");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Frame, Options => "-sticky w -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Frame & " " & Y_Scroll);
            Progress_Bar :=
              Create
                (pathName =>
                   Progress_Frame & ".experience" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options =>
                   "-value" &
                   Float'Image
                     (Float(Member.Attributes(I).Experience) /
                      Float(Member.Attributes(I).Level * 250)) &
                   " -maximum 1.0 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Progress_Bar,
               Message => "Experience need to reach the next level");
            Tcl.Tk.Ada.Place.Place
              (Slave => Progress_Bar,
               Options =>
                 "-in " & Progress_Frame & " -relheight 1.0 -relwidth 1.0");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Bar & " " & Y_Scroll);
         end loop Load_Statistics_Loop;
         Load_Statistics_Experience_Loop :
         for I in Member.Attributes'Range loop
            Progress_Bar :=
              Get_Widget
                (pathName =>
                   Frame & ".level" &
                   Trim(Source => Positive'Image(I), Side => Left));
            configure(Widgt => Progress_Bar, options => "-length 360");
            Progress_Frame :=
              Get_Widget
                (pathName =>
                   Frame & ".experienceframe" &
                   Trim(Source => Positive'Image(I), Side => Left));
            configure(Widgt => Progress_Frame, options => "-width 360");
            Progress_Bar :=
              Get_Widget
                (pathName =>
                   Progress_Frame & ".experience" &
                   Trim(Source => Positive'Image(I), Side => Left));
            configure(Widgt => Progress_Bar, options => "-length 360");
         end loop Load_Statistics_Experience_Loop;
         -- Skills of the selected crew member
         Frame := Create(pathName => Member_Canvas & ".skills");
         Tcl_Eval
           (interp => Interp,
            strng => "SetScrollbarBindings " & Frame & " " & Y_Scroll);
         Load_Skills_Loop :
         for I in
           Skills_Container.First_Index(Container => Member.Skills) ..
             Skills_Container.Last_Index(Container => Member.Skills) loop
            Progress_Frame :=
              Create
                (pathName =>
                   Frame & ".skillinfo" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
            Member_Label :=
              Create
                (pathName =>
                   Progress_Frame & ".label" &
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
                   ": " &
                   Get_Skill_Level_Name
                     (Skill_Level =>
                        Skills_Container.Element
                          (Container => Member.Skills, Index => I)
                          .Level) &
                   "}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Member_Label, Options => "-sticky we");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Member_Label & " " & Y_Scroll);
            Tcl.Tk.Ada.Grid.Column_Configure
              (Master => Progress_Frame, Slave => Member_Label,
               Options => "-weight 1");
            Tcl.Tk.Ada.Grid.Row_Configure
              (Master => Progress_Frame, Slave => Member_Label,
               Options => "-weight 1");
            Info_Button :=
              Create
                (pathName => Progress_Frame & ".button",
                 options =>
                   "-image helpicon -style Header.Toolbutton -command {ShowCrewSkillInfo" &
                   Skills_Amount_Range'Image
                     (Skills_Container.Element
                        (Container => Member.Skills, Index => I)
                        .Index) &
                   " " & CArgv.Arg(Argv => Argv, N => 1) & " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Info_Button,
               Message =>
                 "Show detailed information about the selected skill.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Info_Button,
               Options => "-column 1 -row 0 -padx {5 0}");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Info_Button & " " & Y_Scroll);
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Frame, Options => "-sticky we -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Frame & " " & Y_Scroll);
            Tcl_Eval(interp => Interp, strng => "update");
            Progress_Bar :=
              Create
                (pathName =>
                   Frame & ".level" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left),
                 options =>
                   "-value" &
                   Positive'Image
                     (Skills_Container.Element
                        (Container => Member.Skills, Index => I)
                        .Level));
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Progress_Bar,
               Message => "The current level of the skill.");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Bar, Options => "-sticky w -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Bar & " " & Y_Scroll);
            Progress_Frame :=
              Create
                (pathName =>
                   Frame & ".experienceframe" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left),
                 options => "-height 12");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Progress_Frame, Options => "-sticky w -padx 5");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Frame & " " & Y_Scroll);
            Progress_Bar :=
              Create
                (pathName =>
                   Progress_Frame & ".experience" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left),
                 options =>
                   "-value" &
                   Float'Image
                     (Float
                        (Skills_Container.Element
                           (Container => Member.Skills, Index => I)
                           .Experience) /
                      Float
                        ((Skills_Container.Element
                            (Container => Member.Skills, Index => I)
                            .Level *
                          25))) &
                   " -maximum 1.0 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (Widget => Progress_Bar,
               Message => "Experience need to reach the next level");
            Tcl.Tk.Ada.Place.Place
              (Slave => Progress_Bar,
               Options =>
                 "-in " & Progress_Frame & " -relheight 1.0 -relwidth 1.0");
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "SetScrollbarBindings " & Progress_Bar & " " & Y_Scroll);
            Tcl_Eval(interp => Interp, strng => "update");
         end loop Load_Skills_Loop;
         Load_Skill_Experience_Loop :
         for I in
           Skills_Container.First_Index(Container => Member.Skills) ..
             Skills_Container.Last_Index(Container => Member.Skills) loop
            Progress_Bar :=
              Get_Widget
                (pathName =>
                   Frame & ".level" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
            configure(Widgt => Progress_Bar, options => "-length 360");
            Progress_Frame :=
              Get_Widget
                (pathName =>
                   Frame & ".experienceframe" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
            configure(Widgt => Progress_Frame, options => "-width 360");
            Progress_Bar :=
              Get_Widget
                (pathName =>
                   Progress_Frame & ".experience" &
                   Trim(Source => Skills_Amount_Range'Image(I), Side => Left));
            configure(Widgt => Progress_Bar, options => "-length 360");
         end loop Load_Skill_Experience_Loop;
      end if;
      Bind
        (Widgt => Close_Button, Sequence => "<Tab>",
         Script => "{focus " & Member_Dialog & ".buttonbox.general;break}");
      Show_Dialog
        (Dialog => Member_Dialog, Relative_Y => 0.2, Relative_X => 0.2);
      return
        Show_Member_Tab_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 1,
           Argv => CArgv.Empty & "ShowMemberTab");
   end Show_Member_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Stats_Info_Command
   -- FUNCTION
   -- Show the detailed information about the selected crew member statistic
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrewStatsInfo statindex
   -- Statindex is the index of statistic which info will be show
   -- SOURCE
   function Show_Crew_Stats_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Stats_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Short_String;
      Attribute: constant Attribute_Record :=
        AttributesData_Container.Element
          (Container => Attributes_List,
           Index =>
             Attributes_Amount_Range'Value(CArgv.Arg(Argv => Argv, N => 1)));
   begin
      Show_Info
        (Text => To_String(Source => Attribute.Description),
         Parent_Name => CArgv.Arg(Argv => Argv, N => 2),
         Title => To_String(Source => Attribute.Name));
      return TCL_OK;
   end Show_Crew_Stats_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Skill_Info_Command
   -- FUNCTION
   -- Show the detailed information about the selected crew member skill
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrewSkillInfo skillindex memberindex
   -- Skillindex is the index of skill which info will be show.
   -- Memberindex is the index of the crew member which skill will be show.
   -- SOURCE
   function Show_Crew_Skill_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Skill_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Short_String;

      Skill_Index: constant Skills_Amount_Range :=
        Skills_Amount_Range'Value(CArgv.Arg(Argv => Argv, N => 1));
      Message_Text: Unbounded_String;
      Item_Index: Objects_Container.Extended_Index;
      Quality: Natural;
   begin
      Append(Source => Message_Text, New_Item => "Related attribute: ");
      Append
        (Source => Message_Text,
         New_Item =>
           To_String
             (Source =>
                AttributesData_Container.Element
                  (Container => Attributes_List,
                   Index =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => Skill_Index)
                       .Attribute)
                  .Name));
      if SkillsData_Container.Element
          (Container => Skills_List, Index => Skill_Index)
          .Tool /=
        Tiny_String.Null_Bounded_String then
         Append
           (Source => Message_Text, New_Item => "." & LF & "Training tool: ");
         Quality := 0;
         if CArgv.Arg(Argv => Argv, N => 3) = ".memberdialog" then
            Find_Training_Tool_Loop :
            for I in
              Objects_Container.First_Index(Container => Items_List) ..
                Objects_Container.Last_Index(Container => Items_List) loop
               if Objects_Container.Element
                   (Container => Items_List, Index => I)
                   .I_Type =
                 SkillsData_Container.Element
                   (Container => Skills_List, Index => Skill_Index)
                   .Tool
                 and then
                 (Objects_Container.Element
                    (Container => Items_List, Index => I)
                    .Value
                    (1) <=
                  Get_Training_Tool_Quality
                    (Member_Index =>
                       Positive'Value(CArgv.Arg(Argv => Argv, N => 2)),
                     Skill_Index => Natural(Skill_Index))) then
                  if Objects_Container.Element
                      (Container => Items_List, Index => I)
                      .Value
                      (1) >
                    Quality then
                     Item_Index := I;
                     Quality :=
                       Objects_Container.Element
                         (Container => Items_List, Index => I)
                         .Value
                         (1);
                  end if;
               end if;
            end loop Find_Training_Tool_Loop;
         else
            Find_Training_Tool_2_Loop :
            for I in
              Objects_Container.First_Index(Container => Items_List) ..
                Objects_Container.Last_Index(Container => Items_List) loop
               if Objects_Container.Element
                   (Container => Items_List, Index => I)
                   .I_Type =
                 SkillsData_Container.Element
                   (Container => Skills_List, Index => Skill_Index)
                   .Tool
                 and then
                 (Objects_Container.Element
                    (Container => Items_List, Index => I)
                    .Value
                    (1) <=
                  Positive'Value(CArgv.Arg(Argv => Argv, N => 2))) then
                  if Objects_Container.Element
                      (Container => Items_List, Index => I)
                      .Value
                      (1) >
                    Quality then
                     Item_Index := I;
                     Quality :=
                       Objects_Container.Element
                         (Container => Items_List, Index => I)
                         .Value
                         (1);
                  end if;
               end if;
            end loop Find_Training_Tool_2_Loop;
         end if;
         Append
           (Source => Message_Text,
            New_Item =>
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => Item_Index)
                     .Name));
      end if;
      Append(Source => Message_Text, New_Item => "." & LF);
      Append
        (Source => Message_Text,
         New_Item =>
           To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List, Index => Skill_Index)
                  .Description));
      Show_Info
        (Text => To_String(Source => Message_Text),
         Parent_Name => CArgv.Arg(Argv => Argv, N => 3),
         Title =>
           To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List, Index => Skill_Index)
                  .Name));
      return TCL_OK;
   end Show_Crew_Skill_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Priorities_Command
   -- FUNCTION
   -- Show information about the selected crew member priorities
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberPriorities memberindex
   -- MemberIndex is the index of the crew member to show
   -- SOURCE
   function Show_Member_Priorities_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Priorities_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Member: constant Member_Data := Player_Ship.Crew(Member_Index);
      Member_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog",
           Title => "Priorities for " & To_String(Source => Member.Name),
           Columns => 2);
      Close_Dialog_Button: constant Ttk_Button :=
        Create
          (pathName => Member_Dialog & ".button",
           options =>
             "-text Close -command {CloseDialog " & Member_Dialog & "}");
      Label: Ttk_Label;
      Priorities_Names: constant array
        (Member.Orders'Range) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "Piloting"),
         2 => To_Unbounded_String(Source => "Engineering"),
         3 => To_Unbounded_String(Source => "Operating guns"),
         4 => To_Unbounded_String(Source => "Repair ship"),
         5 => To_Unbounded_String(Source => "Manufacturing"),
         6 => To_Unbounded_String(Source => "Upgrading ship"),
         7 => To_Unbounded_String(Source => "Talking in bases"),
         8 => To_Unbounded_String(Source => "Healing wounded"),
         9 => To_Unbounded_String(Source => "Cleaning ship"),
         10 => To_Unbounded_String(Source => "Defend ship"),
         11 => To_Unbounded_String(Source => "Board enemy ship"),
         12 => To_Unbounded_String(Source => "Train skill"));
      Combo_Box: Ttk_ComboBox;
   begin
      Label :=
        Create
          (pathName => Member_Dialog & ".name", options => "-text {Priority}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-pady {5 0}");
      Label :=
        Create
          (pathName => Member_Dialog & ".level", options => "-text {Level}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 1 -row 1 -pady {5 0}");
      Load_Priorities_Loop :
      for I in Member.Orders'Range loop
         Label :=
           Create
             (pathName =>
                Member_Dialog & ".name" &
                Trim(Source => Positive'Image(I), Side => Left),
              options =>
                "-text {" & To_String(Source => Priorities_Names(I)) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label, Options => "-sticky w -padx {5 0}");
         Combo_Box :=
           Create
             (pathName =>
                Member_Dialog & ".level" &
                Trim(Source => Positive'Image(I), Side => Left),
              options =>
                "-values [list None Normal Highest] -state readonly -width 8");
         Current
           (ComboBox => Combo_Box,
            NewIndex => Natural'Image(Member.Orders(I)));
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script =>
              "{SetPriority" & Positive'Image(I) & " [" & Combo_Box &
              " current]" & Positive'Image(Member_Index) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Combo_Box,
            Options =>
              "-column 1 -row" & Positive'Image(I + 1) & " -padx {0 5}");
         Bind
           (Widgt => Combo_Box, Sequence => "<Escape>",
            Script => "{" & Close_Dialog_Button & " invoke;break}");
      end loop Load_Priorities_Loop;
      Bind
        (Widgt => Combo_Box, Sequence => "<Tab>",
         Script => "{focus " & Close_Dialog_Button & ";break}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Dialog_Button, Options => "-columnspan 2 -pady {0 5}");
      Focus(Widgt => Close_Dialog_Button);
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Tab>",
         Script => "{focus " & Member_Dialog & ".level1;break}");
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Show_Dialog(Dialog => Member_Dialog, Relative_Y => 0.05);
      return TCL_OK;
   end Show_Member_Priorities_Command;

   -- ****o* SUCrew/SUCrew.Set_Priority_Command
   -- FUNCTION
   -- Set the selected priority of the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetPriority orderindex priority memberindex
   -- Orderindex is the index of the order priority which will be changed,
   -- priority is the new level of the priority of the selected order,
   -- memberindex is the index of the crew member which priority order will
   -- be set
   -- SOURCE
   function Set_Priority_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Priority_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Combo_Box: Ttk_ComboBox;
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 3));
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "2" then
         Set_Priority_Loop :
         for Order of Player_Ship.Crew(Member_Index).Orders loop
            if Order = 2 then
               Order := 1;
               exit Set_Priority_Loop;
            end if;
         end loop Set_Priority_Loop;
      end if;
      Player_Ship.Crew(Member_Index).Orders
        (Positive'Value(CArgv.Arg(Argv => Argv, N => 1))) :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 2));
      Update_Orders(Ship => Player_Ship);
      Update_Header;
      Update_Messages;
      Update_Crew_Info;
      Combo_Box.Interp := Interp;
      Update_Priority_Info_Loop :
      for I in Player_Ship.Crew(Member_Index).Orders'Range loop
         Combo_Box.Name :=
           New_String
             (Str =>
                ".memberdialog.level" &
                Trim(Source => Positive'Image(I), Side => Left));
         Current
           (ComboBox => Combo_Box,
            NewIndex =>
              Natural'Image(Player_Ship.Crew(Member_Index).Orders(I)));
      end loop Update_Priority_Info_Loop;
      return TCL_OK;
   end Set_Priority_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Menu_Command
   -- FUNCTION
   -- Show the menu with options for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberMenu memberindex
   -- MemberIndex is the index of the crew member to show menu
   -- SOURCE
   function Show_Member_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Member: constant Member_Data :=
        Player_Ship.Crew(Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      Crew_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".membermenu",
           Title => To_String(Source => Member.Name) & " actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Crew_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Crew_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Crew_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Crew_Menu & ".rename;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".rename", Label => "Rename crew member",
         Command =>
           "GetString {Enter a new name for the " &
           To_String(Source => Member.Name) & ":} crewname" &
           CArgv.Arg(Argv => Argv, N => 1) &
           " {Renaming crew member} {Rename}");
      Add_Button
        (Name => ".info", Label => "Show more info about the crew member",
         Command => "ShowMemberInfo " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".inventory", Label => "Show inventory of the crew member",
         Command => "ShowMemberInventory " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".priorities",
         Label => "Set order priorities of the crew member",
         Command => "ShowMemberPriorities " & CArgv.Arg(Argv => Argv, N => 1));
      if CArgv.Arg(Argv => Argv, N => 1) /= "1" and
        Player_Ship.Speed = DOCKED then
         Add_Button
           (Name => ".dismiss", Label => "Dismiss",
            Command => "Dismiss " & CArgv.Arg(Argv => Argv, N => 1));
      end if;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog
        (Dialog => Crew_Menu, Parent_Frame => ".", Relative_Y => 0.15);
      return TCL_OK;
   end Show_Member_Menu_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Command
   -- FUNCTION
   -- Show the list of the player's ship crew to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrew page skill
   -- Page parameter is a index of page from which starts showing
   -- crew. Skill is the index of skill to show
   -- SOURCE
   function Show_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Update_Crew_Info
        (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)),
         Skill => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
      return TCL_OK;
   end Show_Crew_Command;

   -- ****it* SUCrew/SUCrew.Crew_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the player ship crew list
   -- OPTIONS
   -- NAMEASC     - Sort members by name ascending
   -- NAMEDESC    - Sort members by name descending
   -- ORDERASC    - Sort members by order ascending
   -- ORDERDESC   - Sort members by order descending
   -- SKILLASC    - Sort members by skill ascending
   -- SKILLDESC   - Sort members by skill descending
   -- HEALTHASC   - Sort members by health ascending
   -- HEALTHDESC  - Sort members by health descending
   -- FATIGUEASC  - Sort members by fatigue ascending
   -- FATIGUEDESC - Sort members by fatigue descending
   -- THIRTSASC   - Sort members by thirst ascending
   -- THIRSTDESC  - Sort members by thirst descending
   -- HUNGERASC   - Sort members by hunger ascending
   -- HUNGERDESC  - Sort members by hunger descending
   -- MORALEASC   - Sort members by morale ascending
   -- MORALEDESC  - Sort members by morale descending
   -- NONE        - No sorting crew (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Crew_Sort_Orders is
     (NAMEASC, NAMEDESC, ORDERASC, ORDERDESC, SKILLASC, SKILLDESC, HEALTHASC,
      HEALTHDESC, FATIGUEASC, FATIGUEDESC, THIRSTASC, THIRSTDESC, HUNGERASC,
      HUNGERDESC, MORALEASC, MORALEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUCrew/SUCrew.Default_Crew_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's crew
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Crew_Sort_Order: constant Crew_Sort_Orders := NONE;
   -- ****

   -- ****iv* SUCrew/SUCrew.Crew_Sort_Order
   -- FUNCTION
   -- The current sorting order of the player's ship's crew
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Crew_Sort_Order: Crew_Sort_Orders := Default_Crew_Sort_Order;
   -- ****

   -- ****o* SUCrew/SUCrew.Sort_Crew_Command
   -- FUNCTION
   -- Sort the player's ship's crew list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipCrew x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crew_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Column: constant Positive :=
        Get_Column_Number
          (Table => Crew_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      Skill_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned &
             ".shipinfoframe.crew.canvas.frame.selectskill.combox",
           Interp => Interp);
      Skill_Index: constant Natural :=
        Natural'Value(Current(ComboBox => Skill_Box));
      type Local_Member_Data is record
         Name: Tiny_String.Bounded_String;
         Order: Crew_Orders;
         Skill: Tiny_String.Bounded_String;
         Health: Skill_Range;
         Fatigue: Integer;
         Thirst: Skill_Range;
         Hunger: Skill_Range;
         Morale: Skill_Range;
         Id: Positive;
      end record;
      type Crew_Array is array(Positive range <>) of Local_Member_Data;
      Local_Crew: Crew_Array(1 .. Positive(Player_Ship.Crew.Length));
      function "<"(Left, Right: Local_Member_Data) return Boolean is
      begin
         if Crew_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Crew_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Crew_Sort_Order = ORDERASC
           and then Crew_Orders'Image(Left.Order) <
             Crew_Orders'Image(Right.Order) then
            return True;
         end if;
         if Crew_Sort_Order = ORDERDESC
           and then Crew_Orders'Image(Left.Order) >
             Crew_Orders'Image(Right.Order) then
            return True;
         end if;
         if Crew_Sort_Order = SKILLASC and then Left.Skill < Right.Skill then
            return True;
         end if;
         if Crew_Sort_Order = SKILLDESC and then Left.Skill > Right.Skill then
            return True;
         end if;
         if Crew_Sort_Order = HEALTHASC
           and then Left.Health < Right.Health then
            return True;
         end if;
         if Crew_Sort_Order = HEALTHDESC
           and then Left.Health > Right.Health then
            return True;
         end if;
         if Crew_Sort_Order = FATIGUEASC
           and then Left.Fatigue < Right.Fatigue then
            return True;
         end if;
         if Crew_Sort_Order = FATIGUEDESC
           and then Left.Fatigue > Right.Fatigue then
            return True;
         end if;
         if Crew_Sort_Order = THIRSTASC
           and then Left.Thirst < Right.Thirst then
            return True;
         end if;
         if Crew_Sort_Order = THIRSTDESC
           and then Left.Thirst > Right.Thirst then
            return True;
         end if;
         if Crew_Sort_Order = HUNGERASC
           and then Left.Hunger < Right.Hunger then
            return True;
         end if;
         if Crew_Sort_Order = HUNGERDESC
           and then Left.Hunger > Right.Hunger then
            return True;
         end if;
         if Crew_Sort_Order = MORALEASC
           and then Left.Morale < Right.Morale then
            return True;
         end if;
         if Crew_Sort_Order = MORALEDESC
           and then Left.Morale > Right.Morale then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Crew is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Member_Data,
         Array_Type => Crew_Array);
   begin
      case Column is
         when 1 =>
            if Crew_Sort_Order = NAMEASC then
               Crew_Sort_Order := NAMEDESC;
            else
               Crew_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Crew_Sort_Order = ORDERASC then
               Crew_Sort_Order := ORDERDESC;
            else
               Crew_Sort_Order := ORDERASC;
            end if;
         when 3 =>
            if Crew_Sort_Order = SKILLASC then
               Crew_Sort_Order := SKILLDESC;
            else
               Crew_Sort_Order := SKILLASC;
            end if;
         when 4 =>
            if Crew_Sort_Order = HEALTHASC then
               Crew_Sort_Order := HEALTHDESC;
            else
               Crew_Sort_Order := HEALTHASC;
            end if;
         when 5 =>
            if Crew_Sort_Order = FATIGUEASC then
               Crew_Sort_Order := FATIGUEDESC;
            else
               Crew_Sort_Order := FATIGUEASC;
            end if;
         when 6 =>
            if Crew_Sort_Order = THIRSTASC then
               Crew_Sort_Order := THIRSTDESC;
            else
               Crew_Sort_Order := THIRSTASC;
            end if;
         when 7 =>
            if Crew_Sort_Order = HUNGERASC then
               Crew_Sort_Order := HUNGERDESC;
            else
               Crew_Sort_Order := HUNGERASC;
            end if;
         when 8 =>
            if Crew_Sort_Order = MORALEASC then
               Crew_Sort_Order := MORALEDESC;
            else
               Crew_Sort_Order := MORALEASC;
            end if;
         when others =>
            null;
      end case;
      if Crew_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Crew_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Local_Crew(Crew_Container.To_Index(Position => I)) :=
           (Name => Player_Ship.Crew(I).Name,
            Order => Player_Ship.Crew(I).Order,
            Skill =>
              To_Bounded_String
                (Source =>
                   (if Skill_Index = 0 then
                      Get_Highest_Skill
                        (Member_Index =>
                           Crew_Container.To_Index(Position => I))
                    else Get_Skill_Level_Name
                        (Skill_Level =>
                           Get_Skill_Level
                             (Member => Player_Ship.Crew(I),
                              Skill_Index =>
                                Skills_Amount_Range(Skill_Index))))),
            Health => Player_Ship.Crew(I).Health,
            Fatigue =>
              Player_Ship.Crew(I).Tired -
              Player_Ship.Crew(I).Attributes(Positive(Condition_Index)).Level,
            Thirst => Player_Ship.Crew(I).Thirst,
            Hunger => Player_Ship.Crew(I).Hunger,
            Morale => Player_Ship.Crew(I).Morale(1),
            Id => Crew_Container.To_Index(Position => I));
      end loop Fill_Local_Crew_Loop;
      Sort_Crew(Container => Local_Crew);
      Crew_Indexes.Clear;
      Fill_Crew_Indexes_Loop :
      for Member of Local_Crew loop
         Crew_Indexes.Append(New_Item => Member.Id);
      end loop Fill_Crew_Indexes_Loop;
      Update_Crew_Info(Skill => Skill_Index);
      return TCL_OK;
   end Sort_Crew_Command;

   -- ****o* SUCrew/SUCrew.Select_Crew_Skill_Command
   -- FUNCTION
   -- Show the list of the player's ship crew with selected skill from combobox
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SelectCrewSkill
   -- SOURCE
   function Select_Crew_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Select_Crew_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Skill_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned &
             ".shipinfoframe.crew.canvas.frame.selectskill.combox",
           Interp => Interp);
   begin
      Update_Crew_Info(Skill => Natural'Value(Current(ComboBox => Skill_Box)));
      return TCL_OK;
   end Select_Crew_Skill_Command;

   -- ****if* SUCrew/SUCrew.Set_Available_Orders
   -- FUNCTION
   -- Set the list of available orders for the selected crew member
   -- PARAMETERS
   -- Member_Index - The crew index of the crew member which list of orders
   --                will be set
   -- Orders_Box   - The Ttk_ComboBox widget in which the list will be set
   -- HISTORY
   -- 7.9 - Added
   -- SOURCE
   procedure Set_Available_Orders
     (Member_Index: Positive; Orders_Box: Ttk_ComboBox) is
     -- ****
      Member: constant Member_Data := Player_Ship.Crew(Member_Index);
      Available_Orders, Tcl_Commands: Unbounded_String :=
        Null_Unbounded_String;
      Need_Repair, Need_Clean: Boolean := False;
      function Is_Working
        (Owners: Natural_Container.Vector; Member_Index: Positive)
         return Boolean is
      begin
         Find_Owner_Loop :
         for Owner of Owners loop
            if Owner = Member_Index then
               return True;
            end if;
         end loop Find_Owner_Loop;
         return False;
      end Is_Working;
   begin
      Check_Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.Durability < Module.Max_Durability then
            Need_Repair := True;
         end if;
         if (Module.Durability > 0 and Module.M_Type = CABIN)
           and then Module.Cleanliness < Module.Quality then
            Need_Clean := True;
         end if;
         exit Check_Modules_Loop when Need_Clean and Need_Repair;
      end loop Check_Modules_Loop;
      if
        ((Member.Tired = 100 or Member.Hunger = 100 or Member.Thirst = 100) and
         Member.Order /= REST) or
        (Skills_Container.Length(Container => Member.Skills) = 0 or
         Member.Contract_Length = 0) then
         Append(Source => Available_Orders, New_Item => " {Go on break}");
         Append
           (Source => Tcl_Commands,
            New_Item => " {Rest" & Member_Index'Img & "}");
      else
         if Member.Order /= PILOT then
            Append
              (Source => Available_Orders,
               New_Item => " {Go piloting the ship}");
            Append
              (Source => Tcl_Commands,
               New_Item => " {Pilot" & Member_Index'Img & "}");
         end if;
         if Member.Order /= ENGINEER then
            Append
              (Source => Available_Orders,
               New_Item => " {Go engineering the ship}");
            Append
              (Source => Tcl_Commands,
               New_Item => " {Engineer" & Member_Index'Img & "}");
         end if;
         Set_Work_Orders_Loop :
         for J in Player_Ship.Modules.Iterate loop
            if Player_Ship.Modules(J).Durability <
              Player_Ship.Modules(J).Max_Durability then
               Need_Repair := True;
            end if;
            if Player_Ship.Modules(J).Durability > 0 then
               case Player_Ship.Modules(J).M_Type is
                  when GUN | HARPOON_GUN =>
                     if Player_Ship.Modules(J).Owner(1) /= Member_Index then
                        Append
                          (Source => Available_Orders,
                           New_Item =>
                             " {Operate " &
                             To_String(Source => Player_Ship.Modules(J).Name) &
                             "}");
                        Append
                          (Source => Tcl_Commands,
                           New_Item =>
                             " {Gunner" & Member_Index'Img &
                             Positive'Image
                               (Positive
                                  (Modules_Container.To_Index
                                     (Position => J))) &
                             "}");
                     end if;
                  when WORKSHOP =>
                     if not Is_Working
                         (Owners => Player_Ship.Modules(J).Owner,
                          Member_Index => Member_Index) and
                       Player_Ship.Modules(J).Crafting_Index /=
                         Null_Bounded_String then
                        Append
                          (Source => Available_Orders,
                           New_Item =>
                             " {" &
                             (if
                                Length
                                  (Source =>
                                     Player_Ship.Modules(J).Crafting_Index) >
                                6
                                and then
                                  Slice
                                    (Source =>
                                       Player_Ship.Modules(J).Crafting_Index,
                                     Low => 1, High => 5) =
                                  "Study"
                              then
                                "Study " &
                                To_String
                                  (Source =>
                                     Objects_Container.Element
                                       (Container => Items_List,
                                        Index =>
                                          Positive'Value
                                            (Slice
                                               (Source =>
                                                  Player_Ship.Modules(J)
                                                    .Crafting_Index,
                                                Low => 7,
                                                High =>
                                                  Length
                                                    (Source =>
                                                       Player_Ship.Modules(J)
                                                         .Crafting_Index))))
                                       .Name)
                              elsif
                                Length
                                  (Source =>
                                     Player_Ship.Modules(J).Crafting_Index) >
                                12
                                and then
                                  Slice
                                    (Source =>
                                       Player_Ship.Modules(J).Crafting_Index,
                                     Low => 1, High => 11) =
                                  "Deconstruct"
                              then
                                "Deconstruct " &
                                To_String
                                  (Source =>
                                     Objects_Container.Element
                                       (Container => Items_List,
                                        Index =>
                                          Positive'Value
                                            (Slice
                                               (Source =>
                                                  Player_Ship.Modules(J)
                                                    .Crafting_Index,
                                                Low => 13,
                                                High =>
                                                  Length
                                                    (Source =>
                                                       Player_Ship.Modules(J)
                                                         .Crafting_Index))))
                                       .Name)
                              else "Manufacture" &
                                Positive'Image
                                  (Player_Ship.Modules(J).Crafting_Amount) &
                                "x " &
                                To_String
                                  (Source =>
                                     Objects_Container.Element
                                       (Container => Items_List,
                                        Index =>
                                          Recipes_List
                                            (To_Bounded_String
                                               (Source =>
                                                  To_String
                                                    (Source =>
                                                       Player_Ship.Modules(J)
                                                         .Crafting_Index)))
                                            .Result_Index)
                                       .Name)) &
                             "}");
                        Append
                          (Source => Tcl_Commands,
                           New_Item =>
                             " {Craft" & Member_Index'Img &
                             Positive'Image
                               (Positive
                                  (Modules_Container.To_Index
                                     (Position => J))) &
                             "}");
                     end if;
                  when CABIN =>
                     if Player_Ship.Modules(J).Cleanliness <
                       Player_Ship.Modules(J).Quality and
                       Member.Order /= CLEAN and Need_Clean then
                        Append
                          (Source => Available_Orders,
                           New_Item => " {Clean ship}");
                        Append
                          (Source => Tcl_Commands,
                           New_Item => " {Clean" & Member_Index'Img & "}");
                        Need_Clean := False;
                     end if;
                  when TRAINING_ROOM =>
                     if not Is_Working
                         (Owners => Player_Ship.Modules(J).Owner,
                          Member_Index => Member_Index) then
                        Append
                          (Source => Available_Orders,
                           New_Item =>
                             " {Go on training in " &
                             To_String(Source => Player_Ship.Modules(J).Name) &
                             "}");
                        Append
                          (Source => Tcl_Commands,
                           New_Item =>
                             " {Train" & Member_Index'Img &
                             Positive'Image
                               (Positive
                                  (Modules_Container.To_Index
                                     (Position => J))) &
                             "}");
                     end if;
                  when others =>
                     null;
               end case;
               if Need_Repair then
                  Append
                    (Source => Available_Orders, New_Item => " {Repair ship}");
                  Append
                    (Source => Tcl_Commands,
                     New_Item => " {Repair" & Member_Index'Img & "}");
                  Need_Repair := False;
               end if;
            end if;
         end loop Set_Work_Orders_Loop;
         Check_Heal_Order_Loop :
         for J in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(J).Health < 100 and
              Crew_Container.To_Index(Position => J) /= Member_Index and
              Player_Ship.Crew(J).Order /= HEAL then
               Append
                 (Source => Available_Orders,
                  New_Item => " {Heal wounded crew members}");
               Append
                 (Source => Tcl_Commands,
                  New_Item => " {Heal" & Member_Index'Img & "}");
               exit Check_Heal_Order_Loop;
            end if;
         end loop Check_Heal_Order_Loop;
         if Player_Ship.Upgrade_Module > 0 and Member.Order /= UPGRADING then
            Append
              (Source => Available_Orders, New_Item => " {Upgrade module}");
            Append
              (Source => Tcl_Commands,
               New_Item => " {Upgrading" & Member_Index'Img & "}");
         end if;
         if Member.Order /= TALK then
            Append
              (Source => Available_Orders, New_Item => " {Talk with others}");
            Append
              (Source => Tcl_Commands,
               New_Item => " {Talk" & Member_Index'Img & "}");
         end if;
         if Member.Order /= REST then
            Append(Source => Available_Orders, New_Item => " {Go on break}");
            Append
              (Source => Tcl_Commands,
               New_Item => " {Rest" & Member_Index'Img & "}");
         end if;
      end if;
      configure
        (Widgt => Orders_Box,
         options =>
           "-values [list" & To_String(Source => Available_Orders) & "]");
      Unbind(Widgt => Orders_Box, Sequence => "<<ComboboxSelected>>");
      Current(ComboBox => Orders_Box, NewIndex => "0");
      Bind
        (Widgt => Orders_Box, Sequence => "<<ComboboxSelected>>",
         Script =>
           "{SelectCrewOrder {" & To_String(Source => Tcl_Commands) & "}" &
           Member_Index'Img & "}");
   end Set_Available_Orders;

   -- ****o* SUCrew/SUCrew.Show_Crew_Order_Command
   -- FUNCTION
   -- Show the dialog to change the order of the currently selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrewOrder memberindex
   -- MemberIndex is the index of the crew member which order will be changed
   -- SOURCE
   function Show_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Member: constant Member_Data := Player_Ship.Crew(Member_Index);
      Member_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog",
           Title => "Change order for " & To_String(Source => Member.Name),
           Columns => 2);
      Order_Info: constant Ttk_Label :=
        Create
          (pathName => Member_Dialog & ".orderinfo",
           options => "-text {Current order:}");
      Order_Label: constant Ttk_Label :=
        Create
          (pathName => Member_Dialog & ".current",
           options =>
             "-text {" &
             To_String
               (Source => Get_Current_Order(Member_Index => Member_Index)) &
             "} -wraplength 275");
      Orders_Info: constant Ttk_Label :=
        Create
          (pathName => Member_Dialog & ".ordersinfo",
           options => "-text {New order:}");
      Orders_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Member_Dialog & ".list", options => "-state readonly");
      Close_Dialog_Button: constant Ttk_Button :=
        Create
          (pathName => Member_Dialog & ".button",
           options =>
             "-text Close -command {CloseDialog " & Member_Dialog &
             "} -image exiticon -style Dialog.TButton");
   begin
      Tcl.Tk.Ada.Grid.Grid(Slave => Order_Info, Options => "-padx 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Order_Label,
         Options => "-padx 5 -column 1 -row 1 -sticky w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Orders_Info, Options => "-padx 5 -sticky w");
      Bind
        (Widgt => Orders_Box, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Set_Available_Orders
        (Member_Index => Member_Index, Orders_Box => Orders_Box);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Orders_Box, Options => "-padx 5 -column 1 -row 2 -sticky w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Dialog_Button, Options => "-columnspan 2 -pady {0 5}");
      Focus(Widgt => Close_Dialog_Button);
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Tab>",
         Script => "{focus " & Orders_Box & ";break}");
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Show_Dialog(Dialog => Member_Dialog);
      return TCL_OK;
   end Show_Crew_Order_Command;

   -- ****o* SUCrew/SUCrew.Select_Crew_Order_Command
   -- FUNCTION
   -- Set the selected order for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SelectCrewOrder orderslist memberindex
   -- Orderslist is the list of the available orders with their parameters,
   -- memberindex is the crew index of the selected crew member
   -- SOURCE
   function Select_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Select_Crew_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Orders_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".memberdialog.list", Interp => Interp);
      Order_Index: constant Natural :=
        Natural'Value(Current(ComboBox => Orders_Box));
      Tokens: Slice_Set;
      Order_Label: constant Ttk_Label :=
        Get_Widget(pathName => ".memberdialog.current");
      Member_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Arguments: CArgv.Chars_Ptr_Ptr := CArgv.Empty & "SetCrewOrder";
   begin
      Tcl_Eval
        (interp => Interp,
         strng =>
           "lindex {" & CArgv.Arg(Argv => Argv, N => 1) & "}" &
           Order_Index'Img);
      Create
        (S => Tokens, From => Tcl_GetResult(interp => Interp),
         Separators => " ");
      Build_Arguments_Loop :
      for I in 1 .. Slice_Count(S => Tokens) loop
         Arguments := Arguments & Slice(S => Tokens, Index => I);
      end loop Build_Arguments_Loop;
      if Set_Crew_Order_Command
          (Client_Data => Client_Data, Interp => Interp,
           Argc => int(Slice_Count(S => Tokens) + 1), Argv => Arguments) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      configure
        (Widgt => Order_Label,
         options =>
           "-text {" &
           To_String
             (Source => Get_Current_Order(Member_Index => Member_Index)) &
           "}");
      Set_Available_Orders
        (Member_Index => Member_Index, Orders_Box => Orders_Box);
      Focus(Widgt => Orders_Box);
      return TCL_OK;
   end Select_Crew_Order_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "OrderForAll", Ada_Command => Order_For_All_Command'Access);
      Add_Command(Name => "Dismiss", Ada_Command => Dismiss_Command'Access);
      Add_Command
        (Name => "SetCrewOrder", Ada_Command => Set_Crew_Order_Command'Access);
      Add_Command
        (Name => "ShowMemberInfo",
         Ada_Command => Show_Member_Info_Command'Access);
      Add_Command
        (Name => "ShowMemberTab",
         Ada_Command => Show_Member_Tab_Command'Access);
      Add_Command
        (Name => "ShowCrewStatsInfo",
         Ada_Command => Show_Crew_Stats_Info_Command'Access);
      Add_Command
        (Name => "ShowCrewSkillInfo",
         Ada_Command => Show_Crew_Skill_Info_Command'Access);
      Add_Command
        (Name => "ShowMemberPriorities",
         Ada_Command => Show_Member_Priorities_Command'Access);
      Add_Command
        (Name => "SetPriority", Ada_Command => Set_Priority_Command'Access);
      Add_Command
        (Name => "ShowMemberMenu",
         Ada_Command => Show_Member_Menu_Command'Access);
      Add_Command(Name => "ShowCrew", Ada_Command => Show_Crew_Command'Access);
      Add_Command
        (Name => "SortShipCrew", Ada_Command => Sort_Crew_Command'Access);
      Add_Command
        (Name => "SelectCrewSkill",
         Ada_Command => Select_Crew_Skill_Command'Access);
      Add_Command
        (Name => "ShowCrewOrder",
         Ada_Command => Show_Crew_Order_Command'Access);
      Add_Command
        (Name => "SelectCrewOrder",
         Ada_Command => Select_Crew_Order_Command'Access);
      Ships.UI.Crew.Inventory.Add_Commands;
   end Add_Commands;

end Ships.UI.Crew;
