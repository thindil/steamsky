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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
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
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Crew; use Ships.Crew;
with Ships.UI.Crew.Inventory;
with Table; use Table;
with Themes; use Themes;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew is

   -- ****iv* SUCrew/SUCrew.CrewTable
   -- FUNCTION
   -- Table with info about the player's ship crew
   -- SOURCE
   CrewTable: Table_Widget (8);
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
   -- MemberIndex - The index of the selected crew member which skill will
   --               be get
   -- RESULT
   -- The name of the highest skill of the selected crew member
   -- HISTORY
   -- 6.9 - Added
   -- SOURCE
   function Get_Highest_Skill(MemberIndex: Positive) return String is
      -- ****
      use Tiny_String;

      HighestLevel, HighestIndex: Positive := 1;
   begin
      Get_Highest_Skill_Level_Loop :
      for Skill of Player_Ship.Crew(MemberIndex).Skills loop
         if Skill.Level > HighestLevel then
            HighestLevel := Skill.Level;
            HighestIndex := Skill.Index;
         end if;
      end loop Get_Highest_Skill_Level_Loop;
      return
        To_String
          (SkillsData_Container.Element(Skills_List, HighestIndex).Name);
   end Get_Highest_Skill;

   procedure UpdateCrewInfo(Page: Positive := 1; Skill: Natural := 0) is
      ButtonsFrame: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ShipCanvas: Tk_Canvas;
      NeedRepair, NeedClean: Boolean := False;
      Button: Ttk_Button;
      TiredLevel: Integer;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      CrewInfoFrame: constant Ttk_Frame :=
        Get_Widget(Main_Paned & ".shipinfoframe.crew.canvas.frame");
      Orders_Label: Ttk_Label;
      SkillBox: Ttk_ComboBox;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, CrewInfoFrame);
      ButtonsFrame := Create(CrewInfoFrame & ".ordersbuttons");
      Check_Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.Durability < Module.Max_Durability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.M_Type = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit Check_Modules_Loop when NeedClean and NeedRepair;
      end loop Check_Modules_Loop;
      if NeedClean then
         Orders_Label :=
           Create(ButtonsFrame & ".label", "-text {Orders for all:}");
         Add(Orders_Label, "Give the selected order to the whole crew.");
         Tcl.Tk.Ada.Grid.Grid(Orders_Label, "-padx {5 2}");
         Button :=
           Create
             (ButtonsFrame & ".clean",
              "-text {" &
              Encode
                ("" &
                 Themes_List(To_String(Game_Settings.Interface_Theme))
                   .Clean_Icon) &
              "} -style Header.Toolbutton -command {OrderForAll Clean}");
         Add(Button, "Clean ship everyone");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1 -padx {0 2}");
      end if;
      if NeedRepair then
         Button :=
           Create
             (ButtonsFrame & ".repair",
              "-text {" &
              Encode
                ("" &
                 Themes_List(To_String(Game_Settings.Interface_Theme))
                   .Repair_Icon) &
              "} -style Header.Toolbutton -command {OrderForAll Repair}");
         Add(Button, "Repair ship everyone");
         if NeedClean then
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 2");
         else
            Orders_Label :=
              Create(ButtonsFrame & ".label", "-text {Orders for all:}");
            Add(Orders_Label, "Give the selected order to the whole crew.");
            Tcl.Tk.Ada.Grid.Grid(Orders_Label, "-padx {5 2}");
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky w");
      ButtonsFrame := Create(CrewInfoFrame & ".selectskill");
      Orders_Label := Create(ButtonsFrame & ".label", "-text {Skill:}");
      Add
        (Orders_Label,
         "Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.");
      Tcl.Tk.Ada.Grid.Grid(Orders_Label, "-padx {5 2}");
      declare
         use Tiny_String;

         Skills: Unbounded_String := To_Unbounded_String(" {Highest}");
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
         SkillBox :=
           Create
             (CrewInfoFrame & ".selectskill.combox",
              "-state readonly -values [list" & To_String(Skills) & "]");
         Bind(SkillBox, "<<ComboboxSelected>>", "SelectCrewSkill");
         Current(SkillBox, Natural'Image(Skill));
         Add
           (SkillBox,
            "Show the level of the selected skill for the crew\nmembers.If selected option 'Highest', show the\nhighest skill of the crew members.");
         Tcl.Tk.Ada.Grid.Grid(SkillBox, "-row 0 -column 1");
      end;
      Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky w");
      CrewTable :=
        CreateTable
          (Widget_Image(CrewInfoFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Order"),
            To_Unbounded_String("Skill"), To_Unbounded_String("Health"),
            To_Unbounded_String("Fatigue"), To_Unbounded_String("Thirst"),
            To_Unbounded_String("Hunger"), To_Unbounded_String("Morale")),
           Get_Widget(".gameframe.paned.shipinfoframe.crew.scrolly"),
           "SortShipCrew", "Press mouse button to sort the crew.");
      if Crew_Indexes.Length /= Player_Ship.Crew.Length then
         Crew_Indexes.Clear;
         for I in Player_Ship.Crew.Iterate loop
            Crew_Indexes.Append(Crew_Container.To_Index(I));
         end loop;
      end if;
      Load_Crew_Loop :
      for I of Crew_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         AddButton
           (CrewTable, To_String(Player_Ship.Crew(I).Name),
            "Show available crew member's options",
            "ShowMemberMenu" & Positive'Image(I), 1);
         AddButton
           (CrewTable,
            Crew_Orders'Image(Player_Ship.Crew(I).Order)(1) &
            To_Lower
              (Crew_Orders'Image(Player_Ship.Crew(I).Order)
                 (2 .. Crew_Orders'Image(Player_Ship.Crew(I).Order)'Last)),
            "The current order for the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 2);
         if Skill = 0 then
            AddButton
              (CrewTable, Get_Highest_Skill(I),
               "The highest skill of the selected crew member",
               "ShowMemberMenu" & Positive'Image(I), 3);
         else
            AddButton
              (CrewTable,
               Get_Skill_Level_Name(GetSkillLevel(Player_Ship.Crew(I), Skill)),
               "The level of the " & Get(SkillBox) &
               " of the selected crew member",
               "ShowMemberMenu" & Positive'Image(I), 3);
         end if;
         AddProgressBar
           (CrewTable, Player_Ship.Crew(I).Health, Skill_Range'Last,
            "The current health level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 4);
         TiredLevel :=
           Player_Ship.Crew(I).Tired -
           Player_Ship.Crew(I).Attributes(Positive(Condition_Index)).Level;
         if TiredLevel < 0 then
            TiredLevel := 0;
         end if;
         AddProgressBar
           (CrewTable, TiredLevel, Skill_Range'Last,
            "The current tired level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 5, False, True);
         AddProgressBar
           (CrewTable, Player_Ship.Crew(I).Thirst, Skill_Range'Last,
            "The current thirst level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 6, False, True);
         AddProgressBar
           (CrewTable, Player_Ship.Crew(I).Hunger, Skill_Range'Last,
            "The current hunger level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 7, False, True);
         AddProgressBar
           (CrewTable, Player_Ship.Crew(I).Morale(1), Skill_Range'Last,
            "The current morale level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(I), 8, True);
         exit Load_Crew_Loop when CrewTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Crew_Loop;
      if Page > 1 then
         AddPagination
           (CrewTable,
            "ShowCrew" & Positive'Image(Page - 1) & Natural'Image(Skill),
            (if CrewTable.Row < Game_Settings.Lists_Limit + 1 then ""
             else "ShowCrew" & Positive'Image(Page + 1)) &
            Natural'Image(Skill));
      elsif CrewTable.Row = Game_Settings.Lists_Limit + 1 then
         AddPagination
           (CrewTable, "",
            "ShowCrew" & Positive'Image(Page + 1) & Natural'Image(Skill));
      end if;
      UpdateTable(CrewTable);
      Tcl_Eval(Get_Context, "update");
      ShipCanvas := Get_Widget(Main_Paned & ".shipinfoframe.crew.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
   end UpdateCrewInfo;

   -- ****o* SUCrew/SUCrew.Order_For_All_Command
   -- FUNCTION
   -- Set the selected order for the whole crew
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- OrderForAll order
   -- Order is the name of the order which will be assigned to the whole
   -- player ship crew
   -- SOURCE
   function Order_For_All_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Order_For_All_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      Give_Orders_Loop :
      for I in Player_Ship.Crew.Iterate loop
         GiveOrders
           (Player_Ship, Crew_Container.To_Index(I),
            Crew_Orders'Value(CArgv.Arg(Argv, 1)));
      end loop Give_Orders_Loop;
      UpdateHeader;
      Update_Messages;
      UpdateCrewInfo;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error =>
         Add_Message(Exception_Message(An_Exception), ORDERMESSAGE);
         UpdateHeader;
         Update_Messages;
         return TCL_OK;
   end Order_For_All_Command;

   -- ****o* SUCrew/SUCrew.Dismiss_Command
   -- FUNCTION
   -- Dismiss the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Dismiss memberindex
   -- Memberindex is the index of the player ship crew member which will be
   -- dismissed
   -- SOURCE
   function Dismiss_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Dismiss_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      ShowQuestion
        ("Are you sure want to dismiss " &
         To_String(Player_Ship.Crew(MemberIndex).Name) & "?",
         CArgv.Arg(Argv, 1));
      return TCL_OK;
   end Dismiss_Command;

   -- ****o* SUCrew/SUCrew.Set_Crew_Order_Command
   -- FUNCTION
   -- Set order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crew_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp);
      ModuleIndex: Natural := 0;
   begin
      if Argc = 4 then
         ModuleIndex := Natural'Value(CArgv.Arg(Argv, 3));
      end if;
      GiveOrders
        (Player_Ship, Positive'Value(CArgv.Arg(Argv, 2)),
         Crew_Orders'Value(CArgv.Arg(Argv, 1)), ModuleIndex);
      UpdateHeader;
      Update_Messages;
      UpdateCrewInfo;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
         Add_Message(Exception_Message(An_Exception), ORDERMESSAGE, RED);
         Update_Messages;
         return TCL_OK;
   end Set_Crew_Order_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Info_Command
   -- FUNCTION
   -- Show information about the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberInfo memberindex
   -- MemberIndex is the index of the crew member to show
   -- SOURCE
   function Show_Member_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := Player_Ship.Crew(MemberIndex);
      MemberDialog: constant Ttk_Frame :=
        Create_Dialog(".memberdialog", To_String(Member.Name));
      YScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".yscroll",
           "-orient vertical -command [list .memberdialog.canvas yview]");
      MemberCanvas: constant Tk_Canvas :=
        Create
          (MemberDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      CloseButton: constant Ttk_Button :=
        Get_Widget(MemberDialog & ".button", Interp);
      Height, NewHeight: Positive := 1;
      ProgressFrame: Ttk_Frame;
      MemberInfo: Unbounded_String;
      MemberLabel: Ttk_Label;
      Width, NewWidth: Positive := 1;
      TiredPoints: Integer;
      ProgressBar: Ttk_ProgressBar;
      TabButton: Ttk_RadioButton;
      InfoButton: Ttk_Button;
      Frame: Ttk_Frame;
   begin
      Frame := Create(MemberDialog & ".buttonbox");
      Tcl_SetVar(Interp, "newtab", "general");
      TabButton :=
        Create
          (Frame & ".general",
           " -text General -state selected -style Radio.Toolbutton -value general -variable newtab -command ShowMemberTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton);
      Bind(TabButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Height := Positive'Value(Winfo_Get(TabButton, "reqheight"));
      if Member.Skills.Length > 0 and Member.Contract_Length /= 0 then
         TabButton :=
           Create
             (Frame & ".stats",
              " -text Attributes -style Radio.Toolbutton -value stats -variable newtab -command ShowMemberTab");
         Tcl.Tk.Ada.Grid.Grid(TabButton, "-column 1 -row 0");
         Bind(TabButton, "<Escape>", "{" & CloseButton & " invoke;break}");
         TabButton :=
           Create
             (Frame & ".skills",
              " -text Skills -style Radio.Toolbutton -value skills -variable newtab -command ShowMemberTab");
         Tcl.Tk.Ada.Grid.Grid(TabButton, "-column 2 -row 0");
         Bind(TabButton, "<Escape>", "{" & CloseButton & " invoke;break}");
         Bind(TabButton, "<Tab>", "{focus " & CloseButton & ";break}");
      else
         Bind(TabButton, "<Tab>", "{focus " & CloseButton & ";break}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(MemberCanvas, "-sticky nwes -pady 5 -padx 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, " -sticky ns -pady 5 -padx {0 5} -row 1 -column 1");
      Add_Close_Button
        (MemberDialog & ".button", "Close", "CloseDialog " & MemberDialog, 2);
      Autoscroll(YScroll);
      -- General info about the selected crew member
      Frame := Create(MemberCanvas & ".general");
      if Member.Health < 100 then
         if Game_Settings.Show_Numbers then
            MemberLabel :=
              Create
                (Frame & ".health",
                 "-text {Health:" & Natural'Image(Member.Health) & "%}");
         else
            case Member.Health is
               when 81 .. 99 =>
                  MemberLabel :=
                    Create(Frame & ".health", "-text {Slightly wounded}");
               when 51 .. 80 =>
                  MemberLabel := Create(Frame & ".health", "-text {Wounded}");
               when 1 .. 50 =>
                  MemberLabel :=
                    Create(Frame & ".health", "-text {Heavily wounded}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
         Height :=
           Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      end if;
      TiredPoints :=
        Member.Tired - Member.Attributes(Positive(Condition_Index)).Level;
      if TiredPoints < 0 then
         TiredPoints := 0;
      end if;
      if TiredPoints > 0 then
         if Game_Settings.Show_Numbers then
            MemberLabel :=
              Create
                (Frame & ".tired",
                 "-text {Tiredness:" & Natural'Image(TiredPoints) & "%}");
         else
            case TiredPoints is
               when 1 .. 40 =>
                  MemberLabel := Create(Frame & ".tired", "-text {Bit tired}");
               when 41 .. 80 =>
                  MemberLabel := Create(Frame & ".tired", "-text {Tired}");
               when 81 .. 99 =>
                  MemberLabel :=
                    Create(Frame & ".tired", "-text {Very tired}");
               when 100 =>
                  MemberLabel :=
                    Create(Frame & ".tired", "-text {Unconscious}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
         Height :=
           Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      end if;
      if Member.Thirst > 0 then
         if Game_Settings.Show_Numbers then
            MemberLabel :=
              Create
                (Frame & ".thirst",
                 "-text {Thirst:" & Natural'Image(Member.Thirst) & "%}");
         else
            case Member.Thirst is
               when 1 .. 40 =>
                  MemberLabel :=
                    Create(Frame & ".thirst", "-text {Bit thirsty}");
               when 41 .. 80 =>
                  MemberLabel := Create(Frame & ".thirst", "-text {Thirsty}");
               when 81 .. 99 =>
                  MemberLabel :=
                    Create(Frame & ".thirst", "-text {Very thirsty}");
               when 100 =>
                  MemberLabel :=
                    Create(Frame & ".thirst", "-text {Dehydrated}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w -padx 5");
         Height :=
           Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      end if;
      if Member.Hunger > 0 then
         if Game_Settings.Show_Numbers then
            MemberLabel :=
              Create
                (Frame & ".hunger",
                 "-text {Hunger:" & Natural'Image(Member.Hunger) & "%}");
         else
            case Member.Hunger is
               when 1 .. 40 =>
                  MemberLabel :=
                    Create(Frame & ".hunger", "-text {Bit hungry}");
               when 41 .. 80 =>
                  MemberLabel := Create(Frame & ".hunger", "-text {Hungry}");
               when 81 .. 99 =>
                  MemberLabel :=
                    Create(Frame & ".hunger", "-text {Very hungry}");
               when 100 =>
                  MemberLabel := Create(Frame & ".hunger", "-text {Starving}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w -padx 5");
         Height :=
           Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      end if;
      if Member.Morale(1) /= 50 then
         if Game_Settings.Show_Numbers then
            MemberLabel :=
              Create
                (Frame & ".morale",
                 "-text {Morale:" & Natural'Image(Member.Morale(1)) & "%}");
         else
            case Member.Morale(1) is
               when 0 .. 24 =>
                  MemberLabel := Create(Frame & ".morale", "-text {Upset}");
               when 25 .. 49 =>
                  MemberLabel := Create(Frame & ".morale", "-text {Unhappy}");
               when 51 .. 74 =>
                  MemberLabel := Create(Frame & ".morale", "-text {Happy}");
               when 75 .. 100 =>
                  MemberLabel := Create(Frame & ".morale", "-text {Excited}");
               when others =>
                  null;
            end case;
         end if;
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w -padx 5");
         Height :=
           Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      end if;
      if Factions_List(Member.Faction).Flags.Find_Index
          (To_Unbounded_String("nogender")) =
        UnboundedString_Container.No_Index then
         MemberInfo :=
           (if Member.Gender = 'M' then To_Unbounded_String("Male")
            else To_Unbounded_String("Female"));
      end if;
      Append
        (MemberInfo,
         LF & "Faction: " & Factions_List(Member.Faction).Name & LF &
         "Home base: " & Sky_Bases(Member.Home_Base).Name);
      if Member.Skills.Length = 0 or Member.Contract_Length = 0 then
         Append(MemberInfo, LF & "Passenger");
         if Member.Contract_Length > 0 then
            Append(MemberInfo, LF & "Time limit:");
            Minutes_To_Date(Member.Contract_Length, MemberInfo);
         end if;
      else
         if MemberIndex > 1 then
            Append(MemberInfo, LF & "Contract length:");
            Append
              (MemberInfo,
               (if Member.Contract_Length > 0 then
                  Integer'Image(Member.Contract_Length) & " days."
                else " pernament.") &
               LF & "Payment:" & Natural'Image(Member.Payment(1)) & " " &
               To_String(Money_Name) & " each day");
            if Member.Payment(2) > 0 then
               Append
                 (MemberInfo,
                  " and " & Natural'Image(Member.Payment(2)) &
                  " percent of profit from each trade");
            end if;
            Append(MemberInfo, ".");
         end if;
      end if;
      MemberLabel :=
        Create
          (Frame & ".label",
           "-text {" & To_String(MemberInfo) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky nw -padx 5");
      Height := Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      Width := Positive'Value(Winfo_Get(MemberLabel, "reqwidth")) + 15;
      Tcl.Tk.Ada.Grid.Grid(Frame);
      if Member.Skills.Length > 0 and Member.Contract_Length /= 0 then
         -- Statistics of the selected crew member
         Frame := Create(MemberCanvas & ".stats");
         Load_Statistics_Loop :
         for I in Member.Attributes'Range loop
            ProgressFrame :=
              Create(Frame & ".statinfo" & Trim(Positive'Image(I), Left));
            MemberLabel :=
              Create
                (ProgressFrame & ".label",
                 "-text {" &
                 To_String
                   (AttributesData_Container.Element(Attributes_List, I)
                      .Name) &
                 ": " & Get_Attribute_Level_Name(Member.Attributes(I).Level) &
                 "}");
            Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky we");
            Tcl.Tk.Ada.Grid.Column_Configure
              (ProgressFrame, MemberLabel, "-weight 1");
            Tcl.Tk.Ada.Grid.Row_Configure
              (ProgressFrame, MemberLabel, "-weight 1");
            InfoButton :=
              Create
                (ProgressFrame & ".button",
                 "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewStatsInfo" &
                 Positive'Image(I) & " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (InfoButton,
               "Show detailed information about the selected attribute.");
            Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0 -padx {5 0}");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame, "-sticky we -padx 5");
            Tcl_Eval(Interp, "update");
            if Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) + 15 >
              NewWidth then
               NewWidth :=
                 Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) + 15;
            end if;
            ProgressBar :=
              Create
                (Frame & ".level" & Trim(Positive'Image(I), Left),
                 "-value" &
                 Positive'Image
                   ((if Member.Attributes(I).Level > 2 then
                       Member.Attributes(I).Level * 2
                     else 6)));
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "The current level of the attribute.");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-sticky w -padx 5");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
            ProgressFrame :=
              Create
                (Frame & ".experienceframe" & Trim(Positive'Image(I), Left),
                 "-height 12");
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame, "-sticky w -padx 5");
            ProgressBar :=
              Create
                (ProgressFrame & ".experience" & Trim(Positive'Image(I), Left),
                 "-value" &
                 Float'Image
                   (Float(Member.Attributes(I).Experience) /
                    Float(Member.Attributes(I).Level * 250)) &
                 " -maximum 1.0 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "Experience need to reach the next level");
            Tcl.Tk.Ada.Place.Place
              (ProgressBar,
               "-in " & ProgressFrame & " -relheight 1.0 -relwidth 1.0");
            NewHeight :=
              NewHeight +
              Positive'Value(Winfo_Get(ProgressFrame, "reqheight"));
            if Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) >
              NewWidth then
               NewWidth :=
                 Positive'Value(Winfo_Get(ProgressFrame, "reqwidth"));
            end if;
         end loop Load_Statistics_Loop;
         for I in Member.Attributes'Range loop
            ProgressBar :=
              Get_Widget(Frame & ".level" & Trim(Positive'Image(I), Left));
            configure
              (ProgressBar,
               "-length" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
            ProgressFrame :=
              Get_Widget
                (Frame & ".experienceframe" & Trim(Positive'Image(I), Left));
            configure
              (ProgressFrame,
               "-width" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
            ProgressBar :=
              Get_Widget
                (ProgressFrame & ".experience" &
                 Trim(Positive'Image(I), Left));
            configure
              (ProgressBar,
               "-length" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
         end loop;
         if NewHeight > Height then
            Height := NewHeight;
         end if;
         if NewWidth > Width then
            Width := NewWidth;
         end if;
         -- Skills of the selected crew member
         Frame := Create(MemberCanvas & ".skills");
         NewHeight := 1;
         Load_Skills_Loop :
         for I in Member.Skills.Iterate loop
            ProgressFrame :=
              Create
                (Frame & ".skillinfo" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
            MemberLabel :=
              Create
                (ProgressFrame & ".label" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-text {" &
                 To_String
                   (SkillsData_Container.Element
                      (Skills_List, Member.Skills(I).Index)
                      .Name) &
                 ": " & Get_Skill_Level_Name(Member.Skills(I).Level) & "}");
            Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky we");
            Tcl.Tk.Ada.Grid.Column_Configure
              (ProgressFrame, MemberLabel, "-weight 1");
            Tcl.Tk.Ada.Grid.Row_Configure
              (ProgressFrame, MemberLabel, "-weight 1");
            InfoButton :=
              Create
                (ProgressFrame & ".button",
                 "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewSkillInfo" &
                 Positive'Image(Member.Skills(I).Index) & " " &
                 CArgv.Arg(Argv, 1) & " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (InfoButton,
               "Show detailed information about the selected skill.");
            Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0 -padx {5 0}");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame, "-sticky we -padx 5");
            Tcl_Eval(Interp, "update");
            if Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) + 15 >
              NewWidth then
               NewWidth :=
                 Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) + 15;
            end if;
            ProgressBar :=
              Create
                (Frame & ".level" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-value" & Positive'Image(Member.Skills(I).Level));
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "The current level of the skill.");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar, "-sticky w -padx 5");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
            ProgressFrame :=
              Create
                (Frame & ".experienceframe" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-height 12");
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame, "-sticky w -padx 5");
            ProgressBar :=
              Create
                (ProgressFrame & ".experience" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-value" &
                 Float'Image
                   (Float(Member.Skills(I).Experience) /
                    Float((Member.Skills(I).Level * 25))) &
                 " -maximum 1.0 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "Experience need to reach the next level");
            Tcl.Tk.Ada.Place.Place
              (ProgressBar,
               "-in " & ProgressFrame & " -relheight 1.0 -relwidth 1.0");
            NewHeight :=
              NewHeight +
              Positive'Value(Winfo_Get(ProgressFrame, "reqheight"));
            Tcl_Eval(Interp, "update");
            if Positive'Value(Winfo_Get(ProgressFrame, "reqwidth")) >
              NewWidth then
               NewWidth :=
                 Positive'Value(Winfo_Get(ProgressFrame, "reqwidth"));
            end if;
         end loop Load_Skills_Loop;
         for I in Member.Skills.Iterate loop
            ProgressBar :=
              Get_Widget
                (Frame & ".level" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
            configure
              (ProgressBar,
               "-length" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
            ProgressFrame :=
              Get_Widget
                (Frame & ".experienceframe" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
            configure
              (ProgressFrame,
               "-width" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
            ProgressBar :=
              Get_Widget
                (ProgressFrame & ".experience" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
            configure
              (ProgressBar,
               "-length" &
               (if NewWidth - 15 > 200 then Positive'Image(NewWidth - 15)
                else " 200"));
         end loop;
         if NewHeight > Height then
            Height := NewHeight;
         end if;
         if NewWidth > Width then
            Width := NewWidth;
         end if;
      end if;
      if Height > 500 then
         Height := 500;
      end if;
      if Width < 250 then
         Width := 250;
      end if;
      Frame.Name := New_String(MemberCanvas & ".general");
      declare
         XPos: Integer :=
           (Width - Positive'Value(Winfo_Get(Frame, "reqwidth"))) / 2;
      begin
         if XPos < 0 then
            XPos := 0;
         end if;
         Canvas_Create
           (MemberCanvas, "window",
            Trim(Positive'Image(XPos), Left) & " 0 -anchor nw -window " &
            MemberCanvas & ".general -tag info");
      end;
      Tcl_Eval(Interp, "update");
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "] -width" &
         Positive'Image(Width) & " -height" & Positive'Image(Height));
      Bind
        (CloseButton, "<Tab>",
         "{focus " & MemberDialog & ".buttonbox.general;break}");
      Show_Dialog(Dialog => MemberDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Member_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Tab_Command
   -- FUNCTION
   -- Show the selected information about the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberTab
   -- SOURCE
   function Show_Member_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MemberCanvas: constant Tk_Canvas :=
        Get_Widget(".memberdialog.canvas", Interp);
      Frame: constant Ttk_Frame :=
        Get_Widget(MemberCanvas & "." & Tcl_GetVar(Interp, "newtab"));
      XPos: Integer :=
        (Positive'Value(Winfo_Get(MemberCanvas, "reqwidth")) -
         Positive'Value(Winfo_Get(Frame, "reqwidth"))) /
        2;
   begin
      if XPos < 0 then
         XPos := 0;
      end if;
      Delete(MemberCanvas, "info");
      Canvas_Create
        (MemberCanvas, "window",
         Trim(Positive'Image(XPos), Left) & " 0 -anchor nw -window " & Frame &
         " -tag info");
      Tcl_Eval(Interp, "update");
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "]");
      return TCL_OK;
   end Show_Member_Tab_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Stats_Info_Command
   -- FUNCTION
   -- Show the detailed information about the selected crew member statistic
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Stats_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Short_String;
      use Tiny_String;

      Attribute: constant Attribute_Record :=
        AttributesData_Container.Element
          (Attributes_List, Attributes_Amount_Range'Value(CArgv.Arg(Argv, 1)));
   begin
      ShowInfo
        (To_String(Attribute.Description), CArgv.Arg(Argv, 2),
         To_String(Attribute.Name));
      return TCL_OK;
   end Show_Crew_Stats_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Skill_Info_Command
   -- FUNCTION
   -- Show the detailed information about the selected crew member skill
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Skill_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Short_String;
      use Tiny_String;

      SkillIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      MessageText: Unbounded_String;
      ItemIndex: Tiny_String.Bounded_String;
      Quality: Natural;
   begin
      Append(MessageText, "Related attribute: ");
      Append
        (MessageText,
         To_String
           (AttributesData_Container.Element
              (Attributes_List,
               SkillsData_Container.Element(Skills_List, SkillIndex).Attribute)
              .Name));
      if SkillsData_Container.Element(Skills_List, SkillIndex).Tool /=
        Tiny_String.Null_Bounded_String then
         Append(MessageText, "." & LF & "Training tool: ");
         Quality := 0;
         if CArgv.Arg(Argv, 3) = ".memberdialog" then
            Find_Training_Tool_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).I_Type =
                 To_Unbounded_String
                   (To_String
                      (SkillsData_Container.Element(Skills_List, SkillIndex)
                         .Tool))
                 and then
                 (Items_List(I).Value.Length > 0
                  and then Items_List(I).Value(1) <=
                    Get_Training_Tool_Quality
                      (Positive'Value(CArgv.Arg(Argv, 2)), SkillIndex)) then
                  if Items_List(I).Value(1) > Quality then
                     ItemIndex := Objects_Container.Key(I);
                     Quality := Items_List(I).Value(1);
                  end if;
               end if;
            end loop Find_Training_Tool_Loop;
         else
            Find_Training_Tool_2_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).I_Type =
                 To_Unbounded_String
                   (To_String
                      (SkillsData_Container.Element(Skills_List, SkillIndex)
                         .Tool))
                 and then
                 (Items_List(I).Value.Length > 0
                  and then Items_List(I).Value(1) <=
                    Positive'Value(CArgv.Arg(Argv, 2))) then
                  if Items_List(I).Value(1) > Quality then
                     ItemIndex := Objects_Container.Key(I);
                     Quality := Items_List(I).Value(1);
                  end if;
               end if;
            end loop Find_Training_Tool_2_Loop;
         end if;
         Append(MessageText, Items_List(ItemIndex).Name);
      end if;
      Append(MessageText, "." & LF);
      Append
        (MessageText,
         To_String
           (SkillsData_Container.Element(Skills_List, SkillIndex)
              .Description));
      ShowInfo
        (To_String(MessageText), CArgv.Arg(Argv, 3),
         To_String
           (SkillsData_Container.Element(Skills_List, SkillIndex).Name));
      return TCL_OK;
   end Show_Crew_Skill_Info_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Priorities_Command
   -- FUNCTION
   -- Show information about the selected crew member priorities
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberPriorities memberindex
   -- MemberIndex is the index of the crew member to show
   -- SOURCE
   function Show_Member_Priorities_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Priorities_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := Player_Ship.Crew(MemberIndex);
      MemberDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog",
           Title => "Priorities for " & To_String(Member.Name), Columns => 2);
      CloseButton: constant Ttk_Button :=
        Create
          (MemberDialog & ".button",
           "-text Close -command {CloseDialog " & MemberDialog & "}");
      Label: Ttk_Label;
      PrioritiesNames: constant array
        (Member.Orders'Range) of Unbounded_String :=
        (To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"),
         To_Unbounded_String("Operating guns"),
         To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading ship"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleaning ship"),
         To_Unbounded_String("Defend ship"),
         To_Unbounded_String("Board enemy ship"),
         To_Unbounded_String("Train skill"));
      ComboBox: Ttk_ComboBox;
   begin
      Label := Create(MemberDialog & ".name", "-text {Priority}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-pady {5 0}");
      Label := Create(MemberDialog & ".level", "-text {Level}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 1 -pady {5 0}");
      Load_Priorities_Loop :
      for I in Member.Orders'Range loop
         Label :=
           Create
             (MemberDialog & ".name" & Trim(Positive'Image(I), Left),
              "-text {" & To_String(PrioritiesNames(I)) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w -padx {5 0}");
         ComboBox :=
           Create
             (MemberDialog & ".level" & Trim(Positive'Image(I), Left),
              "-values [list None Normal Highest] -state readonly -width 8");
         Current(ComboBox, Natural'Image(Member.Orders(I)));
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetPriority" & Positive'Image(I) & " [" & ComboBox &
            " current]" & Positive'Image(MemberIndex) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-column 1 -row" & Positive'Image(I + 1) & " -padx {0 5}");
         Bind(ComboBox, "<Escape>", "{" & CloseButton & " invoke;break}");
      end loop Load_Priorities_Loop;
      Bind(ComboBox, "<Tab>", "{focus " & CloseButton & ";break}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 2 -pady {0 5}");
      Focus(CloseButton);
      Bind(CloseButton, "<Tab>", "{focus " & MemberDialog & ".level1;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Show_Dialog(Dialog => MemberDialog, Relative_Y => 0.05);
      return TCL_OK;
   end Show_Member_Priorities_Command;

   -- ****o* SUCrew/SUCrew.Set_Priority_Command
   -- FUNCTION
   -- Set the selected priority of the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Priority_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ComboBox: Ttk_ComboBox;
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 3));
   begin
      if CArgv.Arg(Argv, 2) = "2" then
         Set_Priority_Loop :
         for Order of Player_Ship.Crew(MemberIndex).Orders loop
            if Order = 2 then
               Order := 1;
               exit Set_Priority_Loop;
            end if;
         end loop Set_Priority_Loop;
      end if;
      Player_Ship.Crew(MemberIndex).Orders
        (Positive'Value(CArgv.Arg(Argv, 1))) :=
        Natural'Value(CArgv.Arg(Argv, 2));
      UpdateOrders(Player_Ship);
      UpdateHeader;
      Update_Messages;
      UpdateCrewInfo;
      ComboBox.Interp := Interp;
      Update_Priority_Info_Loop :
      for I in Player_Ship.Crew(MemberIndex).Orders'Range loop
         ComboBox.Name :=
           New_String(".memberdialog.level" & Trim(Positive'Image(I), Left));
         Current
           (ComboBox, Natural'Image(Player_Ship.Crew(MemberIndex).Orders(I)));
      end loop Update_Priority_Info_Loop;
      return TCL_OK;
   end Set_Priority_Command;

   -- ****o* SUCrew/SUCrew.Show_Member_Menu_Command
   -- FUNCTION
   -- Show the menu with options for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberMenu memberindex
   -- MemberIndex is the index of the crew member to show menu
   -- SOURCE
   function Show_Member_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Member: constant Member_Data :=
        Player_Ship.Crew(Positive'Value(CArgv.Arg(Argv, 1)));
      NeedRepair, NeedClean: Boolean := False;
      function IsWorking
        (Owners: Natural_Container.Vector; MemberIndex: Positive)
         return Boolean is
      begin
         Find_Owner_Loop :
         for Owner of Owners loop
            if Owner = MemberIndex then
               return True;
            end if;
         end loop Find_Owner_Loop;
         return False;
      end IsWorking;
      Crew_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".membermenu",
           Title => To_String(Source => Member.Name) & " actions",
           Parent_Name => ".");
      Repair_Button: constant Ttk_Button :=
        Get_Widget(pathName => Crew_Menu & ".repair");
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
      Check_Modules_Loop :
      for Module of Player_Ship.Modules loop
         if Module.Durability < Module.Max_Durability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.M_Type = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit Check_Modules_Loop when NeedClean and NeedRepair;
      end loop Check_Modules_Loop;
      Add_Button
        (Name => ".rename", Label => "Rename crew member",
         Command =>
           "GetString {Enter a new name for the " & To_String(Member.Name) &
           ":} crewname" & CArgv.Arg(Argv, 1) & " {Renaming crew member}");
      if
        ((Member.Tired = 100 or Member.Hunger = 100 or Member.Thirst = 100) and
         Member.Order /= REST) or
        (Member.Skills.Length = 0 or Member.Contract_Length = 0) then
         Add_Button
           (Name => ".rest", Label => "Go on break",
            Command => "SetCrewOrder Rest " & CArgv.Arg(Argv => Argv, N => 1));
      else
         if Member.Order /= PILOT then
            Add_Button
              (Name => ".pilot", Label => "Go piloting the ship",
               Command =>
                 "SetCrewOrder Pilot " & CArgv.Arg(Argv => Argv, N => 1));
         end if;
         if Member.Order /= ENGINEER then
            Add_Button
              (Name => ".engineer", Label => "Go engineering the ship",
               Command =>
                 "SetCrewOrder Engineer " & CArgv.Arg(Argv => Argv, N => 1));
         end if;
         Set_Work_Orders_Loop :
         for J in Player_Ship.Modules.Iterate loop
            if Player_Ship.Modules(J).Durability <
              Player_Ship.Modules(J).Max_Durability then
               NeedRepair := True;
            end if;
            if Player_Ship.Modules(J).Durability > 0 then
               case Player_Ship.Modules(J).M_Type is
                  when GUN | HARPOON_GUN =>
                     if Player_Ship.Modules(J).Owner(1) /=
                       Positive'Value(CArgv.Arg(Argv, 1)) then
                        Add_Button
                          (Name => ".gunner",
                           Label =>
                             "Operate " &
                             To_String(Player_Ship.Modules(J).Name),
                           Command =>
                             "SetCrewOrder Gunner " &
                             CArgv.Arg(Argv => Argv, N => 1) &
                             Positive'Image
                               (Positive(Modules_Container.To_Index(J))));
                     end if;
                  when WORKSHOP =>
                     if not IsWorking
                         (Player_Ship.Modules(J).Owner,
                          Positive'Value(CArgv.Arg(Argv, 1))) and
                       Player_Ship.Modules(J).Crafting_Index /=
                         Null_Unbounded_String then
                        Add_Button
                          (Name => ".worker",
                           Label =>
                             "Work in " &
                             To_String(Player_Ship.Modules(J).Name),
                           Command =>
                             "SetCrewOrder Craft " &
                             CArgv.Arg(Argv => Argv, N => 1) &
                             Positive'Image
                               (Positive(Modules_Container.To_Index(J))));
                     end if;
                  when CABIN =>
                     if Player_Ship.Modules(J).Cleanliness <
                       Player_Ship.Modules(J).Quality and
                       Member.Order /= CLEAN and NeedClean then
                        Add_Button
                          (Name => ".clean", Label => "Clean ship",
                           Command =>
                             "SetCrewOrder Clean " &
                             CArgv.Arg(Argv => Argv, N => 1));
                        NeedClean := False;
                     end if;
                  when TRAINING_ROOM =>
                     if not IsWorking
                         (Player_Ship.Modules(J).Owner,
                          Positive'Value(CArgv.Arg(Argv, 1))) then
                        Add_Button
                          (Name => ".worker",
                           Label =>
                             "Go on training in " &
                             To_String(Player_Ship.Modules(J).Name),
                           Command =>
                             "SetCrewOrder Train " &
                             CArgv.Arg(Argv => Argv, N => 1) &
                             Positive'Image
                               (Positive(Modules_Container.To_Index(J))));
                     end if;
                  when others =>
                     null;
               end case;
               if Winfo_Get(Repair_Button, "exists") = "0" and NeedRepair then
                  Add_Button
                    (Name => ".repair", Label => "Repair ship",
                     Command =>
                       "SetCrewOrder Repair " &
                       CArgv.Arg(Argv => Argv, N => 1));
                  NeedRepair := False;
               end if;
            end if;
         end loop Set_Work_Orders_Loop;
         Check_Heal_Order_Loop :
         for J in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(J).Health < 100 and
              Crew_Container.To_Index(J) /=
                Positive'Value(CArgv.Arg(Argv, 1)) and
              Player_Ship.Crew(J).Order /= HEAL then
               Add_Button
                 (Name => ".heal", Label => "Heal wounded crew members",
                  Command =>
                    "SetCrewOrder Heal " & CArgv.Arg(Argv => Argv, N => 1));
               exit Check_Heal_Order_Loop;
            end if;
         end loop Check_Heal_Order_Loop;
         if Player_Ship.Upgrade_Module > 0 and Member.Order /= UPGRADING then
            Add_Button
              (Name => ".upgrade", Label => "Upgrade module",
               Command =>
                 "SetCrewOrder Upgrading " & CArgv.Arg(Argv => Argv, N => 1));
         end if;
         if Member.Order /= TALK then
            Add_Button
              (Name => ".talk", Label => "Talk in bases",
               Command =>
                 "SetCrewOrder Talk " & CArgv.Arg(Argv => Argv, N => 1));
         end if;
         if Member.Order /= REST then
            Add_Button
              (Name => ".gobreak", Label => "Go on break",
               Command =>
                 "SetCrewOrder Rest " & CArgv.Arg(Argv => Argv, N => 1));
         end if;
      end if;
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
      if CArgv.Arg(Argv, 1) /= "1" and Player_Ship.Speed = DOCKED then
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrew page skill
   -- Page parameter is a index of page from which starts showing
   -- crew. Skill is the index of skill to show
   -- SOURCE
   function Show_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      UpdateCrewInfo
        (Positive'Value(CArgv.Arg(Argv, 1)),
         Natural'Value(CArgv.Arg(Argv, 2)));
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipCrew x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crew_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(CrewTable, Natural'Value(CArgv.Arg(Argv, 1)));
      SkillBox: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned &
             ".shipinfoframe.crew.canvas.frame.selectskill.combox",
           Interp => Interp);
      Skill_Index: constant Natural := Natural'Value(Current(SkillBox));
      type Local_Member_Data is record
         Name: Unbounded_String;
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
      for I in Player_Ship.Crew.Iterate loop
         Local_Crew(Crew_Container.To_Index(I)) :=
           (Name => Player_Ship.Crew(I).Name,
            Order => Player_Ship.Crew(I).Order,
            Skill =>
              To_Bounded_String
                ((if Skill_Index = 0 then
                    Get_Highest_Skill(Crew_Container.To_Index(I))
                  else Get_Skill_Level_Name
                      (GetSkillLevel(Player_Ship.Crew(I), Skill_Index)))),
            Health => Player_Ship.Crew(I).Health,
            Fatigue =>
              Player_Ship.Crew(I).Tired -
              Player_Ship.Crew(I).Attributes(Positive(Condition_Index)).Level,
            Thirst => Player_Ship.Crew(I).Thirst,
            Hunger => Player_Ship.Crew(I).Hunger,
            Morale => Player_Ship.Crew(I).Morale(1),
            Id => Crew_Container.To_Index(I));
      end loop;
      Sort_Crew(Local_Crew);
      Crew_Indexes.Clear;
      for Member of Local_Crew loop
         Crew_Indexes.Append(Member.Id);
      end loop;
      UpdateCrewInfo(Skill => Skill_Index);
      return TCL_OK;
   end Sort_Crew_Command;

   -- ****o* SUCrew/SUCrew.Select_Crew_Skill_Command
   -- FUNCTION
   -- Show the list of the player's ship crew with selected skill from combobox
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SelectCrewSkill
   -- SOURCE
   function Select_Crew_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Select_Crew_Skill_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SkillBox: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned &
             ".shipinfoframe.crew.canvas.frame.selectskill.combox",
           Interp => Interp);
   begin
      UpdateCrewInfo(Skill => Natural'Value(Current(SkillBox)));
      return TCL_OK;
   end Select_Crew_Skill_Command;

   procedure AddCommands is
   begin
      Add_Command("OrderForAll", Order_For_All_Command'Access);
      Add_Command("Dismiss", Dismiss_Command'Access);
      Add_Command("SetCrewOrder", Set_Crew_Order_Command'Access);
      Add_Command("ShowMemberInfo", Show_Member_Info_Command'Access);
      Add_Command("ShowMemberTab", Show_Member_Tab_Command'Access);
      Add_Command("ShowCrewStatsInfo", Show_Crew_Stats_Info_Command'Access);
      Add_Command("ShowCrewSkillInfo", Show_Crew_Skill_Info_Command'Access);
      Add_Command
        ("ShowMemberPriorities", Show_Member_Priorities_Command'Access);
      Add_Command("SetPriority", Set_Priority_Command'Access);
      Add_Command("ShowMemberMenu", Show_Member_Menu_Command'Access);
      Add_Command("ShowCrew", Show_Crew_Command'Access);
      Add_Command("SortShipCrew", Sort_Crew_Command'Access);
      Add_Command("SelectCrewSkill", Select_Crew_Skill_Command'Access);
      Ships.UI.Crew.Inventory.AddCommands;
   end AddCommands;

end Ships.UI.Crew;
