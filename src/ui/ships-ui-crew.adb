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
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
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
   CrewTable: Table_Widget (7);
   -- ****

   procedure UpdateCrewInfo(Page: Positive := 1) is
      CrewInfoFrame, ButtonsFrame: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ShipCanvas: Tk_Canvas;
      NeedRepair, NeedClean: Boolean := False;
      Button: Ttk_Button;
      TiredLevel: Integer;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
   begin
      CrewInfoFrame.Interp := Get_Context;
      CrewInfoFrame.Name :=
        New_String(".gameframe.paned.shipinfoframe.crew.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, CrewInfoFrame);
      ButtonsFrame := Create(CrewInfoFrame & ".ordersbuttons");
      Check_Modules_Loop :
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.MType = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit Check_Modules_Loop when NeedClean and NeedRepair;
      end loop Check_Modules_Loop;
      if NeedClean then
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
         Tcl.Tk.Ada.Grid.Grid(Button, "-padx 5");
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
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1 -padx 5");
      end if;
      Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky w");
      CrewTable :=
        CreateTable
          (Widget_Image(CrewInfoFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Order"),
            To_Unbounded_String("Health"), To_Unbounded_String("Fatigue"),
            To_Unbounded_String("Thirst"), To_Unbounded_String("Hunger"),
            To_Unbounded_String("Morale")),
           Get_Widget(".gameframe.paned.shipinfoframe.crew.scrolly"));
      Load_Crew_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         AddButton
           (CrewTable, To_String(PlayerShip.Crew(I).Name),
            "Show available crew member's options",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 1);
         AddButton
           (CrewTable,
            Crew_Orders'Image(PlayerShip.Crew(I).Order)(1) &
            To_Lower
              (Crew_Orders'Image(PlayerShip.Crew(I).Order)
                 (2 .. Crew_Orders'Image(PlayerShip.Crew(I).Order)'Last)),
            "The current order for the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 2);
         AddProgressBar
           (CrewTable, PlayerShip.Crew(I).Health, Skill_Range'Last,
            "The current health level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 3);
         TiredLevel :=
           PlayerShip.Crew(I).Tired -
           PlayerShip.Crew(I).Attributes(Condition_Index)(1);
         if TiredLevel < 0 then
            TiredLevel := 0;
         end if;
         AddProgressBar
           (CrewTable, TiredLevel, Skill_Range'Last,
            "The current tired level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 4,
            False, True);
         AddProgressBar
           (CrewTable, PlayerShip.Crew(I).Thirst, Skill_Range'Last,
            "The current thirst level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 5,
            False, True);
         AddProgressBar
           (CrewTable, PlayerShip.Crew(I).Hunger, Skill_Range'Last,
            "The current hunger level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 6,
            False, True);
         AddProgressBar
           (CrewTable, PlayerShip.Crew(I).Morale(1), Skill_Range'Last,
            "The current morale level of the selected crew member",
            "ShowMemberMenu" & Positive'Image(Crew_Container.To_Index(I)), 7,
            True);
         exit Load_Crew_Loop when CrewTable.Row = 26;
         <<End_Of_Loop>>
      end loop Load_Crew_Loop;
      if Page > 1 then
         if CrewTable.Row < 26 then
            AddPagination
              (CrewTable, "ShowCrew" & Positive'Image(Page - 1), "");
         else
            AddPagination
              (CrewTable, "ShowCrew" & Positive'Image(Page - 1),
               "ShowCrew" & Positive'Image(Page + 1));
         end if;
      elsif CrewTable.Row = 26 then
         AddPagination(CrewTable, "", "ShowCrew" & Positive'Image(Page + 1));
      end if;
      UpdateTable(CrewTable);
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Interp := Get_Context;
      ShipCanvas.Name :=
        New_String(".gameframe.paned.shipinfoframe.crew.canvas");
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
      for I in PlayerShip.Crew.Iterate loop
         GiveOrders
           (PlayerShip, Crew_Container.To_Index(I),
            Crew_Orders'Value(CArgv.Arg(Argv, 1)));
      end loop Give_Orders_Loop;
      UpdateHeader;
      UpdateMessages;
      UpdateCrewInfo;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error =>
         AddMessage(Exception_Message(An_Exception), OrderMessage);
         UpdateHeader;
         UpdateMessages;
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
         To_String(PlayerShip.Crew(MemberIndex).Name) & "?",
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
        (PlayerShip, Positive'Value(CArgv.Arg(Argv, 2)),
         Crew_Orders'Value(CArgv.Arg(Argv, 1)), ModuleIndex);
      UpdateHeader;
      UpdateMessages;
      UpdateCrewInfo;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
         AddMessage(Exception_Message(An_Exception), OrderMessage, RED);
         UpdateMessages;
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
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := PlayerShip.Crew(MemberIndex);
      MemberDialog: constant Ttk_Frame :=
        Create(".memberdialog", "-style Dialog.TFrame");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".yscroll",
           "-orient vertical -command [list .memberdialog.canvas yview]");
      MemberCanvas: constant Tk_Canvas :=
        Create
          (MemberDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      CloseButton: constant Ttk_Button :=
        Create
          (MemberDialog & ".button",
           "-text Close -command {CloseDialog " & MemberDialog & "}");
      Height, NewHeight: Positive := 1;
      ProgressFrame: Ttk_Frame;
      MemberInfo: Unbounded_String;
      MemberLabel: Ttk_Label;
      Width, NewWidth: Positive;
      TiredPoints: Integer;
      ProgressBar: Ttk_ProgressBar;
      TabButton: Ttk_RadioButton;
      InfoButton: Ttk_Button;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Create(MemberDialog & ".buttonbox");
      Tcl_SetVar(Interp, "newtab", "general");
      TabButton :=
        Create
          (Frame & ".general",
           " -text General -state selected -style Radio.Toolbutton -value general -variable newtab -command ShowMemberTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton);
      Bind(TabButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Height := Positive'Value(Winfo_Get(TabButton, "reqheight"));
      if Member.Skills.Length > 0 and Member.ContractLength /= 0 then
         TabButton :=
           Create
             (Frame & ".stats",
              " -text Statistics -style Radio.Toolbutton -value stats -variable newtab -command ShowMemberTab");
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
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-pady {0 5} -columnspan 2");
      Focus(CloseButton);
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
      TiredPoints := Member.Tired - Member.Attributes(Condition_Index)(1);
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
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
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
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
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
         Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
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
      Append(MemberInfo, LF & "Faction: ");
      Append(MemberInfo, Factions_List(Member.Faction).Name);
      Append(MemberInfo, LF & "Home base: ");
      Append(MemberInfo, SkyBases(Member.HomeBase).Name);
      if Member.Skills.Length = 0 or Member.ContractLength = 0 then
         Append(MemberInfo, LF & "Passenger");
         if Member.ContractLength > 0 then
            Append(MemberInfo, LF & "Time limit:");
            MinutesToDate(Member.ContractLength, MemberInfo);
         end if;
      else
         if MemberIndex > 1 then
            Append(MemberInfo, LF & "Contract length:");
            if Member.ContractLength > 0 then
               Append
                 (MemberInfo, Integer'Image(Member.ContractLength) & " days.");
            else
               Append(MemberInfo, " pernament.");
            end if;
            Append
              (MemberInfo,
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
      Tcl.Tk.Ada.Grid.Grid(MemberLabel, "-sticky w");
      Height := Height + Positive'Value(Winfo_Get(MemberLabel, "reqheight"));
      Width := Positive'Value(Winfo_Get(MemberLabel, "reqwidth"));
      Tcl.Tk.Ada.Grid.Grid(Frame);
      if Member.Skills.Length > 0 and Member.ContractLength /= 0 then
         -- Statistics of the selected crew member
         Frame := Create(MemberCanvas & ".stats");
         Load_Statistics_Loop :
         for I in Member.Attributes.Iterate loop
            ProgressFrame :=
              Create
                (Frame & ".statinfo" &
                 Trim(Positive'Image(Attributes_Container.To_Index(I)), Left));
            MemberLabel :=
              Create
                (ProgressFrame & ".label",
                 "-text {" &
                 To_String
                   (Attributes_List(Attributes_Container.To_Index(I)).Name) &
                 ": " & GetAttributeLevelName(Member.Attributes(I)(1)) & "}");
            Tcl.Tk.Ada.Grid.Grid(MemberLabel);
            InfoButton :=
              Create
                (ProgressFrame & ".button",
                 "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewStatsInfo" &
                 Positive'Image(Attributes_Container.To_Index(I)) &
                 " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (InfoButton,
               "Show detailed information about the selected statistic.");
            Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
            ProgressBar :=
              Create
                (Frame & ".level" &
                 Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
                 "-value" & Positive'Image(Member.Attributes(I)(1) * 2) &
                 " -length 200");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "The current level of the attribute.");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar);
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
            ProgressFrame :=
              Create
                (Frame & ".experienceframe" &
                 Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
                 "-height 12 -width 200");
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
            ProgressBar :=
              Create
                (ProgressFrame & ".experience" &
                 Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
                 "-value" &
                 Float'Image
                   (Float(Member.Attributes(I)(2)) /
                    Float(Member.Attributes(I)(1) * 250)) &
                 " -maximum 1.0 -length 200 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "Experience need to reach next level");
            Tcl.Tk.Ada.Place.Place
              (ProgressBar,
               "-in " & ProgressFrame & " -relheight 1.0 -relwidth 1.0");
            NewHeight :=
              NewHeight +
              Positive'Value(Winfo_Get(ProgressFrame, "reqheight"));
            NewWidth := Positive'Value(Winfo_Get(ProgressFrame, "reqwidth"));
         end loop Load_Statistics_Loop;
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
                 "-text {" & To_String(Skills_List(Member.Skills(I)(1)).Name) &
                 ": " & GetSkillLevelName(Member.Skills(I)(2)) & "}");
            Tcl.Tk.Ada.Grid.Grid(MemberLabel);
            InfoButton :=
              Create
                (ProgressFrame & ".button",
                 "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewSkillInfo" &
                 Positive'Image(Member.Skills(I)(1)) & " " &
                 CArgv.Arg(Argv, 1) & " .memberdialog}");
            Tcl.Tklib.Ada.Tooltip.Add
              (InfoButton,
               "Show detailed information about the selected skill.");
            Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0");
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
            ProgressBar :=
              Create
                (Frame & ".level" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-value" & Positive'Image(Member.Skills(I)(2)) &
                 " -length 200");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "The current level of the skill.");
            Tcl.Tk.Ada.Grid.Grid(ProgressBar);
            NewHeight :=
              NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
            ProgressFrame :=
              Create
                (Frame & ".experienceframe" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-height 12 -width 200");
            Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
            ProgressBar :=
              Create
                (ProgressFrame & ".experience" &
                 Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
                 "-value" &
                 Float'Image
                   (Float(Member.Skills(I)(3)) /
                    Float((Member.Skills(I)(2) * 25))) &
                 " -maximum 1.0 -length 200 -style experience.Horizontal.TProgressbar");
            Tcl.Tklib.Ada.Tooltip.Add
              (ProgressBar, "Experience need to reach next level");
            Tcl.Tk.Ada.Place.Place
              (ProgressBar,
               "-in " & ProgressFrame & " -relheight 1.0 -relwidth 1.0");
            NewHeight :=
              NewHeight +
              Positive'Value(Winfo_Get(ProgressFrame, "reqheight"));
            NewWidth := Positive'Value(Winfo_Get(ProgressFrame, "reqwidth"));
         end loop Load_Skills_Loop;
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
      Canvas_Create
        (MemberCanvas, "window",
         "0 0 -anchor nw -window " & MemberCanvas & ".general -tag info");
      Tcl_Eval(Interp, "update");
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "] -width" &
         Positive'Image(Width) & " -height" & Positive'Image(Height));
      Tcl.Tk.Ada.Place.Place
        (MemberDialog, "-in .gameframe -relx 0.3 -rely 0.2");
      Bind
        (CloseButton, "<Tab>",
         "{focus " & MemberDialog & ".buttonbox.general;break}");
      Bind(MemberDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
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
      XPos: constant Natural :=
        (Positive'Value(Winfo_Get(MemberCanvas, "reqwidth")) -
         Positive'Value(Winfo_Get(Frame, "reqwidth"))) /
        2;
   begin
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
   begin
      ShowInfo
        (To_String
           (Attributes_List(Positive'Value(CArgv.Arg(Argv, 1))).Description),
         CArgv.Arg(Argv, 2));
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
      SkillIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      MessageText, ItemIndex: Unbounded_String;
      Quality: Natural;
   begin
      Append(MessageText, "Related statistic: ");
      Append
        (MessageText, Attributes_List(Skills_List(SkillIndex).Attribute).Name);
      if Skills_List(SkillIndex).Tool /= Null_Unbounded_String then
         Append(MessageText, "." & LF & "Training tool: ");
         Quality := 0;
         if CArgv.Arg(Argv, 3) = ".memberdialog" then
            Find_Training_Tool_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).IType = Skills_List(SkillIndex).Tool
                 and then
                 (Items_List(I).Value.Length > 0
                  and then Items_List(I).Value(1) <=
                    GetTrainingToolQuality
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
               if Items_List(I).IType = Skills_List(SkillIndex).Tool
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
      Append(MessageText, Skills_List(SkillIndex).Description);
      ShowInfo(To_String(MessageText), CArgv.Arg(Argv, 3));
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
      Member: constant Member_Data := PlayerShip.Crew(MemberIndex);
      MemberDialog: constant Ttk_Frame :=
        Create(".memberdialog", "-style Dialog.TFrame");
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
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Label := Create(MemberDialog & ".name", "-text {Priority}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-pady {5 0}");
      Label := Create(MemberDialog & ".level", "-text {Level}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0 -pady {5 0}");
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
           (ComboBox, "-column 1 -row" & Positive'Image(I) & " -padx {0 5}");
         Bind(ComboBox, "<Escape>", "{" & CloseButton & " invoke;break}");
      end loop Load_Priorities_Loop;
      Bind(ComboBox, "<Tab>", "{focus " & CloseButton & ";break}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 2 -pady {0 5}");
      Focus(CloseButton);
      Tcl.Tk.Ada.Place.Place
        (MemberDialog, "-in .gameframe -relx 0.3 -rely 0.05");
      Bind(CloseButton, "<Tab>", "{focus " & MemberDialog & ".level1;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
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
         for Order of PlayerShip.Crew(MemberIndex).Orders loop
            if Order = 2 then
               Order := 1;
               exit Set_Priority_Loop;
            end if;
         end loop Set_Priority_Loop;
      end if;
      PlayerShip.Crew(MemberIndex).Orders
        (Positive'Value(CArgv.Arg(Argv, 1))) :=
        Natural'Value(CArgv.Arg(Argv, 2));
      UpdateOrders(PlayerShip);
      UpdateHeader;
      UpdateMessages;
      UpdateCrewInfo;
      ComboBox.Interp := Interp;
      Update_Priority_Info_Loop :
      for I in PlayerShip.Crew(MemberIndex).Orders'Range loop
         ComboBox.Name :=
           New_String(".memberdialog.level" & Trim(Positive'Image(I), Left));
         Current
           (ComboBox, Natural'Image(PlayerShip.Crew(MemberIndex).Orders(I)));
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
      pragma Unreferenced(ClientData, Argc);
      CrewMenu: Tk_Menu := Get_Widget(".membermenu", Interp);
      Member: constant Member_Data :=
        PlayerShip.Crew(Positive'Value(CArgv.Arg(Argv, 1)));
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
   begin
      Check_Modules_Loop :
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.MType = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit Check_Modules_Loop when NeedClean and NeedRepair;
      end loop Check_Modules_Loop;
      if (Winfo_Get(CrewMenu, "exists")) = "0" then
         CrewMenu := Create(".membermenu", "-tearoff false");
      end if;
      Delete(CrewMenu, "0", "end");
      Menu.Add
        (CrewMenu, "command",
         "-label {Rename crew member} -command {GetString {Enter a new name for the " &
         To_String(Member.Name) & ":} crewname" & CArgv.Arg(Argv, 1) &
         " {Renaming crew member}}");
      if
        ((Member.Tired = 100 or Member.Hunger = 100 or Member.Thirst = 100) and
         Member.Order /= Rest) or
        (Member.Skills.Length = 0 or Member.ContractLength = 0) then
         Menu.Add
           (CrewMenu, "command",
            "-label {Go on break} -command {SetCrewOrder Rest " &
            CArgv.Arg(Argv, 1) & "}");
      else
         if Member.Order /= Pilot then
            Menu.Add
              (CrewMenu, "command",
               "-label {Go piloting the ship} -command {SetCrewOrder Pilot " &
               CArgv.Arg(Argv, 1) & "}");
         end if;
         if Member.Order /= Engineer then
            Menu.Add
              (CrewMenu, "command",
               "-label {Go engineering the ship} -command {SetCrewOrder Engineer " &
               CArgv.Arg(Argv, 1) & "}");
         end if;
         Set_Work_Orders_Loop :
         for J in PlayerShip.Modules.Iterate loop
            if PlayerShip.Modules(J).Durability <
              PlayerShip.Modules(J).MaxDurability then
               NeedRepair := True;
            end if;
            if PlayerShip.Modules(J).Durability > 0 then
               case PlayerShip.Modules(J).MType is
                  when GUN | HARPOON_GUN =>
                     if PlayerShip.Modules(J).Owner(1) /=
                       Positive'Value(CArgv.Arg(Argv, 1)) then
                        Menu.Add
                          (CrewMenu, "command",
                           "-label {Operate " &
                           To_String(PlayerShip.Modules(J).Name) &
                           "} -command {SetCrewOrder Gunner " &
                           CArgv.Arg(Argv, 1) &
                           Positive'Image
                             (Positive(Modules_Container.To_Index(J))) &
                           "}");
                     end if;
                  when WORKSHOP =>
                     if not IsWorking
                         (PlayerShip.Modules(J).Owner,
                          Positive'Value(CArgv.Arg(Argv, 1))) and
                       PlayerShip.Modules(J).CraftingIndex /=
                         Null_Unbounded_String then
                        Menu.Add
                          (CrewMenu, "command",
                           "-label {Work in " &
                           To_String(PlayerShip.Modules(J).Name) &
                           "} -command {SetCrewOrder Craft " &
                           CArgv.Arg(Argv, 1) &
                           Positive'Image
                             (Positive(Modules_Container.To_Index(J))) &
                           "}");
                     end if;
                  when CABIN =>
                     if PlayerShip.Modules(J).Cleanliness <
                       PlayerShip.Modules(J).Quality and
                       Member.Order /= Clean and NeedClean then
                        Menu.Add
                          (CrewMenu, "command",
                           "-label {Clean ship} -command {SetCrewOrder Clean " &
                           CArgv.Arg(Argv, 1) & "}");
                        NeedClean := False;
                     end if;
                  when TRAINING_ROOM =>
                     if not IsWorking
                         (PlayerShip.Modules(J).Owner,
                          Positive'Value(CArgv.Arg(Argv, 1))) then
                        Menu.Add
                          (CrewMenu, "command",
                           "-label {Go on training in " &
                           To_String(PlayerShip.Modules(J).Name) &
                           "} -command {SetCrewOrder Train " &
                           CArgv.Arg(Argv, 1) &
                           Positive'Image
                             (Positive(Modules_Container.To_Index(J))) &
                           "}");
                     end if;
                  when others =>
                     null;
               end case;
               if PlayerShip.Modules(J).Durability <
                 PlayerShip.Modules(J).MaxDurability and
                 NeedRepair then
                  Menu.Add
                    (CrewMenu, "command",
                     "-label {Repair ship} -command {SetCrewOrder Repair " &
                     CArgv.Arg(Argv, 1) & "}");
                  NeedRepair := False;
               end if;
            end if;
         end loop Set_Work_Orders_Loop;
         Check_Heal_Order_Loop :
         for J in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(J).Health < 100 and
              Crew_Container.To_Index(J) /=
                Positive'Value(CArgv.Arg(Argv, 1)) and
              PlayerShip.Crew(J).Order /= Heal then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Heal wounded crew members} -command {SetCrewOrder Heal " &
                  CArgv.Arg(Argv, 1) & "}");
               exit Check_Heal_Order_Loop;
            end if;
         end loop Check_Heal_Order_Loop;
         if PlayerShip.UpgradeModule > 0 and Member.Order /= Upgrading then
            Menu.Add
              (CrewMenu, "command",
               "-label {Upgrade module} -command {SetCrewOrder Upgrading " &
               CArgv.Arg(Argv, 1) & "}");
         end if;
         if Member.Order /= Talk then
            Menu.Add
              (CrewMenu, "command",
               "-label {Talking in bases} -command {SetCrewOrder Talk " &
               CArgv.Arg(Argv, 1) & "}");
         end if;
         if Member.Order /= Rest then
            Menu.Add
              (CrewMenu, "command",
               "-label {Go on break} -command {SetCrewOrder Rest " &
               CArgv.Arg(Argv, 1) & "}");
         end if;
      end if;
      Menu.Add
        (CrewMenu, "command",
         "-label {Show more info about the crew member} -command {ShowMemberInfo " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (CrewMenu, "command",
         "-label {Show inventory of the crew member} -command {ShowMemberInventory " &
         CArgv.Arg(Argv, 1) & "}");
      if Member.Skills.Length > 0 and Member.ContractLength /= 0 then
         Menu.Add
           (CrewMenu, "command",
            "-label {Set order priorities of the crew member} -command {ShowMemberPriorities " &
            CArgv.Arg(Argv, 1) & "}");
      end if;
      if CArgv.Arg(Argv, 1) /= "1" and PlayerShip.Speed = DOCKED then
         Menu.Add
           (CrewMenu, "command",
            "-label {Dismiss} -command {Dismiss " & CArgv.Arg(Argv, 1) & "}");
      end if;
      Tk_Popup
        (CrewMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Member_Menu_Command;

   -- ****o* SUCrew/SUCrew.Show_Crew_Command
   -- FUNCTION
   -- Show the list of the player's ship crew to a player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrew ?page?
   -- Page parameter is a index of page from which starts showing
   -- crew.
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
      UpdateCrewInfo(Positive'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end Show_Crew_Command;

   procedure AddCommands is
   begin
      AddCommand("OrderForAll", Order_For_All_Command'Access);
      AddCommand("Dismiss", Dismiss_Command'Access);
      AddCommand("SetCrewOrder", Set_Crew_Order_Command'Access);
      AddCommand("ShowMemberInfo", Show_Member_Info_Command'Access);
      AddCommand("ShowMemberTab", Show_Member_Tab_Command'Access);
      AddCommand("ShowCrewStatsInfo", Show_Crew_Stats_Info_Command'Access);
      AddCommand("ShowCrewSkillInfo", Show_Crew_Skill_Info_Command'Access);
      AddCommand
        ("ShowMemberPriorities", Show_Member_Priorities_Command'Access);
      AddCommand("SetPriority", Set_Priority_Command'Access);
      AddCommand("ShowMemberMenu", Show_Member_Menu_Command'Access);
      AddCommand("ShowCrew", Show_Crew_Command'Access);
      Ships.UI.Crew.Inventory.AddCommands;
   end AddCommands;

end Ships.UI.Crew;
