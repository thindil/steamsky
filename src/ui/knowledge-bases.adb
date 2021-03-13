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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place; use Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Bases is

   -- ****iv* KBases/KBases.CargoTable
   -- FUNCTION
   -- Table with info about the know bases
   -- SOURCE
   BasesTable: Table_Widget (6);
   -- ****

   procedure UpdateBasesList(BaseName: String := "") is
      BasesCanvas: constant Tk_Canvas :=
        Get_Widget(".gameframe.paned.knowledgeframe.bases.canvas");
      BasesFrame: constant Ttk_Frame := Get_Widget(BasesCanvas & ".frame");
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(BasesFrame & ".options.search");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ComboBox: Ttk_ComboBox := Get_Widget(BasesFrame & ".options.types");
      BasesType, BasesOwner, BasesStatus: Unbounded_String;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(BasesFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(2, Rows - 1, BasesFrame);
      BasesTable :=
        CreateTable
          (Widget_Image(BasesFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
            To_Unbounded_String("Population"), To_Unbounded_String("Size"),
            To_Unbounded_String("Owner"), To_Unbounded_String("Type")),
           False);
      if BaseName'Length = 0 then
         Delete(SearchEntry, "0", "end");
      end if;
      BasesType := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.status");
      BasesStatus := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.owner");
      BasesOwner := To_Unbounded_String(Get(ComboBox));
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            if BaseName'Length > 0
              and then
                Index
                  (To_Lower(To_String(SkyBases(I).Name)), To_Lower(BaseName),
                   1) =
                0 then
               goto End_Of_Loop;
            end if;
            if BasesStatus = To_Unbounded_String("Only not visited") and
              SkyBases(I).Visited.Year /= 0 then
               goto End_Of_Loop;
            end if;
            if BasesStatus = To_Unbounded_String("Only visited") and
              SkyBases(I).Visited.Year = 0 then
               goto End_Of_Loop;
            end if;
            if SkyBases(I).Visited.Year = 0
              and then
              (BasesType /= To_Unbounded_String("Any") or
               BasesOwner /= To_Unbounded_String("Any")) then
               goto End_Of_Loop;
            end if;
            AddButton
              (BasesTable, To_String(SkyBases(I).Name),
               "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 1);
            AddButton
              (BasesTable,
               Natural'Image
                 (CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)),
               "The distance to the base", "ShowBasesMenu" & Positive'Image(I),
               2);
            if SkyBases(I).Visited.Year > 0 then
               if SkyBases(I).Population = 0 then
                  AddButton
                    (BasesTable, "empty", "The population size of the base",
                     "ShowBasesMenu" & Positive'Image(I), 3);
               elsif SkyBases(I).Population < 150 then
                  AddButton
                    (BasesTable, "small", "The population size of the base",
                     "ShowBasesMenu" & Positive'Image(I), 3);
               elsif SkyBases(I).Population < 300 then
                  AddButton
                    (BasesTable, "medium", "The population size of the base",
                     "ShowBasesMenu" & Positive'Image(I), 3);
               else
                  AddButton
                    (BasesTable, "large", "The population size of the base",
                     "ShowBasesMenu" & Positive'Image(I), 3);
               end if;
               AddButton
                 (BasesTable, To_Lower(Bases_Size'Image(SkyBases(I).Size)),
                  "The size of the base", "ShowBasesMenu" & Positive'Image(I),
                  4);
               AddButton
                 (BasesTable, To_String(Factions_List(SkyBases(I).Owner).Name),
                  "The faction which own the base",
                  "ShowBasesMenu" & Positive'Image(I), 5);
               AddButton
                 (BasesTable,
                  To_String(BasesTypes_List(SkyBases(I).BaseType).Name),
                  "The type of the base", "ShowBasesMenu" & Positive'Image(I),
                  6, True);
            else
               AddButton
                 (BasesTable, "not", "Show available base's options",
                  "ShowBasesMenu" & Positive'Image(I), 3);
               AddButton
                 (BasesTable, "visited", "Show available base's options",
                  "ShowBasesMenu" & Positive'Image(I), 4);
               AddButton
                 (BasesTable, "yet", "Show available base's options",
                  "ShowBasesMenu" & Positive'Image(I), 5);
               AddButton
                 (BasesTable, "", "Show available base's options",
                  "ShowBasesMenu" & Positive'Image(I), 6, True);
            end if;
         end if;
         <<End_Of_Loop>>
      end loop;
      UpdateTable(BasesTable);
      Tcl_Eval(Get_Context, "update");
      configure
        (BasesCanvas, "-scrollregion [list " & BBox(BasesCanvas, "all") & "]");
      Xview_Move_To(BasesCanvas, "0.0");
      Yview_Move_To(BasesCanvas, "0.0");
   end UpdateBasesList;

   -- ****o* KBases/KBases.Show_Bases_Command
   -- FUNCTION
   -- Show the list of known bases to a player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBases ?basename?
   -- Basename parameter is a string which will be looking for in the bases
   -- names
   -- SOURCE
   function Show_Bases_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Bases_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
   begin
      if Argc > 1 then
         UpdateBasesList(CArgv.Arg(Argv, 1));
      else
         UpdateBasesList;
      end if;
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Bases_Command;

   -- ****if* KBases/KBases.Show_Bases_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected base options
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseMenu baseindex
   -- BaseIndex is the index of the base's menu to show
   -- SOURCE
   function Show_Bases_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Bases_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      BaseMenu: Tk_Menu := Get_Widget(".baseslistmenu", Interp);
      BaseIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if Winfo_Get(BaseMenu, "exists") = "0" then
         BaseMenu := Create(".baseslistmenu", "-tearoff false");
      end if;
      Delete(BaseMenu, "0", "end");
      Menu.Add
        (BaseMenu, "command",
         "-label {Show the base on map} -command {ShowBase " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (BaseMenu, "command",
         "-label {Set the base as destination for the ship} -command {SetBase2 " &
         CArgv.Arg(Argv, 1) & "}");
      if SkyBases(BaseIndex).Visited.Year > 0 then
         Menu.Add
           (BaseMenu, "command",
            "-label {Show more information about the base} -command {ShowBaseInfo " &
            CArgv.Arg(Argv, 1) & "}");
      end if;
      Tk_Popup
        (BaseMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Bases_Menu_Command;

   -- ****if* KBases/KBases.Show_Base_Command
   -- FUNCTION
   -- Show the selected base on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBase baseidex
   -- BaseIndex is the index of the base to show
   -- SOURCE
   function Show_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      BaseIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      CenterX := SkyBases(BaseIndex).SkyX;
      CenterY := SkyBases(BaseIndex).SkyY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Base_Command;

   -- ****if* KBases/KBases.Set_Base_Command
   -- FUNCTION
   -- Set the selected base as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBase2 baseidex
   -- BaseIndex is the index of the base to show
   -- SOURCE
   function Set_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      BaseIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if SkyBases(BaseIndex).SkyX = PlayerShip.SkyX and
        SkyBases(BaseIndex).SkyY = PlayerShip.SkyY then
         ShowMessage("You are at this base now.");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
      PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      ShowSkyMap(True);
      return TCL_OK;
   end Set_Base_Command;

   -- ****o* KBases/KBases.Show_Base_Info_Command
   -- FUNCTION
   -- Show information about the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseInfo baseindex
   -- BaseIndex is the index of the base to show
   -- SOURCE
   function Show_Base_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      BaseIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      BaseDialog: constant Ttk_Frame :=
        Create(".basedialog", "-style Dialog.TFrame");
      CloseButton: constant Ttk_Button :=
        Create
          (BaseDialog & ".button",
           "-text Close -command {CloseDialog " & BaseDialog & "}");
      BaseLabel: Ttk_Label;
      BaseInfo: Unbounded_String;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
      procedure SetReputationText(ReputationText: String) is
         ReputationBar: constant Ttk_ProgressBar :=
           Create(BaseDialog & ".reputation", "-maximum 200.0 -length 150");
         ReputationLabel: constant Ttk_Label :=
           Create(BaseDialog & ".reputationlabel");
         ProgressBarStyle: Unbounded_String;
      begin
         if SkyBases(BaseIndex).Reputation(1) < 0 then
            ProgressBarStyle := Null_Unbounded_String;
         elsif SkyBases(BaseIndex).Reputation(1) < 20 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         end if;
         if SkyBases(BaseIndex).Reputation(1) = 0 then
            configure(ReputationLabel, "-text {Reputation: Unknown}");
         else
            configure(ReputationLabel, "-text {Reputation:}");
            configure
              (ReputationBar,
               "-value" &
               Integer'Image((SkyBases(BaseIndex).Reputation(1)) + 100) &
               To_String(ProgressBarStyle));
            Tcl.Tk.Ada.Grid.Grid(ReputationBar, "-row 1 -column 1");
            Add(ReputationBar, ReputationText);
         end if;
         Tcl.Tk.Ada.Grid.Grid(ReputationLabel, "-row 1 -sticky w -padx {5 0}");
      end SetReputationText;
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      BaseInfo :=
        To_Unbounded_String
          ("X:" & Positive'Image(SkyBases(BaseIndex).SkyX) & " Y:" &
           Positive'Image(SkyBases(BaseIndex).SkyY));
      Append
        (BaseInfo,
         LF & "Last visited: " & FormatedTime(SkyBases(BaseIndex).Visited));
      declare
         TimeDiff: Integer;
      begin
         if SkyBases(BaseIndex).Population > 0 and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff := 30 - DaysDifference(SkyBases(BaseIndex).RecruitDate);
            if TimeDiff > 0 then
               Append
                 (BaseInfo,
                  LF & "New recruits available in" & Natural'Image(TimeDiff) &
                  " days.");
            else
               Append(BaseInfo, LF & "New recruits available now.");
            end if;
         else
            Append
              (BaseInfo, LF & "You can't recruit crew members at this base.");
         end if;
         if SkyBases(BaseIndex).Population > 0 and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff := DaysDifference(SkyBases(BaseIndex).AskedForEvents);
            if TimeDiff < 7 then
               Append
                 (BaseInfo,
                  LF & "You asked for events" & Natural'Image(TimeDiff) &
                  " days ago.");
            else
               Append(BaseInfo, LF & "You can ask for events again.");
            end if;
         else
            Append(BaseInfo, LF & "You can't ask for events at this base.");
         end if;
         if SkyBases(BaseIndex).Population > 0 and
           SkyBases(BaseIndex).Reputation(1) > -1 then
            TimeDiff := 7 - DaysDifference(SkyBases(BaseIndex).MissionsDate);
            if TimeDiff > 0 then
               Append
                 (BaseInfo,
                  LF & "New missions available in" & Natural'Image(TimeDiff) &
                  " days.");
            else
               Append(BaseInfo, LF & "New missions available now.");
            end if;
         else
            Append(BaseInfo, LF & "You can't take missions at this base.");
         end if;
      end;
      case SkyBases(BaseIndex).Reputation(1) is
         when -100 .. -75 =>
            SetReputationText("Hated");
         when -74 .. -50 =>
            SetReputationText("Outlaw");
         when -49 .. -25 =>
            SetReputationText("Hostile");
         when -24 .. -1 =>
            SetReputationText("Unfriendly");
         when 0 =>
            SetReputationText("Unknown");
         when 1 .. 25 =>
            SetReputationText("Visitor");
         when 26 .. 50 =>
            SetReputationText("Trader");
         when 51 .. 75 =>
            SetReputationText("Friend");
         when 76 .. 100 =>
            SetReputationText("Well known");
         when others =>
            null;
      end case;
      if BaseIndex = PlayerShip.HomeBase then
         Append(BaseInfo, LF & "It is your home base.");
      end if;
      BaseLabel :=
        Create
          (BaseDialog & ".info",
           "-text {" & To_String(BaseInfo) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid
        (BaseLabel, "-row 0 -columnspan 2 -padx 5 -pady {5 0}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 2 -columnspan 2 -pady {0 5}");
      Focus(CloseButton);
      Tcl.Tk.Ada.Place.Place(BaseDialog, "-in .gameframe -relx 0.3 -rely 0.3");
      Bind(CloseButton, "<Tab>", "{focus " & CloseButton & ";break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      return TCL_OK;
   end Show_Base_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBases", Show_Bases_Command'Access);
      AddCommand("ShowBasesMenu", Show_Bases_Menu_Command'Access);
      AddCommand("ShowBase", Show_Base_Command'Access);
      AddCommand("SetBase2", Set_Base_Command'Access);
      AddCommand("ShowBaseInfo", Show_Base_Info_Command'Access);
   end AddCommands;

end Knowledge.Bases;
