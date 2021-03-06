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
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Game; use Game;
with Maps; use Maps;
with Messages; use Messages;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Bases is

   -- ****iv* KBases/KBases.BasesTable
   -- FUNCTION
   -- Table with info about the know bases
   -- SOURCE
   BasesTable: Table_Widget (7);
   -- ****

   -- ****if* KBases/KBases.Get_Reputation_Text
   -- FUNCTION
   -- Get the name of the reputation level in the selected base
   -- PARAMETERS
   -- Reputation_Level - The numerical level of reputation in a base
   -- RESULT
   -- The name of the reputation level in the selected base
   -- SOURCE
   function Get_Reputation_Text(Reputation_Level: Integer) return String is
      -- ****
   begin
      case Reputation_Level is
         when -100 .. -75 =>
            return "Hated";
         when -74 .. -50 =>
            return "Outlaw";
         when -49 .. -25 =>
            return "Hostile";
         when -24 .. -1 =>
            return "Unfriendly";
         when 0 =>
            return "Unknown";
         when 1 .. 25 =>
            return "Visitor";
         when 26 .. 50 =>
            return "Trader";
         when 51 .. 75 =>
            return "Friend";
         when 76 .. 100 =>
            return "Well known";
         when others =>
            return "";
      end case;
   end Get_Reputation_Text;

   procedure UpdateBasesList(BaseName: String := ""; Page: Positive := 1) is
      BasesCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".knowledgeframe.bases.canvas");
      BasesFrame: constant Ttk_Frame := Get_Widget(BasesCanvas & ".frame");
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(BasesFrame & ".options.search");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ComboBox: Ttk_ComboBox := Get_Widget(BasesFrame & ".options.types");
      BasesType, BasesOwner, BasesStatus: Unbounded_String;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(BasesFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      if BasesTable.Row > 1 then
         ClearTable(BasesTable);
      end if;
      Delete_Widgets(2, Rows - 1, BasesFrame);
      BasesTable :=
        CreateTable
          (Widget_Image(BasesFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
            To_Unbounded_String("Population"), To_Unbounded_String("Size"),
            To_Unbounded_String("Owner"), To_Unbounded_String("Type"),
            To_Unbounded_String("Reputation")),
           Get_Widget(".gameframe.paned.knowledgeframe.bases.scrolly"));
      if BaseName'Length = 0 then
         configure(SearchEntry, "-validatecommand {}");
         Delete(SearchEntry, "0", "end");
         configure(SearchEntry, "-validatecommand {ShowBases %P}");
      end if;
      BasesType := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.status");
      BasesStatus := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.owner");
      BasesOwner := To_Unbounded_String(Get(ComboBox));
      Rows := 0;
      Load_Bases_Loop :
      for I in 1 .. SkyBases'Last loop
         if not SkyBases(I).Known then
            goto End_Of_Loop;
         end if;
         if BaseName'Length > 0
           and then
             Index
               (To_Lower(To_String(SkyBases(I).Name)), To_Lower(BaseName), 1) =
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
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         AddButton
           (BasesTable, To_String(SkyBases(I).Name),
            "Show available base's options",
            "ShowBasesMenu" & Positive'Image(I), 1);
         AddButton
           (BasesTable,
            Natural'Image(CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)),
            "The distance to the base", "ShowBasesMenu" & Positive'Image(I),
            2);
         if SkyBases(I).Visited.Year > 0 then
            AddButton
              (BasesTable,
               (case SkyBases(I).Population is when 0 => "empty",
                  when 1 .. 150 => "small", when 151 .. 299 => "medium",
                  when others => "large"),
               "The population size of the base",
               "ShowBasesMenu" & Positive'Image(I), 3);
            AddButton
              (BasesTable, To_Lower(Bases_Size'Image(SkyBases(I).Size)),
               "The size of the base", "ShowBasesMenu" & Positive'Image(I), 4);
            AddButton
              (BasesTable, To_String(Factions_List(SkyBases(I).Owner).Name),
               "The faction which own the base",
               "ShowBasesMenu" & Positive'Image(I), 5);
            AddButton
              (BasesTable,
               To_String(BasesTypes_List(SkyBases(I).BaseType).Name),
               "The type of the base", "ShowBasesMenu" & Positive'Image(I), 6);
            AddButton
              (BasesTable, Get_Reputation_Text(SkyBases(I).Reputation(1)),
               "Your reputation in the base",
               "ShowBasesMenu" & Positive'Image(I), 7, True);
         else
            AddButton
              (BasesTable, "not", "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 3);
            AddButton
              (BasesTable, "", "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 4);
            AddButton
              (BasesTable, "visited", "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 5);
            AddButton
              (BasesTable, "", "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 6);
            AddButton
              (BasesTable, "yet", "Show available base's options",
               "ShowBasesMenu" & Positive'Image(I), 7, True);
         end if;
         Rows := Rows + 1;
         exit Load_Bases_Loop when Rows = 25 and I < SkyBases'Last;
         <<End_Of_Loop>>
      end loop Load_Bases_Loop;
      if Page > 1 then
         if Rows < 25 then
            AddPagination
              (BasesTable,
               "ShowBases {" & BaseName & "} " & Positive'Image(Page - 1), "");
         else
            AddPagination
              (BasesTable,
               "ShowBases {" & BaseName & "} " & Positive'Image(Page - 1),
               "ShowBases {" & BaseName & "}" & Positive'Image(Page + 1));
         end if;
      elsif Rows > 24 then
         AddPagination
           (BasesTable, "",
            "ShowBases {" & BaseName & "}" & Positive'Image(Page + 1));
      end if;
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
   -- ShowBases ?basename? ?page?
   -- Basename parameter is a string which will be looking for in the bases
   -- names, page parameter is a index of page from which starts showing
   -- bases.
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
      case Argc is
         when 3 =>
            UpdateBasesList
              (CArgv.Arg(Argv, 1), Positive'Value(CArgv.Arg(Argv, 2)));
         when 2 =>
            UpdateBasesList(CArgv.Arg(Argv, 1));
         when others =>
            UpdateBasesList;
      end case;
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
         "-label {Show the base on map} -command {ShowOnMap" &
         Map_X_Range'Image(SkyBases(BaseIndex).SkyX) &
         Map_Y_Range'Image(SkyBases(BaseIndex).SkyY) & "}");
      Menu.Add
        (BaseMenu, "command",
         "-label {Set the base as destination for the ship} -command {SetDestination2 " &
         Map_X_Range'Image(SkyBases(BaseIndex).SkyX) &
         Map_Y_Range'Image(SkyBases(BaseIndex).SkyY) & "}");
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
        Create_Dialog
          (Name => ".basedialog", Title => To_String(SkyBases(BaseIndex).Name),
           Columns => 2);
      BaseLabel: Ttk_Label;
      BaseInfo: Unbounded_String;
      procedure SetReputationText(ReputationText: String) is
         ReputationBar: constant Ttk_Frame :=
           Create
             (BaseDialog & ".reputation",
              "-width 204 -height 24 -style ProgressBar.TFrame");
         ReputationLabel: constant Ttk_Label :=
           Create(BaseDialog & ".reputationlabel");
         ReputationProgress: constant Ttk_Frame :=
           Create(ReputationBar & ".reputation", "-height 18");
      begin
         if SkyBases(BaseIndex).Reputation(1) = 0 then
            configure(ReputationLabel, "-text {Reputation: Unknown}");
         else
            configure(ReputationLabel, "-text {Reputation:}");
            Tcl.Tk.Ada.Grid.Grid(ReputationBar, "-row 2 -column 1 -padx 5");
            Tcl.Tk.Ada.Grid.Grid_Propagate(ReputationBar, "off");
            configure
              (ReputationProgress,
               "-width" &
               Positive'Image(abs (SkyBases(BaseIndex).Reputation(1))));
            if SkyBases(BaseIndex).Reputation(1) > 0 then
               configure(ReputationProgress, "-style GreenProgressBar.TFrame");
               Tcl.Tk.Ada.Grid.Grid
                 (ReputationProgress, "-padx {100 0} -pady 3");
            else
               configure(ReputationProgress, "-style RedProgressBar.TFrame");
               Tcl.Tk.Ada.Grid.Grid
                 (ReputationProgress,
                  "-padx {" &
                  Trim
                    (Positive'Image(100 + SkyBases(BaseIndex).Reputation(1)),
                     Left) &
                  " 0} -pady 3");
            end if;
            Add(ReputationBar, ReputationText);
         end if;
         Tcl.Tk.Ada.Grid.Grid(ReputationLabel, "-row 2 -sticky w -padx {5 0}");
      end SetReputationText;
   begin
      BaseInfo :=
        To_Unbounded_String
          ("Coordinates X:" & Positive'Image(SkyBases(BaseIndex).SkyX) &
           " Y:" & Positive'Image(SkyBases(BaseIndex).SkyY));
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
      SetReputationText
        (Get_Reputation_Text(SkyBases(BaseIndex).Reputation(1)));
      if BaseIndex = Player_Ship.Home_Base then
         Append(BaseInfo, LF & "It is your home base.");
      end if;
      BaseLabel :=
        Create
          (BaseDialog & ".info",
           "-text {" & To_String(BaseInfo) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid
        (BaseLabel, "-row 1 -columnspan 2 -padx 5 -pady {5 0} -sticky w");
      Add_Close_Button
        (BaseDialog & ".button", "Close", "CloseDialog " & BaseDialog, 2);
      Show_Dialog(BaseDialog);
      return TCL_OK;
   end Show_Base_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBases", Show_Bases_Command'Access);
      AddCommand("ShowBasesMenu", Show_Bases_Menu_Command'Access);
      AddCommand("ShowBaseInfo", Show_Base_Info_Command'Access);
   end AddCommands;

end Knowledge.Bases;
