-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Factions; use Factions;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body BasesList is

   -- ****f* BasesList/Show_Bases_Command
   -- FUNCTION
   -- Show the list of known bases to a player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Bases_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Bases_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Paned: Ttk_PanedWindow;
      BasesCanvas: Tk_Canvas;
      BasesFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ComboBox: Ttk_ComboBox;
      ComboValues, BaseValues, BasesType, BasesOwner, BasesStatus,
      BasesName: Unbounded_String;
      BasesView: Ttk_Tree_View;
      SearchEntry: Ttk_Entry;
      FirstIndex: Natural := 0;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      BasesFrame.Interp := Interp;
      BasesFrame.Name := New_String(Widget_Image(Paned) & ".basesframe");
      BasesCanvas.Interp := Interp;
      BasesCanvas.Name := New_String(Widget_Image(BasesFrame) & ".canvas");
      ComboBox.Interp := Interp;
      if Winfo_Get(BasesCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "baseslist.tcl");
         Bind(BasesFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         ComboBox.Name :=
           New_String(Widget_Image(BasesCanvas) & ".bases.options.types");
         Append(ComboValues, " {Any}");
         for BaseType of BasesTypes_List loop
            Append(ComboValues, " {" & BaseType.Name & "}");
         end loop;
         configure(ComboBox, "-values [list" & To_String(ComboValues) & "]");
         Current(ComboBox, "0");
         ComboValues := To_Unbounded_String(" {Any}");
         ComboBox.Name :=
           New_String(Widget_Image(BasesCanvas) & ".bases.options.owner");
         for I in Factions_List.Iterate loop
            Append(ComboValues, " {" & Factions_List(I).Name & "}");
         end loop;
         configure(ComboBox, "-values [list" & To_String(ComboValues) & "]");
         Current(ComboBox, "0");
      elsif Winfo_Get(BasesCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      SearchEntry.Interp := Interp;
      SearchEntry.Name :=
        New_String(Widget_Image(BasesCanvas) & ".bases.options.search");
      if Argc = 1 or else CArgv.Arg(Argv, 1) /= "search" then
         Delete(SearchEntry, "0", "end");
      elsif CArgv.Arg(Argv, 1) = "search" then
         BasesName := To_Unbounded_String(CArgv.Arg(Argv, 2));
      end if;
      BasesView.Interp := Interp;
      BasesView.Name :=
        New_String(Widget_Image(BasesCanvas) & ".bases.list.view");
      Delete(BasesView, "[list " & Children(BasesView, "{}") & "]");
      ComboBox.Name :=
        New_String(Widget_Image(BasesCanvas) & ".bases.options.types");
      BasesType := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name :=
        New_String(Widget_Image(BasesCanvas) & ".bases.options.status");
      BasesStatus := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name :=
        New_String(Widget_Image(BasesCanvas) & ".bases.options.owner");
      BasesOwner := To_Unbounded_String(Get(ComboBox));
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            if BasesName /= Null_Unbounded_String
              and then Index(SkyBases(I).Name, To_String(BasesName), 1) =
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
            if BasesStatus /= To_Unbounded_String("Only not visited") then
               if BasesType /= To_Unbounded_String("Any")
                 and then BasesTypes_List(SkyBases(I).BaseType).Name /=
                   BasesType then
                  goto End_Of_Loop;
               end if;
               if BasesOwner /= To_Unbounded_String("Any")
                 and then Factions_List(SkyBases(I).Owner).Name /=
                   BasesOwner then
                  goto End_Of_Loop;
               end if;
            end if;
            if FirstIndex = 0 then
               FirstIndex := I;
            end if;
            BaseValues := " {" & SkyBases(I).Name & "}";
            Append
              (BaseValues,
               " " &
               Natural'Image
                 (CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)));
            if SkyBases(I).Visited.Year /= 0 then
               if SkyBases(I).Population = 0 then
                  Append(BaseValues, " empty");
               elsif SkyBases(I).Population < 150 then
                  Append(BaseValues, " small");
               elsif SkyBases(I).Population < 300 then
                  Append(BaseValues, " medium");
               else
                  Append(BaseValues, " large");
               end if;
               Append
                 (BaseValues,
                  " {" & To_Lower(Bases_Size'Image(SkyBases(I).Size)) & "}");
               Append
                 (BaseValues,
                  " {" & Factions_List(SkyBases(I).Owner).Name & "}");
               Append
                 (BaseValues,
                  " {" & BasesTypes_List(SkyBases(I).BaseType).Name & "}");
            else
               Append
                 (BaseValues,
                  " {not visited} {not visited} {not visited} {not visited}");
            end if;
            Insert
              (BasesView,
               "{} end -id" & Positive'Image(I) & " -values [list" &
               To_String(BaseValues) & "]");
         end if;
         <<End_Of_Loop>>
      end loop;
      if FirstIndex > 0 then
         Selection_Set(BasesView, "[list" & Natural'Image(FirstIndex) & "]");
      end if;
      BasesFrame.Name := New_String(Widget_Image(BasesCanvas) & ".bases.base");
      if GameSettings.ShowCargoInfo then
         Tcl.Tk.Ada.Grid.Grid(BasesFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(BasesFrame);
      end if;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      BasesFrame.Name := New_String(Widget_Image(BasesCanvas) & ".bases");
      configure
        (BasesCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (BasesCanvas, "window",
         "[expr " & Winfo_Get(BasesFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(BasesFrame, "reqheight") & " / 2] -window " &
         Widget_Image(BasesFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (BasesCanvas, "-scrollregion [list " & BBox(BasesCanvas, "all") & "]");
      ShowScreen("basesframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Bases_Command;

   -- ****f* BasesList/Show_Base_Info_Command
   -- FUNCTION
   -- Show the information about the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Base_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      BaseInfo: Unbounded_String;
      BaseIndex: Positive;
      BasesView: Ttk_Tree_View;
      BaseLabel: Ttk_Label;
      procedure SetReputationText(ReputationText: String) is
         ReputationBar: Ttk_ProgressBar;
         ReputationLabel: Ttk_Label;
      begin
         ReputationBar.Interp := Interp;
         ReputationBar.Name :=
           New_String
             (".paned.basesframe.canvas.bases.base.info.minusreputation");
         if SkyBases(BaseIndex).Reputation(1) < 0 then
            configure
              (ReputationBar,
               "-value" &
               Positive'Image(abs (SkyBases(BaseIndex).Reputation(1))));
            Tcl.Tk.Ada.Grid.Grid(ReputationBar);
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(ReputationBar);
         end if;
         Add(ReputationBar, ReputationText);
         ReputationBar.Name :=
           New_String
             (".paned.basesframe.canvas.bases.base.info.plusreputation");
         if SkyBases(BaseIndex).Reputation(1) > 0 then
            configure
              (ReputationBar,
               "-value" & Positive'Image(SkyBases(BaseIndex).Reputation(1)));
            Tcl.Tk.Ada.Grid.Grid(ReputationBar);
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(ReputationBar);
         end if;
         Add(ReputationBar, ReputationText);
         ReputationLabel.Interp := Interp;
         ReputationLabel.Name :=
           New_String
             (".paned.basesframe.canvas.bases.base.info.reputationlbl");
         if SkyBases(BaseIndex).Reputation(1) = 0 then
            configure(ReputationLabel, "-text {Reputation: Unknown}");
         else
            configure(ReputationLabel, "-text {Reputation:}");
         end if;
         if SkyBases(BaseIndex).Visited.Year > 0 then
            Tcl.Tk.Ada.Grid.Grid(ReputationLabel);
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(ReputationLabel);
            Tcl.Tk.Ada.Grid.Grid_Remove(ReputationBar);
            ReputationBar.Name :=
              New_String
                (".paned.basesframe.canvas.bases.base.info.minusreputation");
            Tcl.Tk.Ada.Grid.Grid_Remove(ReputationLabel);
         end if;
      end SetReputationText;
   begin
      BasesView.Interp := Interp;
      BasesView.Name := New_String(".paned.basesframe.canvas.bases.list.view");
      BaseLabel.Interp := Interp;
      BaseLabel.Name :=
        New_String(".paned.basesframe.canvas.bases.base.info.text");
      if not GameSettings.ShowBaseInfo or Selection(BasesView)'Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(BaseLabel);
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Grid.Grid(BaseLabel);
      BaseIndex := Positive'Value(Selection(BasesView));
      if SkyBases(BaseIndex).Visited.Year > 0 then
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
               TimeDiff :=
                 30 - DaysDifference(SkyBases(BaseIndex).RecruitDate);
               if TimeDiff > 0 then
                  Append
                    (BaseInfo,
                     LF & "New recruits available in" &
                     Natural'Image(TimeDiff) & " days.");
               else
                  Append(BaseInfo, LF & "New recruits available now.");
               end if;
            else
               Append
                 (BaseInfo,
                  LF & "You can't recruit crew members at this base.");
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
               TimeDiff :=
                 7 - DaysDifference(SkyBases(BaseIndex).MissionsDate);
               if TimeDiff > 0 then
                  Append
                    (BaseInfo,
                     LF & "New missions available in" &
                     Natural'Image(TimeDiff) & " days.");
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
      else
         BaseInfo := To_Unbounded_String("Not visited yet.");
         SetReputationText("Unknown");
      end if;
      configure(BaseLabel, "-text {" & To_String(BaseInfo) & "}");
      return TCL_OK;
   end Show_Base_Info_Command;

   -- ****if* BasesList/Show_Base_Command
   -- FUNCTION
   -- Show the selected base on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      BasesView: Ttk_Tree_View;
      BaseIndex: Positive;
   begin
      BasesView.Interp := Interp;
      BasesView.Name := New_String(".paned.basesframe.canvas.bases.list.view");
      BaseIndex := Positive'Value(Selection(BasesView));
      CenterX := SkyBases(BaseIndex).SkyX;
      CenterY := SkyBases(BaseIndex).SkyY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Base_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBases", Show_Bases_Command'Access);
      AddCommand("ShowBaseInfo", Show_Base_Info_Command'Access);
      AddCommand("ShowBase", Show_Base_Command'Access);
   end AddCommands;

end BasesList;
