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
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Factions; use Factions;
with Maps; use Maps;
with Utils.UI; use Utils.UI;

package body Knowledge.Bases is

   procedure UpdateBasesList(BaseName: String := "") is
      BasesCanvas: constant Tk_Canvas :=
        Get_Widget(".paned.knowledgeframe.bases.canvas");
      BasesFrame: constant Ttk_Frame := Get_Widget(BasesCanvas & ".frame");
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(BasesFrame & ".options.search");
      Item: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ComboBox: Ttk_Combobox := Get_Widget(BasesFrame & ".options.types");
      BasesType, BasesOwner, BasesStatus: Unbounded_String;
      BaseButton: Ttk_MenuButton;
      Row: Positive := 3;
      BaseLabel: Ttk_Label;
      BaseMenu: Tk_Menu;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(BasesFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 3 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (BasesFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      if BaseName'Length = 0 then
         Delete(SearchEntry, "0", "end");
      end if;
      BasesType := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.status");
      BasesStatus := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(BasesFrame & ".options.owner");
      BasesOwner := To_Unbounded_String(Get(ComboBox));
      BaseMenu.Interp := Get_Context;
      for I in SkyBases'Range loop
         BaseMenu.Name :=
           New_String(".baselistmenu" & Trim(Positive'Image(I), Left));
         if Winfo_Get(BaseMenu, "exists") = "0" then
            BaseMenu :=
              Create
                (".baselistmenu" & Trim(Positive'Image(I), Left),
                 "-tearoff false");
              Menu.Add
                 (BaseMenu, "command",
                 "-label {Show the base on map} -command {ShowBase " &
                 Positive'Image(I) & "}");
              Menu.Add
                 (BaseMenu, "command",
                 "-label {Set the base as destination for the ship} -command {SetBaseDestination " &
                 Positive'Image(I) & "}");
              Menu.Add
                 (BaseMenu, "command",
                 "-label {Show more information about the base} -command {ShowBaseInfo " &
                 Positive'Image(I) & "}");
         end if;
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
            BaseButton :=
              Create
                (BasesFrame & ".name" & Trim(Positive'Image(I), Left),
                 "-text {" & To_String(SkyBases(I).Name) & "} -menu {" &
                 BaseMenu & "}");
            Add(BaseButton, "Show available base's options");
            Tcl.Tk.Ada.Grid.Grid
              (BaseButton, "-row" & Natural'Image(Row) & " -sticky w");
            BaseLabel :=
              Create
                (BasesFrame & ".distance" & Trim(Positive'Image(I), Left),
                 "-text {" &
                 Natural'Image
                   (CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)) &
                 "}");
            Tcl.Tk.Ada.Grid.Grid
              (BaseLabel, "-row" & Natural'Image(Row) & " -column 1");
            if SkyBases(I).Visited.Year > 0 then
               BaseLabel :=
                 Create
                   (BasesFrame & ".population" &
                    Trim(Positive'Image(I), Left));
               if SkyBases(I).Population = 0 then
                  configure(BaseLabel, "-text {empty}");
               elsif SkyBases(I).Population < 150 then
                  configure(BaseLabel, "-text {small}");
               elsif SkyBases(I).Population < 300 then
                  configure(BaseLabel, "-text {medium}");
               else
                  configure(BaseLabel, "-text {large}");
               end if;
               Tcl.Tk.Ada.Grid.Grid
                 (BaseLabel, "-row" & Natural'Image(Row) & " -column 2");
               BaseLabel :=
                 Create
                   (BasesFrame & ".size" & Trim(Positive'Image(I), Left),
                    "-text {" & To_Lower(Bases_Size'Image(SkyBases(I).Size)) &
                    "}");
               Tcl.Tk.Ada.Grid.Grid
                 (BaseLabel, "-row" & Natural'Image(Row) & " -column 3");
               BaseLabel :=
                 Create
                   (BasesFrame & ".owner" & Trim(Positive'Image(I), Left),
                    "-text {" &
                    To_String(Factions_List(SkyBases(I).Owner).Name) & "}");
               Tcl.Tk.Ada.Grid.Grid
                 (BaseLabel, "-row" & Natural'Image(Row) & " -column 4");
               BaseLabel :=
                 Create
                   (BasesFrame & ".type" & Trim(Positive'Image(I), Left),
                    "-text {" &
                    To_String(BasesTypes_List(SkyBases(I).BaseType).Name) &
                    "}");
               Tcl.Tk.Ada.Grid.Grid
                 (BaseLabel, "-row" & Natural'Image(Row) & " -column 5");
            else
               BaseLabel :=
                 Create
                   (BasesFrame & ".population" & Trim(Positive'Image(I), Left),
                    "-text {not visited yet}");
               Tcl.Tk.Ada.Grid.Grid
                 (BaseLabel,
                  "-row" & Natural'Image(Row) & " -column 2 -columnspan 5");
            end if;
            Row := Row + 1;
         end if;
         <<End_Of_Loop>>
      end loop;
      Tcl_Eval(Get_Context, "update");
      configure
        (BasesCanvas, "-scrollregion [list " & BBox(BasesCanvas, "all") & "]");
      Xview_Move_To(BasesCanvas, "0.0");
      Yview_Move_To(BasesCanvas, "0.0");
   end UpdateBasesList;

   -- ****o* KBases/Show_Bases_Command
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
   begin
      if Argc > 1 then
         UpdateBasesList(CArgv.Arg(Argv, 1));
      else
         UpdateBasesList;
      end if;
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Bases_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBases", Show_Bases_Command'Access);
   end AddCommands;

end Knowledge.Bases;
