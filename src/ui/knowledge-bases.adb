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
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Factions; use Factions;
with Maps; use Maps;

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
      ComboBox: Ttk_Combobox :=
        Get_Widget(BasesFrame & ".options.types");
      BasesType, BasesOwner, BasesStatus: Unbounded_String;
      BaseButton: Ttk_MenuButton;
      Row: Positive := 3;
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
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            if BaseName'Length > 0
              and then Index(SkyBases(I).Name, BaseName, 1) =
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
         BaseButton :=
           Create
             (BasesFrame & ".name" &
              Trim(Positive'Image(I), Left),
              "-text {" & To_String(SkyBases(I).Name) & "}");
         Add(BaseButton, "Show available base's options");
         Tcl.Tk.Ada.Grid.Grid
           (BaseButton, "-row" & Natural'Image(Row) & " -sticky w");
--            Append
--              (BaseValues,
--               " " &
--               Natural'Image
--                 (CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)));
--            if SkyBases(I).Visited.Year /= 0 then
--               if SkyBases(I).Population = 0 then
--                  Append(BaseValues, " empty");
--               elsif SkyBases(I).Population < 150 then
--                  Append(BaseValues, " small");
--               elsif SkyBases(I).Population < 300 then
--                  Append(BaseValues, " medium");
--               else
--                  Append(BaseValues, " large");
--               end if;
--               Append
--                 (BaseValues,
--                  " {" & To_Lower(Bases_Size'Image(SkyBases(I).Size)) & "}");
--               Append
--                 (BaseValues,
--                  " {" & Factions_List(SkyBases(I).Owner).Name & "}");
--               Append
--                 (BaseValues,
--                  " {" & BasesTypes_List(SkyBases(I).BaseType).Name & "}");
--            else
--               Append
--                 (BaseValues,
--                  " {not visited} {not visited} {not visited} {not visited}");
--            end if;
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

   procedure AddCommands is
   begin
      null;
   end AddCommands;

end Knowledge.Bases;
