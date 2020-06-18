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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Crew; use Crew;
with Events; use Events;
with Items; use Items;
with Maps; use Maps;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   -- ****if* CUI/UpdateCombatUI
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure UpdateCombatUI is
      -- ****
      Tokens: Slice_Set;
      Frame, Item: Ttk_Frame;
      Rows: Positive;
      Label: Ttk_Label;
      CrewList: Unbounded_String := To_Unbounded_String("Nobody");
      ComboBox: Ttk_ComboBox;
      GunnerOrders: constant String :=
        "{Don't shoot} {Precise fire} {Fire at will} {Aim for their engine} {Aim for their weapon} {Aim for their hull}";
      GunIndex: Unbounded_String;
      HaveAmmo: Boolean;
      AmmoAmount, AmmoIndex: Natural := 0;
   begin
      Frame.Interp := Get_Context;
      Item.Interp := Get_Context;
      Frame.Name := New_String(".paned.combatframe.canvas.combat.left.crew");
      for Member of PlayerShip.Crew loop
         Append(CrewList, " {" & Member.Name & "}");
      end loop;
      ComboBox.Interp := Get_Context;
      ComboBox.Name := New_String(Widget_Image(Frame) & ".pilotcrew");
      configure(ComboBox, "-values [list " & To_String(CrewList) & "]");
      Current(ComboBox, Natural'Image(FindMember(Pilot)));
      ComboBox.Name := New_String(Widget_Image(Frame) & ".pilotorder");
      Current(ComboBox, Integer'Image(PilotOrder - 1));
      ComboBox.Name := New_String(Widget_Image(Frame) & ".engineercrew");
      configure(ComboBox, "-values [list " & To_String(CrewList) & "]");
      Current(ComboBox, Natural'Image(FindMember(Engineer)));
      ComboBox.Name := New_String(Widget_Image(Frame) & ".engineerorder");
      Current(ComboBox, Natural'Image(EngineerOrder - 1));
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Positive'Value(Slice(Tokens, 2));
      for I in 3 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Name := New_String(Slice(Tokens, J));
            Tcl.Tk.Ada.Grid.Grid_Remove(Item);
         end loop;
      end loop;
      for I in Guns.Iterate loop
         HaveAmmo := False;
         declare
            AmmoIndex: Natural;
         begin
            if PlayerShip.Modules(Guns(I)(1)).MType = GUN then
               AmmoIndex := PlayerShip.Modules(Guns(I)(1)).AmmoIndex;
            else
               AmmoIndex := PlayerShip.Modules(Guns(I)(1)).HarpoonIndex;
            end if;
            if
              (AmmoIndex in
                 PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index)
              and then
                Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex).IType =
                Items_Types
                  (Modules_List(PlayerShip.Modules(Guns(I)(1)).ProtoIndex)
                     .Value) then
               AmmoAmount := PlayerShip.Cargo(AmmoIndex).Amount;
               HaveAmmo := True;
            end if;
         end;
         if not HaveAmmo then
            AmmoAmount := 0;
            for J in Items_List.Iterate loop
               if Items_List(J).IType =
                 Items_Types
                   (Modules_List(PlayerShip.Modules(Guns(I)(1)).ProtoIndex)
                      .Value) then
                  AmmoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(J));
                  if AmmoIndex > 0 then
                     AmmoAmount :=
                       AmmoAmount + PlayerShip.Cargo(AmmoIndex).Amount;
                  end if;
               end if;
            end loop;
         end if;
         GunIndex :=
           To_Unbounded_String
             (Trim(Positive'Image(Guns_Container.To_Index(I)), Left));
         Label :=
           Create
             (Widget_Image(Frame) & ".gunlabel" & To_String(GunIndex),
              "-text {" & To_String(PlayerShip.Modules(Guns(I)(1)).Name) &
              ":" & LF & "(Ammo:" & Natural'Image(AmmoAmount) & ")}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Guns_Container.To_Index(I) + 2));
         ComboBox :=
           Create
             (Widget_Image(Frame) & ".guncrew" & To_String(GunIndex),
              "-values [list " & To_String(CrewList) & "]");
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
            " -column 1");
         ComboBox :=
           Create
             (Widget_Image(Frame) & ".gunorders" & To_String(GunIndex),
              "-values [list " & GunnerOrders & "]");
         Current(ComboBox, Natural'Image(Guns(I)(2) - 1));
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
            " -column 2");
      end loop;
   end UpdateCombatUI;

   procedure ShowCombatUI(NewCombat: Boolean := True) is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CombatCanvas: Tk_Canvas;
      CombatFrame: Ttk_Frame;
      CombatStarted: Boolean;
   begin
      if NewCombat then
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0
           and then EnemyName /=
             ProtoShips_List
               (Events_List
                  (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                  .ShipIndex)
               .Name then
            CombatStarted :=
              StartCombat
                (Events_List
                   (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                   .ShipIndex,
                 False);
            if not CombatStarted then
               return;
            end if;
         end if;
         Paned.Interp := Get_Context;
         Paned.Name := New_String(".paned");
         CombatFrame.Interp := Get_Context;
         CombatFrame.Name := New_String(Widget_Image(Paned) & ".combatframe");
         CombatCanvas.Interp := Get_Context;
         CombatCanvas.Name :=
           New_String(Widget_Image(CombatFrame) & ".canvas");
         Label.Interp := Get_Context;
         Label.Name :=
           New_String
             (Widget_Image(CombatCanvas) & ".combat.right.enemy.description");
         if Winfo_Get(Label, "exists") = "0" then
            Tcl_EvalFile
              (Get_Context,
               To_String(DataDirectory) & "ui" & Dir_Separator & "combat.tcl");
            Bind(CombatFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
            PilotOrder := 2;
            EngineerOrder := 3;
         end if;
         configure(Label, "-text {" & To_String(Enemy.Ship.Description) & "}");
         for Member of PlayerShip.Crew loop
            if Member.Order = Rest
              and then
              (Member.PreviousOrder = Pilot or
               Member.PreviousOrder = Engineer or
               Member.PreviousOrder = Gunner) then
               Member.Order := Member.PreviousOrder;
               Member.OrderTime := 15;
               AddMessage
                 (To_String(Member.Name) & " back to work for combat.",
                  OrderMessage);
            end if;
         end loop;
      end if;
      UpdateCombatUI;
      configure
        (CombatCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      CombatFrame.Name := New_String(Widget_Image(CombatCanvas) & ".combat");
      Canvas_Create
        (CombatCanvas, "window",
         "[expr " & Winfo_Get(CombatFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CombatFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CombatFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      ShowScreen("combatframe");
   end ShowCombatUI;

end Combat.UI;
