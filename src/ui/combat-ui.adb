-- Copyright (c) 2020-2023 Bartek thindil Jasicki
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events;
with Factions; use Factions;
with Items; use Items;
with Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   -- ****if* CUI/CUI.Update_Messages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure Update_Messages with
      Import => True,
      Convention => C,
      External_Name => "updateCombatAdaMessages";
      -- ****

   -- ****if* CUI/CUI.Update_Combat_Ui
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure Update_Combat_Ui is
      -- ****
      use Bases;
      use Short_String;
      use Tiny_String;

      Tokens: Slice_Set;
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe.crew.canvas.frame");
      Combo_Box: Ttk_ComboBox := Get_Widget(pathName => Frame & ".pilotcrew");
      Gunners_Orders: constant array(1 .. 6) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "{Don't shoot"),
         2 => To_Unbounded_String(Source => "{Precise fire "),
         3 => To_Unbounded_String(Source => "{Fire at will "),
         4 => To_Unbounded_String(Source => "{Aim for their engine "),
         5 => To_Unbounded_String(Source => "{Aim for their weapon "),
         6 => To_Unbounded_String(Source => "{Aim for their hull "));
      Gun_Index, Gunner_Orders, Enemy_Info: Unbounded_String :=
        Null_Unbounded_String;
      Have_Ammo: Boolean := True;
      Ammo_Amount, Ammo_Index, Row: Natural := 0;
      Rows: Natural;
      Damage_Percent: Float := 0.0;
      --## rule off IMPROPER_INITIALIZATION
      Label: Ttk_Label;
      Progress_Bar: Ttk_ProgressBar;
      Combat_Canvas: Tk_Canvas;
      --## rule on IMPROPER_INITIALIZATION
      Has_Gunner: Boolean := False;
      Faction: constant Faction_Record :=
        Get_Faction(Index => Player_Ship.Crew(1).Faction);
      function Get_Crew_List(Position: Natural) return String is
         Crew_List: Unbounded_String :=
           To_Unbounded_String(Source => "Nobody");
      begin
         Mark_Skills_Loop :
         for I in
           Player_Ship.Crew.First_Index .. Player_Ship.Crew.Last_Index loop
            if Skills_Container.Length
                (Container => Player_Ship.Crew(I).Skills) >
              0 then
               Append
                 (Source => Crew_List,
                  New_Item =>
                    " {" & To_String(Source => Player_Ship.Crew(I).Name) &
                    Get_Skill_Marks
                      (Skill_Index =>
                         (if Position = 0 then Piloting_Skill
                          elsif Position = 1 then Engineering_Skill
                          else Gunnery_Skill),
                       Member_Index => I) &
                    "}");
            end if;
         end loop Mark_Skills_Loop;
         return To_String(Source => Crew_List);
      end Get_Crew_List;
      function Get_Gun_Speed
        (Position: Natural; Index: Positive) return String is
         Gun_Speed: Integer;
         Firerate: Unbounded_String := Null_Unbounded_String;
      begin
         Gun_Speed :=
           Get_Module
             (Index => Player_Ship.Modules(Guns(Position)(1)).Proto_Index)
             .Speed;
         case Index is
            when 1 =>
               Gun_Speed := 0;
            when 3 =>
               null;
            when others =>
               Gun_Speed :=
                 (if Gun_Speed > 0 then
                    Integer(Float'Ceiling(Float(Gun_Speed) / 2.0))
                  else Gun_Speed - 1);
         end case;
         --## rule off SIMPLIFIABLE_STATEMENTS
         if Gun_Speed > 0 then
            Firerate :=
              To_Unbounded_String
                (Source =>
                   "(" &
                   Trim(Source => Integer'Image(Gun_Speed), Side => Both) &
                   "/round)");
         elsif Gun_Speed < 0 then
            Firerate :=
              To_Unbounded_String
                (Source =>
                   "(1/" &
                   Trim(Source => Integer'Image(Gun_Speed), Side => Both) &
                   " rounds)");
         end if;
         --## rule on SIMPLIFIABLE_STATEMENTS
         return To_String(Source => Firerate);
      end Get_Gun_Speed;
   begin
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 1) & ">",
         Script => "{InvokeButton " & Frame & ".maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 3) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.damage.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 2) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.enemy.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 4) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.status.canvas.frame.maxmin}");
      configure
        (Widgt => Combo_Box,
         options => "-values [list " & Get_Crew_List(Position => 0) & "]");
      Current
        (ComboBox => Combo_Box,
         NewIndex => Natural'Image(Find_Member(Order => PILOT)));
      Combo_Box.Name := New_String(Str => Frame & ".pilotorder");
      Current
        (ComboBox => Combo_Box,
         NewIndex => Integer'Image(Get_Pilot_Order - 1));
      if not Faction.Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) and
        Find_Member(Order => PILOT) = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Combo_Box);
      else
         Tcl.Tk.Ada.Grid.Grid(Slave => Combo_Box);
      end if;
      Combo_Box.Name := New_String(Str => Frame & ".engineercrew");
      configure
        (Widgt => Combo_Box,
         options => "-values [list " & Get_Crew_List(Position => 1) & "]");
      Current
        (ComboBox => Combo_Box,
         NewIndex => Natural'Image(Find_Member(Order => ENGINEER)));
      Combo_Box.Name := New_String(Str => Frame & ".engineerorder");
      Current
        (ComboBox => Combo_Box,
         NewIndex => Natural'Image(Get_Engineer_Order - 1));
      if not Faction.Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) and
        Find_Member(Order => ENGINEER) = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Combo_Box);
      else
         Tcl.Tk.Ada.Grid.Grid(Slave => Combo_Box);
      end if;
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Positive'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 4, End_Index => Rows - 1, Frame => Frame);
      Show_Guns_Info_Loop :
      for I in Guns.Iterate loop
         Have_Ammo := False;
         Has_Gunner := False;
         Check_Ammo_Block :
         declare
            A_Index: constant Natural :=
              (if Player_Ship.Modules(Guns(I)(1)).M_Type = GUN then
                 Player_Ship.Modules(Guns(I)(1)).Ammo_Index
               else Player_Ship.Modules(Guns(I)(1)).Harpoon_Index);
         begin
            if A_Index in
                Inventory_Container.First_Index
                      (Container => Player_Ship.Cargo) ..
                      Inventory_Container.Last_Index
                        (Container => Player_Ship.Cargo)
              and then
                Get_Proto_Item
                  (Index =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo, Index => A_Index)
                       .Proto_Index)
                  .I_Type =
                Get_Ada_Item_Type
                  (Item_Index =>
                     Get_Module
                       (Index => Player_Ship.Modules(Guns(I)(1)).Proto_Index)
                       .Value -
                     1) then
               Ammo_Amount :=
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => A_Index)
                   .Amount;
               Have_Ammo := True;
            end if;
         end Check_Ammo_Block;
         if not Have_Ammo then
            Ammo_Amount := 0;
            Find_Ammo_Loop :
            for J in 1 .. Get_Proto_Amount loop
               if Get_Proto_Item(Index => J).I_Type =
                 Get_Ada_Item_Type
                   (Item_Index =>
                      Get_Module
                        (Index => Player_Ship.Modules(Guns(I)(1)).Proto_Index)
                        .Value -
                      1) then
                  Ammo_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo, Proto_Index => J);
                  if Ammo_Index > 0 then
                     Ammo_Amount :=
                       Ammo_Amount +
                       Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => Ammo_Index)
                         .Amount;
                  end if;
               end if;
            end loop Find_Ammo_Loop;
         end if;
         Gun_Index :=
           To_Unbounded_String
             (Source =>
                Trim
                  (Source =>
                     Positive'Image(Guns_Container.To_Index(Position => I)),
                   Side => Left));
         Label :=
           Create
             (pathName => Frame & ".gunlabel" & To_String(Source => Gun_Index),
              options =>
                "-text {" &
                To_String(Source => Player_Ship.Modules(Guns(I)(1)).Name) &
                ":" & LF & "(Ammo:" & Natural'Image(Ammo_Amount) & ")}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options =>
              "-row" &
              Positive'Image(Guns_Container.To_Index(Position => I) + 3) &
              " -padx {5 0}");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Frame & ".gunlabel" &
              To_String(Source => Gun_Index) & " $combatframe.crew.scrolly");
         Combo_Box :=
           Create
             (pathName => Frame & ".guncrew" & To_String(Source => Gun_Index),
              options =>
                "-values [list " & Get_Crew_List(Position => 2) &
                "] -width 10 -state readonly");
         if Player_Ship.Modules(Guns(I)(1)).Owner(1) = 0 then
            Current(ComboBox => Combo_Box, NewIndex => "0");
         else
            if Player_Ship.Crew(Player_Ship.Modules(Guns(I)(1)).Owner(1))
                .Order =
              GUNNER then
               Current
                 (ComboBox => Combo_Box,
                  NewIndex =>
                    Positive'Image(Player_Ship.Modules(Guns(I)(1)).Owner(1)));
               Has_Gunner := True;
            else
               Current(ComboBox => Combo_Box, NewIndex => "0");
            end if;
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Combo_Box,
            Options =>
              "-row" &
              Positive'Image(Guns_Container.To_Index(Position => I) + 3) &
              " -column 1");
         Bind
           (Widgt => Combo_Box, Sequence => "<Return>",
            Script => "{InvokeButton " & Main_Paned & ".combatframe.next}");
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script =>
              "{SetCombatPosition gunner " & To_String(Source => Gun_Index) &
              "}");
         Add
           (Widget => Combo_Box,
            Message =>
              "Select the crew member which will be the operate the gun during" &
              LF &
              "the combat. The sign + after name means that this crew member" &
              LF &
              "has gunnery skill, the sign ++ after name means that his/her" &
              LF & "gunnery skill is the best in the crew");
         Gunner_Orders := Null_Unbounded_String;
         Show_Gun_Orders_Loop :
         for J in Gunners_Orders'Range loop
            Append
              (Source => Gunner_Orders,
               New_Item =>
                 " " & Gunners_Orders(J) &
                 Get_Gun_Speed
                   (Position => Guns_Container.To_Index(Position => I),
                    Index => J) &
                 "}");
         end loop Show_Gun_Orders_Loop;
         Combo_Box :=
           Get_Widget
             (pathName =>
                Frame & ".gunorder" & To_String(Source => Gun_Index));
         if Winfo_Get(Widgt => Combo_Box, Info => "exists") = "0" then
            Combo_Box :=
              Create
                (pathName =>
                   Frame & ".gunorder" & To_String(Source => Gun_Index),
                 options =>
                   "-values [list" & To_String(Source => Gunner_Orders) &
                   "] -state readonly");
         end if;
         Current
           (ComboBox => Combo_Box, NewIndex => Natural'Image(Guns(I)(2) - 1));
         if Has_Gunner then
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Combo_Box,
               Options =>
                 "-row" &
                 Positive'Image(Guns_Container.To_Index(Position => I) + 3) &
                 " -column 2 -padx {0 5}");
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Combo_Box);
         end if;
         Bind
           (Widgt => Combo_Box, Sequence => "<Return>",
            Script => "{InvokeButton " & Main_Paned & ".combatframe.next}");
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script =>
              "{SetCombatOrder " & To_String(Source => Gun_Index) & "}");
         Add
           (Widget => Combo_Box,
            Message =>
              "Select the order for the gunner. Shooting in the selected" &
              LF & "part of enemy ship is less precise but always hit the" &
              LF & "selected part.");
      end loop Show_Guns_Info_Loop;
      -- Show boarding/defending info
      if (Harpoon_Duration > 0 or Enemy.Harpoon_Duration > 0) and
        Get_Proto_Ship(Proto_Index => Enemy_Ship_Index).Crew.Length > 0 then
         Show_Boarding_Info_Block :
         declare
            Button: Ttk_Button :=
              Create
                (pathName => Frame & ".boarding",
                 options =>
                   "-text {Boarding party:} -command {SetCombatParty boarding}");
            Boarding_Party, Defenders: Unbounded_String :=
              Null_Unbounded_String;
            Label_Length: constant Positive :=
              Positive'Value
                (Winfo_Get
                   (Widgt =>
                      Ttk_Label'
                        (Get_Widget(pathName => Frame & ".engineercrew")),
                    Info => "reqwidth")) +
              Positive'Value
                (Winfo_Get
                   (Widgt =>
                      Ttk_Label'
                        (Get_Widget(pathName => Frame & ".engineerorder")),
                    Info => "reqwidth"));
         begin
            Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx 5");
            Add
              (Widget => Button,
               Message =>
                 "Set your boarding party. If you join it, you will be able" &
                 LF &
                 "to give orders them, but not your gunners or engineer.");
            Button :=
              Create
                (pathName => Frame & ".defending",
                 options =>
                   "-text {Defenders:} -command {SetCombatParty defenders}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-sticky we -padx 5 -pady 5");
            Add
              (Widget => Button,
               Message =>
                 "Set your ship's defenders against the enemy party.");
            Set_Boarding_And_Defenders_Loop :
            for Member of Player_Ship.Crew loop
               case Member.Order is
                  when BOARDING =>
                     Append
                       (Source => Boarding_Party,
                        New_Item => To_String(Source => Member.Name) & ", ");
                  when DEFEND =>
                     Append
                       (Source => Defenders,
                        New_Item => To_String(Source => Member.Name) & ", ");
                  when others =>
                     null;
               end case;
            end loop Set_Boarding_And_Defenders_Loop;
            if Boarding_Party /= Null_Unbounded_String then
               Boarding_Party :=
                 Unbounded_Slice
                   (Source => Boarding_Party, Low => 1,
                    High => Length(Source => Boarding_Party) - 2);
            end if;
            Label := Get_Widget(pathName => Frame & ".boardparty");
            if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
               Label :=
                 Create
                   (pathName => Frame & ".boardparty",
                    options =>
                      "-text {" & To_String(Source => Boarding_Party) &
                      "} -wraplength" & Positive'Image(Label_Length));
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Label,
                  Options =>
                    "-row" & Positive'Image(Natural(Guns.Length) + 4) &
                    " -column 1 -columnspan 2 -sticky w");
               Tcl_Eval
                 (interp => Get_Context,
                  strng =>
                    "SetScrollbarBindings " & Label &
                    " $combatframe.crew.scrolly");
            else
               configure
                 (Widgt => Label,
                  options =>
                    "-text {" & To_String(Source => Boarding_Party) & "}");
            end if;
            if Defenders /= Null_Unbounded_String then
               Defenders :=
                 Unbounded_Slice
                   (Source => Defenders, Low => 1,
                    High => Length(Source => Defenders) - 2);
            end if;
            Label := Get_Widget(pathName => Frame & ".defenders");
            if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
               Label :=
                 Create
                   (pathName => Frame & ".defenders",
                    options =>
                      "-text {" & To_String(Source => Defenders) &
                      "} -wraplength" & Positive'Image(Label_Length));
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Label,
                  Options =>
                    "-row" & Positive'Image(Natural(Guns.Length) + 5) &
                    " -column 1 -columnspan 2 -sticky w");
               Tcl_Eval
                 (interp => Get_Context,
                  strng =>
                    "SetScrollbarBindings " & Label &
                    " $combatframe.crew.scrolly");
            else
               configure
                 (Widgt => Label,
                  options => "-text {" & To_String(Source => Defenders) & "}");
            end if;
         end Show_Boarding_Info_Block;
      end if;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas :=
        Get_Widget(pathName => Main_Paned & ".combatframe.crew.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      -- Show player ship damage info if needed
      Frame.Name :=
        New_String(Str => Main_Paned & ".combatframe.damage.canvas.frame");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 0, End_Index => Rows - 1, Frame => Frame);
      Row := 1;
      Add_Minimize_Button_Block :
      declare
         Button: constant Ttk_Button :=
           Create
             (pathName => Frame & ".maxmin",
              options =>
                "-style Small.TButton -image movemapupicon -command {CombatMaxMin damage show combat}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-sticky w -padx 5 -row 0 -column 0");
         Add
           (Widget => Button,
            Message => "Maximize/minimize the ship status info");
      end Add_Minimize_Button_Block;
      Show_Player_Ship_Damage_Loop :
      for Module of Player_Ship.Modules loop
         Label :=
           Create
             (pathName =>
                Frame & ".lbl" &
                Trim(Source => Natural'Image(Row), Side => Left),
              options =>
                "-text {" & To_String(Source => Module.Name) & "}" &
                (if Module.Durability = 0 then
                   " -font OverstrikedFont -style Gray.TLabel"
                 else ""));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options => "-row" & Natural'Image(Row) & " -sticky w -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Label &
              " $combatframe.damage.scrolly");
         Damage_Percent :=
           Float(Module.Durability) / Float(Module.Max_Durability);
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".dmg" &
                Trim(Source => Natural'Image(Row), Side => Left),
              options =>
                "-orient horizontal -length 150 -maximum 1.0 -value" &
                Float'Image(Damage_Percent) &
                (if Damage_Percent = 1.0 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Damage_Percent > 0.24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options => "-row" & Natural'Image(Row) & " -column 1");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.damage.scrolly");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Frame, Slave => Progress_Bar, Options => "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (Master => Frame, Slave => Progress_Bar, Options => "-weight 1");
         Row := Row + 1;
      end loop Show_Player_Ship_Damage_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas :=
        Get_Widget(pathName => Main_Paned & ".combatframe.damage.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Append
        (Source => Enemy_Info,
         New_Item =>
           "Name: " & To_String(Source => Get_Enemy_Name) & LF & "Type: " &
           To_String(Source => Enemy.Ship.Name) & LF & "Home: " &
           To_String(Source => Sky_Bases(Enemy.Ship.Home_Base).Name) & LF &
           "Distance: " &
           (if Enemy.Distance >= 15_000 then "Escaped"
            elsif Enemy.Distance in 10_000 .. 15_000 then "Long"
            elsif Enemy.Distance in 5_000 .. 10_000 then "Medium"
            elsif Enemy.Distance in 1_000 .. 5_000 then "Short" else "Close") &
           LF & "Status: ");
      if Enemy.Distance < 15_000 then
         if Enemy.Ship.Modules(1).Durability = 0 then
            Append(Source => Enemy_Info, New_Item => "Destroyed");
         else
            Show_Enemy_Status_Block :
            declare
               Enemy_Status: Unbounded_String :=
                 To_Unbounded_String(Source => "Ok");
            begin
               Check_Enemy_Ship_Status_Loop :
               for Module of Enemy.Ship.Modules loop
                  if Module.Durability < Module.Max_Durability then
                     Enemy_Status := To_Unbounded_String(Source => "Damaged");
                     exit Check_Enemy_Ship_Status_Loop;
                  end if;
               end loop Check_Enemy_Ship_Status_Loop;
               Append(Source => Enemy_Info, New_Item => Enemy_Status);
            end Show_Enemy_Status_Block;
            Check_Enemy_Status_Loop :
            for Module of Enemy.Ship.Modules loop
               if Module.Durability > 0 then
                  case Get_Module(Index => Module.Proto_Index).M_Type is
                     when ARMOR =>
                        Append(Source => Enemy_Info, New_Item => " (armored)");
                     when GUN =>
                        Append(Source => Enemy_Info, New_Item => " (gun)");
                     when BATTERING_RAM =>
                        Append
                          (Source => Enemy_Info,
                           New_Item => " (battering ram)");
                     when HARPOON_GUN =>
                        Append
                          (Source => Enemy_Info, New_Item => " (harpoon gun)");
                     when others =>
                        null;
                  end case;
               end if;
            end loop Check_Enemy_Status_Loop;
         end if;
      else
         Append(Source => Enemy_Info, New_Item => "Unknown");
      end if;
      Append(Source => Enemy_Info, New_Item => LF & "Speed: ");
      if Enemy.Distance < 15_000 then
         case Enemy.Ship.Speed is
            when Ships.FULL_STOP =>
               Append(Source => Enemy_Info, New_Item => "Stopped");
            when QUARTER_SPEED =>
               Append(Source => Enemy_Info, New_Item => "Slow");
            when HALF_SPEED =>
               Append(Source => Enemy_Info, New_Item => "Medium");
            when FULL_SPEED =>
               Append(Source => Enemy_Info, New_Item => "Fast");
            when others =>
               null;
         end case;
         if Enemy.Ship.Speed /= Ships.FULL_STOP then
            Show_Enemy_Ship_Speed_Block :
            declare
               use Ships.Movement;

               Speed_Diff: constant Integer :=
                 Real_Speed(Ship => Enemy.Ship) -
                 Real_Speed(Ship => Player_Ship);
            begin
               --## rule off SIMPLIFIABLE_STATEMENTS
               if Speed_Diff > 250 then
                  Append(Source => Enemy_Info, New_Item => " (much faster)");
               elsif Speed_Diff > 0 then
                  Append(Source => Enemy_Info, New_Item => " (faster)");
               elsif Speed_Diff = 0 then
                  Append(Source => Enemy_Info, New_Item => " (equal)");
               elsif Speed_Diff > -250 then
                  Append(Source => Enemy_Info, New_Item => " (slower)");
               else
                  Append(Source => Enemy_Info, New_Item => " (much slower)");
               end if;
               --## rule on SIMPLIFIABLE_STATEMENTS
            end Show_Enemy_Ship_Speed_Block;
         end if;
      else
         Append(Source => Enemy_Info, New_Item => "Unknown");
      end if;
      if Length(Source => Enemy.Ship.Description) > 0 then
         Append
           (Source => Enemy_Info,
            New_Item => LF & LF & To_String(Source => Enemy.Ship.Description));
      end if;
      Label :=
        Get_Widget
          (pathName => Main_Paned & ".combatframe.enemy.canvas.frame.info");
      configure
        (Widgt => Label,
         options => "-text {" & To_String(Source => Enemy_Info) & "}");
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas :=
        Get_Widget(pathName => Main_Paned & ".combatframe.enemy.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Frame.Name :=
        New_String(Str => Main_Paned & ".combatframe.status.canvas.frame");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Frame);
      Row := 1;
      if Get_End_Combat then
         Enemy.Distance := 100;
      end if;
      Show_Enemy_Ship_Status_Loop :
      for I in Enemy.Ship.Modules.Iterate loop
         if Get_End_Combat then
            Enemy.Ship.Modules(I).Durability := 0;
         end if;
         Label :=
           Create
             (pathName =>
                Frame & ".lbl" &
                Trim(Source => Natural'Image(Row), Side => Left),
              options =>
                "-text {" &
                To_String
                  (Source =>
                     (if Enemy.Distance > 1_000 then
                        To_Unbounded_String
                          (Source =>
                             Get_Module_Type
                               (Module_Index =>
                                  Enemy.Ship.Modules(I).Proto_Index))
                      else To_Unbounded_String
                          (Source =>
                             To_String
                               (Source =>
                                  Get_Module
                                    (Index =>
                                       Enemy.Ship.Modules(I).Proto_Index)
                                    .Name)))) &
                "}" &
                (if Enemy.Ship.Modules(I).Durability = 0 then
                   " -font OverstrikedFont -style Gray.TLabel"
                 else ""));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options =>
              "-row" & Natural'Image(Row) & " -column 0 -sticky w -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Label &
              " $combatframe.status.scrolly");
         Damage_Percent :=
           Float(Enemy.Ship.Modules(I).Durability) /
           Float(Enemy.Ship.Modules(I).Max_Durability);
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".dmg" &
                Trim(Source => Natural'Image(Row), Side => Left),
              options =>
                "-orient horizontal -length 150 -maximum 1.0 -value" &
                Float'Image(Damage_Percent) &
                (if Damage_Percent = 1.0 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Damage_Percent > 0.24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options => "-row" & Natural'Image(Row) & " -column 1");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.status.scrolly");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Frame, Slave => Progress_Bar, Options => "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (Master => Frame, Slave => Progress_Bar, Options => "-weight 1");
         Row := Row + 1;
      end loop Show_Enemy_Ship_Status_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas :=
        Get_Widget(pathName => Main_Paned & ".combatframe.status.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Update_Messages;
   end Update_Combat_Ui;

   -- ****if* CUI/CUI.ShowCombatFrame
   -- FUNCTION
   -- Show ship to ship combat UI or boarding UI
   -- PARAMETERS
   -- Frame_Name - The parent frame, ship combat or boarding
   -- SOURCE
   procedure Show_Combat_Frame(Frame_Name: String) is
      -- ****
      Combat_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".gameframe.paned.combatframe");
      Child_Frame: Ttk_Frame :=
        Get_Widget
          (pathName =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Combat_Frame, Option => "-row 0 -column 0"));
      Combat_Children: constant array(1 .. 5) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => ".crew"),
         2 => To_Unbounded_String(Source => ".damage"),
         3 => To_Unbounded_String(Source => ".enemy"),
         4 => To_Unbounded_String(Source => ".status"),
         5 => To_Unbounded_String(Source => ".next"));
      Boarding_Children: constant array(1 .. 3) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => ".left"),
         2 => To_Unbounded_String(Source => ".right"),
         3 => To_Unbounded_String(Source => ".next"));
   begin
      if Frame_Name = ".combat" then
         if Widget_Image(Win => Child_Frame) =
           Combat_Frame & To_String(Source => Combat_Children(1)) then
            return;
         end if;
         Hide_Boarding_UI_Loop :
         for BoardingChild of Boarding_Children loop
            Child_Frame :=
              Get_Widget
                (pathName =>
                   Combat_Frame & To_String(Source => BoardingChild));
            Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Child_Frame);
         end loop Hide_Boarding_UI_Loop;
         Show_Combat_UI_Loop :
         for CombatChild of Combat_Children loop
            Child_Frame :=
              Get_Widget
                (pathName => Combat_Frame & To_String(Source => CombatChild));
            Tcl.Tk.Ada.Grid.Grid(Slave => Child_Frame);
         end loop Show_Combat_UI_Loop;
      else
         if Widget_Image(Win => Child_Frame) =
           Combat_Frame & To_String(Source => Boarding_Children(1)) then
            return;
         end if;
         Hide_Combat_UI_Loop :
         for CombatChild of Combat_Children loop
            Child_Frame :=
              Get_Widget
                (pathName => Combat_Frame & To_String(Source => CombatChild));
            Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Child_Frame);
         end loop Hide_Combat_UI_Loop;
         Show_Boarding_UI_Loop :
         for BoardingChild of Boarding_Children loop
            Child_Frame :=
              Get_Widget
                (pathName =>
                   Combat_Frame & To_String(Source => BoardingChild));
            Tcl.Tk.Ada.Grid.Grid(Slave => Child_Frame);
         end loop Show_Boarding_UI_Loop;
      end if;
   end Show_Combat_Frame;

   -- ****if* CUI/CUI.UpdateBoardingUI
   -- FUNCTION
   -- Update information about boarding party: remove old UI and create new elements
   -- SOURCE
   procedure Update_Boarding_Ui is
      -- ****
      use Ada.Characters.Handling;
      use Tiny_String;

      Orders_List, Order_Name: Unbounded_String := Null_Unbounded_String;
      Frame_Name: constant String := Main_Paned & ".combatframe";
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Frame_Name & ".right.canvas.frame");
      Tokens: Slice_Set;
      Rows: Natural;
      Order_Index: Positive := 1;
      --## rule off IMPROPER_INITIALIZATION
      Progress_Bar: Ttk_ProgressBar;
      Label: Ttk_Label;
      Combo_Box: Ttk_ComboBox;
      Combat_Canvas: Tk_Canvas;
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
   begin
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 1) & ">",
         Script => "{InvokeButton " & Frame & ".maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 2) & ">",
         Script =>
           "{InvokeButton " & Frame_Name & ".left.canvas.frame.maxmin}");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Frame);
      Show_Enemy_Crew_Loop :
      for I in Enemy.Ship.Crew.Iterate loop
         Append
           (Source => Orders_List,
            New_Item =>
              "{Attack " & To_String(Source => Enemy.Ship.Crew(I).Name) &
              "} ");
         Button :=
           Create
             (pathName =>
                Frame & ".name" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Enemy.Ship.Crew(I).Name) &
                "} -command {ShowCombatInfo enemy" &
                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
         Add
           (Widget => Button,
            Message => "Show more information about the enemy's crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-row" & Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {5 0}");
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".health" &
                Trim
                  (Source =>
                     Natural'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-orient horizontal -value " &
                Natural'Image(Enemy.Ship.Crew(I).Health) & " -length 150" &
                (if Enemy.Ship.Crew(I).Health > 74 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Enemy.Ship.Crew(I).Health > 24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Add(Widget => Progress_Bar, Message => "Enemy's health");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options =>
              "-column 1 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.right.scrolly");
         Order_Name :=
           To_Unbounded_String
             (Source => Crew_Orders'Image(Enemy.Ship.Crew(I).Order));
         Replace_Slice
           (Source => Order_Name, Low => 2,
            High => Length(Source => Order_Name),
            By =>
              To_Lower
                (Item =>
                   Slice
                     (Source => Order_Name, Low => 2,
                      High => Length(Source => Order_Name))));
         Label :=
           Create
             (pathName =>
                Frame & ".order" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options => "-text {" & To_String(Source => Order_Name) & "}");
         Add(Widget => Label, Message => "Enemy's current order.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options =>
              "-column 2 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {0 5}");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Label & " $combatframe.right.scrolly");
      end loop Show_Enemy_Crew_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas := Get_Widget(pathName => Frame_Name & ".right.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Append(Source => Orders_List, New_Item => " {Back to the ship}");
      Frame.Name := New_String(Str => Frame_Name & ".left.canvas.frame");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Frame);
      Show_Boarding_Party_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order /= BOARDING then
            goto End_Of_Loop;
         end if;
         Button :=
           Create
             (pathName =>
                Frame & ".name" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
                "} -command {ShowCombatInfo player" &
                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
         Add
           (Widget => Button,
            Message => "Show more information about the crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-row" & Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {5 0}");
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".health" &
                Trim
                  (Source =>
                     Natural'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-orient horizontal -value " &
                Natural'Image(Player_Ship.Crew(I).Health) & " -length 150" &
                (if Player_Ship.Crew(I).Health > 74 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Player_Ship.Crew(I).Health > 24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Add(Widget => Progress_Bar, Message => "The crew member health.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options =>
              "-column 1 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.left.scrolly");
         Combo_Box :=
           Create
             (pathName =>
                Frame & ".order" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-values [list " & To_String(Source => Orders_List) &
                "] -state readonly -width 15");
         Current
           (ComboBox => Combo_Box,
            NewIndex => Natural'Image(Boarding_Orders(Order_Index)));
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script =>
              "{SetBoardingOrder" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              Positive'Image(Order_Index) & "}");
         Add(Widget => Combo_Box, Message => "The crew member current order.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Combo_Box,
            Options =>
              "-column 2 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {0 5}");
         Order_Index := Order_Index + 1;
         <<End_Of_Loop>>
      end loop Show_Boarding_Party_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas := Get_Widget(pathName => Frame_Name & ".left.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Update_Messages;
   end Update_Boarding_Ui;

   -- ****if* CUI/CUI.Next_Turn_Command
   -- FUNCTION
   -- Execute combat orders and go to next turn
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NextTurn
   -- SOURCE
   function Next_Turn_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Next_Turn_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Combat_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe", Interp => Interp);
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Combat_Frame & ".crew", Interp => Interp);
      Next_Button: constant Ttk_Button :=
        Get_Widget(pathName => Combat_Frame & ".next", Interp => Interp);
   begin
      Combat_Turn;
      Update_Header;
      if Get_End_Combat then
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 1) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 2) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 3) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 4) & ">");
         Update_Combat_Ui;
         configure(Widgt => Close_Button, options => "-command {ShowSkyMap}");
         Tcl_SetVar
           (interp => Interp, varName => "gamestate", newValue => "general");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Close_Button, Options => "-row 0 -column 1");
         Frame.Name :=
           New_String(Str => Widget_Image(Win => Combat_Frame) & ".left");
         if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
            Show_Combat_Frame(Frame_Name => ".combat");
         end if;
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Next_Button);
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING and
        Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         Update_Boarding_Ui;
         Show_Combat_Frame(Frame_Name => ".boarding");
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order /= BOARDING and
        Winfo_Get(Widgt => Frame, Info => "ismapped") = "0" then
         Update_Combat_Ui;
         Show_Combat_Frame(Frame_Name => ".combat");
         return TCL_OK;
      end if;
      if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         Update_Combat_Ui;
      else
         Update_Boarding_Ui;
      end if;
      return TCL_OK;
   end Next_Turn_Command;

   -- ****if* CUI/CUI.Show_Combat_UI_Command
   -- FUNCTION
   -- Show combat UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCombatUI
   -- SOURCE
   function Show_Combat_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Combat_Ui(New_Combat => False);
      return TCL_OK;
   end Show_Combat_Ui_Command;

   -- ****if* CUI/CUI.Set_Combat_Order_Command
   -- FUNCTION
   -- Set combat order for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatOrder Position
   -- Position argument can be pilot, engineer or number of the gun which
   -- gunner will take a new combat order
   -- SOURCE
   function Set_Combat_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Combo_Box: Ttk_ComboBox;
      --## rule on IMPROPER_INITIALIZATION
      Gun_Index: Positive := 1;
      Frame_Name: constant String :=
        Main_Paned & ".combatframe.crew.canvas.frame";
      Faction: constant Faction_Record :=
        Get_Faction(Index => Player_Ship.Crew(1).Faction);
   begin
      Combo_Box.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "pilot" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".pilotorder");
         --## rule off IMPROPER_INITIALIZATION
         Set_Pilot_Order
           (New_Order => Positive'Value(Current(ComboBox => Combo_Box)) + 1);
         --## rule on IMPROPER_INITIALIZATION
         if Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "sentientships")) then
            Add_Message
              (Message =>
                 "Order for ship was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         else
            Add_Message
              (Message =>
                 "Order for " &
                 To_String
                   (Source =>
                      Player_Ship.Crew(Find_Member(Order => PILOT)).Name) &
                 " was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "engineer" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".engineerorder");
         --## rule off IMPROPER_INITIALIZATION
         Set_Engineer_Order
           (New_Order => Positive'Value(Current(ComboBox => Combo_Box)) + 1);
         --## rule on IMPROPER_INITIALIZATION
         if Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "sentientships")) then
            Add_Message
              (Message =>
                 "Order for ship was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         else
            Add_Message
              (Message =>
                 "Order for " &
                 To_String
                   (Source =>
                      Player_Ship.Crew(Find_Member(Order => ENGINEER)).Name) &
                 " was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         end if;
      else
         Combo_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".gunorder" & CArgv.Arg(Argv => Argv, N => 1));
         Gun_Index := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
         --## rule off IMPROPER_INITIALIZATION
         Guns(Gun_Index)(2) :=
           Positive'Value(Current(ComboBox => Combo_Box)) + 1;
         Guns(Gun_Index)(3) :=
           (if Current(ComboBox => Combo_Box) = "0" then 0
            else Get_Module
                (Index => Player_Ship.Modules(Guns(Gun_Index)(1)).Proto_Index)
                .Speed);
         Add_Message
           (Message =>
              "Order for " &
              To_String
                (Source =>
                   Player_Ship.Crew
                     (Player_Ship.Modules(Guns(Gun_Index)(1)).Owner(1))
                     .Name) &
              " was set on: " & Get(Widgt => Combo_Box),
            M_Type => COMBATMESSAGE);
         --## rule on IMPROPER_INITIALIZATION
      end if;
      Update_Messages;
      return TCL_OK;
   end Set_Combat_Order_Command;

   -- ****o* CUI/CUI.Set_Boarding_Order_Command
   -- FUNCTION
   -- Set boarding order for the selected player's ship crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBoardingOrder EnemyIndex
   -- EnemyIndex parameter is the index of the enemy in the enemy ship crew
   -- which will be set as target for the selected player ship crew member.
   -- SOURCE
   function Set_Boarding_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Boarding_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".combatframe.left.canvas.frame.order" &
             CArgv.Arg(Argv => Argv, N => 1),
           Interp => Interp);
   begin
      Boarding_Orders(Positive'Value(CArgv.Arg(Argv => Argv, N => 2))) :=
        (if
           Natural'Value(Current(ComboBox => Combo_Box)) + 1 >
           Natural(Enemy.Ship.Crew.Length)
         then -1
         else Natural'Value(Current(ComboBox => Combo_Box)) + 1);
      return TCL_OK;
   end Set_Boarding_Order_Command;

   -- ****o* CUI/CUI.Set_Combat_Party_Command
   -- FUNCTION
   -- Set combat party (boarding or defenders)
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatParty partytype
   -- Partytype is a type of party to set. Possible options are boarding or
   -- defenders
   -- SOURCE
   function Set_Combat_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tklib.Ada.Autoscroll;
      use Tiny_String;

      Crew_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".boardingdialog",
           Title =>
             "Assign a crew members to " &
             (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then
                "boarding party"
              else "defenders"),
           Title_Width => 245);
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Crew_Dialog & ".yscroll",
           options =>
             "-orient vertical -command [list " & Crew_Dialog &
             ".canvas yview]");
      Crew_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Crew_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Crew_Frame: constant Ttk_Frame :=
        Create(pathName => Crew_Canvas & ".frame");
      Buttons_Box2: constant Ttk_Frame :=
        Create(pathName => Crew_Dialog & ".buttons");
      Close_Dialog_Button: constant Ttk_Button :=
        Create
          (pathName => Crew_Dialog & ".buttons.button",
           options =>
             "-text Cancel -command {CloseDialog " & Crew_Dialog &
             "} -image cancelicon -style Dialog.TButton");
      Accept_Button: constant Ttk_Button :=
        Create
          (pathName => Crew_Dialog & ".buttons.button2",
           options =>
             "-text Assign -command {SetParty " &
             CArgv.Arg(Argv => Argv, N => 1) & "; CloseDialog " & Crew_Dialog &
             "} -image giveordericon -style Dialog.TButton");
      Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => Crew_Dialog & ".selectframe");
      Height: Positive := 10;
      Width: Positive := 250;
      --## rule off IMPROPER_INITIALIZATION
      Crew_Button: Ttk_CheckButton;
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then BOARDING
         else DEFEND);
   begin
      Button :=
        Create
          (pathName => Buttons_Frame & ".selectallbutton",
           options =>
             "-image selectallicon -command {ToggleAllCombat select " &
             CArgv.Arg(Argv => Argv, N => 1) & "} -style Small.TButton");
      Add(Widget => Button, Message => "Select all crew members.");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx {5 2}");
      Button :=
        Create
          (pathName => Buttons_Frame & ".unselectallbutton",
           options =>
             "-image unselectallicon -command {ToggleAllCombat unselect " &
             CArgv.Arg(Argv => Argv, N => 1) & "} -style Small.TButton");
      Add(Widget => Button, Message => "Unselect all crew members.");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-sticky w -row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Frame, Options => "-columnspan 2 -sticky w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crew_Canvas, Options => "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Y_Scroll,
         Options => "-sticky ns -padx {0 5} -pady {5 0} -row 2 -column 1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Box2, Options => "-pady {0 5} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(Slave => Accept_Button, Options => "-padx {5 2}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Dialog_Button,
         Options => "-sticky w -row 0 -column 1");
      Focus(Widgt => Close_Dialog_Button);
      Autoscroll(Scroll => Y_Scroll);
      Show_Player_Ship_Crew_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Crew_Button :=
           Create
             (pathName =>
                Crew_Frame & ".crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
                "}");
         if Player_Ship.Crew(I).Order = Order then
            Tcl_SetVar
              (interp => Interp, varName => Widget_Image(Win => Crew_Button),
               newValue => "1");
         else
            Tcl_SetVar
              (interp => Interp, varName => Widget_Image(Win => Crew_Button),
               newValue => "0");
         end if;
         Tcl.Tk.Ada.Pack.Pack(Slave => Crew_Button, Options => "-anchor w");
         Height :=
           Height +
           Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqheight"));
         if Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
           10 >
           Width then
            Width :=
              Positive'Value
                (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
              10;
         end if;
         Bind
           (Widgt => Crew_Button, Sequence => "<Escape>",
            Script => "{" & Close_Dialog_Button & " invoke;break}");
         Bind
           (Widgt => Crew_Button, Sequence => "<Tab>",
            Script =>
              "{focus [GetActiveButton" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              "];break}");
      end loop Show_Player_Ship_Crew_Loop;
      if Height > 500 then
         Height := 500;
      end if;
      Canvas_Create
        (Parent => Crew_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Crew_Frame));
      Tcl_Eval(interp => Interp, strng => "update");
      configure
        (Widgt => Crew_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Crew_Canvas, TagOrId => "all") & "] -height" &
           Positive'Image(Height) & " -width" & Positive'Image(Width));
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Tab>",
         Script => "{focus [GetActiveButton 0];break}");
      Show_Dialog(Dialog => Crew_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Set_Combat_Party_Command;

   -- ****if* CUI/CUI.Set_Combat_Position_Command
   -- FUNCTION
   -- Set crew member position (pilot, engineer, gunner) in combat
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatPosition position
   -- Position is the combat crew member position which will be set
   -- SOURCE
   function Set_Combat_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      --## rule off IMPROPER_INITIALIZATION
      Combo_Box: Ttk_ComboBox;
      --## rule on IMPROPER_INITIALIZATION
      Gun_Index: Positive := 1;
      Crew_Index: Natural;
      Frame_Name: constant String :=
        ".gameframe.paned.combatframe.crew.canvas.frame";
   begin
      Combo_Box.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "pilot" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".pilotcrew");
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => PILOT);
         else
            Crew_Index := Find_Member(Order => PILOT);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "engineer" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".engineercrew");
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => ENGINEER);
         else
            Crew_Index := Find_Member(Order => ENGINEER);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      else
         Combo_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".guncrew" & CArgv.Arg(Argv => Argv, N => 2));
         Gun_Index := Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => GUNNER, Module_Index => Guns(Gun_Index)(1));
         else
            Crew_Index := Player_Ship.Modules(Guns(Gun_Index)(1)).Owner(1);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      end if;
      Update_Combat_Ui;
      return TCL_OK;
   end Set_Combat_Position_Command;

   -- ****if* CUI/CUI.Show_Combat_Info_Command
   -- FUNCTION
   -- Show information about the selected mob in combat
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatPosition player|enemy index
   -- Position is the combat crew member position which will be set
   -- SOURCE
   function Show_Combat_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Crew_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Info: Unbounded_String;
   begin
      Info := To_Unbounded_String(Source => "Uses: ");
      if CArgv.Arg(Argv => Argv, N => 1) = "player" then
         Show_Player_Crew_Equipment_Loop :
         for Item of Player_Ship.Crew(Crew_Index).Equipment loop
            if Item /= 0 then
               Append
                 (Source => Info,
                  New_Item =>
                    LF &
                    Get_Item_Name
                      (Item =>
                         Inventory_Container.Element
                           (Container =>
                              Player_Ship.Crew(Crew_Index).Inventory,
                            Index => Item)));
            end if;
         end loop Show_Player_Crew_Equipment_Loop;
      else
         Show_Enemy_Crew_Equipment_Loop :
         for Item of Enemy.Ship.Crew(Crew_Index).Equipment loop
            if Item /= 0 then
               Append
                 (Source => Info,
                  New_Item =>
                    LF &
                    Get_Item_Name
                      (Item =>
                         Inventory_Container.Element
                           (Container => Enemy.Ship.Crew(Crew_Index).Inventory,
                            Index => Item)));
            end if;
         end loop Show_Enemy_Crew_Equipment_Loop;
      end if;
      Show_Info(Text => To_String(Source => Info), Title => "More info");
      return TCL_OK;
   end Show_Combat_Info_Command;

   -- ****o* CUI/CUI.Combat_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of the combat UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CombatMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Combat_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Combat_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      --## rule off TYPE_INITIAL_VALUES
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      type Frames_Array is array(Positive range <>) of Frame_Info;
      Combat_Frames: constant Frames_Array(1 .. 4) :=
        (1 =>
           (Name => To_Unbounded_String(Source => "crew"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "damage"), Column => 0,
            Row => 1),
         3 =>
           (Name => To_Unbounded_String(Source => "enemy"), Column => 1,
            Row => 0),
         4 =>
           (Name => To_Unbounded_String(Source => "status"), Column => 1,
            Row => 1));
      Boarding_Frames: constant Frames_Array(1 .. 2) :=
        (1 =>
           (Name => To_Unbounded_String(Source => "left"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "right"), Column => 1,
            Row => 0));
      Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".combatframe.crew", Interp => Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName =>
             Main_Paned & ".combatframe." & CArgv.Arg(Argv => Argv, N => 1) &
             ".canvas.frame.maxmin",
           Interp => Interp);
      Frames: constant Frames_Array :=
        (if CArgv.Arg(Argv => Argv, N => 3) = "combat" then Combat_Frames
         else Boarding_Frames);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "show" then
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".combatframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options => "-columnspan 2 -rowspan 2 -row 0 -column 0");
            else
               Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Frame);
            end if;
         end loop Hide_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapdownicon -command {CombatMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " hide " &
              CArgv.Arg(Argv => Argv, N => 3) & "}");
      else
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".combatframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options =>
                    "-columnspan 1 -rowspan 1 -column" &
                    Natural'Image(FrameInfo.Column) & " -row" &
                    Natural'Image(FrameInfo.Row));
            end if;
         end loop Show_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapupicon -command {CombatMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " show " &
              CArgv.Arg(Argv => Argv, N => 3) & "}");
      end if;
      return TCL_OK;
   end Combat_Max_Min_Command;

   -- ****o* CUI/CUI.Toggle_All_Command
   -- FUNCTION
   -- Select or deselect all crew members in boarding and defending party
   -- setting
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleAllCombat action order
   -- Action is the action which will be performed. Possible values are
   -- select or deselect. Order is the order to give to the player's ship
   -- crew. Possible values are boarding and defending.
   -- SOURCE
   function Toggle_All_Combat_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_All_Combat_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      Boarding_Orders.Clear;
      Set_Crew_Selection_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Tcl_SetVar
           (interp => Interp,
            varName =>
              ".boardingdialog.canvas.frame.crewbutton" &
              Trim
                (Source => Crew_Container.To_Index(Position => I)'Image,
                 Side => Left),
            newValue =>
              (if CArgv.Arg(Argv => Argv, N => 1) = "select" then "1"
               else "0"));
      end loop Set_Crew_Selection_Loop;
      return TCL_OK;
   end Toggle_All_Combat_Command;

   -- ****o* CUI/CUI.Set_Party_Command
   -- FUNCTION
   -- Set crew members in or out of boarding and defending party
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetParty order
   -- Order is the order to give to the player's ship crew. Possible
   -- values are boarding and defending.
   -- SOURCE
   function Set_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then BOARDING
         else DEFEND);
      Selected: Boolean := False;
   begin
      Boarding_Orders.Clear;
      Set_Crew_Selection_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Selected :=
           Tcl_GetVar
             (interp => Interp,
              varName =>
                ".boardingdialog.canvas.frame.crewbutton" &
                Trim
                  (Source => Crew_Container.To_Index(Position => I)'Image,
                   Side => Left)) =
           "1";
         if Player_Ship.Crew(I).Order = Order and then not Selected then
            Give_Orders
              (Ship => Player_Ship,
               Member_Index => Crew_Container.To_Index(Position => I),
               Given_Order => REST);
         elsif Selected and Player_Ship.Crew(I).Order /= Order then
            Give_Orders
              (Ship => Player_Ship,
               Member_Index => Crew_Container.To_Index(Position => I),
               Given_Order => Order, Module_Index => 0);
            if Order = BOARDING then
               Boarding_Orders.Append(New_Item => 0);
            end if;
         end if;
      end loop Set_Crew_Selection_Loop;
      Update_Combat_Ui;
      return TCL_OK;
   end Set_Party_Command;

   procedure Show_Combat_Ui(New_Combat: Boolean := True) is
      use GNAT.Directory_Operations;
      use Events;
      use Maps;
      use Tiny_String;

      Combat_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe");
      Combat_Started: Boolean := False;
      Button: constant Ttk_Button :=
        Get_Widget(pathName => Combat_Frame & ".next");
      Enemy_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Combat_Frame & ".status");
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      if New_Combat then
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0
           and then Get_Enemy_Name /=
             Get_Proto_Ship
               (Proto_Index =>
                  Get_Event
                    (Index =>
                       Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                         .Event_Index)
                    .Ship_Index)
               .Name then
            Combat_Started :=
              Start_Combat
                (Enemy_Index =>
                   Get_Event
                     (Index =>
                        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .Event_Index)
                     .Ship_Index,
                 New_Combat => False);
            if not Combat_Started then
               return;
            end if;
         end if;
         if Winfo_Get(Widgt => Combat_Frame, Info => "exists") = "0" then
            Tcl_EvalFile
              (interp => Get_Context,
               fileName =>
                 To_String(Source => Data_Directory) & "ui" & Dir_Separator &
                 "combat.tcl");
            Set_Pilot_Order(New_Order => 2);
            Set_Engineer_Order(New_Order => 3);
            Add_Command
              (Name => "NextTurn", Ada_Command => Next_Turn_Command'Access);
            Add_Command
              (Name => "ShowCombatUI",
               Ada_Command => Show_Combat_Ui_Command'Access);
            Add_Command
              (Name => "SetCombatOrder",
               Ada_Command => Set_Combat_Order_Command'Access);
            Add_Command
              (Name => "SetBoardingOrder",
               Ada_Command => Set_Boarding_Order_Command'Access);
            Add_Command
              (Name => "SetCombatParty",
               Ada_Command => Set_Combat_Party_Command'Access);
            Add_Command
              (Name => "SetCombatPosition",
               Ada_Command => Set_Combat_Position_Command'Access);
            Add_Command
              (Name => "ShowCombatInfo",
               Ada_Command => Show_Combat_Info_Command'Access);
            Add_Command
              (Name => "CombatMaxMin",
               Ada_Command => Combat_Max_Min_Command'Access);
            Add_Command
              (Name => "ToggleAllCombat",
               Ada_Command => Toggle_All_Combat_Command'Access);
            Add_Command
              (Name => "SetParty", Ada_Command => Set_Party_Command'Access);
         else
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
            Tcl.Tk.Ada.Grid.Grid(Slave => Enemy_Frame);
         end if;
         configure(Widgt => Close_Button, options => "-command ShowCombatUI");
         Tcl_SetVar
           (interp => Get_Context, varName => "gamestate",
            newValue => "combat");
         Back_To_Work_Loop :
         for Member of Player_Ship.Crew loop
            if Member.Order = REST
              and then Member.Previous_Order in PILOT | ENGINEER | GUNNER then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               Add_Message
                 (Message =>
                    To_String(Source => Member.Name) &
                    " back to work for combat.",
                  M_Type => ORDERMESSAGE);
            end if;
         end loop Back_To_Work_Loop;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING then
         Update_Boarding_Ui;
         Show_Combat_Frame(Frame_Name => ".boarding");
      else
         Update_Combat_Ui;
         Show_Combat_Frame(Frame_Name => ".combat");
      end if;
      Show_Screen(New_Screen_Name => "combatframe");
   end Show_Combat_Ui;

end Combat.UI;
