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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
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
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   -- ****if* CUI/CUI.Get_Gun_Speed
   -- FUNCTION
   -- Get information about fire rate of selected gun with selected order
   -- PARAMETERS
   -- Position - Number of gun to check
   -- Index    - Index of the gunner's order
   -- RESULT
   -- String with info about gun fire rate
   -- SOURCE
   function Get_Gun_Speed(Position: Natural; Index: Positive) return String is
      -- ****
      Gun_Speed: Integer;
      Firerate: Unbounded_String;
   begin
      Gun_Speed :=
        Modules_List(Player_Ship.Modules(Guns(Position)(1)).Proto_Index).Speed;
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
      if Gun_Speed > 0 then
         Firerate :=
           To_Unbounded_String
             (Source =>
                "(" & Trim(Source => Integer'Image(Gun_Speed), Side => Both) &
                "/round)");
      elsif Gun_Speed < 0 then
         Firerate :=
           To_Unbounded_String
             (Source =>
                "(1/" &
                Trim(Source => Integer'Image(Gun_Speed), Side => Both) &
                " rounds)");
      end if;
      return To_String(Source => Firerate);
   end Get_Gun_Speed;

   -- ****if* CUI/CUI.Update_Messages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure Update_Messages is
      -- ****
      Loop_Start: Integer := 0 - Messages_Amount;
      Message: Message_Data;
      Current_Turn_Time: Unbounded_String :=
        To_Unbounded_String(Source => Formated_Time);
      Messages_View: constant Tk_Text :=
        Get_Widget(pathName => Main_Paned & ".controls.messages.view");
      procedure Show_Message is
         Tag_Names: constant array(1 .. 5) of Unbounded_String :=
           (1 => To_Unbounded_String(Source => "yellow"),
            2 => To_Unbounded_String(Source => "green"),
            3 => To_Unbounded_String(Source => "red"),
            4 => To_Unbounded_String(Source => "blue"),
            5 => To_Unbounded_String(Source => "cyan"));
      begin
         if Unbounded_Slice
             (Source => Message.Message, Low => 1,
              High => Length(Source => Current_Turn_Time)) =
           Current_Turn_Time then
            if Message.Color = WHITE then
               Insert
                 (TextWidget => Messages_View, Index => "end",
                  Text => "{" & To_String(Source => Message.Message) & "}");
            else
               Insert
                 (TextWidget => Messages_View, Index => "end",
                  Text =>
                    "{" & To_String(Source => Message.Message) & "} [list " &
                    To_String
                      (Source => Tag_Names(Message_Color'Pos(Message.Color))) &
                    "]");
            end if;
         else
            Insert
              (TextWidget => Messages_View, Index => "end",
               Text =>
                 "{" & To_String(Source => Message.Message) & "} [list gray]");
         end if;
      end Show_Message;
   begin
      Tcl.Tk.Ada.Widgets.configure
        (Widgt => Messages_View, options => "-state normal");
      Delete
        (TextWidget => Messages_View, StartIndex => "1.0", Indexes => "end");
      if Loop_Start = 0 then
         Tcl.Tk.Ada.Widgets.configure
           (Widgt => Messages_View, options => "-state disable");
         return;
      end if;
      if Loop_Start < -10 then
         Loop_Start := -10;
      end if;
      Message := Get_Message(Message_Index => Get_Last_Message_Index);
      if Unbounded_Slice
          (Source => Message.Message, Low => 1,
           High => Length(Source => Current_Turn_Time)) /=
        Current_Turn_Time then
         Current_Turn_Time :=
           Unbounded_Slice
             (Source => Message.Message, Low => 1,
              High => Length(Source => Current_Turn_Time));
      end if;
      if Game_Settings.Messages_Order = OLDER_FIRST then
         Show_Older_Messages_First_Loop :
         for I in Loop_Start .. -1 loop
            Message := Get_Message(Message_Index => I + 1);
            if (Get_Last_Message_Index + I + 1) >= Messages_Starts then
               Show_Message;
               if I < -1 then
                  Insert
                    (TextWidget => Messages_View, Index => "end",
                     Text => "{" & LF & "}");
               end if;
            end if;
         end loop Show_Older_Messages_First_Loop;
         See(TextWidget => Messages_View, Index => "end");
      else
         Show_New_Messages_First_Loop :
         for I in reverse Loop_Start .. -1 loop
            Message := Get_Message(Message_Index => I + 1);
            exit Show_New_Messages_First_Loop when
              (Get_Last_Message_Index + I + 1) <
              Messages_Starts;
            Show_Message;
            if I > Loop_Start then
               Insert
                 (TextWidget => Messages_View, Index => "end",
                  Text => "{" & LF & "}");
            end if;
         end loop Show_New_Messages_First_Loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure
        (Widgt => Messages_View, options => "-state disable");
   end Update_Messages;

   -- ****if* CUI/CUI.Update_Combat_Ui
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure Update_Combat_Ui is
      -- ****
      use Short_String;
      use Tiny_String;

      Tokens: Slice_Set;
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe.crew.canvas.frame");
      Label: Ttk_Label;
      Combo_Box: Ttk_ComboBox := Get_Widget(pathName => Frame & ".pilotcrew");
      Gunners_Orders: constant array(1 .. 6) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "{Don't shoot"),
         2 => To_Unbounded_String(Source => "{Precise fire "),
         3 => To_Unbounded_String(Source => "{Fire at will "),
         4 => To_Unbounded_String(Source => "{Aim for their engine "),
         5 => To_Unbounded_String(Source => "{Aim for their weapon "),
         6 => To_Unbounded_String(Source => "{Aim for their hull "));
      Gun_Index, Gunner_Orders, Enemy_Info: Unbounded_String;
      Have_Ammo: Boolean;
      Ammo_Amount, Ammo_Index, Row, Rows: Natural := 0;
      Progress_Bar: Ttk_ProgressBar;
      Damage_Percent: Float;
      Combat_Canvas: Tk_Canvas;
      Has_Gunner: Boolean := False;
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
   begin
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & To_String(Source => General_Accelerators(1)) & ">",
         Script => "{InvokeButton " & Frame & ".maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & To_String(Source => General_Accelerators(3)) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.damage.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & To_String(Source => General_Accelerators(2)) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.enemy.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & To_String(Source => General_Accelerators(4)) & ">",
         Script =>
           "{InvokeButton " & Main_Paned &
           ".combatframe.status.canvas.frame.maxmin}");
      configure
        (Widgt => Combo_Box,
         options => "-values [list " & Get_Crew_List(Position => 0) & "]");
      Current
        (ComboBox => Combo_Box,
         NewIndex => Natural'Image(Find_Member(Order => PILOT)));
      Combo_Box.Name := New_String(Str => Frame & ".Pilot_Order");
      Current
        (ComboBox => Combo_Box, NewIndex => Integer'Image(Pilot_Order - 1));
      if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
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
      Combo_Box.Name := New_String(Str => Frame & ".Engineer_Order");
      Current
        (ComboBox => Combo_Box, NewIndex => Natural'Image(Engineer_Order - 1));
      if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
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
      Delete_Widgets
        (Start_Index => 4, End_Index => (Rows - 1), Frame => Frame);
      Show_Guns_Info_Loop :
      for I in Guns.Iterate loop
         Have_Ammo := False;
         Has_Gunner := False;
         Check_Ammo_Block :
         declare
            Ammo_Index: constant Natural :=
              (if Player_Ship.Modules(Guns(I)(1)).M_Type = GUN then
                 Player_Ship.Modules(Guns(I)(1)).Ammo_Index
               else Player_Ship.Modules(Guns(I)(1)).Harpoon_Index);
         begin
            if
              (Ammo_Index in
                 Inventory_Container.First_Index
                       (Container => Player_Ship.Cargo) ..
                       Inventory_Container.Last_Index
                         (Container => Player_Ship.Cargo))
              and then
                Items_List
                  (Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Ammo_Index)
                     .Proto_Index)
                  .I_Type =
                Items_Types
                  (Modules_List(Player_Ship.Modules(Guns(I)(1)).Proto_Index)
                     .Value) then
               Ammo_Amount :=
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Ammo_Index)
                   .Amount;
               Have_Ammo := True;
            end if;
         end Check_Ammo_Block;
         if not Have_Ammo then
            Ammo_Amount := 0;
            Find_Ammo_Loop :
            for J in Items_List.Iterate loop
               if Items_List(J).I_Type =
                 Items_Types
                   (Modules_List(Player_Ship.Modules(Guns(I)(1)).Proto_Index)
                      .Value) then
                  Ammo_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Proto_Index => Objects_Container.Key(Position => J));
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
         if Player_Ship.Modules(Guns(I)(1)).Owner(1) /= 0 then
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
         else
            Current(ComboBox => Combo_Box, NewIndex => "0");
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
        Proto_Ships_List(Enemy_Ship_Index).Crew.Length > 0 then
         Show_Boarding_Info_Block :
         declare
            Button: Ttk_Button :=
              Create
                (pathName => Frame & ".boarding",
                 options =>
                   "-text {Boarding party:} -command {SetCombatParty boarding}");
            Boarding_Party, Defenders: Unbounded_String;
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
                        (Get_Widget(pathName => Frame & ".Engineer_Order")),
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
               if Member.Order = BOARDING then
                  Append
                    (Source => Boarding_Party,
                     New_Item => To_String(Source => Member.Name) & ", ");
               elsif Member.Order = DEFEND then
                  Append
                    (Source => Defenders,
                     New_Item => To_String(Source => Member.Name) & ", ");
               end if;
            end loop Set_Boarding_And_Defenders_Loop;
            if Boarding_Party /= Null_Unbounded_String then
               Boarding_Party :=
                 Unbounded_Slice
                   (Source => Boarding_Party, Low => 1,
                    High => Length(Source => Boarding_Party) - 2);
            end if;
            Label :=
              Create
                (pathName => Frame & ".boardparty",
                 options =>
                   "-text {" & To_String(Source => Boarding_Party) &
                   "} -wraplength" & Positive'Image(Label_Length));
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Label,
               Options =>
                 "-row" & Positive'Image(Natural(Guns.Length) + 3) &
                 " -column 1 -columnspan 2 -sticky w");
            Tcl_Eval
              (interp => Get_Context,
               strng =>
                 "SetScrollbarBindings " & Label &
                 " $combatframe.crew.scrolly");
            if Defenders /= Null_Unbounded_String then
               Defenders :=
                 Unbounded_Slice
                   (Source => Defenders, Low => 1,
                    High => Length(Source => Defenders) - 2);
            end if;
            Label :=
              Create
                (pathName => Frame & ".defenders",
                 options =>
                   "-text {" & To_String(Source => Defenders) &
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
             (Frame & ".maxmin",
              "-style Small.TButton -image movemapupicon -command {CombatMaxMin damage show combat}");
      begin
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky w -padx 5 -row 0 -column 0");
         Add(Button, "Maximize/minimize the ship status info");
      end Add_Minimize_Button_Block;
      Show_Player_Ship_Damage_Loop :
      for Module of Player_Ship.Modules loop
         Label :=
           Create
             (Frame & ".lbl" & Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Module.Name) & "}" &
              (if Module.Durability = 0 then
                 " -font OverstrikedFont -style Gray.TLabel"
               else ""));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -sticky w -padx 5");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & Label & " $combatframe.damage.scrolly");
         Damage_Percent :=
           (Float(Module.Durability) / Float(Module.Max_Durability));
         Progress_Bar :=
           Create
             (Frame & ".dmg" & Trim(Natural'Image(Row), Left),
              "-orient horizontal -length 150 -maximum 1.0 -value" &
              Float'Image(Damage_Percent) &
              (if Damage_Percent = 1.0 then
                 " -style green.Horizontal.TProgressbar"
               elsif Damage_Percent > 0.24 then
                 " -style yellow.Horizontal.TProgressbar"
               else " -style Horizontal.TProgressbar"));
         Tcl.Tk.Ada.Grid.Grid
           (Progress_Bar, "-row" & Natural'Image(Row) & " -column 1");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & Progress_Bar &
            " $combatframe.damage.scrolly");
         Tcl.Tk.Ada.Grid.Column_Configure(Frame, Progress_Bar, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure(Frame, Progress_Bar, "-weight 1");
         Row := Row + 1;
      end loop Show_Player_Ship_Damage_Loop;
      Tcl_Eval(Get_Context, "update");
      Combat_Canvas := Get_Widget(Main_Paned & ".combatframe.damage.canvas");
      configure
        (Combat_Canvas,
         "-scrollregion [list " & BBox(Combat_Canvas, "all") & "]");
      Xview_Move_To(Combat_Canvas, "0.0");
      Yview_Move_To(Combat_Canvas, "0.0");
      Append
        (Enemy_Info,
         "Name: " & To_String(Source => Enemy_Name) & LF & "Type: " &
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
            Append(Enemy_Info, "Destroyed");
         else
            declare
               EnemyStatus: Unbounded_String := To_Unbounded_String("Ok");
            begin
               Check_Enemy_Ship_Status_Loop :
               for Module of Enemy.Ship.Modules loop
                  if Module.Durability < Module.Max_Durability then
                     EnemyStatus := To_Unbounded_String("Damaged");
                     exit Check_Enemy_Ship_Status_Loop;
                  end if;
               end loop Check_Enemy_Ship_Status_Loop;
               Append(Enemy_Info, EnemyStatus);
            end;
            Check_Enemy_Status_Loop :
            for Module of Enemy.Ship.Modules loop
               if Module.Durability > 0 then
                  case Modules_List(Module.Proto_Index).M_Type is
                     when ARMOR =>
                        Append(Enemy_Info, " (armored)");
                     when GUN =>
                        Append(Enemy_Info, " (gun)");
                     when BATTERING_RAM =>
                        Append(Enemy_Info, " (battering ram)");
                     when HARPOON_GUN =>
                        Append(Enemy_Info, " (harpoon gun)");
                     when others =>
                        null;
                  end case;
               end if;
            end loop Check_Enemy_Status_Loop;
         end if;
      else
         Append(Enemy_Info, "Unknown");
      end if;
      Append(Enemy_Info, LF & "Speed: ");
      if Enemy.Distance < 15_000 then
         case Enemy.Ship.Speed is
            when Ships.FULL_STOP =>
               Append(Enemy_Info, "Stopped");
            when QUARTER_SPEED =>
               Append(Enemy_Info, "Slow");
            when HALF_SPEED =>
               Append(Enemy_Info, "Medium");
            when FULL_SPEED =>
               Append(Enemy_Info, "Fast");
            when others =>
               null;
         end case;
         if Enemy.Ship.Speed /= Ships.FULL_STOP then
            declare
               SpeedDiff: constant Integer :=
                 RealSpeed(Enemy.Ship) - RealSpeed(Player_Ship);
            begin
               if SpeedDiff > 250 then
                  Append(Enemy_Info, " (much faster)");
               elsif SpeedDiff > 0 then
                  Append(Enemy_Info, " (faster)");
               elsif SpeedDiff = 0 then
                  Append(Enemy_Info, " (equal)");
               elsif SpeedDiff > -250 then
                  Append(Enemy_Info, " (slower)");
               else
                  Append(Enemy_Info, " (much slower)");
               end if;
            end;
         end if;
      else
         Append(Enemy_Info, "Unknown");
      end if;
      if Length(Enemy.Ship.Description) > 0 then
         Append
           (Enemy_Info, LF & LF & To_String(Source => Enemy.Ship.Description));
      end if;
      Label := Get_Widget(Main_Paned & ".combatframe.enemy.canvas.frame.info");
      configure(Label, "-text {" & To_String(Enemy_Info) & "}");
      Tcl_Eval(Get_Context, "update");
      Combat_Canvas := Get_Widget(Main_Paned & ".combatframe.enemy.canvas");
      configure
        (Combat_Canvas,
         "-scrollregion [list " & BBox(Combat_Canvas, "all") & "]");
      Xview_Move_To(Combat_Canvas, "0.0");
      Yview_Move_To(Combat_Canvas, "0.0");
      Frame.Name :=
        New_String(Main_Paned & ".combatframe.status.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, Frame);
      Row := 1;
      if End_Combat then
         Enemy.Distance := 100;
      end if;
      Show_Enemy_Ship_Status_Loop :
      for I in Enemy.Ship.Modules.Iterate loop
         if End_Combat then
            Enemy.Ship.Modules(I).Durability := 0;
         end if;
         Label :=
           Create
             (Frame & ".lbl" & Trim(Natural'Image(Row), Left),
              "-text {" &
              To_String
                (if Enemy.Distance > 1_000 then
                   To_Unbounded_String
                     (Get_Module_Type(Enemy.Ship.Modules(I).Proto_Index))
                 else Modules_List(Enemy.Ship.Modules(I).Proto_Index).Name) &
              "}" &
              (if Enemy.Ship.Modules(I).Durability = 0 then
                 " -font OverstrikedFont -style Gray.TLabel"
               else ""));
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-row" & Natural'Image(Row) & " -column 0 -sticky w -padx 5");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & Label & " $combatframe.status.scrolly");
         Damage_Percent :=
           ((Float(Enemy.Ship.Modules(I).Durability) /
             Float(Enemy.Ship.Modules(I).Max_Durability)));
         Progress_Bar :=
           Create
             (Frame & ".dmg" & Trim(Natural'Image(Row), Left),
              "-orient horizontal -length 150 -maximum 1.0 -value" &
              Float'Image(Damage_Percent) &
              (if Damage_Percent = 1.0 then
                 " -style green.Horizontal.TProgressbar"
               elsif Damage_Percent > 0.24 then
                 " -style yellow.Horizontal.TProgressbar"
               else " -style Horizontal.TProgressbar"));
         Tcl.Tk.Ada.Grid.Grid
           (Progress_Bar, "-row" & Natural'Image(Row) & " -column 1");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & Progress_Bar &
            " $combatframe.status.scrolly");
         Tcl.Tk.Ada.Grid.Column_Configure(Frame, Progress_Bar, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure(Frame, Progress_Bar, "-weight 1");
         Row := Row + 1;
      end loop Show_Enemy_Ship_Status_Loop;
      Tcl_Eval(Get_Context, "update");
      Combat_Canvas := Get_Widget(Main_Paned & ".combatframe.status.canvas");
      configure
        (Combat_Canvas,
         "-scrollregion [list " & BBox(Combat_Canvas, "all") & "]");
      Xview_Move_To(Combat_Canvas, "0.0");
      Yview_Move_To(Combat_Canvas, "0.0");
      Update_Messages;
   end Update_Combat_Ui;

   -- ****if* CUI/CUI.Set_Party_Order_Command
   -- FUNCTION
   -- Set boarding or defending order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBoarding MemberIndex Order
   -- MemberIndex is a index of the player ship crew member which will get the
   -- order. Order is the order to give. Possible values are boarding or
   -- defend.
   -- SOURCE
   function Set_Party_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Party_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      OrderIndex: Natural := 0;
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv, 2) = "boarding" then BOARDING else DEFEND);
   begin
      Give_Boarding_Orders_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order = BOARDING then
            OrderIndex := OrderIndex + 1;
         end if;
         if Crew_Container.To_Index(I) = MemberIndex then
            if Player_Ship.Crew(I).Order /= Order then
               Give_Orders(Player_Ship, Crew_Container.To_Index(I), Order, 0);
               if Order = BOARDING then
                  Boarding_Orders.Append(New_Item => 0);
               end if;
            else
               Give_Orders(Player_Ship, Crew_Container.To_Index(I), REST);
               if Order = BOARDING then
                  Boarding_Orders.Delete(Index => OrderIndex);
               end if;
               OrderIndex := OrderIndex - 1;
            end if;
            exit Give_Boarding_Orders_Loop;
         end if;
      end loop Give_Boarding_Orders_Loop;
      Update_Combat_Ui;
      return TCL_OK;
   end Set_Party_Order_Command;

   -- ****if* CUI/CUI.ShowCombatFrame
   -- FUNCTION
   -- Show ship to ship combat UI or boarding UI
   -- SOURCE
   procedure ShowCombatFrame(FrameName: String) is
      -- ****
      CombatFrame: constant Ttk_Frame :=
        Get_Widget(".gameframe.paned.combatframe");
      ChildFrame: Ttk_Frame :=
        Get_Widget
          (Tcl.Tk.Ada.Grid.Grid_Slaves(CombatFrame, "-row 0 -column 0"));
      CombatChildren: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String(".crew"), To_Unbounded_String(".damage"),
         To_Unbounded_String(".enemy"), To_Unbounded_String(".status"),
         To_Unbounded_String(".next"));
      BoardingChildren: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String(".left"), To_Unbounded_String(".right"),
         To_Unbounded_String(".next"));
   begin
      if FrameName = ".combat" then
         if Widget_Image(ChildFrame) =
           CombatFrame & To_String(CombatChildren(1)) then
            return;
         end if;
         Hide_Boarding_UI_Loop :
         for BoardingChild of BoardingChildren loop
            ChildFrame := Get_Widget(CombatFrame & To_String(BoardingChild));
            Tcl.Tk.Ada.Grid.Grid_Remove(ChildFrame);
         end loop Hide_Boarding_UI_Loop;
         Show_Combat_UI_Loop :
         for CombatChild of CombatChildren loop
            ChildFrame := Get_Widget(CombatFrame & To_String(CombatChild));
            Tcl.Tk.Ada.Grid.Grid(ChildFrame);
         end loop Show_Combat_UI_Loop;
      else
         if Widget_Image(ChildFrame) =
           CombatFrame & To_String(BoardingChildren(1)) then
            return;
         end if;
         Hide_Combat_UI_Loop :
         for CombatChild of CombatChildren loop
            ChildFrame := Get_Widget(CombatFrame & To_String(CombatChild));
            Tcl.Tk.Ada.Grid.Grid_Remove(ChildFrame);
         end loop Hide_Combat_UI_Loop;
         Show_Boarding_UI_Loop :
         for BoardingChild of BoardingChildren loop
            ChildFrame := Get_Widget(CombatFrame & To_String(BoardingChild));
            Tcl.Tk.Ada.Grid.Grid(ChildFrame);
         end loop Show_Boarding_UI_Loop;
      end if;
   end ShowCombatFrame;

   -- ****if* CUI/CUI.UpdateBoardingUI
   -- FUNCTION
   -- Update information about boarding party: remove old UI and create new elements
   -- SOURCE
   procedure UpdateBoardingUI is
      -- ****
      use Tiny_String;

      OrdersList, OrderName: Unbounded_String;
      FrameName: constant String := Main_Paned & ".combatframe";
      Frame: Ttk_Frame := Get_Widget(FrameName & ".right.canvas.frame");
      Label: Ttk_Label;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ProgressBar: Ttk_ProgressBar;
      ComboBox: Ttk_ComboBox;
      OrderIndex: Positive := 1;
      CombatCanvas: Tk_Canvas;
      Button: Ttk_Button;
   begin
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(General_Accelerators(1)) & ">",
         "{InvokeButton " & Frame & ".maxmin}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(General_Accelerators(2)) & ">",
         "{InvokeButton " & FrameName & ".left.canvas.frame.maxmin}");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, Frame);
      Show_Enemy_Crew_Loop :
      for I in Enemy.Ship.Crew.Iterate loop
         Append
           (OrdersList,
            "{Attack " & To_String(Source => Enemy.Ship.Crew(I).Name) & "} ");
         Button :=
           Create
             (Frame & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(Enemy.Ship.Crew(I).Name) &
              "} -command {ShowCombatInfo enemy" &
              Positive'Image(Crew_Container.To_Index(I)) & "}");
         Add(Button, "Show more information about the enemy's crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Button,
            "-row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx {5 0}");
         ProgressBar :=
           Create
             (Frame & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(Enemy.Ship.Crew(I).Health) & " -length 150" &
              (if Enemy.Ship.Crew(I).Health > 74 then
                 " -style green.Horizontal.TProgressbar"
               elsif Enemy.Ship.Crew(I).Health > 24 then
                 " -style yellow.Horizontal.TProgressbar"
               else " -style Horizontal.TProgressbar"));
         Add(ProgressBar, "Enemy's health");
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx 5");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & ProgressBar &
            " $combatframe.right.scrolly");
         OrderName :=
           To_Unbounded_String(Crew_Orders'Image(Enemy.Ship.Crew(I).Order));
         Replace_Slice
           (OrderName, 2, Length(OrderName),
            To_Lower(Slice(OrderName, 2, Length(OrderName))));
         Label :=
           Create
             (Frame & ".order" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(OrderName) & "}");
         Add(Label, "Enemy's current order.");
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-column 2 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx {0 5}");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & Label & " $combatframe.right.scrolly");
      end loop Show_Enemy_Crew_Loop;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(FrameName & ".right.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      Append(OrdersList, " {Back to the ship}");
      Frame.Name := New_String(FrameName & ".left.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, Frame);
      Show_Boarding_Party_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order /= BOARDING then
            goto End_Of_Loop;
         end if;
         Button :=
           Create
             (Frame & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(Player_Ship.Crew(I).Name) &
              "} -command {ShowCombatInfo player" &
              Positive'Image(Crew_Container.To_Index(I)) & "}");
         Add(Button, "Show more information about the crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Button,
            "-row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx {5 0}");
         ProgressBar :=
           Create
             (Frame & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(Player_Ship.Crew(I).Health) & " -length 150" &
              (if Player_Ship.Crew(I).Health > 74 then
                 " -style green.Horizontal.TProgressbar"
               elsif Player_Ship.Crew(I).Health > 24 then
                 " -style yellow.Horizontal.TProgressbar"
               else " -style Horizontal.TProgressbar"));
         Add(ProgressBar, "The crew member health.");
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx 5");
         Tcl_Eval
           (Get_Context,
            "SetScrollbarBindings " & ProgressBar &
            " $combatframe.left.scrolly");
         ComboBox :=
           Create
             (Frame & ".order" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-values [list " & To_String(OrdersList) &
              "] -state readonly -width 15");
         Current(ComboBox, Natural'Image(Boarding_Orders(OrderIndex)));
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetBoardingOrder" & Positive'Image(Crew_Container.To_Index(I)) &
            Positive'Image(OrderIndex) & "}");
         Add(ComboBox, "The crew member current order.");
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-column 2 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx {0 5}");
         OrderIndex := OrderIndex + 1;
         <<End_Of_Loop>>
      end loop Show_Boarding_Party_Loop;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(FrameName & ".left.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      Update_Messages;
   end UpdateBoardingUI;

   -- ****if* CUI/CUI.Next_Turn_Command
   -- FUNCTION
   -- Execute combat orders and go to next turn
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NextTurn
   -- SOURCE
   function Next_Turn_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Next_Turn_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CombatFrame: constant Ttk_Frame :=
        Get_Widget(Main_Paned & ".combatframe", Interp);
      Frame: Ttk_Frame := Get_Widget(CombatFrame & ".crew", Interp);
      Next_Button: constant Ttk_Button :=
        Get_Widget(CombatFrame & ".next", Interp);
   begin
      Combat_Turn;
      Update_Header;
      if End_Combat then
         Unbind_From_Main_Window
           (Interp, "<" & To_String(General_Accelerators(1)) & ">");
         Unbind_From_Main_Window
           (Interp, "<" & To_String(General_Accelerators(2)) & ">");
         Unbind_From_Main_Window
           (Interp, "<" & To_String(General_Accelerators(3)) & ">");
         Unbind_From_Main_Window
           (Interp, "<" & To_String(General_Accelerators(4)) & ">");
         Update_Combat_Ui;
         configure(Close_Button, "-command {ShowSkyMap}");
         Tcl_SetVar(Interp, "gamestate", "general");
         Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
         Frame.Name := New_String(Widget_Image(CombatFrame) & ".left");
         if Winfo_Get(Frame, "ismapped") = "1" then
            ShowCombatFrame(".combat");
         end if;
         Tcl.Tk.Ada.Grid.Grid_Remove(Next_Button);
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING and
        Winfo_Get(Frame, "ismapped") = "1" then
         UpdateBoardingUI;
         ShowCombatFrame(".boarding");
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order /= BOARDING and
        Winfo_Get(Frame, "ismapped") = "0" then
         Update_Combat_Ui;
         ShowCombatFrame(".combat");
         return TCL_OK;
      end if;
      if Winfo_Get(Frame, "ismapped") = "1" then
         Update_Combat_Ui;
      else
         UpdateBoardingUI;
      end if;
      return TCL_OK;
   end Next_Turn_Command;

   -- ****if* CUI/CUI.Show_Combat_UI_Command
   -- FUNCTION
   -- Show combat UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCombatUI
   -- SOURCE
   function Show_Combat_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      Show_Combat_Ui(False);
      return TCL_OK;
   end Show_Combat_UI_Command;

   -- ****if* CUI/CUI.Set_Combat_Order_Command
   -- FUNCTION
   -- Set combat order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatOrder Position
   -- Position argument can be pilot, engineer or number of the gun which
   -- gunner will take a new combat order
   -- SOURCE
   function Set_Combat_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      ComboBox: Ttk_ComboBox;
      GunIndex: Positive;
      FrameName: constant String :=
        Main_Paned & ".combatframe.crew.canvas.frame";
   begin
      ComboBox.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "pilot" then
         ComboBox.Name := New_String(FrameName & ".Pilot_Order");
         Pilot_Order := Positive'Value(Current(ComboBox)) + 1;
         if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            Add_Message
              ("Order for " &
               To_String(Player_Ship.Crew(Find_Member(PILOT)).Name) &
               " was set on: " & Get(ComboBox),
               COMBATMESSAGE);
         else
            Add_Message
              ("Order for ship was set on: " & Get(ComboBox), COMBATMESSAGE);
         end if;
      elsif CArgv.Arg(Argv, 1) = "engineer" then
         ComboBox.Name := New_String(FrameName & ".Engineer_Order");
         Engineer_Order := Positive'Value(Current(ComboBox)) + 1;
         if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            Add_Message
              ("Order for " &
               To_String(Player_Ship.Crew(Find_Member(ENGINEER)).Name) &
               " was set on: " & Get(ComboBox),
               COMBATMESSAGE);
         else
            Add_Message
              ("Order for ship was set on: " & Get(ComboBox), COMBATMESSAGE);
         end if;
      else
         ComboBox.Name :=
           New_String(FrameName & ".gunorder" & CArgv.Arg(Argv, 1));
         GunIndex := Positive'Value(CArgv.Arg(Argv, 1));
         Guns(GunIndex)(2) := Positive'Value(Current(ComboBox)) + 1;
         Guns(GunIndex)(3) :=
           (if Current(ComboBox) = "0" then 0
            else Modules_List
                (Player_Ship.Modules(Guns(GunIndex)(1)).Proto_Index)
                .Speed);
         Add_Message
           ("Order for " &
            To_String
              (Player_Ship.Crew
                 (Player_Ship.Modules(Guns(GunIndex)(1)).Owner(1))
                 .Name) &
            " was set on: " & Get(ComboBox),
            COMBATMESSAGE);
      end if;
      Update_Messages;
      return TCL_OK;
   end Set_Combat_Order_Command;

   -- ****o* CUI/CUI.Set_Boarding_Order_Command
   -- FUNCTION
   -- Set boarding order for the selected player's ship crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBoardingOrder EnemyIndex
   -- EnemyIndex parameter is the index of the enemy in the enemy ship crew
   -- which will be set as target for the selected player ship crew member.
   -- SOURCE
   function Set_Boarding_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Boarding_Order_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Combobox: constant Ttk_ComboBox :=
        Get_Widget
          (Main_Paned & ".combatframe.left.canvas.frame.order" &
           CArgv.Arg(Argv, 1),
           Interp);
   begin
      Boarding_Orders(Positive'Value(CArgv.Arg(Argv, 2))) :=
        (if
           Natural'Value(Current(Combobox)) + 1 >
           Natural(Enemy.Ship.Crew.Length)
         then -1
         else Natural'Value(Current(Combobox)) + 1);
      return TCL_OK;
   end Set_Boarding_Order_Command;

   -- ****o* CUI/CUI.Set_Combat_Party_Command
   -- FUNCTION
   -- Set combat party (boarding or defenders)
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatParty partytype
   -- Partytype is a type of party to set. Possible options are boarding or
   -- defenders
   -- SOURCE
   function Set_Combat_Party_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Party_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      CrewDialog: constant Ttk_Frame :=
        Create_Dialog
          (".boardingdialog",
           "Assign a crew members to " &
           (if CArgv.Arg(Argv, 1) = "boarding" then "boarding party"
            else "defenders"),
           245);
      YScroll: constant Ttk_Scrollbar :=
        Create
          (CrewDialog & ".yscroll",
           "-orient vertical -command [list " & CrewDialog & ".canvas yview]");
      CrewCanvas: constant Tk_Canvas :=
        Create
          (CrewDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      CrewFrame: constant Ttk_Frame := Create(CrewCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (CrewDialog & ".button",
           "-text Close -command {CloseDialog " & Widget_Image(CrewDialog) &
           "}");
      Height: Positive := 10;
      Width: Positive := 250;
      CrewButton: Ttk_CheckButton;
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv, 1) = "boarding" then BOARDING else DEFEND);
   begin
      Tcl.Tk.Ada.Grid.Grid(CrewCanvas, "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, "-sticky ns -padx {0 5} -pady {5 0} -row 1 -column 1");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-pady {0 5} -columnspan 2");
      Focus(CloseButton);
      Autoscroll(YScroll);
      Show_Player_Ship_Crew_Loop :
      for I in Player_Ship.Crew.Iterate loop
         CrewButton :=
           Create
             (CrewFrame & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(Player_Ship.Crew(I).Name) &
              "} -command {SetPartyOrder" &
              Positive'Image(Crew_Container.To_Index(I)) & " " &
              CArgv.Arg(Argv, 1) & "}");
         if Player_Ship.Crew(I).Order /= Order then
            Tcl_SetVar(Interp, Widget_Image(CrewButton), "0");
         else
            Tcl_SetVar(Interp, Widget_Image(CrewButton), "1");
         end if;
         Tcl.Tk.Ada.Pack.Pack(CrewButton, "-anchor w");
         Height := Height + Positive'Value(Winfo_Get(CrewButton, "reqheight"));
         if Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10 > Width then
            Width := Positive'Value(Winfo_Get(CrewButton, "reqwidth")) + 10;
         end if;
         Bind(CrewButton, "<Escape>", "{" & CloseButton & " invoke;break}");
         Bind
           (CrewButton, "<Tab>",
            "{focus [GetActiveButton" &
            Positive'Image(Crew_Container.To_Index(I)) & "];break}");
      end loop Show_Player_Ship_Crew_Loop;
      if Height > 500 then
         Height := 500;
      end if;
      Canvas_Create
        (CrewCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(CrewFrame));
      Tcl_Eval(Interp, "update");
      configure
        (CrewCanvas,
         "-scrollregion [list " & BBox(CrewCanvas, "all") & "] -height" &
         Positive'Image(Height) & " -width" & Positive'Image(Width));
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Tab>", "{focus [GetActiveButton 0];break}");
      Show_Dialog(Dialog => CrewDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Set_Combat_Party_Command;

   -- ****if* CUI/CUI.Set_Combat_Position_Command
   -- FUNCTION
   -- Set crew member position (pilot, engineer, gunner) in combat
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatPosition position
   -- Position is the combat crew member position which will be set
   -- SOURCE
   function Set_Combat_Position_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Position_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ComboBox: Ttk_ComboBox;
      GunIndex: Positive;
      CrewIndex: Natural;
      FrameName: constant String :=
        ".gameframe.paned.combatframe.crew.canvas.frame";
   begin
      ComboBox.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "pilot" then
         ComboBox.Name := New_String(FrameName & ".pilotcrew");
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            Give_Orders(Player_Ship, CrewIndex, PILOT);
         else
            CrewIndex := Find_Member(PILOT);
            if CrewIndex > 0 then
               Give_Orders(Player_Ship, CrewIndex, REST);
            end if;
         end if;
      elsif CArgv.Arg(Argv, 1) = "engineer" then
         ComboBox.Name := New_String(FrameName & ".engineercrew");
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            Give_Orders(Player_Ship, CrewIndex, ENGINEER);
         else
            CrewIndex := Find_Member(ENGINEER);
            if CrewIndex > 0 then
               Give_Orders(Player_Ship, CrewIndex, REST);
            end if;
         end if;
      else
         ComboBox.Name :=
           New_String(FrameName & ".guncrew" & CArgv.Arg(Argv, 2));
         GunIndex := Positive'Value(CArgv.Arg(Argv, 2));
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            Give_Orders(Player_Ship, CrewIndex, GUNNER, Guns(GunIndex)(1));
         else
            CrewIndex := Player_Ship.Modules(Guns(GunIndex)(1)).Owner(1);
            if CrewIndex > 0 then
               Give_Orders(Player_Ship, CrewIndex, REST);
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
   -- ClientData - Custom data send to the command. Unused
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      CrewIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      Info: Unbounded_String;
   begin
      Info := To_Unbounded_String("Uses: ");
      if CArgv.Arg(Argv, 1) = "player" then
         Show_Player_Crew_Equipment_Loop :
         for Item of Player_Ship.Crew(CrewIndex).Equipment loop
            if Item /= 0 then
               Append
                 (Info,
                  LF &
                  Get_Item_Name
                    (Inventory_Container.Element
                       (Container => Player_Ship.Crew(CrewIndex).Inventory,
                        Index => Item)));
            end if;
         end loop Show_Player_Crew_Equipment_Loop;
      else
         Show_Enemy_Crew_Equipment_Loop :
         for Item of Enemy.Ship.Crew(CrewIndex).Equipment loop
            if Item /= 0 then
               Append
                 (Info,
                  LF &
                  Get_Item_Name
                    (Inventory_Container.Element
                       (Container => Enemy.Ship.Crew(CrewIndex).Inventory,
                        Index => Item)));
            end if;
         end loop Show_Enemy_Crew_Equipment_Loop;
      end if;
      Show_Info(Text => To_String(Info), Title => "More info");
      return TCL_OK;
   end Show_Combat_Info_Command;

   -- ****o* CUI/CUI.Combat_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of the combat UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CombatMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Combat_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Combat_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      type Frames_Array is array(Positive range <>) of Frame_Info;
      Combat_Frames: constant Frames_Array :=
        ((To_Unbounded_String("crew"), 0, 0),
         (To_Unbounded_String("damage"), 0, 1),
         (To_Unbounded_String("enemy"), 1, 0),
         (To_Unbounded_String("status"), 1, 1));
      Boarding_Frames: constant Frames_Array :=
        ((To_Unbounded_String("left"), 0, 0),
         (To_Unbounded_String("right"), 1, 0));
      Frame: Ttk_Frame := Get_Widget(Main_Paned & ".combatframe.crew", Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (Main_Paned & ".combatframe." & CArgv.Arg(Argv, 1) &
           ".canvas.frame.maxmin",
           Interp);
      Frames: constant Frames_Array :=
        (if CArgv.Arg(Argv, 3) = "combat" then Combat_Frames
         else Boarding_Frames);
   begin
      if CArgv.Arg(Argv, 2) /= "show" then
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Main_Paned & ".combatframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame,
                  "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop Show_Frames_Loop;
         configure
           (Button,
            "-image movemapupicon -command {CombatMaxMin " &
            CArgv.Arg(Argv, 1) & " show " & CArgv.Arg(Argv, 3) & "}");
      else
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Main_Paned & ".combatframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Hide_Frames_Loop;
         configure
           (Button,
            "-image movemapdownicon -command {CombatMaxMin " &
            CArgv.Arg(Argv, 1) & " hide " & CArgv.Arg(Argv, 3) & "}");
      end if;
      return TCL_OK;
   end Combat_Max_Min_Command;

   procedure Show_Combat_Ui(New_Combat: Boolean := True) is
      use Tiny_String;

      CombatFrame: constant Ttk_Frame :=
        Get_Widget(Main_Paned & ".combatframe");
      CombatStarted: Boolean;
      Button: constant Ttk_Button := Get_Widget(CombatFrame & ".next");
      EnemyFrame: constant Ttk_Frame := Get_Widget(CombatFrame & ".status");
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
      if New_Combat then
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0
           and then Enemy_Name /=
             Proto_Ships_List
               (Events_List
                  (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                  .Ship_Index)
               .Name then
            CombatStarted :=
              Start_Combat
                (Events_List
                   (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                   .Ship_Index,
                 False);
            if not CombatStarted then
               return;
            end if;
         end if;
         if Winfo_Get(CombatFrame, "exists") = "0" then
            Tcl_EvalFile
              (Get_Context,
               To_String(Data_Directory) & "ui" & Dir_Separator &
               "combat.tcl");
            Pilot_Order := 2;
            Engineer_Order := 3;
            Add_Command("SetPartyOrder", Set_Party_Order_Command'Access);
            Add_Command("NextTurn", Next_Turn_Command'Access);
            Add_Command("ShowCombatUI", Show_Combat_UI_Command'Access);
            Add_Command("SetCombatOrder", Set_Combat_Order_Command'Access);
            Add_Command("SetBoardingOrder", Set_Boarding_Order_Command'Access);
            Add_Command("SetCombatParty", Set_Combat_Party_Command'Access);
            Add_Command
              ("SetCombatPosition", Set_Combat_Position_Command'Access);
            Add_Command("ShowCombatInfo", Show_Combat_Info_Command'Access);
            Add_Command("CombatMaxMin", Combat_Max_Min_Command'Access);
         else
            Tcl.Tk.Ada.Grid.Grid(Button);
            Tcl.Tk.Ada.Grid.Grid(EnemyFrame);
         end if;
         configure(Close_Button, "-command ShowCombatUI");
         Tcl_SetVar(Get_Context, "gamestate", "combat");
         Back_To_Work_Loop :
         for Member of Player_Ship.Crew loop
            if Member.Order = REST
              and then Member.Previous_Order in PILOT | ENGINEER | GUNNER then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               Add_Message
                 (To_String(Member.Name) & " back to work for combat.",
                  ORDERMESSAGE);
            end if;
         end loop Back_To_Work_Loop;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING then
         UpdateBoardingUI;
         ShowCombatFrame(".boarding");
      else
         Update_Combat_Ui;
         ShowCombatFrame(".combat");
      end if;
      Show_Screen("combatframe");
   end Show_Combat_Ui;

end Combat.UI;
