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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with Crew; use Crew;
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

   -- ****if* CUI/CUI.GetGunSpeed
   -- FUNCTION
   -- Get information about fire rate of selected gun with selected order
   -- PARAMETERS
   -- Position - Number of gun to check
   -- Index    - Index of the gunner's order
   -- RESULT
   -- String with info about gun fire rate
   -- SOURCE
   function GetGunSpeed(Position: Natural; Index: Positive) return String is
      -- ****
      GunSpeed: Integer;
      Firerate: Unbounded_String;
   begin
      GunSpeed :=
        Modules_List(PlayerShip.Modules(Guns(Position)(1)).ProtoIndex).Speed;
      case Index is
         when 1 =>
            GunSpeed := 0;
         when 3 =>
            null;
         when others =>
            GunSpeed :=
              (if GunSpeed > 0 then
                 Integer(Float'Ceiling(Float(GunSpeed) / 2.0))
               else GunSpeed - 1);
      end case;
      if GunSpeed > 0 then
         Firerate :=
           To_Unbounded_String
             ("(" & Trim(Integer'Image(GunSpeed), Both) & "/round)");
      elsif GunSpeed < 0 then
         Firerate :=
           To_Unbounded_String
             ("(1/" & Trim(Integer'Image(GunSpeed), Both) & " rounds)");
      end if;
      return To_String(Firerate);
   end GetGunSpeed;

   -- ****if* CUI/CUI.UpdateMessages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure UpdateMessages is
      -- ****
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      CurrentTurnTime: Unbounded_String := To_Unbounded_String(FormatedTime);
      MessagesView: constant Tk_Text :=
        Get_Widget(".gameframe.paned.controls.messages.view");
      procedure ShowMessage is
         TagNames: constant array(1 .. 5) of Unbounded_String :=
           (To_Unbounded_String("yellow"), To_Unbounded_String("green"),
            To_Unbounded_String("red"), To_Unbounded_String("blue"),
            To_Unbounded_String("cyan"));
      begin
         if Unbounded_Slice(Message.Message, 1, Length(CurrentTurnTime)) =
           CurrentTurnTime then
            if Message.Color = WHITE then
               Insert
                 (MessagesView, "end", "{" & To_String(Message.Message) & "}");
            else
               Insert
                 (MessagesView, "end",
                  "{" & To_String(Message.Message) & "} [list " &
                  To_String(TagNames(Message_Color'Pos(Message.Color))) & "]");
            end if;
         else
            Insert
              (MessagesView, "end",
               "{" & To_String(Message.Message) & "} [list gray]");
         end if;
      end ShowMessage;
   begin
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state normal");
      Delete(MessagesView, "1.0", "end");
      if LoopStart = 0 then
         Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state disable");
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      Message := GetMessage(GetLastMessageIndex);
      if Unbounded_Slice(Message.Message, 1, Length(CurrentTurnTime)) /=
        CurrentTurnTime then
         CurrentTurnTime :=
           Unbounded_Slice(Message.Message, 1, Length(CurrentTurnTime));
      end if;
      if Game_Settings.Messages_Order = OLDER_FIRST then
         Show_Older_Messages_First_Loop :
         for I in LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            if (GetLastMessageIndex + I + 1) >= MessagesStarts then
               ShowMessage;
               if I < -1 then
                  Insert(MessagesView, "end", "{" & LF & "}");
               end if;
            end if;
         end loop Show_Older_Messages_First_Loop;
         See(MessagesView, "end");
      else
         Show_New_Messages_First_Loop :
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            exit Show_New_Messages_First_Loop when
              (GetLastMessageIndex + I + 1) <
              MessagesStarts;
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop Show_New_Messages_First_Loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state disable");
   end UpdateMessages;

   -- ****if* CUI/CUI.UpdateCombatUI
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure UpdateCombatUI is
      -- ****
      Tokens: Slice_Set;
      Frame: Ttk_Frame :=
        Get_Widget(".gameframe.paned.combatframe.crew.canvas.frame");
      Label: Ttk_Label;
      ComboBox: Ttk_ComboBox := Get_Widget(Frame & ".pilotcrew");
      GunnersOrders: constant array(1 .. 6) of Unbounded_String :=
        (To_Unbounded_String("{Don't shoot"),
         To_Unbounded_String("{Precise fire "),
         To_Unbounded_String("{Fire at will "),
         To_Unbounded_String("{Aim for their engine "),
         To_Unbounded_String("{Aim for their weapon "),
         To_Unbounded_String("{Aim for their hull "));
      GunIndex, GunnerOrders, EnemyInfo, ProgressBarStyle,
      Font: Unbounded_String;
      HaveAmmo, HasDamage: Boolean;
      AmmoAmount, AmmoIndex, Row, Rows: Natural := 0;
      ProgressBar: Ttk_ProgressBar;
      DamagePercent: Float;
      CombatCanvas: Tk_Canvas;
      Has_Gunner: Boolean := False;
      function GetCrewList(Position: Natural) return String is
         SkillIndex, SkillValue: Natural := 0;
         SkillString: Unbounded_String;
         CrewList: Unbounded_String := To_Unbounded_String("Nobody");
      begin
         Get_Highest_Skills_Loop :
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            case Position is
               when 0 =>
                  if GetSkillLevel(PlayerShip.Crew(I), Piloting_Skill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), Piloting_Skill);
                  end if;
               when 1 =>
                  if GetSkillLevel(PlayerShip.Crew(I), Engineering_Skill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), Engineering_Skill);
                  end if;
               when others =>
                  if GetSkillLevel(PlayerShip.Crew(I), Gunnery_Skill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), Gunnery_Skill);
                  end if;
            end case;
         end loop Get_Highest_Skills_Loop;
         Mark_Skills_Loop :
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew(I).Skills.Length > 0 then
               SkillString := Null_Unbounded_String;
               case Position is
                  when 0 =>
                     if GetSkillLevel(PlayerShip.Crew(I), Piloting_Skill) >
                       0 then
                        SkillString := To_Unbounded_String(" +");
                     end if;
                  when 1 =>
                     if GetSkillLevel(PlayerShip.Crew(I), Engineering_Skill) >
                       0 then
                        SkillString := To_Unbounded_String(" +");
                     end if;
                  when others =>
                     if GetSkillLevel(PlayerShip.Crew(I), Gunnery_Skill) >
                       0 then
                        SkillString := To_Unbounded_String(" +");
                     end if;
               end case;
               if I = SkillIndex then
                  SkillString := SkillString & To_Unbounded_String("+");
               end if;
               Append
                 (CrewList,
                  " {" & PlayerShip.Crew(I).Name & SkillString & "}");
            end if;
         end loop Mark_Skills_Loop;
         return To_String(CrewList);
      end GetCrewList;
   begin
      configure(ComboBox, "-values [list " & GetCrewList(0) & "]");
      Current(ComboBox, Natural'Image(FindMember(Pilot)));
      ComboBox.Name := New_String(Frame & ".pilotorder");
      Current(ComboBox, Integer'Image(PilotOrder - 1));
      if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) and
        FindMember(Pilot) = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(ComboBox);
      else
         Tcl.Tk.Ada.Grid.Grid(ComboBox);
      end if;
      ComboBox.Name := New_String(Frame & ".engineercrew");
      configure(ComboBox, "-values [list " & GetCrewList(1) & "]");
      Current(ComboBox, Natural'Image(FindMember(Engineer)));
      ComboBox.Name := New_String(Frame & ".engineerorder");
      Current(ComboBox, Natural'Image(EngineerOrder - 1));
      if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) and
        FindMember(Engineer) = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(ComboBox);
      else
         Tcl.Tk.Ada.Grid.Grid(ComboBox);
      end if;
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Positive'Value(Slice(Tokens, 2));
      Delete_Widgets(3, (Rows - 1), Frame);
      Show_Guns_Info_Loop :
      for I in Guns.Iterate loop
         HaveAmmo := False;
         Has_Gunner := False;
         declare
            AmmoIndex: constant Natural :=
              (if PlayerShip.Modules(Guns(I)(1)).MType = GUN then
                 PlayerShip.Modules(Guns(I)(1)).AmmoIndex
               else PlayerShip.Modules(Guns(I)(1)).HarpoonIndex);
         begin
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
            Find_Ammo_Loop :
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
            end loop Find_Ammo_Loop;
         end if;
         GunIndex :=
           To_Unbounded_String
             (Trim(Positive'Image(Guns_Container.To_Index(I)), Left));
         Label :=
           Create
             (Frame & ".gunlabel" & To_String(GunIndex),
              "-text {" & To_String(PlayerShip.Modules(Guns(I)(1)).Name) &
              ":" & LF & "(Ammo:" & Natural'Image(AmmoAmount) & ")}");
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
            " -padx {5 0}");
         ComboBox :=
           Create
             (Frame & ".guncrew" & To_String(GunIndex),
              "-values [list " & GetCrewList(2) &
              "] -width 10 -state readonly");
         if PlayerShip.Modules(Guns(I)(1)).Owner(1) /= 0 then
            if PlayerShip.Crew(PlayerShip.Modules(Guns(I)(1)).Owner(1)).Order =
              Gunner then
               Current
                 (ComboBox,
                  Positive'Image(PlayerShip.Modules(Guns(I)(1)).Owner(1)));
               Has_Gunner := True;
            else
               Current(ComboBox, "0");
            end if;
         else
            Current(ComboBox, "0");
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
            " -column 1");
         Bind
           (ComboBox, "<Return>",
            "{InvokeButton .gameframe.paned.combatframe.next}");
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetCombatPosition gunner " & To_String(GunIndex) & "}");
         Add
           (ComboBox,
            "Select the crew member which will be the operate the gun during" &
            LF &
            "the combat. The sign + after name means that this crew member" &
            LF &
            "has gunnery skill, the sign ++ after name means that his/her" &
            LF & "gunnery skill is the best in the crew");
         GunnerOrders := Null_Unbounded_String;
         Show_Gun_Orders_Loop :
         for J in GunnersOrders'Range loop
            Append
              (GunnerOrders,
               " " & GunnersOrders(J) &
               GetGunSpeed(Guns_Container.To_Index(I), J) & "}");
         end loop Show_Gun_Orders_Loop;
         ComboBox := Get_Widget(Frame & ".gunorder" & To_String(GunIndex));
         if Winfo_Get(ComboBox, "exists") = "0" then
            ComboBox :=
               Create
                  (Frame & ".gunorder" & To_String(GunIndex),
               "-values [list" & To_String(GunnerOrders) & "] -state readonly");
         end if;
         Current(ComboBox, Natural'Image(Guns(I)(2) - 1));
         if Has_Gunner then
            Tcl.Tk.Ada.Grid.Grid
              (ComboBox,
               "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
               " -column 2 -padx {0 5}");
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(ComboBox);
         end if;
         Bind
           (ComboBox, "<Return>",
            "{InvokeButton .gameframe.paned.combatframe.next}");
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetCombatOrder " & To_String(GunIndex) & "}");
         Add
           (ComboBox,
            "Select the order for the gunner. Shooting in the selected" & LF &
            "part of enemy ship is less precise but always hit the" & LF &
            "selected part.");
      end loop Show_Guns_Info_Loop;
      -- Show boarding/defending info
      if (HarpoonDuration > 0 or Enemy.HarpoonDuration > 0) and
        ProtoShips_List(EnemyShipIndex).Crew.Length > 0 then
         declare
            Button: Ttk_Button :=
              Create
                (Frame & ".boarding",
                 "-text {Boarding party:} -command {SetCombatParty boarding}");
            BoardingParty, Defenders: Unbounded_String;
            LabelLength: constant Positive :=
              Positive'Value
                (Winfo_Get
                   (Ttk_Label'(Get_Widget(Frame & ".engineercrew")),
                    "reqwidth")) +
              Positive'Value
                (Winfo_Get
                   (Ttk_Label'(Get_Widget(Frame & ".engineerorder")),
                    "reqwidth"));
         begin
            Tcl.Tk.Ada.Grid.Grid(Button, "-padx 5");
            Add
              (Button,
               "Set your boarding party. If you join it, you will be able" &
               LF & "to give orders them, but not your gunners or engineer.");
            Button :=
              Create
                (Frame & ".defending",
                 "-text {Defenders:} -command {SetCombatParty defenders}");
            Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -padx 5 -pady 5");
            Add(Button, "Set your ship's defenders against the enemy party.");
            Set_Boarding_And_Defenders_Loop :
            for Member of PlayerShip.Crew loop
               if Member.Order = Boarding then
                  Append(BoardingParty, Member.Name & ", ");
               elsif Member.Order = Defend then
                  Append(Defenders, Member.Name & ", ");
               end if;
            end loop Set_Boarding_And_Defenders_Loop;
            if BoardingParty /= Null_Unbounded_String then
               BoardingParty :=
                 Unbounded_Slice(BoardingParty, 1, Length(BoardingParty) - 2);
            end if;
            Label :=
              Create
                (Frame & ".boardparty",
                 "-text {" & To_String(BoardingParty) & "} -wraplength" &
                 Positive'Image(LabelLength));
            Tcl.Tk.Ada.Grid.Grid
              (Label,
               "-row" & Positive'Image(Positive(Guns.Length) + 3) &
               " -column 1 -columnspan 2 -sticky w");
            if Defenders /= Null_Unbounded_String then
               Defenders :=
                 Unbounded_Slice(Defenders, 1, Length(Defenders) - 2);
            end if;
            Label :=
              Create
                (Frame & ".defenders",
                 "-text {" & To_String(Defenders) & "} -wraplength" &
                 Positive'Image(LabelLength));
            Tcl.Tk.Ada.Grid.Grid
              (Label,
               "-row" & Positive'Image(Positive(Guns.Length) + 4) &
               " -column 1 -columnspan 2 -sticky w");
         end;
      end if;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.crew.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      -- Show player ship damage info if needed
      Frame.Name :=
        New_String(".gameframe.paned.combatframe.damage.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(0, Rows - 1, Frame);
      HasDamage := False;
      Show_Player_Ship_Damage_Loop :
      for Module of PlayerShip.Modules loop
         if Module.Durability = Module.MaxDurability then
            goto End_Of_Player_Ship_Damage_Loop;
         end if;
         Font :=
           (if Module.Durability = 0 then
              To_Unbounded_String(" -font OverstrikedFont -style Gray.TLabel")
            else Null_Unbounded_String);
         Label :=
           Create
             (Frame & ".lbl" & Trim(Natural'Image(Row), Left),
              "-text {" & To_String(Module.Name) & "}" & To_String(Font));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -sticky w -padx 5");
         DamagePercent :=
           (Float(Module.Durability) / Float(Module.MaxDurability));
         ProgressBarStyle :=
           (if DamagePercent = 1.0 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif DamagePercent > 0.24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         ProgressBar :=
           Create
             (Frame & ".dmg" & Trim(Natural'Image(Row), Left),
              "-orient horizontal -length 150 -maximum 1.0 -value" &
              Float'Image(DamagePercent) & To_String(ProgressBarStyle));
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar, "-row" & Natural'Image(Row) & " -column 1");
         Tcl.Tk.Ada.Grid.Column_Configure(Frame, ProgressBar, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure(Frame, ProgressBar, "-weight 1");
         Row := Row + 1;
         HasDamage := True;
         <<End_Of_Player_Ship_Damage_Loop>>
      end loop Show_Player_Ship_Damage_Loop;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.damage.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      Frame.Name := New_String(".gameframe.paned.combatframe.damage");
      if not HasDamage then
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      else
         Tcl.Tk.Ada.Grid.Grid(Frame);
      end if;
      Append(EnemyInfo, "Name: " & EnemyName & LF);
      Append(EnemyInfo, "Type: " & Enemy.Ship.Name & LF);
      Append(EnemyInfo, "Home: " & SkyBases(Enemy.Ship.HomeBase).Name & LF);
      Append(EnemyInfo, "Distance: ");
      if Enemy.Distance >= 15_000 then
         Append(EnemyInfo, "Escaped");
      elsif Enemy.Distance in 10_000 .. 15_000 then
         Append(EnemyInfo, "Long");
      elsif Enemy.Distance in 5_000 .. 10_000 then
         Append(EnemyInfo, "Medium");
      elsif Enemy.Distance in 1_000 .. 5_000 then
         Append(EnemyInfo, "Short");
      else
         Append(EnemyInfo, "Close");
      end if;
      Append(EnemyInfo, LF);
      Append(EnemyInfo, "Status: ");
      if Enemy.Distance < 15000 then
         if Enemy.Ship.Modules(1).Durability = 0 then
            Append(EnemyInfo, "Destroyed");
         else
            declare
               EnemyStatus: Unbounded_String := To_Unbounded_String("Ok");
            begin
               Check_Enemy_Ship_Status_Loop :
               for Module of Enemy.Ship.Modules loop
                  if Module.Durability < Module.MaxDurability then
                     EnemyStatus := To_Unbounded_String("Damaged");
                     exit Check_Enemy_Ship_Status_Loop;
                  end if;
               end loop Check_Enemy_Ship_Status_Loop;
               Append(EnemyInfo, EnemyStatus);
            end;
            Check_Enemy_Status_Loop :
            for Module of Enemy.Ship.Modules loop
               if Module.Durability > 0 then
                  case Modules_List(Module.ProtoIndex).MType is
                     when ARMOR =>
                        Append(EnemyInfo, " (armored)");
                     when GUN =>
                        Append(EnemyInfo, " (gun)");
                     when BATTERING_RAM =>
                        Append(EnemyInfo, " (battering ram)");
                     when HARPOON_GUN =>
                        Append(EnemyInfo, " (harpoon gun)");
                     when others =>
                        null;
                  end case;
               end if;
            end loop Check_Enemy_Status_Loop;
         end if;
      else
         Append(EnemyInfo, "Unknown");
      end if;
      Append(EnemyInfo, LF);
      Append(EnemyInfo, "Speed: ");
      if Enemy.Distance < 15000 then
         case Enemy.Ship.Speed is
            when Ships.FULL_STOP =>
               Append(EnemyInfo, "Stopped");
            when QUARTER_SPEED =>
               Append(EnemyInfo, "Slow");
            when HALF_SPEED =>
               Append(EnemyInfo, "Medium");
            when FULL_SPEED =>
               Append(EnemyInfo, "Fast");
            when others =>
               null;
         end case;
         if Enemy.Ship.Speed /= Ships.FULL_STOP then
            declare
               SpeedDiff: constant Integer :=
                 RealSpeed(Enemy.Ship) - RealSpeed(PlayerShip);
            begin
               if SpeedDiff > 250 then
                  Append(EnemyInfo, " (much faster)");
               elsif SpeedDiff > 0 then
                  Append(EnemyInfo, " (faster)");
               elsif SpeedDiff = 0 then
                  Append(EnemyInfo, " (equal)");
               elsif SpeedDiff > -250 then
                  Append(EnemyInfo, " (slower)");
               else
                  Append(EnemyInfo, " (much slower)");
               end if;
            end;
         end if;
      else
         Append(EnemyInfo, "Unknown");
      end if;
      if Length(Enemy.Ship.Description) > 0 then
         Append(EnemyInfo, LF & LF & Enemy.Ship.Description);
      end if;
      Label := Get_Widget(".gameframe.paned.combatframe.enemy.canvas.info");
      configure(Label, "-text {" & To_String(EnemyInfo) & "}");
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.enemy.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      declare
         ModuleName: Unbounded_String;
      begin
         Frame.Name :=
           New_String(".gameframe.paned.combatframe.status.canvas.frame");
         Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
         Rows := Natural'Value(Slice(Tokens, 2));
         Delete_Widgets(0, Rows - 1, Frame);
         if Enemy.Ship.Modules(1).Durability = 0 then
            Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            goto End_Of_Enemy_Modules_Block;
         end if;
         Row := 0;
         Show_Enemy_Ship_Status_Loop :
         for I in Enemy.Ship.Modules.Iterate loop
            if Enemy.Distance > 1000 then
               ModuleName :=
                 To_Unbounded_String
                   (GetModuleType(Enemy.Ship.Modules(I).ProtoIndex));
            else
               ModuleName :=
                 Modules_List(Enemy.Ship.Modules(I).ProtoIndex).Name;
            end if;
            Font :=
              (if Enemy.Ship.Modules(I).Durability = 0 then
                 To_Unbounded_String
                   (" -font OverstrikedFont -style Gray.TLabel")
               else Null_Unbounded_String);
            Label :=
              Create
                (Frame & ".lbl" & Trim(Natural'Image(Row), Left),
                 "-text {" & To_String(ModuleName) & "}" & To_String(Font));
            Tcl.Tk.Ada.Grid.Grid
              (Label,
               "-row" & Natural'Image(Row) & " -column 0 -sticky w -padx 5");
            DamagePercent :=
              ((Float(Enemy.Ship.Modules(I).Durability) /
                Float(Enemy.Ship.Modules(I).MaxDurability)));
            ProgressBarStyle :=
              (if DamagePercent = 1.0 then
                 To_Unbounded_String(" -style green.Horizontal.TProgressbar")
               elsif DamagePercent > 0.24 then
                 To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
               else To_Unbounded_String(" -style Horizontal.TProgressbar"));
            ProgressBar :=
              Create
                (Frame & ".dmg" & Trim(Natural'Image(Row), Left),
                 "-orient horizontal -length 150 -maximum 1.0 -value" &
                 Float'Image(DamagePercent) & To_String(ProgressBarStyle));
            Tcl.Tk.Ada.Grid.Grid
              (ProgressBar, "-row" & Natural'Image(Row) & " -column 1");
            Tcl.Tk.Ada.Grid.Column_Configure(Frame, ProgressBar, "-weight 1");
            Tcl.Tk.Ada.Grid.Row_Configure(Frame, ProgressBar, "-weight 1");
            Row := Row + 1;
         end loop Show_Enemy_Ship_Status_Loop;
         <<End_Of_Enemy_Modules_Block>>
      end;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.status.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      UpdateMessages;
   end UpdateCombatUI;

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
        (if CArgv.Arg(Argv, 2) = "boarding" then Boarding else Defend);
   begin
      Give_Boarding_Orders_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Boarding then
            OrderIndex := OrderIndex + 1;
         end if;
         if Crew_Container.To_Index(I) = MemberIndex then
            if PlayerShip.Crew(I).Order /= Order then
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Order, 0);
               if Order = Boarding then
                  BoardingOrders.Append(New_Item => 0);
               end if;
            else
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
               if Order = Boarding then
                  BoardingOrders.Delete(Index => OrderIndex);
               end if;
               OrderIndex := OrderIndex - 1;
            end if;
            exit Give_Boarding_Orders_Loop;
         end if;
      end loop Give_Boarding_Orders_Loop;
      UpdateCombatUI;
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
      OrdersList, OrderName: Unbounded_String;
      Frame: Ttk_Frame :=
        Get_Widget(".gameframe.paned.combatframe.right.canvas.frame");
      Label: Ttk_Label;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ProgressBar: Ttk_ProgressBar;
      ComboBox: Ttk_ComboBox;
      OrderIndex: Positive := 1;
      CombatCanvas: Tk_Canvas;
      Button: Ttk_Button;
      ProgressBarStyle: Unbounded_String;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, Frame);
      Show_Enemy_Crew_Loop :
      for I in Enemy.Ship.Crew.Iterate loop
         Append(OrdersList, "{Attack " & Enemy.Ship.Crew(I).Name & "} ");
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
         ProgressBarStyle :=
           (if Enemy.Ship.Crew(I).Health > 74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif Enemy.Ship.Crew(I).Health > 24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         ProgressBar :=
           Create
             (Frame & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(Enemy.Ship.Crew(I).Health) & " -length 150" &
              To_String(ProgressBarStyle));
         Add(ProgressBar, "Enemy's health");
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx 5");
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
      end loop Show_Enemy_Crew_Loop;
      Tcl_Eval(Get_Context, "update");
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.right.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      Append(OrdersList, " {Back to the ship}");
      Frame.Name :=
        New_String(".gameframe.paned.combatframe.left.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, Frame);
      Show_Boarding_Party_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order /= Boarding then
            goto End_Of_Loop;
         end if;
         Button :=
           Create
             (Frame & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) &
              "} -command {ShowCombatInfo player" &
              Positive'Image(Crew_Container.To_Index(I)) & "}");
         Add(Button, "Show more information about the crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Button,
            "-row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx {5 0}");
         ProgressBarStyle :=
           (if PlayerShip.Crew(I).Health > 74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif PlayerShip.Crew(I).Health > 24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         ProgressBar :=
           Create
             (Frame & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(PlayerShip.Crew(I).Health) & " -length 150" &
              To_String(ProgressBarStyle));
         Add(ProgressBar, "The crew member health.");
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)) &
            " -padx 5");
         ComboBox :=
           Create
             (Frame & ".order" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-values [list " & To_String(OrdersList) &
              "] -state readonly -width 15");
         Current(ComboBox, Natural'Image(BoardingOrders(OrderIndex)));
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
      CombatCanvas := Get_Widget(".gameframe.paned.combatframe.left.canvas");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
      Xview_Move_To(CombatCanvas, "0.0");
      Yview_Move_To(CombatCanvas, "0.0");
      UpdateMessages;
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
        Get_Widget(".gameframe.paned.combatframe", Interp);
      Frame: Ttk_Frame := Get_Widget(CombatFrame & ".crew", Interp);
      Button: Ttk_Button := Get_Widget(CombatFrame & ".next", Interp);
   begin
      CombatTurn;
      UpdateHeader;
      if EndCombat then
         UpdateCombatUI;
         Button.Name := New_String(".gameframe.header.closebutton");
         configure(Button, "-command {ShowSkyMap}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
         Frame.Name := New_String(Widget_Image(CombatFrame) & ".left");
         if Winfo_Get(Frame, "ismapped") = "1" then
            ShowCombatFrame(".combat");
         end if;
         Frame.Name := New_String(Widget_Image(CombatFrame) & ".status");
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
         CreateGameMenu;
         Button := Get_Widget(CombatFrame & ".next", Interp);
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         return TCL_OK;
      end if;
      if PlayerShip.Crew(1).Order = Boarding and
        Winfo_Get(Frame, "ismapped") = "1" then
         UpdateBoardingUI;
         ShowCombatFrame(".boarding");
         return TCL_OK;
      end if;
      if PlayerShip.Crew(1).Order /= Boarding and
        Winfo_Get(Frame, "ismapped") = "0" then
         UpdateCombatUI;
         ShowCombatFrame(".combat");
         return TCL_OK;
      end if;
      if Winfo_Get(Frame, "ismapped") = "1" then
         UpdateCombatUI;
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
      ShowCombatUI(False);
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
      ComboBox: Ttk_ComboBox;
      GunIndex: Positive;
   begin
      ComboBox.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "pilot" then
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.pilotorder");
         PilotOrder := Positive'Value(Current(ComboBox)) + 1;
         if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            AddMessage
              ("Order for " &
               To_String(PlayerShip.Crew(FindMember(Pilot)).Name) &
               " was set on: " & Get(ComboBox),
               CombatMessage);
         else
            AddMessage
              ("Order for ship was set on: " & Get(ComboBox), CombatMessage);
         end if;
      elsif CArgv.Arg(Argv, 1) = "engineer" then
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.engineerorder");
         EngineerOrder := Positive'Value(Current(ComboBox)) + 1;
         if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            AddMessage
              ("Order for " &
               To_String(PlayerShip.Crew(FindMember(Engineer)).Name) &
               " was set on: " & Get(ComboBox),
               CombatMessage);
         else
            AddMessage
              ("Order for ship was set on: " & Get(ComboBox), CombatMessage);
         end if;
      else
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.gunorder" &
              CArgv.Arg(Argv, 1));
         GunIndex := Positive'Value(CArgv.Arg(Argv, 1));
         Guns(GunIndex)(2) := Positive'Value(Current(ComboBox)) + 1;
         Guns(GunIndex)(3) :=
           (if Current(ComboBox) = "0" then 0
            else Modules_List(PlayerShip.Modules(Guns(GunIndex)(1)).ProtoIndex)
                .Speed);
         AddMessage
           ("Order for " &
            To_String
              (PlayerShip.Crew(PlayerShip.Modules(Guns(GunIndex)(1)).Owner(1))
                 .Name) &
            " was set on: " & Get(ComboBox),
            CombatMessage);
      end if;
      UpdateMessages;
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
          (".gameframe.paned.combatframe.left.canvas.frame.order" &
           CArgv.Arg(Argv, 1),
           Interp);
   begin
      BoardingOrders(Positive'Value(CArgv.Arg(Argv, 2))) :=
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
      CrewDialog: constant Ttk_Frame :=
        Create(".boardingdialog", "-style Dialog.TFrame");
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
      InfoLabel: constant Ttk_Label :=
        Create
          (CrewFrame & ".titlelabel",
           "-text {Assign a crew members to " &
           (if CArgv.Arg(Argv, 1) = "boarding" then "boarding party"
            else "defenders") &
           "} -wraplength 250");
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv, 1) = "boarding" then Boarding else Defend);
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Tcl.Tk.Ada.Grid.Grid(CrewCanvas, "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, "-sticky ns -padx {0 5} -pady {5 0} -row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-pady {0 5} -columnspan 2");
      Focus(CloseButton);
      Autoscroll(YScroll);
      Tcl.Tk.Ada.Pack.Pack(InfoLabel);
      Height := Height + Positive'Value(Winfo_Get(InfoLabel, "reqheight"));
      Show_Player_Ship_Crew_Loop :
      for I in PlayerShip.Crew.Iterate loop
         CrewButton :=
           Create
             (CrewFrame & ".crewbutton" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) &
              "} -command {SetPartyOrder" &
              Positive'Image(Crew_Container.To_Index(I)) & " " &
              CArgv.Arg(Argv, 1) & "}");
         if PlayerShip.Crew(I).Order /= Order then
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
      if Positive'Value(Winfo_Get(InfoLabel, "reqwidth")) > Width then
         Width := Positive'Value(Winfo_Get(InfoLabel, "reqwidth"));
      end if;
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
      Tcl.Tk.Ada.Place.Place(CrewDialog, "-in .gameframe -relx 0.3 -rely 0.2");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Tab>", "{focus [GetActiveButton 0];break}");
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
   begin
      ComboBox.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "pilot" then
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.pilotcrew");
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            GiveOrders(PlayerShip, CrewIndex, Pilot);
         else
            CrewIndex := FindMember(Pilot);
            if CrewIndex > 0 then
               GiveOrders(PlayerShip, CrewIndex, Rest);
            end if;
         end if;
      elsif CArgv.Arg(Argv, 1) = "engineer" then
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.engineercrew");
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            GiveOrders(PlayerShip, CrewIndex, Engineer);
         else
            CrewIndex := FindMember(Engineer);
            if CrewIndex > 0 then
               GiveOrders(PlayerShip, CrewIndex, Rest);
            end if;
         end if;
      else
         ComboBox.Name :=
           New_String
             (".gameframe.paned.combatframe.crew.canvas.frame.guncrew" &
              CArgv.Arg(Argv, 2));
         GunIndex := Positive'Value(CArgv.Arg(Argv, 2));
         CrewIndex := Natural'Value(Current(ComboBox));
         if CrewIndex > 0 then
            GiveOrders(PlayerShip, CrewIndex, Gunner, Guns(GunIndex)(1));
         else
            CrewIndex := PlayerShip.Modules(Guns(GunIndex)(1)).Owner(1);
            if CrewIndex > 0 then
               GiveOrders(PlayerShip, CrewIndex, Rest);
            end if;
         end if;
      end if;
      UpdateCombatUI;
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
         for Item of PlayerShip.Crew(CrewIndex).Equipment loop
            if Item /= 0 then
               Append
                 (Info,
                  LF &
                  GetItemName(PlayerShip.Crew(CrewIndex).Inventory(Item)));
            end if;
         end loop Show_Player_Crew_Equipment_Loop;
      else
         Show_Enemy_Crew_Equipment_Loop :
         for Item of Enemy.Ship.Crew(CrewIndex).Equipment loop
            if Item /= 0 then
               Append
                 (Info,
                  LF &
                  GetItemName(Enemy.Ship.Crew(CrewIndex).Inventory(Item)));
            end if;
         end loop Show_Enemy_Crew_Equipment_Loop;
      end if;
      ShowInfo(To_String(Info));
      return TCL_OK;
   end Show_Combat_Info_Command;

   procedure ShowCombatUI(NewCombat: Boolean := True) is
      Paned: constant Ttk_PanedWindow := Get_Widget(".gameframe.paned");
      CombatFrame: constant Ttk_Frame := Get_Widget(Paned & ".combatframe");
      CombatStarted: Boolean;
      Button: Ttk_Button := Get_Widget(".gameframe.header.closebutton");
      EnemyFrame: constant Ttk_Frame := Get_Widget(CombatFrame & ".status");
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Button);
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
         if Winfo_Get(CombatFrame, "exists") = "0" then
            Tcl_EvalFile
              (Get_Context,
               To_String(Data_Directory) & "ui" & Dir_Separator &
               "combat.tcl");
            PilotOrder := 2;
            EngineerOrder := 3;
            AddCommand("SetPartyOrder", Set_Party_Order_Command'Access);
            AddCommand("NextTurn", Next_Turn_Command'Access);
            AddCommand("ShowCombatUI", Show_Combat_UI_Command'Access);
            AddCommand("SetCombatOrder", Set_Combat_Order_Command'Access);
            AddCommand("SetBoardingOrder", Set_Boarding_Order_Command'Access);
            AddCommand("SetCombatParty", Set_Combat_Party_Command'Access);
            AddCommand
              ("SetCombatPosition", Set_Combat_Position_Command'Access);
            AddCommand("ShowCombatInfo", Show_Combat_Info_Command'Access);
         else
            Button.Name := New_String(CombatFrame & ".next");
            Tcl.Tk.Ada.Grid.Grid(Button);
            Tcl.Tk.Ada.Grid.Grid(EnemyFrame);
         end if;
         Button.Name := New_String(".gameframe.header.closebutton");
         configure(Button, "-command ShowCombatUI");
         Back_To_Work_Loop :
         for Member of PlayerShip.Crew loop
            if Member.Order = Rest
              and then Member.PreviousOrder in Pilot | Engineer | Gunner then
               Member.Order := Member.PreviousOrder;
               Member.OrderTime := 15;
               AddMessage
                 (To_String(Member.Name) & " back to work for combat.",
                  OrderMessage);
            end if;
         end loop Back_To_Work_Loop;
         Delete(GameMenu, "1");
         Delete(GameMenu, "4");
      end if;
      UpdateCombatUI;
      ShowCombatFrame(".combat");
      ShowScreen("combatframe");
   end ShowCombatUI;

end Combat.UI;
