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
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
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

   -- ****if* CUI/GetGunSpeed
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
            if GunSpeed > 0 then
               GunSpeed := Integer(Float'Ceiling(Float(GunSpeed) / 2.0));
            else
               GunSpeed := GunSpeed - 1;
            end if;
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

   -- ****if* CUI/UpdateMessages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure UpdateMessages is
      -- ****
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      CurrentTurnTime: Unbounded_String := To_Unbounded_String(FormatedTime);
      MessagesView: Tk_Text;
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
      MessagesView.Interp := Get_Context;
      MessagesView.Name := New_String(".paned.controls.messages.view");
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
      if GameSettings.MessagesOrder = OLDER_FIRST then
         for I in LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            if (GetLastMessageIndex + I + 1) >= MessagesStarts then
               ShowMessage;
               if I < -1 then
                  Insert(MessagesView, "end", "{" & LF & "}");
               end if;
            end if;
         end loop;
      else
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            exit when (GetLastMessageIndex + I + 1) < MessagesStarts;
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state disable");
   end UpdateMessages;

   -- ****if* CUI/UpdateCombatUI
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure UpdateCombatUI is
      -- ****
      Tokens: Slice_Set;
      Frame, Item: Ttk_Frame;
      Label: Ttk_Label;
      ComboBox: Ttk_ComboBox;
      GunnersOrders: constant array(1 .. 6) of Unbounded_String :=
        (To_Unbounded_String("{Don't shoot"),
         To_Unbounded_String("{Precise fire "),
         To_Unbounded_String("{Fire at will "),
         To_Unbounded_String("{Aim for their engine "),
         To_Unbounded_String("{Aim for their weapon "),
         To_Unbounded_String("{Aim for their hull "));
      GunIndex, GunnerOrders, EnemyInfo: Unbounded_String;
      HaveAmmo: Boolean;
      AmmoAmount, AmmoIndex, Row, Rows: Natural := 0;
      ProgressBar: Ttk_ProgressBar;
      DamagePercent: Float;
      function GetCrewList(Position: Natural) return String is
         SkillIndex, SkillValue: Natural := 0;
         SkillString: Unbounded_String;
         CrewList: Unbounded_String := To_Unbounded_String("Nobody");
      begin
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            case Position is
               when 0 =>
                  if GetSkillLevel(PlayerShip.Crew(I), PilotingSkill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), PilotingSkill);
                  end if;
               when 1 =>
                  if GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill);
                  end if;
               when others =>
                  if GetSkillLevel(PlayerShip.Crew(I), GunnerySkill) >
                    SkillValue then
                     SkillIndex := I;
                     SkillValue :=
                       GetSkillLevel(PlayerShip.Crew(I), GunnerySkill);
                  end if;
            end case;
         end loop;
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew(I).Skills.Length > 0 then
               SkillString := Null_Unbounded_String;
               case Position is
                  when 0 =>
                     if GetSkillLevel(PlayerShip.Crew(I), PilotingSkill) >
                       0 then
                        SkillString := To_Unbounded_String(" +");
                     end if;
                  when 1 =>
                     if GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill) >
                       0 then
                        SkillString := To_Unbounded_String(" +");
                     end if;
                  when others =>
                     if GetSkillLevel(PlayerShip.Crew(I), GunnerySkill) >
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
         end loop;
         return To_String(CrewList);
      end GetCrewList;
   begin
      Frame.Interp := Get_Context;
      Item.Interp := Get_Context;
      Frame.Name := New_String(".paned.combatframe.canvas.combat.left.crew");
      ComboBox.Interp := Get_Context;
      ComboBox.Name := New_String(Widget_Image(Frame) & ".pilotcrew");
      configure(ComboBox, "-values [list " & GetCrewList(0) & "]");
      Current(ComboBox, Natural'Image(FindMember(Pilot)));
      ComboBox.Name := New_String(Widget_Image(Frame) & ".pilotorder");
      Current(ComboBox, Integer'Image(PilotOrder - 1));
      ComboBox.Name := New_String(Widget_Image(Frame) & ".engineercrew");
      configure(ComboBox, "-values [list " & GetCrewList(1) & "]");
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
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
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
              "-values [list " & GetCrewList(2) & "]");
         if PlayerShip.Modules(Guns(I)(1)).Owner(1) /= 0 then
            if PlayerShip.Crew(PlayerShip.Modules(Guns(I)(1)).Owner(1)).Order =
              Gunner then
               Current
                 (ComboBox,
                  Positive'Image(PlayerShip.Modules(Guns(I)(1)).Owner(1)));
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
         GunnerOrders := Null_Unbounded_String;
         for J in GunnersOrders'Range loop
            Append
              (GunnerOrders,
               " " & GunnersOrders(J) &
               GetGunSpeed(Guns_Container.To_Index(I), J) & "}");
         end loop;
         ComboBox :=
           Create
             (Widget_Image(Frame) & ".gunorder" & To_String(GunIndex),
              "-values [list" & To_String(GunnerOrders) & "]");
         Current(ComboBox, Natural'Image(Guns(I)(2) - 1));
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetCombatOrder " & To_String(GunIndex) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-row" & Positive'Image(Guns_Container.To_Index(I) + 2) &
            " -column 2");
      end loop;
      Frame.Name := New_String(".paned.combatframe.canvas.combat.left.damage");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 0 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            Label :=
              Create
                (Widget_Image(Frame) & ".lbl" & Trim(Natural'Image(Row), Left),
                 "-text {" & To_String(Module.Name) & "}");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-row" & Natural'Image(Row) & " -column 0");
            DamagePercent :=
              100.0 -
              ((Float(Module.Durability) / Float(Module.MaxDurability)) *
               100.0);
            ProgressBar :=
              Create
                (Widget_Image(Frame) & ".dmg" & Trim(Natural'Image(Row), Left),
                 "-orient horizontal -value " & Float'Image(DamagePercent));
            Tcl.Tk.Ada.Grid.Grid
              (ProgressBar, "-row" & Natural'Image(Row) & " -column 1");
            Row := Row + 1;
         end if;
      end loop;
      Append(EnemyInfo, "Name: ");
      Append(EnemyInfo, EnemyName);
      Append(EnemyInfo, LF);
      Append(EnemyInfo, "Type: ");
      Append(EnemyInfo, Enemy.Ship.Name);
      Append(EnemyInfo, LF);
      Append(EnemyInfo, "Home: ");
      Append(EnemyInfo, SkyBases(Enemy.Ship.HomeBase).Name);
      Append(EnemyInfo, LF);
      Append(EnemyInfo, "Distance: ");
      if Enemy.Distance >= 15000 then
         Append(EnemyInfo, "Escaped");
      elsif Enemy.Distance < 15000 and Enemy.Distance >= 10000 then
         Append(EnemyInfo, "Long");
      elsif Enemy.Distance < 10000 and Enemy.Distance >= 5000 then
         Append(EnemyInfo, "Medium");
      elsif Enemy.Distance < 5000 and Enemy.Distance >= 1000 then
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
               for Module of Enemy.Ship.Modules loop
                  if Module.Durability < Module.MaxDurability then
                     EnemyStatus := To_Unbounded_String("Damaged");
                     exit;
                  end if;
               end loop;
               Append(EnemyInfo, EnemyStatus);
            end;
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
            end loop;
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
      Label.Name :=
        New_String(".paned.combatframe.canvas.combat.right.enemy.info");
      configure(Label, "-text {" & To_String(EnemyInfo) & "}");
      declare
         SpaceIndex: Natural;
         ModuleName: Unbounded_String;
      begin
         Frame.Name :=
           New_String(".paned.combatframe.canvas.combat.right.enemy.damage");
         Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
         Rows := Natural'Value(Slice(Tokens, 2));
         for I in 0 .. (Rows - 1) loop
            Create
              (Tokens,
               Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
               " ");
            for J in 1 .. Slice_Count(Tokens) loop
               Item.Interp := Get_Context;
               Item.Name := New_String(Slice(Tokens, J));
               Destroy(Item);
            end loop;
         end loop;
         Row := 0;
         for I in Enemy.Ship.Modules.Iterate loop
            if Enemy.Distance > 1000 then
               ModuleName :=
                 To_Unbounded_String
                   (ModuleType'Image
                      (Modules_List(Enemy.Ship.Modules(I).ProtoIndex).MType));
               Replace_Slice
                 (ModuleName, 2, Length(ModuleName),
                  To_Lower(Slice(ModuleName, 2, Length(ModuleName))));
               SpaceIndex := Index(ModuleName, "_");
               while SpaceIndex > 0 loop
                  Replace_Element(ModuleName, SpaceIndex, ' ');
                  SpaceIndex := Index(ModuleName, "_");
               end loop;
            else
               ModuleName :=
                 Modules_List(Enemy.Ship.Modules(I).ProtoIndex).Name;
            end if;
            if Enemy.Ship.Modules(I).Durability = 0 then
               Append(ModuleName, "(destroyed)");
            end if;
            Label :=
              Create
                (Widget_Image(Frame) & ".lbl" & Trim(Natural'Image(Row), Left),
                 "-text {" & To_String(ModuleName) & "}");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-row" & Natural'Image(Row) & " -column 0");
            DamagePercent :=
              ((Float(Enemy.Ship.Modules(I).Durability) /
                Float(Enemy.Ship.Modules(I).MaxDurability)) *
               100.0);
            ProgressBar :=
              Create
                (Widget_Image(Frame) & ".dmg" & Trim(Natural'Image(Row), Left),
                 "-orient horizontal -value " & Float'Image(DamagePercent));
            Tcl.Tk.Ada.Grid.Grid
              (ProgressBar, "-row" & Natural'Image(Row) & " -column 1");
            Row := Row + 1;
         end loop;
      end;
      if (HarpoonDuration > 0 or Enemy.HarpoonDuration > 0) and
        ProtoShips_List(EnemyShipIndex).Crew.Length > 0 then
         Frame.Name :=
           New_String(".paned.combatframe.canvas.combat.right.boarding");
         Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
         Rows := Natural'Value(Slice(Tokens, 2));
         for I in 0 .. (Rows - 1) loop
            Create
              (Tokens,
               Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
               " ");
            for J in 1 .. Slice_Count(Tokens) loop
               Item.Interp := Get_Context;
               Item.Name := New_String(Slice(Tokens, J));
               Destroy(Item);
            end loop;
         end loop;
         declare
            CheckButton: Ttk_CheckButton;
         begin
            Row := 1;
            for Member of PlayerShip.Crew loop
               CheckButton :=
                 Create
                   (Widget_Image(Frame) & ".board" &
                    Trim(Positive'Image(Row), Left),
                    "-text {" & To_String(Member.Name) & "} -variable board" &
                    Trim(Positive'Image(Row), Left) &
                    " -command {SetBoarding" & Positive'Image(Row) & "}");
               if Member.Order = Boarding then
                  Tcl_SetVar
                    (Frame.Interp, "board" & Trim(Positive'Image(Row), Left),
                     "1");
               else
                  Tcl_SetVar
                    (Frame.Interp, "board" & Trim(Positive'Image(Row), Left),
                     "0");
               end if;
               Tcl.Tk.Ada.Grid.Grid(CheckButton, "-row" & Positive'Image(Row));
               Row := Row + 1;
            end loop;
         end;
      end if;
      UpdateMessages;
   end UpdateCombatUI;

   -- ****if* CUI/Set_Boarding_Command
   -- FUNCTION
   -- Set boarding order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Boarding_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Boarding_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      OrderIndex: Natural := 0;
   begin
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Boarding then
            OrderIndex := OrderIndex + 1;
         end if;
         if Crew_Container.To_Index(I) = MemberIndex then
            if PlayerShip.Crew(I).Order /= Boarding then
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Boarding, 0);
               BoardingOrders.Append(New_Item => 0);
            else
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
               BoardingOrders.Delete(Index => OrderIndex);
               OrderIndex := OrderIndex - 1;
            end if;
            exit;
         end if;
      end loop;
      UpdateCombatUI;
      return TCL_OK;
   end Set_Boarding_Command;

   -- ****if* Combat.UI/ShowCombatFrame
   -- FUNCTION
   -- Show ship to ship combat UI or boarding UI
   -- SOURCE
   procedure ShowCombatFrame(FrameName: String) is
      -- ****
      CombatCanvas: Tk_Canvas;
      CombatFrame: Ttk_Frame;
   begin
      CombatCanvas.Interp := Get_Context;
      CombatCanvas.Name := New_String(".paned.combatframe.canvas");
      CombatFrame.Interp := Get_Context;
      CombatFrame.Name := New_String(Widget_Image(CombatCanvas) & FrameName);
      Canvas_Create
        (CombatCanvas, "window",
         "[expr " & Winfo_Get(CombatFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CombatFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CombatFrame));
      Add_Tag(CombatCanvas, "child", "all");
      Tcl_Eval(Get_Context, "update");
      configure
        (CombatCanvas,
         "-scrollregion [list " & BBox(CombatCanvas, "all") & "]");
   end ShowCombatFrame;

   -- ****if* CUI/UpdateBoardingUI
   -- FUNCTION
   -- Update information about boarding party: remove old UI and create new elements
   -- SOURCE
   procedure UpdateBoardingUI is
      -- ****
      OrdersList, OrderName, Tooltip: Unbounded_String;
      Label: Ttk_Label;
      Frame, Item: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ProgressBar: Ttk_ProgressBar;
      ComboBox: Ttk_ComboBox;
      OrderIndex: Positive := 1;
   begin
      Frame.Interp := Get_Context;
      Frame.Name :=
        New_String(".paned.combatframe.canvas.boarding.right.enemy");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for I in Enemy.Ship.Crew.Iterate loop
         Append(OrdersList, "{Attack " & Enemy.Ship.Crew(I).Name & "} ");
         Tooltip := To_Unbounded_String("Uses: ");
         for Item of Enemy.Ship.Crew(I).Equipment loop
            if Item /= 0 then
               Append
                 (Tooltip,
                  LF & GetItemName(Enemy.Ship.Crew(I).Inventory(Item)));
            end if;
         end loop;
         Label :=
           Create
             (Widget_Image(Frame) & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(Enemy.Ship.Crew(I).Name) & "}");
         Add(Label, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Crew_Container.To_Index(I)));
         ProgressBar :=
           Create
             (Widget_Image(Frame) & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(Enemy.Ship.Crew(I).Health));
         Add(ProgressBar, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)));
         OrderName :=
           To_Unbounded_String(Crew_Orders'Image(Enemy.Ship.Crew(I).Order));
         Replace_Slice
           (OrderName, 2, Length(OrderName),
            To_Lower(Slice(OrderName, 2, Length(OrderName))));
         Label :=
           Create
             (Widget_Image(Frame) & ".order" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(OrderName) & "}");
         Add(Label, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-column 2 -row" & Positive'Image(Crew_Container.To_Index(I)));
      end loop;
      Append(OrdersList, " {Back to the ship}");
      Frame.Name := New_String(".paned.combatframe.canvas.boarding.left.crew");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(Frame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order /= Boarding then
            goto End_Of_Loop;
         end if;
         Tooltip := To_Unbounded_String("Uses: ");
         for Item of PlayerShip.Crew(I).Equipment loop
            if Item /= 0 then
               Append
                 (Tooltip,
                  LF & GetItemName(PlayerShip.Crew(I).Inventory(Item)));
            end if;
         end loop;
         Label :=
           Create
             (Widget_Image(Frame) & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) & "}");
         Add(Label, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Crew_Container.To_Index(I)));
         ProgressBar :=
           Create
             (Widget_Image(Frame) & ".health" &
              Trim(Natural'Image(Crew_Container.To_Index(I)), Left),
              "-orient horizontal -value " &
              Natural'Image(PlayerShip.Crew(I).Health));
         Add(ProgressBar, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" & Positive'Image(Crew_Container.To_Index(I)));
         ComboBox :=
           Create
             (Widget_Image(Frame) & ".order" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-values [list " & To_String(OrdersList) & "] -state readonly");
         Current
           (ComboBox,
            Natural'Image(BoardingOrders(Crew_Container.To_Index(I))));
         Bind
           (ComboBox, "<<ComboboxSelected>>",
            "{SetBoardingOrder" & Positive'Image(Crew_Container.To_Index(I)) &
            Positive'Image(OrderIndex) & "}");
         Add(ComboBox, To_String(Tooltip));
         Tcl.Tk.Ada.Grid.Grid
           (ComboBox,
            "-column 2 -row" & Positive'Image(Crew_Container.To_Index(I)));
         OrderIndex := OrderIndex + 1;
         <<End_Of_Loop>>
      end loop;
      UpdateMessages;
   end UpdateBoardingUI;

   -- ****if* CUI/Next_Turn_Command
   -- FUNCTION
   -- Execute combat orders and go to next turn
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Next_Turn_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Next_Turn_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Button: Ttk_Button;
      Frame: Ttk_Frame;
      CombatCanvas: Tk_Canvas;
   begin
      CombatTurn;
      UpdateHeader;
      CombatCanvas.Interp := Interp;
      CombatCanvas.Name := New_String(".paned.combatframe.canvas");
      Frame.Interp := Interp;
      Frame.Name := New_String(Widget_Image(CombatCanvas) & ".combat");
      if EndCombat then
         UpdateCombatUI;
         Button.Interp := Interp;
         Button.Name := New_String(Widget_Image(Frame) & ".next");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name := New_String(".header.closebutton");
         configure(Button, "-command {ShowSkyMap}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
         Frame.Name := New_String(Widget_Image(CombatCanvas) & ".boarding");
         if Winfo_Get(Frame, "ismapped") = "1" then
            Tcl.Tk.Ada.Widgets.Canvas.Delete
              (CombatCanvas, ".paned.combatframe.canvas.boarding");
            ShowCombatFrame(".combat");
         end if;
         CreateGameMenu;
         return TCL_OK;
      end if;
      if PlayerShip.Crew(1).Order = Boarding and
        Winfo_Get(Frame, "ismapped") = "1" then
         Tcl.Tk.Ada.Widgets.Canvas.Delete(CombatCanvas, "child");
         ShowCombatFrame(".boarding");
      end if;
      if PlayerShip.Crew(1).Order /= Boarding and
        Winfo_Get(Frame, "ismapped") = "0" then
         Tcl.Tk.Ada.Widgets.Canvas.Delete(CombatCanvas, "child");
         ShowCombatFrame(".combat");
      end if;
      if Winfo_Get(Frame, "ismapped") = "1" then
         UpdateCombatUI;
      else
         UpdateBoardingUI;
      end if;
      return TCL_OK;
   end Next_Turn_Command;

   -- ****if* CUI/Show_Combat_UI_Command
   -- FUNCTION
   -- Show combat UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Combat_UI_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_UI_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowCombatUI(False);
      return TCL_OK;
   end Show_Combat_UI_Command;

   -- ****if* CUI/Set_Combat_Order_Command
   -- FUNCTION
   -- Set combat order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Combat_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ComboBox: Ttk_ComboBox;
      GunIndex: Positive;
   begin
      ComboBox.Interp := Interp;
      if CArgv.Arg(Argv, 1) = "pilot" then
         ComboBox.Name :=
           New_String(".paned.combatframe.canvas.combat.left.crew.pilotorder");
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
             (".paned.combatframe.canvas.combat.left.crew.engineerorder");
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
             (".paned.combatframe.canvas.combat.left.crew.gunorder" &
              CArgv.Arg(Argv, 1));
         GunIndex := Positive'Value(CArgv.Arg(Argv, 1));
         Guns(GunIndex)(2) := Positive'Value(Current(ComboBox)) + 1;
         if Current(ComboBox) = "0" then
            Guns(GunIndex)(3) := 0;
         else
            Guns(GunIndex)(3) :=
              Modules_List(PlayerShip.Modules(Guns(GunIndex)(1)).ProtoIndex)
                .Speed;
         end if;
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

   -- ****if* CUI/Set_Boarding_Order_Command
   -- FUNCTION
   -- Set boarding order for the selected player's ship crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Set_Boarding_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Boarding_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Combobox: Ttk_ComboBox;
   begin
      Combobox.Interp := Interp;
      Combobox.Name :=
        New_String
          (".paned.combatframe.canvas.boarding.left.crew.order" &
           CArgv.Arg(Argv, 1));
      if Natural'Value(Current(Combobox)) + 1 >
        Natural(Enemy.Ship.Crew.Length) then
         BoardingOrders(Positive'Value(CArgv.Arg(Argv, 2))) := -1;
      else
         BoardingOrders(Positive'Value(CArgv.Arg(Argv, 2))) :=
           Natural'Value(Current(Combobox)) + 1;
      end if;
      return TCL_OK;
   end Set_Boarding_Order_Command;

   procedure ShowCombatUI(NewCombat: Boolean := True) is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CombatCanvas: Tk_Canvas;
      CombatFrame: Ttk_Frame;
      CombatStarted: Boolean;
      Button: Ttk_Button;
   begin
      Button.Interp := Get_Context;
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
            AddCommand("SetBoarding", Set_Boarding_Command'Access);
            AddCommand("NextTurn", Next_Turn_Command'Access);
            AddCommand("ShowCombatUI", Show_Combat_UI_Command'Access);
            AddCommand("SetCombatOrder", Set_Combat_Order_Command'Access);
            AddCommand("SetBoardingOrder", Set_Boarding_Order_Command'Access);
         else
            Button.Name := New_String(".paned.combatframe.canvas.combat.next");
            Tcl.Tk.Ada.Grid.Grid(Button);
         end if;
         Button.Name := New_String(".header.closebutton");
         configure(Button, "-command ShowCombatUI");
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
         Delete(GameMenu, "4", "5");
         Delete(GameMenu, "5", "10");
         Delete(GameMenu, "6");
      end if;
      UpdateCombatUI;
      configure
        (CombatCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      ShowCombatFrame(".combat");
      ShowScreen("combatframe");
   end ShowCombatUI;

end Combat.UI;
