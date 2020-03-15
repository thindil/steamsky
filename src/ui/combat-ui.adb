--    Copyright 2018-2020 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Label; use Gtk.Label;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Container; use Gtk.Container;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Button; use Gtk.Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Text_Mark; use Gtk.Text_Mark;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types; use Glib.Types;
with Gtkada.Builder; use Gtkada.Builder;
with Game; use Game;
with Bases; use Bases;
with ShipModules; use ShipModules;
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Crew; use Crew;
with Ships.Crew; use Ships.Crew;
with Messages; use Messages;
with Config; use Config;
with Items; use Items;
with Factions; use Factions;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   -- ****iv* Combat.UI/PilotOrders
   -- FUNCTION
   -- Array of text for each combat order for pilot
   -- SOURCE
   PilotOrders: constant array(1 .. 4) of Unbounded_String :=
     (To_Unbounded_String("Go closer"), To_Unbounded_String("Keep distance"),
      To_Unbounded_String("Evade"), To_Unbounded_String("Escape"));
   -- ****
   -- ****iv* Combat.UI/EngineerOrders
   -- FUNCTION
   -- Array of text for each combat order for engineer
   -- SOURCE
   EngineerOrders: constant array(1 .. 4) of Unbounded_String :=
     (To_Unbounded_String("All stop"), To_Unbounded_String("Quarter speed"),
      To_Unbounded_String("Half speed"), To_Unbounded_String("Full speed"));
   -- ****
   -- ****iv* Combat.UI/GunnerOrders
   -- FUNCTION
   -- Array of text for each combat order for gunners
   -- SOURCE
   GunnerOrders: constant array(1 .. 6) of Unbounded_String :=
     (To_Unbounded_String("Don't shoot"), To_Unbounded_String("Precise fire"),
      To_Unbounded_String("Fire at will"),
      To_Unbounded_String("Aim for their engine"),
      To_Unbounded_String("Aim for their weapon"),
      To_Unbounded_String("Aim for their hull"));
   -- ****

   -- ****if* Combat.UI/RefreshCombatUI
   -- FUNCTION
   -- Reload information on combat screen
   -- SOURCE
   procedure RefreshCombatUI;
   -- ****

   -- ****if* Combat.UI/SetBoardingOrder
   -- FUNCTION
   -- Assign/remove selected crew member to boarding party
   -- PARAMETERS
   -- Self - Gtk_Toggle_Button checked or unchecked
   -- SOURCE
   procedure SetBoardingOrder(Self: access Gtk_Toggle_Button_Record'Class) is
      -- ****
      MemberName: Unbounded_String;
      OrderIndex: Natural := 0;
   begin
      MemberName := To_Unbounded_String(Get_Label(Self));
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Boarding then
            OrderIndex := OrderIndex + 1;
         end if;
         if PlayerShip.Crew(I).Name = MemberName then
            if Get_Active(Self) then
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
      RefreshCombatUI;
   end SetBoardingOrder;

   -- ****if* Combat.UI/SetBoardingParty
   -- FUNCTION
   -- Set boarding party buttons
   -- PARAMETERS
   -- Widget - Button to set
   -- SOURCE
   procedure SetBoardingParty
     (Widget: not null access Gtk_Widget_Record'Class) is
      -- ****
      MemberName: Unbounded_String;
   begin
      MemberName := To_Unbounded_String(Get_Label(Gtk_Button(Widget)));
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Name = MemberName and
           PlayerShip.Crew(I).Order = Boarding then
            Set_Active(Gtk_Toggle_Button(Widget), True);
         end if;
      end loop;
      On_Toggled(Gtk_Toggle_Button(Widget), SetBoardingOrder'Access);
      Show_All(Widget);
   end SetBoardingParty;

   -- ****if* Combat.UI/UpdateMessages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure UpdateMessages is
      -- ****
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      MessagesIter: Gtk_Text_Iter;
      CurrentTurnTime: Unbounded_String := To_Unbounded_String(FormatedTime);
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
                 (MessagesBuffer, MessagesIter, To_String(Message.Message));
            else
               Insert_With_Tags
                 (MessagesBuffer, MessagesIter, To_String(Message.Message),
                  Lookup
                    (Get_Tag_Table(MessagesBuffer),
                     To_String(TagNames(Message_Color'Pos(Message.Color)))));
            end if;
         else
            Insert_With_Tags
              (MessagesBuffer, MessagesIter, To_String(Message.Message),
               Lookup(Get_Tag_Table(MessagesBuffer), "gray"));
         end if;
      end ShowMessage;
   begin
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmessages")), "");
      Get_Start_Iter(MessagesBuffer, MessagesIter);
      if LoopStart = 0 then
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
                  Insert(MessagesBuffer, MessagesIter, "" & LF);
               end if;
            end if;
         end loop;
         declare
            Mark: Gtk_Text_Mark;
         begin
            Mark := Create_Mark(MessagesBuffer, "end", MessagesIter);
            Scroll_Mark_Onscreen
              (Gtk_Text_View(Get_Object(Builder, "messagesview")), Mark);
         end;
      else
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            exit when (GetLastMessageIndex + I + 1) < MessagesStarts;
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesBuffer, MessagesIter, "" & LF);
            end if;
         end loop;
      end if;
   end UpdateMessages;

   procedure RefreshCombatUI is
      DamagePercent: Gint;
      EnemyInfo, ModuleName: Unbounded_String;
      ButtonBox: constant Gtk_Container :=
        Gtk_Container(Get_Object(Builder, "btnboxboard"));
   begin
      if (HarpoonDuration > 0 or Enemy.HarpoonDuration > 0) and
        ProtoShips_List(EnemyShipIndex).Crew.Length > 0 then
         Show_All(Gtk_Widget(Get_Object(Builder, "expboard")));
      else
         Hide(Gtk_Widget(Get_Object(Builder, "expboard")));
      end if;
      Combat.UI.UpdateMessages;
      declare
         CrewIter: Gtk_Tree_Iter;
         CrewList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "crewlist1"));
         MemberIndex: Natural;
         AmmoAmount, AmmoIndex: Natural := 0;
         HaveAmmo: Boolean;
      begin
         Clear(CrewList);
         Append(CrewList, CrewIter);
         MemberIndex := FindMember(Pilot);
         if MemberIndex = 0 then
            Set(CrewList, CrewIter, 0, "Pilot:");
            if Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                (To_Unbounded_String("sentientships")) then
               Set(CrewList, CrewIter, 1, To_String(PilotOrders(PilotOrder)));
            end if;
            Set(CrewList, CrewIter, 2, "Nobody");
         else
            Set(CrewList, CrewIter, 0, "Pilot:");
            Set(CrewList, CrewIter, 1, To_String(PilotOrders(PilotOrder)));
            Set
              (CrewList, CrewIter, 2,
               To_String(PlayerShip.Crew(MemberIndex).Name));
         end if;
         Append(CrewList, CrewIter);
         MemberIndex := FindMember(Engineer);
         if MemberIndex = 0 then
            Set(CrewList, CrewIter, 0, "Engineer:");
            if Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                (To_Unbounded_String("sentientships")) then
               Set
                 (CrewList, CrewIter, 1,
                  To_String(EngineerOrders(EngineerOrder)));
            end if;
            Set(CrewList, CrewIter, 2, "Nobody");
         else
            Set(CrewList, CrewIter, 0, "Engineer:");
            Set
              (CrewList, CrewIter, 1,
               To_String(EngineerOrders(EngineerOrder)));
            Set
              (CrewList, CrewIter, 2,
               To_String(PlayerShip.Crew(MemberIndex).Name));
         end if;
         for I in Guns.First_Index .. Guns.Last_Index loop
            Append(CrewList, CrewIter);
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
                    PlayerShip.Cargo.First_Index ..
                          PlayerShip.Cargo.Last_Index)
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
            Set
              (CrewList, CrewIter, 0,
               To_String(PlayerShip.Modules(Guns(I)(1)).Name) & LF & "(Ammo:" &
               Natural'Image(AmmoAmount) & ")");
            if PlayerShip.Modules(Guns(I)(1)).Owner(1) /= 0 then
               if PlayerShip.Crew(PlayerShip.Modules(Guns(I)(1)).Owner(1))
                   .Order =
                 Gunner then
                  Set
                    (CrewList, CrewIter, 1,
                     To_String(GunnerOrders(Guns(I)(2))));
                  Set
                    (CrewList, CrewIter, 2,
                     To_String
                       (PlayerShip.Crew
                          (PlayerShip.Modules(Guns(I)(1)).Owner(1))
                          .Name));
               else
                  Set(CrewList, CrewIter, 2, "Nobody");
               end if;
            else
               Set(CrewList, CrewIter, 2, "Nobody");
            end if;
         end loop;
      end;
      declare
         DamageIter: Gtk_Tree_Iter;
         DamageList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "damagelist"));
         IsDamaged: Boolean := False;
      begin
         Clear(DamageList);
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Append(DamageList, DamageIter);
               Set(DamageList, DamageIter, 0, To_String(Module.Name));
               DamagePercent :=
                 100 -
                 Gint
                   ((Float(Module.Durability) / Float(Module.MaxDurability)) *
                    100.0);
               Set(DamageList, DamageIter, 1, DamagePercent);
               IsDamaged := True;
            end if;
         end loop;
         if not IsDamaged then
            Hide(Gtk_Widget(Get_Object(Builder, "damageframe")));
         else
            Show_All(Gtk_Widget(Get_Object(Builder, "damageframe")));
         end if;
      end;
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
      else
         Append(EnemyInfo, "Unknown");
      end if;
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblenemyinfo")), To_String(EnemyInfo));
      declare
         EnemyInfoList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "enemyinfolist"));
         EnemyInfoIter: Gtk_Tree_Iter := Get_Iter_First(EnemyInfoList);
         SpaceIndex: Natural;
      begin
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
            DamagePercent :=
              Gint
                ((Float(Enemy.Ship.Modules(I).Durability) /
                  Float(Enemy.Ship.Modules(I).MaxDurability)) *
                 100.0);
            if Enemy.Ship.Modules(I).Durability > 0 then
               Set(EnemyInfoList, EnemyInfoIter, 0, To_String(ModuleName));
            else
               Set
                 (EnemyInfoList, EnemyInfoIter, 0,
                  To_String(ModuleName) & "(destroyed)");
            end if;
            Set(EnemyInfoList, EnemyInfoIter, 1, DamagePercent);
            Next(EnemyInfoList, EnemyInfoIter);
         end loop;
      end;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "expboard"))) then
         Foreach
           (Gtk_Container(Get_Object(Builder, "btnboxboard")),
            RemoveWidget'Access);
         for Member of PlayerShip.Crew loop
            Add
              (ButtonBox,
               Gtk_Check_Button_New_With_Label(To_String(Member.Name)));
         end loop;
         Foreach
           (Gtk_Container(Get_Object(Builder, "btnboxboard")),
            SetBoardingParty'Access);
      end if;
      UpdateHeader;
   end RefreshCombatUI;

   procedure ShowCombatUI(NewCombat: Boolean := True) is
      CombatStarted: Boolean;
      MenuArray: constant array(1 .. 9) of Unbounded_String :=
        (To_Unbounded_String("menuorders"),
         To_Unbounded_String("menucrafting"),
         To_Unbounded_String("menubaseslist"),
         To_Unbounded_String("menuevents"),
         To_Unbounded_String("menumissions"), To_Unbounded_String("menustory"),
         To_Unbounded_String("menuwait"), To_Unbounded_String("menustats"),
         To_Unbounded_String("menuoptions"));
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
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbldescription")),
            To_String(Enemy.Ship.Description));
         declare
            EnemyList: constant Gtk_List_Store :=
              Gtk_List_Store(Get_Object(Builder, "enemyinfolist"));
            DamagePercent, SpaceIndex: Natural;
            ModuleName: Unbounded_String;
            EnemyIter: Gtk_Tree_Iter;
         begin
            Clear(EnemyList);
            for I in Enemy.Ship.Modules.Iterate loop
               Append(EnemyList, EnemyIter);
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
               Set(EnemyList, EnemyIter, 0, To_String(ModuleName));
               DamagePercent :=
                 100 -
                 Natural
                   ((Float(Enemy.Ship.Modules(I).Durability) /
                     Float(Enemy.Ship.Modules(I).MaxDurability)) *
                    100.0);
               Set(EnemyList, EnemyIter, 1, Gint(DamagePercent));
            end loop;
         end;
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
         declare
            CrewList: constant Gtk_List_Store :=
              Gtk_List_Store(Get_Object(Builder, "crewlist1"));
            CrewIter: Gtk_Tree_Iter := Get_Iter_First(CrewList);
            Index: Positive := 1;
         begin
            loop
               exit when CrewIter = Null_Iter;
               if Get_String(CrewList, CrewIter, 0) = "Pilot:" then
                  for I in PilotOrders'Range loop
                     if Get_String(CrewList, CrewIter, 1) = PilotOrders(I) then
                        PilotOrder := I;
                        exit;
                     end if;
                  end loop;
               elsif Get_String(CrewList, CrewIter, 0) = "Engineer:" then
                  for I in EngineerOrders'Range loop
                     if Get_String(CrewList, CrewIter, 1) =
                       EngineerOrders(I) then
                        EngineerOrder := I;
                        exit;
                     end if;
                  end loop;
               elsif Integer(Guns.Length) >= (Index - 2) then
                  for I in GunnerOrders'Range loop
                     if Get_String(CrewList, CrewIter, 1) =
                       GunnerOrders(I) then
                        Guns(Index - 2)(2) := I;
                        exit;
                     end if;
                  end loop;
               end if;
               Next(CrewList, CrewIter);
               Index := Index + 1;
            end loop;
         end;
      end if;
      Hide(Gtk_Widget(Get_Object(Builder, "btnclose")));
      for I in MenuArray'Range loop
         Hide(Gtk_Widget(Get_Object(Builder, To_String(MenuArray(I)))));
      end loop;
      Hide(Gtk_Widget(Get_Object(Builder, "shipmovementbox")));
      Show_All(Gtk_Widget(Get_Object(Builder, "btnmenu")));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "combat");
      if (HarpoonDuration = 0 and Enemy.HarpoonDuration = 0) or
        ProtoShips_List(EnemyShipIndex).Crew.Length = 0 then
         Hide(Gtk_Widget(Get_Object(Builder, "expboard")));
      end if;
      RefreshCombatUI;
   end ShowCombatUI;

-- ****if* Combat.UI/SetOrdersList
-- SOURCE
   procedure SetOrdersList(Object: access Gtkada_Builder_Record'Class) is
-- ****
      CrewIter, NamesIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      Position: Natural;
      AssignedName: Unbounded_String;
      SkillIndex, SkillValue: Natural := 0;
      SkillString: Unbounded_String;
      CrewList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "crewnames"));
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew1"))),
         CrewModel, CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      Position := Natural'Value(To_String(Get_Path(CrewModel, CrewIter)));
      Clear(CrewList);
      AssignedName := To_Unbounded_String(Get_String(CrewModel, CrewIter, 2));
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
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
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Name /= AssignedName and
           PlayerShip.Crew(I).Skills.Length > 0 then
            SkillString := Null_Unbounded_String;
            case Position is
               when 0 =>
                  if GetSkillLevel(PlayerShip.Crew(I), PilotingSkill) > 0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
               when 1 =>
                  if GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill) >
                    0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
               when others =>
                  if GetSkillLevel(PlayerShip.Crew(I), GunnerySkill) > 0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
            end case;
            if I = SkillIndex then
               SkillString := SkillString & To_Unbounded_String("+");
            end if;
            if PlayerShip.Crew(I).Order /= Rest then
               SkillString := SkillString & To_Unbounded_String(" -");
            end if;
            Append(CrewList, NamesIter);
            Set
              (CrewList, NamesIter, 0,
               To_String(PlayerShip.Crew(I).Name & SkillString));
            Set(CrewList, NamesIter, 1, Gint(I));
         end if;
      end loop;
      declare
         OrdersModel: constant Glib.Types.GType_Interface :=
           Get_Property
             (Get_Object(Object, "renderorders"),
              Gtk.Cell_Renderer_Combo.Model_Property);
         OrdersList: constant Gtk_List_Store := -(Gtk_Tree_Model(OrdersModel));
         OrdersIter: Gtk_Tree_Iter;
         AssignedOrder: Unbounded_String;
      begin
         OrdersList.Clear;
         if AssignedName = To_Unbounded_String("Nobody")
           and then
           (Position > 1
            or else not Factions_List(PlayerShip.Crew(1).Faction).Flags
              .Contains
              (To_Unbounded_String("sentientships"))) then
            return;
         end if;
         AssignedOrder :=
           To_Unbounded_String(Get_String(CrewModel, CrewIter, 1));
         case Position is
            when 0 =>
               for I in PilotOrders'Range loop
                  if AssignedOrder /= PilotOrders(I) then
                     Append(OrdersList, OrdersIter);
                     Set(OrdersList, OrdersIter, 0, To_String(PilotOrders(I)));
                     Set(OrdersList, OrdersIter, 1, Gint(I));
                  end if;
               end loop;
            when 1 =>
               for I in EngineerOrders'Range loop
                  if AssignedOrder /= EngineerOrders(I) then
                     Append(OrdersList, OrdersIter);
                     Set
                       (OrdersList, OrdersIter, 0,
                        To_String(EngineerOrders(I)));
                     Set(OrdersList, OrdersIter, 1, Gint(I));
                  end if;
               end loop;
            when others =>
               for I in GunnerOrders'Range loop
                  if AssignedOrder /= GunnerOrders(I) then
                     Append(OrdersList, OrdersIter);
                     Set
                       (OrdersList, OrdersIter, 0, To_String(GunnerOrders(I)));
                     Set(OrdersList, OrdersIter, 1, Gint(I));
                  end if;
               end loop;
         end case;
      end;
   end SetOrdersList;

-- ****if* Combat.UI/GiveCombatOrders
-- SOURCE
   procedure GiveCombatOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String; New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter) is
-- ****
      Model: constant Glib.Types.GType_Interface :=
        Get_Property(Self, Gtk.Cell_Renderer_Combo.Model_Property);
      OrdersList: constant Gtk_List_Store := -(Gtk_Tree_Model(Model));
      ModuleIndex: Natural := 0;
   begin
      if Self = Gtk_Cell_Renderer_Combo(Get_Object(Builder, "rendercrew")) then
         if Path_String = "0" then
            GiveOrders
              (PlayerShip, Positive(Get_Int(OrdersList, New_Iter, 1)), Pilot,
               ModuleIndex);
         elsif Path_String = "1" then
            GiveOrders
              (PlayerShip, Positive(Get_Int(OrdersList, New_Iter, 1)),
               Engineer, ModuleIndex);
         else
            ModuleIndex := Guns(Positive'Value(Path_String) - 1)(1);
            GiveOrders
              (PlayerShip, Positive(Get_Int(OrdersList, New_Iter, 1)), Gunner,
               ModuleIndex);
         end if;
      else
         if Path_String = "0" then
            PilotOrder := Positive(Get_Int(OrdersList, New_Iter, 1));
            if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                (To_Unbounded_String("sentientships")) then
               AddMessage
                 ("Order for " &
                  To_String(PlayerShip.Crew(FindMember(Pilot)).Name) &
                  " was set on: " & To_String(PilotOrders(PilotOrder)),
                  CombatMessage);
            else
               AddMessage
                 ("Order for ship was set on: " &
                  To_String(PilotOrders(PilotOrder)),
                  CombatMessage);
            end if;
         elsif Path_String = "1" then
            EngineerOrder := Positive(Get_Int(OrdersList, New_Iter, 1));
            if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                (To_Unbounded_String("sentientships")) then
               AddMessage
                 ("Order for " &
                  To_String(PlayerShip.Crew(FindMember(Engineer)).Name) &
                  " was set on: " & To_String(EngineerOrders(EngineerOrder)),
                  CombatMessage);
            else
               AddMessage
                 ("Order for ship was set on: " &
                  To_String(EngineerOrders(EngineerOrder)),
                  CombatMessage);
            end if;
         else
            Guns(Positive'Value(Path_String) - 1)(2) :=
              Positive(Get_Int(OrdersList, New_Iter, 1));
            AddMessage
              ("Order for " &
               To_String
                 (PlayerShip.Crew
                    (PlayerShip.Modules
                       (Guns(Positive'Value(Path_String) - 1)(1))
                       .Owner
                       (1))
                    .Name) &
               " was set on: " &
               To_String
                 (GunnerOrders(Guns(Positive'Value(Path_String) - 1)(2))),
               CombatMessage);
         end if;
      end if;
      RefreshCombatUI;
   end GiveCombatOrders;

-- ****if* Combat.UI/RefreshBoardingUI
-- SOURCE
   procedure RefreshBoardingUI is
-- ****
      OrderIndex: Positive := 1;
      OrdersList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "orders2"));
      OrdersIter: Gtk_Tree_Iter;
   begin
      Combat.UI.UpdateMessages;
      declare
         EnemyCrewList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "enemycrewlist"));
         EnemyCrewIter: Gtk_Tree_Iter;
         OrderName: Unbounded_String;
      begin
         Clear(EnemyCrewList);
         Clear(OrdersList);
         Append(OrdersList, OrdersIter);
         Set(OrdersList, OrdersIter, 0, "Attack");
         for I in Enemy.Ship.Crew.Iterate loop
            Append(EnemyCrewList, EnemyCrewIter);
            Set
              (EnemyCrewList, EnemyCrewIter, 0,
               To_String(Enemy.Ship.Crew(I).Name));
            Set
              (EnemyCrewList, EnemyCrewIter, 1,
               Gint(Enemy.Ship.Crew(I).Health));
            Set
              (EnemyCrewList, EnemyCrewIter, 2,
               Gint(Crew_Container.To_Index(I)));
            OrderName :=
              To_Unbounded_String(Crew_Orders'Image(Enemy.Ship.Crew(I).Order));
            Replace_Slice
              (OrderName, 2, Length(OrderName),
               To_Lower(Slice(OrderName, 2, Length(OrderName))));
            Set(EnemyCrewList, EnemyCrewIter, 3, To_String(OrderName));
            Append(OrdersList, OrdersIter);
            Set
              (OrdersList, OrdersIter, 0,
               "Attack " & To_String(Enemy.Ship.Crew(I).Name));
         end loop;
         if HarpoonDuration > 0 or Enemy.HarpoonDuration > 0 then
            Append(OrdersList, OrdersIter);
            Set(OrdersList, OrdersIter, 0, "Back to ship");
         end if;
      end;
      declare
         CrewList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "crewlist3"));
         CrewIter: Gtk_Tree_Iter;
      begin
         Clear(CrewList);
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Order = Boarding then
               Append(CrewList, CrewIter);
               Set(CrewList, CrewIter, 0, To_String(PlayerShip.Crew(I).Name));
               Set(CrewList, CrewIter, 1, Gint(PlayerShip.Crew(I).Health));
               Set(CrewList, CrewIter, 2, Gint(Crew_Container.To_Index(I)));
               OrdersIter :=
                 Get_Iter_From_String
                   (OrdersList, Natural'Image(BoardingOrders(OrderIndex)));
               Set
                 (CrewList, CrewIter, 3,
                  Get_String(OrdersList, OrdersIter, 0));
               OrderIndex := OrderIndex + 1;
            end if;
         end loop;
      end;
   end RefreshBoardingUI;

-- ****if* Combat.UI/NextTurn
-- SOURCE
   procedure NextTurn(Object: access Gtkada_Builder_Record'Class) is
-- ****
      CombatStack: constant Gtk_Stack :=
        Gtk_Stack(Get_Object(Object, "combatstack"));
   begin
      CombatTurn;
      if EndCombat then
         RefreshCombatUI;
         if Get_Visible_Child_Name(CombatStack) = "boarding" then
            Set_Visible_Child_Name(CombatStack, "shipcombat");
         end if;
         Hide(Gtk_Widget(Get_Object(Object, "btnnextturn")));
         Hide(Gtk_Widget(Get_Object(Object, "expmoreinfo")));
         Hide(Gtk_Widget(Get_Object(Object, "expboard")));
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "treecrew1")), False);
         Show_All(Gtk_Widget(Get_Object(Object, "btnclose")));
         return;
      end if;
      if PlayerShip.Crew(1).Order = Boarding and
        Get_Visible_Child_Name(CombatStack) = "shipcombat" then
         Set_Visible_Child_Name(CombatStack, "boarding");
      end if;
      if PlayerShip.Crew(1).Order /= Boarding and
        Get_Visible_Child_Name(CombatStack) = "boarding" then
         Set_Visible_Child_Name(CombatStack, "shipcombat");
      end if;
      if Get_Visible_Child_Name(CombatStack) = "shipcombat" then
         RefreshCombatUI;
      else
         RefreshBoardingUI;
      end if;
   end NextTurn;

-- ****if* Combat.UI/GiveBoardingOrders
-- SOURCE
   procedure GiveBoardingOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String; New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter) is
-- ****
      OrdersList: Gtk_List_Store;
      Model: Glib.Types.GType_Interface;
      NewOrder: Integer;
      List: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "crewlist3"));
   begin
      Model := Get_Property(Self, Gtk.Cell_Renderer_Combo.Model_Property);
      OrdersList := -(Gtk_Tree_Model(Model));
      Set
        (List, Get_Iter_From_String(List, Path_String), 3,
         Get_String(OrdersList, New_Iter, 0));
      NewOrder := Natural'Value(To_String(Get_Path(OrdersList, New_Iter)));
      if NewOrder > Integer(Enemy.Ship.Crew.Length) then
         NewOrder := -1;
      end if;
      BoardingOrders(Positive'Value(Path_String) + 1) := NewOrder;
   end GiveBoardingOrders;

   procedure CreateCombatUI is
   begin
      Register_Handler(Builder, "Set_Orders_List", SetOrdersList'Access);
      Register_Handler(Builder, "Next_Turn", NextTurn'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderorders")),
         GiveCombatOrders'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "rendercrew")),
         GiveCombatOrders'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderboardorders")),
         GiveBoardingOrders'Access);
   end CreateCombatUI;

end Combat.UI;
