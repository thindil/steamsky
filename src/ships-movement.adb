--    Copyright 2017-2021 Bartek thindil Jasicki
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

with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Statistics; use Statistics;
with Maps; use Maps;
with Messages; use Messages;
with Config; use Config;
with Bases; use Bases;
with Events; use Events;
with Utils; use Utils;
with Factions; use Factions;
with Game.SaveLoad; use Game.SaveLoad;

package body Ships.Movement is

   -- ****it* SMovement/SMovement.SpeedType
   -- FUNCTION
   -- Used in counting ships speed
   -- SOURCE
   type SpeedType is digits 2;
   -- ****

   -- ****if* SMovement/SMovement.HaveOrderRequirements
   -- FUNCTION
   -- Check if all requirements for movement orders are valid
   -- RESULT
   -- Empty string if everything is ok, otherwise message what is missing
   -- SOURCE
   function HaveOrderRequirements return String is
      -- ****
      HaveCockpit, HaveEngine, HavePilot, HaveEngineer: Boolean := False;
   begin
      Find_Modules_Loop :
      for Module of PlayerShip.Modules loop
         if Module.M_Type = COCKPIT and Module.Durability > 0 then
            HaveCockpit := True;
         elsif Module.M_Type = ENGINE
           and then (Module.Durability > 1 and not Module.Disabled) then
            HaveEngine := True;
         end if;
         exit Find_Modules_Loop when HaveEngine and HaveCockpit;
      end loop Find_Modules_Loop;
      if not HaveEngine then
         return
           "You don't have a working engine on your ship or all of the engines are destroyed.";
      end if;
      if not HaveCockpit then
         return
           "You don't have a cockpit on your ship or the cockpit is destroyed.";
      end if;
      if Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) then
         HavePilot := True;
         HaveEngineer := True;
      end if;
      Find_Members_Loop :
      for Member of PlayerShip.Crew loop
         if Member.Order = Pilot then
            HavePilot := True;
         elsif Member.Order = Engineer then
            HaveEngineer := True;
         end if;
         exit Find_Members_Loop when HavePilot and HaveEngineer;
      end loop Find_Members_Loop;
      if not HavePilot then
         return "You don't have a pilot on duty.";
      end if;
      if not HaveEngineer then
         return "You don't have an engineer on duty.";
      end if;
      return "";
   end HaveOrderRequirements;

   function MoveShip
     (X, Y: Integer; Message: in out Unbounded_String) return Natural is
      NewX, NewY: Integer;
      TimePassed, FuelNeeded: Integer := 0;
      Speed: SpeedType;
      FuelIndex: Inventory_Container.Extended_Index;
      function NeedRest(Order: Crew_Orders) return Boolean is
         MemberIndex: Crew_Container.Extended_Index;
      begin
         MemberIndex := FindMember(Order);
         if MemberIndex = 0 then
            Find_Member_Loop :
            for Member of PlayerShip.Crew loop
               if Member.PreviousOrder = Order then
                  return True;
               end if;
            end loop Find_Member_Loop;
         end if;
         return False;
      end NeedRest;
   begin
      case PlayerShip.Speed is
         when DOCKED =>
            Message :=
              To_Unbounded_String
                ("First you must undock your ship from the base.");
            return 0;
         when FULL_STOP =>
            Message :=
              To_Unbounded_String
                ("First you must set the speed of your ship.");
            return 0;
         when others =>
            null;
      end case;
      Message := To_Unbounded_String(HaveOrderRequirements);
      if Length(Message) > 0 then
         return 0;
      end if;
      FuelIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => Fuel_Type);
      if FuelIndex = 0 then
         Message := To_Unbounded_String("You don't have any fuel.");
         return 0;
      end if;
      FuelNeeded := CountFuelNeeded;
      if PlayerShip.Cargo(FuelIndex).Amount < abs FuelNeeded then
         Message :=
           To_Unbounded_String
             ("You don't have enough fuel (" &
              To_String
                (Items_List(PlayerShip.Cargo(FuelIndex).ProtoIndex).Name) &
              ").");
         return 0;
      end if;
      Speed := (SpeedType(RealSpeed(PlayerShip)) / 1_000.0);
      if Speed < 0.5 then
         Message :=
           To_Unbounded_String
             ("You can't fly because your ship is overloaded.");
         return 0;
      end if;
      NewX := PlayerShip.Sky_X + X;
      NewY := PlayerShip.Sky_Y + Y;
      if NewX < 1 or NewX > 1_024 or NewY < 1 or NewY > 1_024 then
         return 0;
      end if;
      PlayerShip.Sky_X := NewX;
      PlayerShip.Sky_Y := NewY;
      UpdateCargo
        (PlayerShip, PlayerShip.Cargo.Element(FuelIndex).ProtoIndex,
         FuelNeeded);
      TimePassed := Integer(100.0 / Speed);
      if TimePassed > 0 then
         case PlayerShip.Speed is
            when QUARTER_SPEED =>
               if TimePassed < 60 then
                  TimePassed := 60;
               end if;
            when HALF_SPEED =>
               if TimePassed < 30 then
                  TimePassed := 30;
               end if;
            when FULL_SPEED =>
               if TimePassed < 15 then
                  TimePassed := 15;
               end if;
            when others =>
               null;
         end case;
         GameStats.DistanceTraveled := GameStats.DistanceTraveled + 1;
         Update_Game(TimePassed);
         FuelIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => Fuel_Type);
         if FuelIndex = 0 then
            AddMessage
              ("Ship falls from the sky due to a lack of fuel.", OtherMessage,
               RED);
            Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
            return 0;
         end if;
      end if;
      if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) then
         if NeedRest(Pilot) then
            if not Game_Settings.Auto_Rest then
               return 6;
            end if;
            return 8;
         end if;
         if NeedRest(Engineer) then
            if not Game_Settings.Auto_Rest then
               return 7;
            end if;
            return 8;
         end if;
      end if;
      return 1;
   end MoveShip;

   function DockShip
     (Docking: Boolean; Escape: Boolean := False) return String is
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(PlayerShip.Sky_X, PlayerShip.Sky_Y).BaseIndex;
      Message: Unbounded_String;
   begin
      Message := To_Unbounded_String(HaveOrderRequirements);
      if Length(Message) > 0 then
         return To_String(Message);
      end if;
      if Docking then
         if SkyBases(BaseIndex).Population > 0 then
            AddMessage
              ("Ship docked to base " & To_String(SkyBases(BaseIndex).Name),
               OrderMessage);
            if Game_Settings.Auto_Save = DOCK then
               SaveGame;
            end if;
            declare
               MemberIndex: Positive := 1;
            begin
               Resign_Crew_Member_Loop :
               while MemberIndex <= PlayerShip.Crew.Last_Index loop
                  if PlayerShip.Crew(MemberIndex).ContractLength = 0 then
                     DeleteMember(MemberIndex, PlayerShip);
                     SkyBases(BaseIndex).Population :=
                       SkyBases(BaseIndex).Population + 1;
                  elsif PlayerShip.Crew(MemberIndex).Loyalty < 20 and
                    GetRandom(0, PlayerShip.Crew(MemberIndex).Loyalty) <
                      10 then
                     AddMessage
                       (To_String(PlayerShip.Crew(MemberIndex).Name) &
                        " resigns from working for you.",
                        OrderMessage);
                     DeleteMember(MemberIndex, PlayerShip);
                     SkyBases(BaseIndex).Population :=
                       SkyBases(BaseIndex).Population + 1;
                     Drop_Morale_Loop :
                     for I in PlayerShip.Crew.Iterate loop
                        UpdateMorale
                          (PlayerShip, Crew_Container.To_Index(I),
                           GetRandom(-5, -1));
                     end loop Drop_Morale_Loop;
                  else
                     MemberIndex := MemberIndex + 1;
                  end if;
               end loop Resign_Crew_Member_Loop;
            end;
            if Game_Settings.Auto_Ask_For_Bases then
               AskForBases;
            end if;
            if Game_Settings.Auto_Ask_For_Events then
               AskForEvents;
            end if;
         else
            AddMessage
              ("Ship docked to base " & To_String(SkyBases(BaseIndex).Name) &
               ".",
               OrderMessage);
         end if;
         PlayerShip.Speed := DOCKED;
         Update_Game(10);
      else
         PlayerShip.Speed := Game_Settings.Undock_Speed;
         declare
            Speed: constant SpeedType :=
              (SpeedType(RealSpeed(PlayerShip)) / 1_000.0);
         begin
            if Speed < 0.5 then
               return "You can't undock because your ship is overloaded.";
            end if;
         end;
         PlayerShip.Speed := DOCKED;
         if not Escape then
            if SkyBases(BaseIndex).Population > 0 then
               declare
                  MoneyIndex2: constant Inventory_Container.Extended_Index :=
                    FindItem(PlayerShip.Cargo, Money_Index);
                  DockingCost: Natural;
                  FuelIndex: Inventory_Container.Extended_Index;
                  TraderIndex: constant Crew_Container.Extended_Index :=
                    FindMember(Talk);
               begin
                  if MoneyIndex2 = 0 then
                     return
                       "You can't undock from this base because you don't have any " &
                       To_String(Money_Name) & " to pay for docking.";
                  end if;
                  Count_Cost_Loop :
                  for Module of PlayerShip.Modules loop
                     if Module.M_Type = HULL then
                        DockingCost := Module.Max_Modules;
                        exit Count_Cost_Loop;
                     end if;
                  end loop Count_Cost_Loop;
                  DockingCost :=
                    Natural
                      (Float(DockingCost) *
                       Float(New_Game_Settings.Prices_Bonus));
                  if DockingCost = 0 then
                     DockingCost := 1;
                  end if;
                  CountPrice(DockingCost, TraderIndex);
                  if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
                     return
                       "You can't undock to this base because you don't have enough " &
                       To_String(Money_Name) & " to pay for docking.";
                  end if;
                  UpdateCargo
                    (Ship => PlayerShip, CargoIndex => MoneyIndex2,
                     Amount => (0 - DockingCost));
                  if TraderIndex > 0 then
                     GainExp(1, Talking_Skill, TraderIndex);
                  end if;
                  FuelIndex :=
                    FindItem
                      (Inventory => PlayerShip.Cargo, ItemType => Fuel_Type);
                  if FuelIndex = 0 then
                     return
                       "You can't undock from base because you don't have any fuel.";
                  end if;
                  AddMessage
                    ("Ship undocked from base " &
                     To_String(SkyBases(BaseIndex).Name) & ". You also paid" &
                     Positive'Image(DockingCost) & " " &
                     To_String(Money_Name) & " of docking fee.",
                     OrderMessage);
               end;
            else
               declare
                  FuelIndex: constant Inventory_Container.Extended_Index :=
                    FindItem
                      (Inventory => PlayerShip.Cargo, ItemType => Fuel_Type);
               begin
                  if FuelIndex = 0 then
                     return
                       "You can't undock from base because you don't have any fuel.";
                  end if;
                  AddMessage
                    ("Ship undocked from base " &
                     To_String(SkyBases(BaseIndex).Name) & ".",
                     OrderMessage);
               end;
            end if;
         else
            declare
               Roll: constant Integer := GetRandom(1, 100);
               MessageText: Unbounded_String;
               Color: Message_Color := WHITE;
               ModuleIndex: Modules_Container.Extended_Index;
            begin
               MessageText :=
                 To_Unbounded_String
                   ("Ship escaped from base " &
                    To_String(SkyBases(BaseIndex).Name) & " without paying.");
               case Roll is
                  when 1 .. 40 =>
                     ModuleIndex :=
                       GetRandom
                         (PlayerShip.Modules.First_Index,
                          PlayerShip.Modules.Last_Index);
                     Append
                       (MessageText,
                        " But your ship (" &
                        To_String(PlayerShip.Modules(ModuleIndex).Name) &
                        ") takes damage.");
                     Color := RED;
                     DamageModule
                       (PlayerShip, ModuleIndex, GetRandom(1, 30),
                        "damage during escaping from the base");
                  when others =>
                     null;
               end case;
               AddMessage(To_String(MessageText), OrderMessage, Color);
               GainRep(BaseIndex, -(GetRandom(10, 30)));
            end;
         end if;
         if PlayerShip.Crew(1).Health > 0 then
            PlayerShip.Speed := Game_Settings.Undock_Speed;
            Update_Game(5);
            if Game_Settings.Auto_Save = UNDOCK then
               SaveGame;
            end if;
         end if;
      end if;
      return "";
   end DockShip;

   function ChangeShipSpeed(SpeedValue: Ship_Speed) return String is
      HaveEngine: Boolean := False;
   begin
      Find_Engine_Loop :
      for Module of PlayerShip.Modules loop
         if Module.M_Type = ENGINE
           and then (Module.Durability > 0 and not Module.Disabled) then
            HaveEngine := True;
            exit Find_Engine_Loop;
         end if;
      end loop Find_Engine_Loop;
      if not HaveEngine then
         return
           "You don't have a working engine on your ship or all of the engines are destroyed.";
      end if;
      if FindMember(Engineer) = 0 and
        not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
          (To_Unbounded_String("sentientships")) then
         return "You don't have an engineer on duty.";
      end if;
      PlayerShip.Speed := SpeedValue;
      return "";
   end ChangeShipSpeed;

   function RealSpeed
     (Ship: Ship_Record; InfoOnly: Boolean := False) return Natural is
      BaseSpeed, Speed: Natural := 0;
      Message: Unbounded_String;
      ShipSetSpeed: Ship_Speed;
   begin
      if Ship = PlayerShip and not InfoOnly then
         Message := To_Unbounded_String(HaveOrderRequirements);
         if Length(Message) > 0 then
            return 0;
         end if;
      end if;
      declare
         Damage: Damage_Factor := 0.0;
      begin
         Find_Engine_Loop :
         for Module of Ship.Modules loop
            if Module.M_Type = ENGINE and then not Module.Disabled then
               BaseSpeed := Module.Power * 10;
               Damage :=
                 1.0 -
                 Damage_Factor
                   (Float(Module.Durability) / Float(Module.Max_Durability));
               Speed :=
                 Speed +
                 (BaseSpeed - Natural(Float(BaseSpeed) * Float(Damage)));
            end if;
         end loop Find_Engine_Loop;
      end;
      Speed :=
        Natural((Float(Speed) / Float(CountShipWeight(Ship))) * 100_000.0);
      if Ship.Crew.Length > 0 then
         if not Factions_List(Ship.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            Sentinent_Ship_Speed_Loop :
            for I in Ship.Crew.Iterate loop
               if Ship.Crew(I).Order = Pilot then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) *
                       (Float(GetSkillLevel(Ship.Crew(I), Piloting_Skill)) /
                        300.0));
               elsif Ship.Crew(I).Order = Engineer then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) *
                       (Float(GetSkillLevel(Ship.Crew(I), Engineering_Skill)) /
                        300.0));
               end if;
            end loop Sentinent_Ship_Speed_Loop;
         else
            Normal_Ship_Speed_Loop :
            for Module of Ship.Modules loop
               if Module.M_Type = HULL then
                  Speed :=
                    Speed +
                    Natural
                      (Float(Speed) * (Float(Module.Max_Modules * 2) / 300.0));
                  exit Normal_Ship_Speed_Loop;
               end if;
            end loop Normal_Ship_Speed_Loop;
         end if;
      end if;
      if Ship = PlayerShip and (Ship.Speed in DOCKED | FULL_STOP) and
        InfoOnly then
         ShipSetSpeed := Game_Settings.Undock_Speed;
         if ShipSetSpeed = FULL_STOP then
            ShipSetSpeed := QUARTER_SPEED;
         end if;
      else
         ShipSetSpeed := Ship.Speed;
      end if;
      case ShipSetSpeed is
         when QUARTER_SPEED =>
            Speed := Integer(Float(Speed) * 0.25);
         when HALF_SPEED =>
            Speed := Integer(Float(Speed) * 0.5);
         when FULL_SPEED =>
            null;
         when others =>
            return 0;
      end case;
      Speed := (Speed / 60);
      return Speed;
   end RealSpeed;

   function CountFuelNeeded return Integer is
      FuelNeeded: Integer := 0;
      Speed: Ship_Speed := PlayerShip.Speed;
   begin
      if Speed in DOCKED | FULL_STOP then
         Speed := Game_Settings.Undock_Speed;
      end if;
      Count_Fuel_Needed_Loop :
      for Module of PlayerShip.Modules loop
         if Module.M_Type = ENGINE and then not Module.Disabled then
            case Speed is
               when QUARTER_SPEED =>
                  FuelNeeded := FuelNeeded - (Module.Fuel_Usage / 4);
               when HALF_SPEED =>
                  FuelNeeded := FuelNeeded - (Module.Fuel_Usage / 2);
               when FULL_SPEED =>
                  FuelNeeded := FuelNeeded - Module.Fuel_Usage;
               when others =>
                  null;
            end case;
         end if;
      end loop Count_Fuel_Needed_Loop;
      return FuelNeeded;
   end CountFuelNeeded;

   procedure WaitInPlace(Minutes: Positive) is
      BaseFuelNeeded, FuelNeeded: Integer := 0;
      FuelIndex: Natural;
   begin
      if PlayerShip.Speed = DOCKED then
         return;
      end if;
      Needed_Fuel_Loop :
      for Module of PlayerShip.Modules loop
         if Module.M_Type = ENGINE and then not Module.Disabled then
            BaseFuelNeeded := BaseFuelNeeded - 1;
         end if;
      end loop Needed_Fuel_Loop;
      FuelNeeded := BaseFuelNeeded * (Minutes / 10);
      if GetRandom(1, 10) < (Minutes rem 10) then
         FuelNeeded := FuelNeeded + BaseFuelNeeded;
      end if;
      FuelIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => Fuel_Type);
      if FuelIndex = 0 then
         AddMessage
           ("Ship falls from the sky due to a lack of fuel.", OtherMessage,
            RED);
         Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
         return;
      end if;
      if PlayerShip.Cargo(FuelIndex).Amount <= abs (FuelNeeded) then
         AddMessage
           ("Ship falls from the sky due to a lack of fuel.", OtherMessage,
            RED);
         Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
         return;
      end if;
      UpdateCargo
        (PlayerShip, PlayerShip.Cargo.Element(FuelIndex).ProtoIndex,
         FuelNeeded);
   end WaitInPlace;

end Ships.Movement;
