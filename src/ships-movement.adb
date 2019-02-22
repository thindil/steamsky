--    Copyright 2017-2019 Bartek thindil Jasicki
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

with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Statistics; use Statistics;
with Maps; use Maps;
with Messages; use Messages;
with Config; use Config;
with Bases; use Bases;
with Utils; use Utils;

package body Ships.Movement is

   function HaveOrderRequirements return String is
      HaveCockpit, HaveEngine, HavePilot, HaveEngineer: Boolean := False;
   begin
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = COCKPIT and
           Module.Durability > 0 then
            HaveCockpit := True;
         elsif Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Durability > 1 and Module.Data(3) = 0 then
            HaveEngine := True;
         end if;
         exit when HaveEngine and HaveCockpit;
      end loop;
      if not HaveEngine then
         return "You don't have working engine on ship or all engines are destroyed.";
      end if;
      if not HaveCockpit then
         return "You don't have cockpit on ship or cockpit is destroyed.";
      end if;
      for Member of PlayerShip.Crew loop
         if Member.Order = Pilot then
            HavePilot := True;
         elsif Member.Order = Engineer then
            HaveEngineer := True;
         end if;
         exit when HavePilot and HaveEngineer;
      end loop;
      if not HavePilot then
         return "You don't have pilot on duty.";
      end if;
      if not HaveEngineer then
         return "You don't have enginner on duty.";
      end if;
      return "";
   end HaveOrderRequirements;

   function MoveShip(ShipIndex, X, Y: Integer;
      Message: in out Unbounded_String) return Natural is
      NewX, NewY: Integer;
      TimePassed, FuelNeeded: Integer := 0;
      type SpeedType is digits 2;
      Speed: SpeedType;
      FuelIndex: Natural;
      function NeedRest(Order: Crew_Orders) return Boolean is
         MemberIndex: Natural;
      begin
         MemberIndex := FindMember(Order);
         if MemberIndex = 0 then
            for Member of PlayerShip.Crew loop
               if Member.PreviousOrder = Order then
                  return True;
               end if;
            end loop;
         end if;
         return False;
      end NeedRest;
   begin
      if ShipIndex = 0 then
         case PlayerShip.Speed is
            when DOCKED =>
               Message :=
                 To_Unbounded_String("First you must undock ship from base.");
               return 0;
            when FULL_STOP =>
               Message :=
                 To_Unbounded_String("First you must set speed for ship.");
               return 0;
            when others =>
               null;
         end case;
         Message := To_Unbounded_String(HaveOrderRequirements);
         if Length(Message) > 0 then
            return 0;
         end if;
         FuelIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
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
         Speed := (SpeedType(RealSpeed(PlayerShip)) / 1000.0);
         if Speed < 0.25 then
            Message :=
              To_Unbounded_String
                ("You can't fly because your ship is overloaded.");
            return 0;
         end if;
         NewX := PlayerShip.SkyX + X;
         NewY := PlayerShip.SkyY + Y;
      end if;
      if NewX < 1 or NewX > 1024 or NewY < 1 or NewY > 1024 then
         return 0;
      end if;
      if ShipIndex = 0 then
         PlayerShip.SkyX := NewX;
         PlayerShip.SkyY := NewY;
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
            UpdateGame(TimePassed);
            FuelIndex :=
              FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
            if FuelIndex = 0 then
               AddMessage
                 ("Ship fall from sky due to lack of fuel.", OtherMessage, 3);
               Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
               return 0;
            end if;
         end if;
      end if;
      if NeedRest(Pilot) then
         if not GameSettings.AutoRest then
            return 6;
         end if;
         return 8;
      end if;
      if NeedRest(Engineer) then
         if not GameSettings.AutoRest then
            return 7;
         end if;
         return 8;
      end if;
      return 1;
   end MoveShip;

   function DockShip(Docking: Boolean) return String is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      DockingCost: Positive;
      TraderIndex: Natural := 0;
      FuelIndex: constant Natural :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
      Message: Unbounded_String;
   begin
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
         return "Here no base to dock or undock.";
      end if;
      if Docking and PlayerShip.Speed = DOCKED then
         return "Ship is docked to base.";
      end if;
      if not Docking and PlayerShip.Speed > DOCKED then
         return "Ship isn't docked to base.";
      end if;
      Message := To_Unbounded_String(HaveOrderRequirements);
      if Length(Message) > 0 then
         return To_String(Message);
      end if;
      if Docking then
         if SkyBases(BaseIndex).Population > 0 then
            if MoneyIndex2 = 0 then
               return "You can't dock to base because you don't have " &
                 To_String(MoneyName) & " to pay for docking.";
            end if;
            for Module of PlayerShip.Modules loop
               if Modules_List(Module.ProtoIndex).MType = HULL then
                  DockingCost := Module.Data(2);
                  exit;
               end if;
            end loop;
            TraderIndex := FindMember(Talk);
            CountPrice(DockingCost, TraderIndex);
            if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
               return "You can't dock to base because you don't have enough " &
                 To_String(MoneyName) & " to pay for docking.";
            end if;
            UpdateCargo
              (Ship => PlayerShip, CargoIndex => MoneyIndex2,
               Amount => (0 - DockingCost));
            AddMessage
              ("Ship docked to base " & To_String(SkyBases(BaseIndex).Name) &
               ". It costs" & Positive'Image(DockingCost) & " " &
               To_String(MoneyName) & ".",
               OrderMessage);
            if TraderIndex > 0 then
               GainExp(1, TalkingSkill, TraderIndex);
            end if;
            declare
               MemberIndex: Positive := 1;
            begin
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
                        " resign from working for you.",
                        OrderMessage);
                     DeleteMember(MemberIndex, PlayerShip);
                     SkyBases(BaseIndex).Population :=
                       SkyBases(BaseIndex).Population + 1;
                     for I in PlayerShip.Crew.Iterate loop
                        UpdateMorale
                          (PlayerShip, Crew_Container.To_Index(I),
                           GetRandom(-5, -1));
                     end loop;
                  else
                     MemberIndex := MemberIndex + 1;
                  end if;
               end loop;
            end;
            if GameSettings.AutoAskForBases then
               AskForBases;
            end if;
            if GameSettings.AutoAskForEvents then
               AskForEvents;
            end if;
         else
            AddMessage
              ("Ship docked to base " & To_String(SkyBases(BaseIndex).Name) &
               ".",
               OrderMessage);
         end if;
         PlayerShip.Speed := DOCKED;
         UpdateGame(10);
      else
         if FuelIndex = 0 then
            return "You can't undock from base because you don't have any fuel.";
         end if;
         PlayerShip.Speed := GameSettings.UndockSpeed;
         AddMessage
           ("Ship undocked from base " & To_String(SkyBases(BaseIndex).Name),
            OrderMessage);
         UpdateGame(5);
      end if;
      return "";
   end DockShip;

   function ChangeShipSpeed(SpeedValue: ShipSpeed) return String is
      HaveEngine: Boolean := False;
   begin
      if PlayerShip.Speed = DOCKED then
         return "First undock from base before you set ship speed.";
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Durability > 0 and Module.Data(3) = 0 then
            HaveEngine := True;
            exit;
         end if;
      end loop;
      if not HaveEngine then
         return "You don't have working engine on ship or all engines are destroyed.";
      end if;
      if FindMember(Engineer) = 0 then
         return "You don't have enginner on duty.";
      end if;
      PlayerShip.Speed := SpeedValue;
      return "";
   end ChangeShipSpeed;

   function RealSpeed(Ship: ShipRecord;
      InfoOnly: Boolean := False) return Natural is
      BaseSpeed, Speed: Natural := 0;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      Message: Unbounded_String;
      ShipSetSpeed: ShipSpeed;
   begin
      if Ship = PlayerShip and not InfoOnly then
         Message := To_Unbounded_String(HaveOrderRequirements);
         if Length(Message) > 0 then
            return 0;
         end if;
      end if;
      for Module of Ship.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Data(3) = 0 then
            BaseSpeed := Module.Data(2) * 10;
            Damage :=
              1.0 -
              DamageFactor
                (Float(Module.Durability) / Float(Module.MaxDurability));
            Speed :=
              Speed + (BaseSpeed - Natural(Float(BaseSpeed) * Float(Damage)));
         end if;
      end loop;
      Speed :=
        Natural((Float(Speed) / Float(CountShipWeight(Ship))) * 100000.0);
      for I in Ship.Crew.Iterate loop
         if Ship.Crew(I).Order = Pilot then
            Speed :=
              Speed +
              Natural
                (Float(Speed) *
                 (Float(GetSkillLevel(Ship.Crew(I), PilotingSkill)) / 300.0));
         elsif Ship.Crew(I).Order = Engineer then
            Speed :=
              Speed +
              Natural
                (Float(Speed) *
                 (Float(GetSkillLevel(Ship.Crew(I), EngineeringSkill)) /
                  300.0));
         end if;
      end loop;
      if Ship = PlayerShip and
        (Ship.Speed = DOCKED or Ship.Speed = FULL_STOP) and InfoOnly then
         ShipSetSpeed := GameSettings.UndockSpeed;
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
      Speed: ShipSpeed := PlayerShip.Speed;
   begin
      if Speed = DOCKED or Speed = FULL_STOP then
         Speed := GameSettings.UndockSpeed;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Data(3) = 0 then
            case Speed is
               when QUARTER_SPEED =>
                  FuelNeeded := FuelNeeded - (Module.Data(1) / 4);
               when HALF_SPEED =>
                  FuelNeeded := FuelNeeded - (Module.Data(1) / 2);
               when FULL_SPEED =>
                  FuelNeeded := FuelNeeded - Module.Data(1);
               when others =>
                  null;
            end case;
         end if;
      end loop;
      return FuelNeeded;
   end CountFuelNeeded;

   procedure WaitInPlace(Minutes: Positive) is
      FuelNeeded: Integer := 0;
      FuelIndex: Natural;
   begin
      if PlayerShip.Speed = DOCKED then
         return;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Data(3) = 0 then
            FuelNeeded := FuelNeeded - 1;
         end if;
      end loop;
      FuelNeeded := FuelNeeded * Minutes;
      FuelIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
      if FuelIndex = 0 then
         AddMessage
           ("Ship fall from sky due to lack of fuel.", OtherMessage, 3);
         Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
         return;
      end if;
      if PlayerShip.Cargo(FuelIndex).Amount <= abs (FuelNeeded) then
         AddMessage
           ("Ship fall from sky due to lack of fuel.", OtherMessage, 3);
         Death(1, To_Unbounded_String("fall of the ship"), PlayerShip);
         return;
      end if;
      UpdateCargo
        (PlayerShip, PlayerShip.Cargo.Element(FuelIndex).ProtoIndex,
         FuelNeeded);
   end WaitInPlace;

end Ships.Movement;
