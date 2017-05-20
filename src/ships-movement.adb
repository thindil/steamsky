--    Copyright 2017 Bartek thindil Jasicki
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
with UserInterface; use UserInterface;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Statistics; use Statistics;
with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with Config; use Config;

package body Ships.Movement is

   function HaveOrderRequirements(ShowInfo: Boolean := True) return Boolean is
      HaveCockpit, HaveEngine, HavePilot, HaveEngineer: Boolean := False;
   begin
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = COCKPIT and
           Module.Durability > 0 then
            HaveCockpit := True;
         elsif Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Durability > 1 then
            HaveEngine := True;
         end if;
         exit when HaveEngine and HaveCockpit;
      end loop;
      if not HaveEngine then
         if ShowInfo then
            ShowDialog
              ("You don't have working engine on ship or all engines are destroyed.");
         end if;
         return False;
      end if;
      if not HaveCockpit then
         if ShowInfo then
            ShowDialog
              ("You don't have cockpit on ship or cockpit is destroyed.");
         end if;
         return False;
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
         if ShowInfo then
            ShowDialog("You don't have pilot on duty.");
         end if;
         return False;
      end if;
      if not HaveEngineer then
         if ShowInfo then
            ShowDialog("You don't have enginner on duty.");
         end if;
         return False;
      end if;
      return True;
   end HaveOrderRequirements;

   function MoveShip(ShipIndex, X, Y: Integer) return Natural is
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
               ShowDialog("First you must undock ship from base.");
               return 0;
            when FULL_STOP =>
               ShowDialog("First you must set speed for ship.");
               return 0;
            when others =>
               null;
         end case;
         if not HaveOrderRequirements then
            return 0;
         end if;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = ENGINE then
               case PlayerShip.Speed is
                  when QUARTER_SPEED =>
                     FuelNeeded := FuelNeeded - (Module.Current_Value / 4);
                  when HALF_SPEED =>
                     FuelNeeded := FuelNeeded - (Module.Current_Value / 2);
                  when FULL_SPEED =>
                     FuelNeeded := FuelNeeded - Module.Current_Value;
                  when others =>
                     null;
               end case;
            end if;
         end loop;
         if FuelNeeded = 0 then
            FuelNeeded := -1;
         end if;
         FuelIndex := FindCargo(ItemType => FuelType);
         if FuelIndex = 0 then
            ShowDialog("You don't have any fuel.");
            return 0;
         end if;
         if PlayerShip.Cargo(FuelIndex).Amount < abs FuelNeeded then
            ShowDialog
              ("You don't have enough fuel (" &
               To_String
                 (Items_List(PlayerShip.Cargo(FuelIndex).ProtoIndex).Name) &
               ").");
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
           (PlayerShip,
            PlayerShip.Cargo.Element(FuelIndex).ProtoIndex,
            FuelNeeded);
         Speed := (SpeedType(RealSpeed(PlayerShip)) / 1000.0);
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

   procedure DockShip(Docking: Boolean) is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ProtoMoneyIndex: constant Positive := FindProtoItem(MoneyIndex);
      MoneyIndex2: constant Natural := FindCargo(ProtoMoneyIndex);
      DockingCost: Positive;
      TraderIndex: Natural := 0;
   begin
      if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
         ShowDialog("Here no base to dock or undock.");
         return;
      end if;
      if Docking and PlayerShip.Speed = DOCKED then
         ShowDialog("Ship is docked to base.");
         return;
      end if;
      if not Docking and PlayerShip.Speed > DOCKED then
         ShowDialog("Ship isn't docked to base.");
         return;
      end if;
      if not HaveOrderRequirements then
         return;
      end if;
      if Docking then
         if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
             .Owner /=
           Abandoned then
            if MoneyIndex2 = 0 then
               ShowDialog
                 ("You can't dock to base because you don't have " &
                  To_String(MoneyName) &
                  " to pay for docking.");
               return;
            end if;
            for Module of PlayerShip.Modules loop
               if Modules_List(Module.ProtoIndex).MType = HULL then
                  DockingCost := Module.Max_Value;
                  exit;
               end if;
            end loop;
            TraderIndex := FindMember(Talk);
            if TraderIndex > 0 then
               DockingCost :=
                 DockingCost -
                 Integer
                   (Float'Floor
                      (Float(DockingCost) *
                       (Float(GetSkillLevel(TraderIndex, 4)) / 200.0)));
            end if;
            case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
              .Reputation
              (1) is
               when -24 .. -1 =>
                  DockingCost :=
                    DockingCost +
                    Integer(Float'Floor(Float(DockingCost) * 0.05));
               when 26 .. 50 =>
                  DockingCost :=
                    DockingCost -
                    Integer(Float'Floor(Float(DockingCost) * 0.05));
               when 51 .. 75 =>
                  DockingCost :=
                    DockingCost -
                    Integer(Float'Floor(Float(DockingCost) * 0.1));
               when 76 .. 100 =>
                  DockingCost :=
                    DockingCost -
                    Integer(Float'Floor(Float(DockingCost) * 0.15));
               when others =>
                  null;
            end case;
            if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
               ShowDialog
                 ("You can't dock to base because you don't have enough " &
                  To_String(MoneyName) &
                  " to pay for docking.");
               return;
            end if;
            UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - DockingCost));
            AddMessage
              ("Ship docked to base " &
               To_String(SkyBases(BaseIndex).Name) &
               ". It costs" &
               Positive'Image(DockingCost) &
               " " &
               To_String(MoneyName) &
               ".",
               OrderMessage);
            if TraderIndex > 0 then
               GainExp(1, 4, TraderIndex);
            end if;
         else
            AddMessage
              ("Ship docked to base " &
               To_String(SkyBases(BaseIndex).Name) &
               ".",
               OrderMessage);
         end if;
         PlayerShip.Speed := DOCKED;
         UpdateGame(10);
      else
         PlayerShip.Speed := QUARTER_SPEED;
         AddMessage
           ("Ship undocked from base " & To_String(SkyBases(BaseIndex).Name),
            OrderMessage);
         UpdateGame(5);
      end if;
   end DockShip;

   procedure ChangeShipSpeed
     (SpeedValue: ShipSpeed;
      ShowInfo: Boolean := True) is
      HaveEngine: Boolean := False;
   begin
      if PlayerShip.Speed = DOCKED then
         if ShowInfo then
            ShowDialog("First undock from base before you set ship speed.");
         end if;
         return;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE and
           Module.Durability > 0 then
            HaveEngine := True;
            exit;
         end if;
      end loop;
      if not HaveEngine then
         if ShowInfo then
            ShowDialog
              ("You don't have working engine on ship or all engines are destroyed.");
         end if;
         return;
      end if;
      if FindMember(Engineer) = 0 then
         if ShowInfo then
            ShowDialog("You don't have enginner on duty.");
         end if;
         return;
      end if;
      PlayerShip.Speed := SpeedValue;
   end ChangeShipSpeed;

   function RealSpeed(Ship: ShipRecord) return Natural is
      Weight: Positive;
      BaseSpeed: Natural := 0;
      Speed: Integer := 0;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
   begin
      if Ship = PlayerShip then
         if not HaveOrderRequirements(False) then
            return 0;
         end if;
      end if;
      Weight := CountShipWeight(Ship) / 500;
      for Module of Ship.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ENGINE then
            BaseSpeed := Module.Max_Value * 100;
            Damage :=
              1.0 -
              DamageFactor
                (Float(Module.Durability) / Float(Module.MaxDurability));
            Speed :=
              Speed + (BaseSpeed - Integer(Float(BaseSpeed) * Float(Damage)));
         end if;
      end loop;
      Speed := Speed - Integer((Float(Weight) / 100.0) * Float(Speed));
      for I in Ship.Crew.Iterate loop
         if Ship.Crew(I).Order = Pilot then
            Speed :=
              Speed +
              Integer
                (Float(Speed) *
                 (Float(GetSkillLevel(Crew_Container.To_Index(I), 1)) /
                  300.0));
         elsif Ship.Crew(I).Order = Engineer then
            Speed :=
              Speed +
              Integer
                (Float(Speed) *
                 (Float(GetSkillLevel(Crew_Container.To_Index(I), 2)) /
                  300.0));
         end if;
      end loop;
      case Ship.Speed is
         when QUARTER_SPEED =>
            Speed := Integer(Float(Speed) * 0.25);
         when HALF_SPEED =>
            Speed := Integer(Float(Speed) * 0.5);
         when FULL_SPEED =>
            null;
         when others =>
            Speed := 0;
      end case;
      if Speed < 0 then
         Speed := 0;
      end if;
      Speed := (Speed / 60);
      return Speed;
   end RealSpeed;

end Ships.Movement;
