--    Copyright 2016 Bartek thindil Jasicki
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

with Maps; use Maps;

package body Ships is

    function MoveShip(ShipIndex, X, Y: Integer) return Boolean is
        NewX, NewY : Integer;
    begin
        if ShipIndex = 0 then
            if PlayerShip.Speed < QUARTER_SPEED then
                return False;
            end if;
            NewX := PlayerShip.SkyX + X;
            NewY := PlayerShip.SkyY + Y;
        end if;
        if NewX < 1 or NewX > 1024 or NewY < 1 or NewY > 1024 then
            return False;
        end if;
        if ShipIndex = 0 then
            PlayerShip.SkyX := NewX;
            PlayerShip.SkyY := NewY;
        end if;
        return True;
    end MoveShip;

    procedure DockShip(Docking : Boolean) is
    begin
        if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
            return;
        end if;
        if Docking and PlayerShip.Speed = DOCKED then
            return;
        end if;
        if not Docking and PlayerShip.Speed > DOCKED then
            return;
        end if;
        if Docking then
            PlayerShip.Speed := DOCKED;
        else
            PlayerShip.Speed := QUARTER_SPEED;
        end if;
    end DockShip;

    procedure ChangeShipSpeed(SpeedValue : ShipSpeed) is
    begin
        if PlayerShip.Speed = DOCKED or PlayerShip.Speed = SpeedValue then
            return;
        end if;
        PlayerShip.Speed := SpeedValue;
    end ChangeShipSpeed;

end Ships;
