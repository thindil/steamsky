--    Copyright 2016-2017 Bartek thindil Jasicki
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

with Ships; use Ships;

package body Statistics is

    procedure UpdateDestroyedShips(ShipName : Unbounded_String) is
        Updated : Boolean := False;
    begin
        for DestroyedShip of GameStats.DestroyedShips loop
            if ProtoShips_List.Element(DestroyedShip.ProtoIndex).Name = ShipName then
                DestroyedShip.Amount := DestroyedShip.Amount + 1;
                Updated := True;
                exit;
            end if;
        end loop;
        if not Updated then
            for I in ProtoShips_List.First_Index..ProtoShips_List.Last_Index loop
                if ProtoShips_List.Element(I).Name = ShipName then
                    GameStats.DestroyedShips.Append(New_Item => (ProtoIndex => I, Amount => 1));
                    exit;
                end if;
            end loop;
        end if;
    end UpdateDestroyedShips;

    procedure ClearGameStats is
    begin
        GameStats.DestroyedShips.Clear;
        GameStats.BasesVisited := 1;
        GameStats.MapVisited := 1;
        GameStats.DistanceTraveled := 0;
        GameStats.CraftingOrders := 0;
        GameStats.AcceptedMissions := 0;
        GameStats.FinishedMissions := 0;
    end ClearGameStats;

end Statistics;
