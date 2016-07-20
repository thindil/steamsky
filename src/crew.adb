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

with Ships; use Ships;

package body Crew is

    procedure GiveOrders(MemberIndex : Positive; GivenOrder : Crew_Orders) is
        NewOrder : Crew_Orders;
        procedure UpdateOrder(Member : in out Member_Data) is
        begin
            Member.Order := NewOrder;
        end UpdateOrder;
    begin
        if GivenOrder = PlayerShip.Crew.Element(MemberIndex).Order then
            return;
        end if;
        if GivenOrder = Duty and MemberIndex > 1 then
            return;
        end if;
        for I in PlayerShip.Crew.First_Index..PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = GivenOrder then
                NewOrder := Rest;
                PlayerShip.Crew.Update_Element(Index => I, Process => UpdateOrder'Access);
            end if;
        end loop;
        NewOrder := GivenOrder;
        PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateOrder'Access);
    end GiveOrders;

end Crew;
