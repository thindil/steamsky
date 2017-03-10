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

package Ships.Crew is
    
    function GetSkillLevel(MemberIndex, SkillIndex : Positive; Ship : ShipRecord := PlayerShip) 
        return Natural; -- Get level of skill of selected crew member
    procedure Death(MemberIndex : Positive; Reason : Unbounded_String; Ship : in out ShipRecord); -- Handle crew member death
    procedure DeleteMember(MemberIndex : Positive; Ship : in out ShipRecord); -- Delete selected member from crew list
    function FindMember(Order : Crew_Orders; Ship : ShipRecord := PlayerShip) 
        return Natural; -- Find index of first crew member with selected order

end Ships.Crew;
