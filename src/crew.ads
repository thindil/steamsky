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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Items; use Items;

package Crew is

    type Skills_Array is array(1..7, 1..2) of Natural;
    type Crew_Orders is (Pilot, Engineer, Gunner, Rest, Repair, Craft, Upgrading);
    type Member_Data is -- Data structure for ship crew member
        record
            Name : Unbounded_String; -- Name of member
            Gender : Character; -- Gender of member
            Health : Natural; -- Level of health of member
            Tired : Natural; -- Tiredness of member
            Skills: Skills_Array; -- Levels and experience in skills of member
            Hunger : Natural; -- Hunger level of member
            Thirst : Natural; -- Thirst level of member
            Order : Crew_Orders; -- Current order for member
            PreviousOrder : Crew_Orders; -- Previous order for member
        end record;
    procedure GiveOrders(MemberIndex : Positive; GivenOrder: Crew_Orders); -- Change order for selected crew member
    function Consume(ItemType : Items_Types) return Boolean; -- Eat/drink by crew member. Returns true if all ok, otherwise false
    procedure GainExp(Amount : Natural; SkillNumber, CrewIndex : Positive); -- Gain experience in selected skill.
    function GenerateMemberName(Gender : Character) return Unbounded_String; -- Generate random name for crew member
    procedure Death(MemberIndex : Positive; Reason : Unbounded_String); -- Handle crew member death
    procedure UpgradeCrew(Times : Positive); -- Upgrade ship crew

end Crew;
