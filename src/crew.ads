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
with Ada.Containers.Vectors; use Ada.Containers;

package Crew is

    type Skill_Array is array(1..3) of Natural;
    package Skills_Container is new Vectors(Positive, Skill_Array);
    type Crew_Orders is (Pilot, Engineer, Gunner, Rest, Repair, Craft,
        Upgrading, Talk, Heal);
    type Member_Data is -- Data structure for ship crew member
        record
            Name : Unbounded_String; -- Name of member
            Gender : Character; -- Gender of member
            Health : Natural; -- Level of health of member
            Tired : Natural; -- Tiredness of member
            Skills: Skills_Container.Vector; -- Names indexes, levels and experience in skills of member
            Hunger : Natural; -- Hunger level of member
            Thirst : Natural; -- Thirst level of member
            Order : Crew_Orders; -- Current order for member
            PreviousOrder : Crew_Orders; -- Previous order for member
            OrderTime : Integer; -- Minutes to next check for order result
        end record;

    procedure GiveOrders(MemberIndex : Positive; GivenOrder: Crew_Orders; ModuleIndex : Natural := 0); -- Change order for selected crew member
    function Consume(ItemType : String) return Boolean; -- Eat/drink by crew member. Returns true if all ok, otherwise false
    procedure GainExp(Amount : Natural; SkillNumber, CrewIndex : Positive); -- Gain experience in selected skill.
    function GenerateMemberName(Gender : Character) return Unbounded_String; -- Generate random name for crew member
    procedure Death(MemberIndex : Positive; Reason : Unbounded_String); -- Handle crew member death
    procedure UpdateCrew(Minutes : Positive; TiredPoints : Natural); -- Update ship crew
    function GetSkillLevel(MemberIndex, SkillIndex : Positive) return Natural; -- Get level of skill of selected crew member

end Crew;
