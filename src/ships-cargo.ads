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

package Ships.Cargo is

   procedure UpdateCargo
     (Ship: in out ShipRecord;
      ProtoIndex: Positive;
      Amount: Integer;
      Durability: Natural := 100); -- Update
   -- selected item in ship cargo
   function FreeCargo
     (Amount: Integer)
     return Integer; -- Return available space in cargo after adding/extracting Amount
   function FindCargo
     (ProtoIndex: Natural := 0;
      ItemType: Unbounded_String :=
        Null_Unbounded_String)
     return Natural; -- Return
   -- index of item, 0 if no item in ship cargo
   function GetCargoName
     (CargoIndex: Positive) return String; -- Get name of cargo
   procedure DamageCargo
     (CargoIndex: Positive;
      CrewIndex,
      SkillIndex: Natural :=
        0); -- Check if item in ship cargo was damaged

end Ships.Cargo;
