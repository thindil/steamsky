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

with Messages; use Messages;

package body Ships.Crew is

    function GetSkillLevel(MemberIndex, SkillIndex : Positive; Ship : ShipRecord := PlayerShip) return Natural is
        SkillLevel : Integer := 0;
        type DamageFactor is digits 2 range 0.0..1.0;
        Damage : DamageFactor := 0.0;
        BaseSkillLevel : Natural;
    begin
        for Skill of Ship.Crew.Element(MemberIndex).Skills loop
            if Skill(1) = SkillIndex then
                BaseSkillLevel := Skill(2);
                Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Health) / 100.0);
                SkillLevel := SkillLevel + (BaseSkillLevel - Integer(Float(BaseSkillLevel) * Float(Damage)));
                if Ship.Crew.Element(MemberIndex).Thirst > 40 then
                    Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Thirst) / 100.0);
                    SkillLevel := SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
                end if;
                if Ship.Crew.Element(MemberIndex).Hunger > 80 then
                    Damage := 1.0 - DamageFactor(Float(Ship.Crew.Element(MemberIndex).Hunger) / 100.0);
                    SkillLevel := SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
                end if;
                if SkillLevel < 0 then
                    SkillLevel := 0;
                end if;
                return SkillLevel;
            end if;
        end loop;
        return SkillLevel;
    end GetSkillLevel;

    procedure Death(MemberIndex : Positive; Reason : Unbounded_String; Ship : in out ShipRecord) is
        procedure UpdateDeath(Member : in out Member_Data) is
        begin
            Member.Order := Rest;
            Member.Health := 0;
        end UpdateDeath;
    begin
        if MemberIndex > 1 then
            if Ship = PlayerShip then
                AddMessage(To_String(Ship.Crew.Element(MemberIndex).Name) & " died from " &
                    To_String(Reason) & ".", CombatMessage);
            end if;
            Ship.Cargo.Append(New_Item => (ProtoIndex => 40, Amount => 1, Name => Ship.Crew.Element(MemberIndex).Name &
                To_Unbounded_String("'s corpse"), Durability => 100));
            DeleteMember(MemberIndex, Ship);
        else
            if Ship = PlayerShip then
                AddMessage("You died from " & To_String(Reason) & ".", CombatMessage);
                PlayerShip.Crew.Update_Element(Index => MemberIndex, Process => UpdateDeath'Access);
            else
                Ship.Cargo.Append(New_Item => (ProtoIndex => 40, Amount => 1, Name => Ship.Crew.Element(MemberIndex).Name &
                    To_Unbounded_String("'s corpse"), Durability => 100));
                DeleteMember(MemberIndex, Ship);
            end if;
        end if;
    end Death;

    procedure DeleteMember(MemberIndex : Positive; Ship : in out ShipRecord) is
    begin
        Ship.Crew.Delete(Index => MemberIndex, Count => 1);
        for Module of Ship.Modules loop
            if Module.Owner = MemberIndex then
                Module.Owner := 0;
            elsif Module.Owner > MemberIndex then
                Module.Owner := Module.Owner - 1;
            end if;
        end loop;
    end DeleteMember;

    function FindMember(Order : Crew_Orders; Ship : ShipRecord := PlayerShip) return Natural is
    begin
        for I in Ship.Crew.First_Index..Ship.Crew.Last_Index loop
            if Ship.Crew.Element(I).Order = Order then
                return I;
            end if;
        end loop;
        return 0;
    end FindMember;

end Ships.Crew;
