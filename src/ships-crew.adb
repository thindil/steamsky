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
with HallOfFame; use HallOfFame;

package body Ships.Crew is

   function GetSkillLevel
     (Member: Member_Data;
      SkillIndex: Positive) return Natural is
      SkillLevel: Integer := 0;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      BaseSkillLevel: Natural;
   begin
      for Skill of Member.Skills loop
         if Skill(1) = SkillIndex then
            BaseSkillLevel :=
              Skill(2) + Member.Attributes(Skills_List(Skill(1)).Attribute)(1);
            Damage := 1.0 - DamageFactor(Float(Member.Health) / 100.0);
            SkillLevel :=
              SkillLevel +
              (BaseSkillLevel -
               Integer(Float(BaseSkillLevel) * Float(Damage)));
            if Member.Thirst > 40 then
               Damage := 1.0 - DamageFactor(Float(Member.Thirst) / 100.0);
               SkillLevel :=
                 SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
            end if;
            if Member.Hunger > 80 then
               Damage := 1.0 - DamageFactor(Float(Member.Hunger) / 100.0);
               SkillLevel :=
                 SkillLevel - (Integer(Float(BaseSkillLevel) * Float(Damage)));
            end if;
            if SkillLevel < 0 then
               SkillLevel := 0;
            end if;
            return SkillLevel;
         end if;
      end loop;
      return SkillLevel;
   end GetSkillLevel;

   procedure Death
     (MemberIndex: Positive;
      Reason: Unbounded_String;
      Ship: in out ShipRecord) is
   begin
      if MemberIndex > 1 then
         if Ship = PlayerShip then
            AddMessage
              (To_String(Ship.Crew(MemberIndex).Name) &
               " died from " &
               To_String(Reason) &
               ".",
               CombatMessage,
               3);
         end if;
      else
         if Ship = PlayerShip then
            AddMessage
              ("You died from " & To_String(Reason) & ".",
               CombatMessage,
               3);
            PlayerShip.Crew(MemberIndex).Order := Rest;
            PlayerShip.Crew(MemberIndex).Health := 0;
            UpdateHallOfFame(PlayerShip.Crew(MemberIndex).Name, Reason);
            return;
         end if;
      end if;
      Ship.Cargo.Append
      (New_Item =>
         (ProtoIndex => FindProtoItem(CorpseIndex),
          Amount => 1,
          Name =>
            Ship.Crew(MemberIndex).Name & To_Unbounded_String("'s corpse"),
          Durability => 100));
      DeleteMember(MemberIndex, Ship);
   end Death;

   procedure DeleteMember(MemberIndex: Positive; Ship: in out ShipRecord) is
   begin
      Ship.Crew.Delete(Index => MemberIndex);
      for Module of Ship.Modules loop
         if Module.Owner = MemberIndex then
            Module.Owner := 0;
         elsif Module.Owner > MemberIndex then
            Module.Owner := Module.Owner - 1;
         end if;
      end loop;
      for I in Ship.Missions.First_Index .. Ship.Missions.Last_Index loop
         if Ship.Missions(I).MType = Passenger and
           Ship.Missions(I).Target = MemberIndex then
            DeleteMission(I);
            exit;
         end if;
      end loop;
      for Mission of Ship.Missions loop
         if Mission.MType = Passenger and Mission.Target > MemberIndex then
            Mission.Target := Mission.Target - 1;
         end if;
      end loop;
   end DeleteMember;

   function FindMember
     (Order: Crew_Orders;
      Crew: Crew_Container.Vector := PlayerShip.Crew) return Natural is
   begin
      for I in Crew.Iterate loop
         if Crew(I).Order = Order then
            return Crew_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindMember;

end Ships.Crew;
