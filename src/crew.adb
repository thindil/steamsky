--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Bases;
with Maps;
with Ships; use Ships;

package body Crew is

   procedure Gain_Exp
     (Amount: Natural; Skill_Number: Skills_Amount_Range;
      Crew_Index: Positive) is
      procedure Gain_Ada_Exp(A, S_Number, C_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "gainAdaExp";
   begin
      Get_Ada_Crew;
      Gain_Ada_Exp
        (A => Amount, S_Number => Integer(Skill_Number),
         C_Index => Crew_Index);
      Set_Ada_Crew(Ship => Player_Ship);
   end Gain_Exp;

   procedure Wait_For_Rest is
      use Bases;
      use Maps;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Wait_Ada_For_Rest with
         Import => True,
         Convention => C,
         External_Name => "waitAdaForRest";
   begin
      Wait_Ada_For_Rest;
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
   end Wait_For_Rest;

   function Member_To_Nim(Member: Member_Data) return Nim_Member_Data is
      use Tiny_String;
      Nim_Member: Nim_Member_Data :=
        (Attributes => (others => (others => 0)),
         Skills => (others => (others => 0)),
         Name => New_String(Str => To_String(Source => Member.Name)),
         Gender => Member.Gender, Health => Member.Health,
         Tired => Member.Tired, Hunger => Member.Hunger,
         Thirst => Member.Thirst, Order => Crew_Orders'Pos(Member.Order),
         Previous_Order => Crew_Orders'Pos(Member.Previous_Order),
         Order_Time => Member.Order_Time, Orders => Member.Orders,
         Equipment => (others => 0), Payment => Member.Payment,
         Contract_Length => Member.Contract_Length, Morale => Member.Morale,
         Loyalty => Member.Loyalty, Home_Base => Member.Home_Base,
         Faction => New_String(Str => To_String(Source => Member.Faction)));
   begin
      Convert_Equipment_Loop :
      for I in Member.Equipment'Range loop
         Nim_Member.Equipment(Equipment_Locations'Pos(I)) :=
           Member.Equipment(I);
      end loop Convert_Equipment_Loop;
      Convert_Atrributes_Loop :
      for I in Member.Attributes'Range loop
         Nim_Member.Attributes(I, 1) := Member.Attributes(I).Level;
         Nim_Member.Attributes(I, 2) := Member.Attributes(I).Experience;
      end loop Convert_Atrributes_Loop;
      Convert_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Member.Skills) ..
          Skills_Container.Last_Index(Container => Member.Skills) loop
         Convert_Skill_Block :
         declare
            Skill: constant Skill_Info :=
              Skills_Container.Element(Container => Member.Skills, Index => I);
         begin
            Nim_Member.Skills(Integer(I), 1) := Integer(Skill.Index);
            Nim_Member.Skills(Integer(I), 2) := Skill.Level;
            Nim_Member.Skills(Integer(I), 3) := Skill.Experience;
         end Convert_Skill_Block;
      end loop Convert_Skills_Loop;
      return Nim_Member;
   end Member_To_Nim;

   procedure Member_From_Nim
     (Member: Nim_Member_Data; Ada_Member: in out Member_Data) is
      use Tiny_String;
   begin
      Ada_Member.Name :=
        To_Bounded_String(Source => Value(Item => Member.Name));
      Ada_Member.Gender := Member.Gender;
      Ada_Member.Health := Member.Health;
      Ada_Member.Tired := Member.Tired;
      Ada_Member.Hunger := Member.Hunger;
      Ada_Member.Thirst := Member.Thirst;
      Ada_Member.Order := Crew_Orders'Val(Member.Order);
      Ada_Member.Previous_Order := Crew_Orders'Val(Member.Previous_Order);
      Ada_Member.Order_Time := Member.Order_Time;
      Ada_Member.Payment := Member.Payment;
      Ada_Member.Contract_Length := Member.Contract_Length;
      Ada_Member.Morale := Member.Morale;
      Ada_Member.Loyalty := Member.Loyalty;
      Convert_Equipment_Loop :
      for I in Member.Equipment'Range loop
         Ada_Member.Equipment(Equipment_Locations'Val(I)) :=
           Member.Equipment(I) + 1;
      end loop Convert_Equipment_Loop;
      Convert_Atrributes_Loop :
      for I in Member.Attributes'Range(1) loop
         exit Convert_Atrributes_Loop when I > Attributes_Amount;
         Ada_Member.Attributes(I).Level := Member.Attributes(I, 1);
         Ada_Member.Attributes(I).Experience := Member.Attributes(I, 2);
      end loop Convert_Atrributes_Loop;
      Skills_Container.Clear(Container => Ada_Member.Skills);
      Convert_Skills_Loop :
      for I in Member.Skills'Range(1) loop
         exit Convert_Skills_Loop when Member.Skills(I, 1) = 0;
         Skills_Container.Append
           (Container => Ada_Member.Skills,
            New_Item =>
              Skill_Info'
                (Index => Skills_Amount_Range(Member.Skills(I, 1)),
                 Level => Member.Skills(I, 2),
                 Experience => Member.Skills(I, 3)));
      end loop Convert_Skills_Loop;
      Ada_Member.Faction :=
        To_Bounded_String(Source => Value(Item => Member.Faction));
      Ada_Member.Orders := Member.Orders;
      Ada_Member.Home_Base := Member.Home_Base;
   end Member_From_Nim;

end Crew;
