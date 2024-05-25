--    Copyright 2017-2024 Bartek thindil Jasicki
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Ships.Crew is

   procedure Death
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) is

      procedure Death_Ada
        (M_Index: Integer; Reas: chars_ptr;
         In_Player_Ship, C_Body: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deathAda";
   begin
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
      Get_Ada_Modules(Ship => Ship);
      Death_Ada
        (M_Index => Member_Index,
         Reas => New_String(Str => To_String(Source => Reason)),
         In_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         C_Body => (if Create_Body then 1 else 0));
      Set_Ada_Crew(Ship => Ship);
      Set_Ada_Modules(Ship => Ship);
   end Death;

   procedure Delete_Member
     (Member_Index: Crew_Container.Extended_Index; Ship: in out Ship_Record) is
      procedure Delete_Ada_Member(M_Index, In_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deleteAdaMember";
   begin
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
      Get_Ada_Modules(Ship => Ship);
      Delete_Ada_Member
        (M_Index => Member_Index,
         In_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Set_Ada_Crew(Ship => Ship);
      Set_Ada_Modules(Ship => Ship);
   end Delete_Member;

   function Find_Member
     (Order: Crew_Orders; Ship_Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Natural is
      use Ships.Crew_Container;

      function Find_Ada_Member
        (Ord, In_Player_Ship: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaMember";
   begin
      Get_Ada_Crew(Ship_Crew => Ship_Crew);
      if Ship_Crew = Player_Ship.Crew then
         return
           Find_Ada_Member(Ord => Crew_Orders'Pos(Order), In_Player_Ship => 1);
      end if;
      return
        Find_Ada_Member(Ord => Crew_Orders'Pos(Order), In_Player_Ship => 0);
   end Find_Member;

   procedure Update_Morale
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Amount: Integer) is
      procedure Update_Ada_Morale
        (Is_Player_Ship, M_Index, N_Value: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMorale";
   begin
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
      Update_Ada_Morale
        (Is_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         M_Index => Member_Index, N_Value => Amount);
      Set_Ada_Crew(Ship => Ship);
   end Update_Morale;

end Ships.Crew;
