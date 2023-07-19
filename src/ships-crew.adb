--    Copyright 2017-2023 Bartek thindil Jasicki
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
with HallOfFame;
with Statistics;

package body Ships.Crew is

   function Get_Skill_Level
     (Member: Member_Data; Skill_Index: Skills_Amount_Range)
      return Skill_Range is
      function Get_Ada_Skill_Level
        (M: Nim_Member_Data; S_Index: Skills_Amount_Range)
         return Skill_Range with
         Import => True,
         Convention => C,
         External_Name => "getAdaSkillLevel";
   begin
      return
        Get_Ada_Skill_Level
          (M => Member_To_Nim(Member => Member), S_Index => Skill_Index);
   end Get_Skill_Level;

   procedure Death
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) is
      use HallOfFame;
      use Statistics;

      procedure Death_Ada
        (M_Index: Integer; Reas: chars_ptr;
         In_Player_Ship, C_Body: Integer) with
         Import => True,
         Convention => C,
         External_Name => "deathAda";
   begin
      Get_Game_Stats;
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
      Get_Ada_Modules(Ship => Ship);
      Death_Ada
        (M_Index => Member_Index,
         Reas => New_String(Str => To_String(Source => Reason)),
         In_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         C_Body => (if Create_Body then 1 else 0));
      Set_Ada_Crew(Ship => Ship);
      Set_Ada_Modules(Ship => Ship);
      Load_Hof_From_Nim;
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

   procedure Give_Orders
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Given_Order: Crew_Orders;
      Module_Index: Modules_Container.Extended_Index := 0;
      Check_Priorities: Boolean := True) is
      use Interfaces.C;

      Message: chars_ptr;
      function Give_Ada_Orders
        (Get_Player_Ship, M_Index, G_Order, Mod_Index, Priorities: Natural)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "giveAdaOrders";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      Message :=
        Give_Ada_Orders
          (Get_Player_Ship => (if Ship = Player_Ship then 1 else 0),
           M_Index => Member_Index, G_Order => Crew_Orders'Pos(Given_Order),
           Mod_Index => Module_Index,
           Priorities => (if Check_Priorities then 1 else 0));
      if Strlen(Item => Message) > 0 then
         if Ship = Player_Ship then
            raise Crew_Order_Error with Value(Item => Message);
         end if;
         return;
      end if;
      Get_Ship_From_Nim(Ship => Ship);
   end Give_Orders;

   procedure Update_Orders
     (Ship: in out Ship_Record; Combat: Boolean := False) is
      procedure Update_Ada_Orders(Get_Player_Ship, Comb: Natural) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaOrders";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      Update_Ada_Orders
        (Get_Player_Ship => (if Ship = Player_Ship then 1 else 0),
         Comb => (if Combat then 1 else 0));
      Get_Ship_From_Nim(Ship => Ship);
   end Update_Orders;

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

   function Get_Current_Order
     (Member_Index: Positive) return Unbounded_String is
      function Get_Ada_Current_Order(M_Index: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCurrentOrder";
   begin
      return
        To_Unbounded_String
          (Source =>
             Value(Item => Get_Ada_Current_Order(M_Index => Member_Index)));
   end Get_Current_Order;

end Ships.Crew;
