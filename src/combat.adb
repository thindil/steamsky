--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Interfaces.C.Strings;
with Messages;
with Events;
with Maps;
with Bases;

package body Combat is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Guns is array(0 .. 9, 0 .. 2) of Integer;
   type Nim_Enemy_Record is record
      Loot: Natural := 0;
      Guns: Nim_Guns;
      Player_Guns: Nim_Guns;
      Distance: Natural := 10_000;
      Harpoon_Duration: Natural := 0;
      Enemy_Harpoon_Duration: Natural := 0;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   procedure Get_Ada_Enemy(N_Enemy: out Nim_Enemy_Record) with
      Import => True,
      Convention => C,
      External_Name => "getAdaEnemy";

   function Start_Combat
     (Enemy_Index: Positive; New_Combat: Boolean := True) return Boolean is
      use Messages;

      Nim_Enemy: Nim_Enemy_Record;
      Result: Integer;
      function Start_Ada_Combat(E_Index, N_Combat: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "startAdaCombat";
   begin
      Set_Ship_In_Nim;
      Enemy_Ship_Index := Enemy_Index;
      Boarding_Orders.Clear;
      Result :=
        Start_Ada_Combat
          (E_Index => Enemy_Index, N_Combat => (if New_Combat then 1 else 0));
      Get_Ada_Enemy(N_Enemy => Nim_Enemy);
      Harpoon_Duration := Nim_Enemy.Harpoon_Duration;
      Enemy.Harpoon_Duration := Nim_Enemy.Enemy_Harpoon_Duration;
      Enemy.Distance := Nim_Enemy.Distance;
      Enemy.Loot := Nim_Enemy.Loot;
      Messages_Starts := Get_Last_Message_Index + 1;
      Enemy.Guns.Clear;
      Convert_Enemy_Guns_Loop :
      for I in 0 .. 9 loop
         exit Convert_Enemy_Guns_Loop when Nim_Enemy.Guns(I, 0) = -1;
         Enemy.Guns.Append
           (New_Item =>
              (1 => Nim_Enemy.Guns(I, 0), 2 => Nim_Enemy.Guns(I, 1),
               3 => Nim_Enemy.Guns(I, 2)));
      end loop Convert_Enemy_Guns_Loop;
      Guns.Clear;
      Convert_Player_Guns_Loop :
      for I in 0 .. 9 loop
         exit Convert_Player_Guns_Loop when Nim_Enemy.Player_Guns(I, 0) = -1;
         Guns.Append
           (New_Item =>
              (1 => Nim_Enemy.Player_Guns(I, 0),
               2 => Nim_Enemy.Player_Guns(I, 1),
               3 => Nim_Enemy.Player_Guns(I, 2)));
      end loop Convert_Player_Guns_Loop;
      Get_Ship_From_Nim(Ship => Enemy.Ship);
      if Get_Pilot_Order = 0 then
         Set_Pilot_Order(New_Order => 2);
         Set_Engineer_Order(New_Order => 3);
      end if;
      if Result = 1 then
         return True;
      end if;
      return False;
   end Start_Combat;

   procedure Combat_Turn is
      use Bases;
      use Events;
      use Maps;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Orders_Array is array(0 .. 49) of Integer;
      --## rule on TYPE_INITIAL_VALUES
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      --## rule off IMPROPER_INITIALIZATION
      Nim_Enemy: Nim_Enemy_Record;
      Nim_G: Nim_Guns;
      --## rule on IMPROPER_INITIALIZATION
      Nim_Orders: Nim_Orders_Array := (others => -1);
      Index: Natural := 0;
      procedure Combat_Ada_Turn with
         Import => True,
         Convention => C,
         External_Name => "combatAdaTurn";
      procedure Set_Ada_Guns(G: Nim_Guns) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGuns";
      procedure Get_Ada_Boarding_Orders(Orders: out Nim_Orders_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBoardingOrders";
      procedure Set_Ada_Boarding_Orders(Orders: Nim_Orders_Array) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBoardingOrders";
   begin
      Get_Game_Date;
      Set_Player_Guns_Loop :
      for Gun of Guns loop
         Nim_G(Index, 0) := Gun(1);
         Nim_G(Index, 1) := Gun(2);
         Nim_G(Index, 2) := Gun(3);
         Index := Index + 1;
      end loop Set_Player_Guns_Loop;
      Fill_Player_Guns_Loop :
      for I in Index .. 9 loop
         Nim_G(I, 0) := -1;
         Nim_G(I, 1) := -1;
         Nim_G(I, 2) := -1;
      end loop Fill_Player_Guns_Loop;
      Set_Ada_Guns(G => Nim_G);
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Index := 0;
      Fill_Nim_Boarding_Orders_Loop :
      for Order of Boarding_Orders loop
         Nim_Orders(Index) := Order;
         Index := Index + 1;
      end loop Fill_Nim_Boarding_Orders_Loop;
      Set_Ada_Boarding_Orders(Orders => Nim_Orders);
      Combat_Ada_Turn;
      Get_Ada_Boarding_Orders(Orders => Nim_Orders);
      Boarding_Orders.Clear;
      Fill_Boarding_Orders_Loop :
      for Order of Nim_Orders loop
         exit Fill_Boarding_Orders_Loop when Order = -1;
         Boarding_Orders.Append(New_Item => Order);
      end loop Fill_Boarding_Orders_Loop;
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Get_Ship_From_Nim(Ship => Enemy.Ship);
      Set_Game_Date;
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      Get_Ada_Enemy(N_Enemy => Nim_Enemy);
      Enemy.Distance := Nim_Enemy.Distance;
      Harpoon_Duration := Nim_Enemy.Harpoon_Duration;
      Enemy.Harpoon_Duration := Nim_Enemy.Enemy_Harpoon_Duration;
      Enemy.Guns.Clear;
      Convert_Enemy_Guns_Loop :
      for I in 0 .. 9 loop
         exit Convert_Enemy_Guns_Loop when Nim_Enemy.Guns(I, 0) = -1;
         Enemy.Guns.Append
           (New_Item =>
              (1 => Nim_Enemy.Guns(I, 0), 2 => Nim_Enemy.Guns(I, 1),
               3 => Nim_Enemy.Guns(I, 2)));
      end loop Convert_Enemy_Guns_Loop;
      Guns.Clear;
      Convert_Player_Guns_Loop :
      for I in 0 .. 9 loop
         exit Convert_Player_Guns_Loop when Nim_Enemy.Player_Guns(I, 0) = -1;
         Guns.Append
           (New_Item =>
              (1 => Nim_Enemy.Player_Guns(I, 0),
               2 => Nim_Enemy.Player_Guns(I, 1),
               3 => Nim_Enemy.Player_Guns(I, 2)));
      end loop Convert_Player_Guns_Loop;
   end Combat_Turn;

   procedure Get_Harpoon_Duration is
      procedure Get_Ada_Harpoon_Duration
        (Player_Duration, Enemy_Duration: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHarpoonDuration";
   begin
      Get_Ada_Harpoon_Duration
        (Player_Duration => Harpoon_Duration,
         Enemy_Duration => Enemy.Harpoon_Duration);
   end Get_Harpoon_Duration;

   function Get_Enemy_Name return Tiny_String.Bounded_String is
      use Interfaces.C.Strings;

      function Set_Ada_Enemy_Name return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "setAdaEnemyName";
   begin
      return
        Tiny_String.To_Bounded_String
          (Source => Value(Item => Set_Ada_Enemy_Name));
   end Get_Enemy_Name;

   function Get_End_Combat return Boolean is
      function Get_Ada_End_Combat return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaEndCombat";
   begin
      if Get_Ada_End_Combat = 1 then
         return True;
      end if;
      return False;
   end Get_End_Combat;

end Combat;
