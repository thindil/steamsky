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
with Messages; use Messages;
with Events;
with Maps;
with Bases;

package body Combat is

   function Start_Combat
     (Enemy_Index: Positive; New_Combat: Boolean := True) return Boolean is
      use Interfaces.C.Strings;
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Guns is array(0 .. 9, 0 .. 2) of Integer;
      type Nim_Enemy_Record is record
         Accuracy: Natural := 0;
         Combat_Ai: Integer := 0;
         Evasion: Natural := 0;
         Loot: Natural := 0;
         Perception: Natural := 0;
         Guns: Nim_Guns;
         Name: chars_ptr;
         Player_Guns: Nim_Guns;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Enemy: Nim_Enemy_Record;
      Result: Integer;
      function Start_Ada_Combat(E_Index, N_Combat: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "startAdaCombat";
      procedure Get_Ada_Enemy(N_Enemy: out Nim_Enemy_Record) with
         Import => True,
         Convention => C,
         External_Name => "getAdaEnemy";
   begin
      Set_Ship_In_Nim;
      Enemy_Ship_Index := Enemy_Index;
      Harpoon_Duration := 0;
      Boarding_Orders.Clear;
      Enemy.Distance := 10_000;
      Enemy.Harpoon_Duration := 0;
      Result :=
        Start_Ada_Combat
          (E_Index => Enemy_Index, N_Combat => (if New_Combat then 1 else 0));
      Get_Ada_Enemy(N_Enemy => Nim_Enemy);
      Enemy.Accuracy := Nim_Enemy.Accuracy;
      Enemy.Combat_Ai := Ship_Combat_Ai'Val(Nim_Enemy.Combat_Ai);
      Enemy.Evasion := Nim_Enemy.Evasion;
      Enemy.Loot := Nim_Enemy.Loot;
      Enemy.Perception := Nim_Enemy.Perception;
      End_Combat := False;
      Enemy_Name := To_Bounded_String(Source => Value(Item => Nim_Enemy.Name));
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
      if Pilot_Order = 0 then
         Pilot_Order := 2;
         Engineer_Order := 3;
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

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Combat_Ada_Turn with
         Import => True,
         Convention => C,
         External_Name => "combatAdaTurn";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Combat_Ada_Turn;
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
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

end Combat;
