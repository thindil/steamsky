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
with Bases; use Bases;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Statistics; use Statistics;
with Maps; use Maps;
with Messages; use Messages;
with Config; use Config;
with Factions; use Factions;

package body Ships.Movement is

   function Move_Ship
     (X, Y: Integer; Message: in out Unbounded_String) return Natural is
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Speed_Type is digits 2;
      --## rule on TYPE_INITIAL_VALUES
      New_X, New_Y: Integer;
      Time_Passed, Fuel_Needed: Integer;
      Speed: Speed_Type;
      Fuel_Index: Inventory_Container.Extended_Index;
      function Need_Rest(Order: Crew_Orders) return Boolean is
         Member_Index: Crew_Container.Extended_Index;
      begin
         Member_Index := Find_Member(Order => Order);
         if Member_Index = 0 then
            Find_Member_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Previous_Order = Order then
                  return True;
               end if;
            end loop Find_Member_Loop;
         end if;
         return False;
      end Need_Rest;
      function Have_Order_Requirements return String is
         function Have_Ada_Order_Requirements return chars_ptr with
            Import => True,
            Convention => C,
            External_Name => "haveAdaOrderRequirements";
      begin
         return Value(Item => Have_Ada_Order_Requirements);
      end Have_Order_Requirements;
   begin
      case Player_Ship.Speed is
         when DOCKED =>
            Message :=
              To_Unbounded_String
                (Source => "First you must undock your ship from the base.");
            return 0;
         when FULL_STOP =>
            Message :=
              To_Unbounded_String
                (Source => "First you must set the speed of your ship.");
            return 0;
         when others =>
            null;
      end case;
      Message := To_Unbounded_String(Source => Have_Order_Requirements);
      if Length(Source => Message) > 0 then
         return 0;
      end if;
      Fuel_Index :=
        Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
      if Fuel_Index = 0 then
         Message := To_Unbounded_String(Source => "You don't have any fuel.");
         return 0;
      end if;
      Fuel_Needed := Count_Fuel_Needed;
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Fuel_Index)
          .Amount <
        abs Fuel_Needed then
         Message :=
           To_Unbounded_String
             (Source =>
                "You don't have enough fuel (" &
                To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Fuel_Index)
                            .Proto_Index)
                       .Name) &
                ").");
         return 0;
      end if;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Speed := (Speed_Type(Real_Speed(Ship => Player_Ship)) / 1_000.0);
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Speed < 0.5 then
         Message :=
           To_Unbounded_String
             (Source => "You can't fly because your ship is overloaded.");
         return 0;
      end if;
      New_X := Player_Ship.Sky_X + X;
      New_Y := Player_Ship.Sky_Y + Y;
      if New_X < 1 or New_X > 1_024 or New_Y < 1 or New_Y > 1_024 then
         return 0;
      end if;
      Player_Ship.Sky_X := New_X;
      Player_Ship.Sky_Y := New_Y;
      Update_Cargo
        (Ship => Player_Ship,
         Proto_Index =>
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Fuel_Index)
             .Proto_Index,
         Amount => Fuel_Needed);
      Time_Passed := Integer(100.0 / Speed);
      if Time_Passed > 0 then
         case Player_Ship.Speed is
            when QUARTER_SPEED =>
               if Time_Passed < 60 then
                  Time_Passed := 60;
               end if;
            when HALF_SPEED =>
               if Time_Passed < 30 then
                  Time_Passed := 30;
               end if;
            when FULL_SPEED =>
               if Time_Passed < 15 then
                  Time_Passed := 15;
               end if;
            when others =>
               null;
         end case;
         Game_Stats.Distance_Traveled := Game_Stats.Distance_Traveled + 1;
         Update_Game(Minutes => Time_Passed);
         Fuel_Index :=
           Find_Item(Inventory => Player_Ship.Cargo, Item_Type => Fuel_Type);
         if Fuel_Index = 0 then
            Add_Message
              (Message => "Ship falls from the sky due to a lack of fuel.",
               M_Type => OTHERMESSAGE, Color => RED);
            Death
              (Member_Index => 1,
               Reason => To_Unbounded_String(Source => "fall of the ship"),
               Ship => Player_Ship);
            return 0;
         end if;
      end if;
      if not Get_Faction(Index => Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         if Need_Rest(Order => PILOT) then
            if not Game_Settings.Auto_Rest then
               return 6;
            end if;
            return 8;
         end if;
         if Need_Rest(Order => ENGINEER) then
            if not Game_Settings.Auto_Rest then
               return 7;
            end if;
            return 8;
         end if;
      end if;
      return 1;
   end Move_Ship;

   function Dock_Ship
     (Docking: Boolean; Escape: Boolean := False) return String is
      function Dock_Ada_Ship(D, E: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "dockAdaShip";
      Message: Unbounded_String;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Set_Ship_In_Nim;
      Message :=
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Dock_Ada_Ship
                    (D => (if Docking then 1 else 0),
                     E => (if Escape then 1 else 0))));
      Get_Ship_From_Nim(Ship => Player_Ship);
      return To_String(Source => Message);
   end Dock_Ship;

   function Change_Ship_Speed(Speed_Value: Ship_Speed) return String is
      Have_Engine: Boolean := False;
   begin
      Find_Engine_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = ENGINE
           and then (Module.Durability > 0 and not Module.Disabled) then
            Have_Engine := True;
            exit Find_Engine_Loop;
         end if;
      end loop Find_Engine_Loop;
      if not Have_Engine then
         return
           "You don't have a working engine on your ship or all of the engines are destroyed.";
      end if;
      if Find_Member(Order => ENGINEER) = 0 and
        not Get_Faction(Index => Player_Ship.Crew(1).Faction).Flags.Contains
          (Item => To_Unbounded_String(Source => "sentientships")) then
         return "You don't have an engineer on duty.";
      end if;
      Player_Ship.Speed := Speed_Value;
      return "";
   end Change_Ship_Speed;

   function Real_Speed
     (Ship: Ship_Record; Info_Only: Boolean := False) return Natural is
      function Real_Ada_Speed
        (Of_Player_Ship, I_Only: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "realAdaSpeed";
   begin
      Set_Ship_In_Nim(Ship => Ship);
      return
        Real_Ada_Speed
          (Of_Player_Ship => (if Ship = Player_Ship then 1 else 0),
           I_Only => (if Info_Only then 1 else 0));
   end Real_Speed;

   function Count_Fuel_Needed return Integer is
      Fuel_Needed: Integer := 0;
      Speed: Ship_Speed := Player_Ship.Speed;
   begin
      if Speed in DOCKED | FULL_STOP then
         Speed := Game_Settings.Undock_Speed;
      end if;
      Count_Fuel_Needed_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = ENGINE and then not Module.Disabled then
            case Speed is
               when QUARTER_SPEED =>
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Fuel_Needed := Fuel_Needed - (Module.Fuel_Usage / 4);
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               when HALF_SPEED =>
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Fuel_Needed := Fuel_Needed - (Module.Fuel_Usage / 2);
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               when FULL_SPEED =>
                  Fuel_Needed := Fuel_Needed - Module.Fuel_Usage;
               when others =>
                  null;
            end case;
         end if;
      end loop Count_Fuel_Needed_Loop;
      return Fuel_Needed;
   end Count_Fuel_Needed;

   procedure Wait_In_Place(Minutes: Positive) is
      procedure Wait_Ada_In_Place(M: Positive) with
         Import => True,
         Convention => C,
         External_Name => "waitAdaInPlace";
   begin
      Set_Ship_In_Nim;
      Wait_Ada_In_Place(M => Minutes);
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Wait_In_Place;

end Ships.Movement;
