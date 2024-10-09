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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Maps;

package body Ships is

   function Get_Cabin_Quality(Quality: Natural) return String is
      function Get_Cabin_Quality_Nim(Q: Natural) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCabinQuality";
   begin
      return Value(Item => Get_Cabin_Quality_Nim(Q => Quality));
   end Get_Cabin_Quality;

   --## rule off TYPE_INITIAL_VALUES
   type Owners_Array is array(1 .. 10) of Integer;
   type Module_Data_Array is array(1 .. 3) of Integer;
   type Nim_Module_Data is record
      Name: chars_ptr;
      Proto_Index: Integer;
      Weight: Integer;
      Durability: Integer;
      Max_Durability: Integer;
      Owner: Owners_Array := (others => 0);
      Upgrade_Progress: Integer;
      Upgrade_Action: Integer;
      M_Type: Integer := -1;
      Data: Module_Data_Array;
      Data_2: chars_ptr;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   procedure Set_Ship_In_Nim(Ship: Ship_Record := Player_Ship) is
      use Maps;

      Nim_Cargo: constant Nim_Inventory_Array :=
        Inventory_To_Nim(Inventory => Ship.Cargo);
      Map_Cell: constant Sky_Cell := Sky_Map(Ship.Sky_X, Ship.Sky_Y);
      procedure Get_Ada_Ship(Ada_Ship: Ship_Record := Player_Ship) is
         --## rule off TYPE_INITIAL_VALUES
         type Nim_Ship_Data is record
            Name: chars_ptr;
            Sky_X: Integer;
            Sky_Y: Integer;
            Speed: Integer;
            Upgrade_Module: Integer;
            Destination_X: Integer;
            Destination_Y: Integer;
            Repair_Module: Integer;
            Description: chars_ptr;
            Home_Base: Integer;
         end record;
         --## rule on TYPE_INITIAL_VALUES
         Nim_Ship: constant Nim_Ship_Data :=
           (Name =>
              New_String
                (Str => Tiny_String.To_String(Source => Ada_Ship.Name)),
            Sky_X => Ada_Ship.Sky_X, Sky_Y => Ada_Ship.Sky_Y,
            Speed => Ship_Speed'Pos(Ada_Ship.Speed),
            Upgrade_Module => Ada_Ship.Upgrade_Module,
            Destination_X => Ada_Ship.Destination_X,
            Destination_Y => Ada_Ship.Destination_Y,
            Repair_Module => Ada_Ship.Repair_Module,
            Description =>
              New_String
                (Str =>
                   Short_String.To_String(Source => Ada_Ship.Description)),
            Home_Base => Ada_Ship.Home_Base);
         procedure Get_Ada_Ship
           (Ship_Data: Nim_Ship_Data; Is_Player_Ship: Integer) with
            Import => True,
            Convention => C,
            External_Name => "getAdaShip";
      begin
         Get_Ada_Ship
           (Ship_Data => Nim_Ship,
            Is_Player_Ship => (if Ada_Ship = Player_Ship then 1 else 0));
      end Get_Ada_Ship;
      procedure Get_Ada_Ship_Cargo
        (Cargo: Nim_Inventory_Array; Get_Player_Ship: Natural := 1) with
         Import => True,
         Convention => C,
         External_Name => "getAdaShipCargo";
      procedure Get_Ada_Modules(S: Ship_Record := Player_Ship) is
         use Tiny_String;

         --## rule off TYPE_INITIAL_VALUES
         type Nim_Modules_Array is array(1 .. 75) of Nim_Module_Data;
         --## rule on TYPE_INITIAL_VALUES
         Nim_Modules: Nim_Modules_Array :=
           (others => Nim_Module_Data'(others => <>));
         Index, Index2: Positive := 1;
         --## rule off IMPROPER_INITIALIZATION
         Tmp_Owners: Owners_Array;
         Tmp_Data: Module_Data_Array;
         --## rule on IMPROPER_INITIALIZATION
         Tmp_Data_2: chars_ptr;
         procedure Get_Ada_Ship_Modules
           (N_Modules: Nim_Modules_Array; Is_Player_Ship: Integer) with
            Import => True,
            Convention => C,
            External_Name => "getAdaShipModules";
      begin
         Convert_Modules_Loop :
         for Module of S.Modules loop
            Tmp_Owners := (others => 0);
            Index2 := 1;
            Convert_Module_Owners_Loop :
            for Owner of Module.Owner loop
               Tmp_Owners(Index2) := Owner;
               Index2 := Index2 + 1;
            end loop Convert_Module_Owners_Loop;
            case Module.M_Type is
               when ENGINE =>
                  Tmp_Data :=
                    (1 => Module.Fuel_Usage, 2 => Module.Power,
                     3 => (if Module.Disabled then 1 else 0));
               when CABIN =>
                  Tmp_Data :=
                    (1 => Module.Cleanliness, 2 => Module.Quality, 3 => 0);
               when TURRET =>
                  Tmp_Data := (1 => Module.Gun_Index, 2 => 0, 3 => 0);
               when GUN =>
                  Tmp_Data :=
                    (1 => Module.Damage, 2 => Module.Ammo_Index, 3 => 0);
               when HULL =>
                  Tmp_Data :=
                    (1 => Module.Installed_Modules, 2 => Module.Max_Modules,
                     3 => 0);
               when WORKSHOP =>
                  Tmp_Data :=
                    (1 => Module.Crafting_Time, 2 => Module.Crafting_Amount,
                     3 => 0);
               when MEDICAL_ROOM | COCKPIT | ARMOR | CARGO_ROOM | ANY =>
                  Tmp_Data := (1 => 0, 2 => 0, 3 => 0);
               when TRAINING_ROOM =>
                  Tmp_Data :=
                    (1 => Integer(Module.Trained_Skill), 2 => 0, 3 => 0);
               when BATTERING_RAM =>
                  Tmp_Data :=
                    (1 => Module.Damage2,
                     2 => (if Module.Cooling_Down then 1 else 0), 3 => 0);
               when HARPOON_GUN =>
                  Tmp_Data :=
                    (1 => Module.Duration, 2 => Module.Harpoon_Index, 3 => 0);
            end case;
            if Module.M_Type = WORKSHOP then
               Tmp_Data_2 :=
                 New_String(Str => To_String(Source => Module.Crafting_Index));
            else
               Tmp_Data_2 := New_String(Str => "");
            end if;
            Nim_Modules(Index) :=
              (Name => New_String(Str => To_String(Source => Module.Name)),
               Proto_Index => Module.Proto_Index, Weight => Module.Weight,
               Durability => Module.Durability,
               Max_Durability => Module.Max_Durability, Owner => Tmp_Owners,
               Upgrade_Progress => Module.Upgrade_Progress,
               Upgrade_Action => Ship_Upgrade'Pos(Module.Upgrade_Action),
               M_Type => Module_Type_2'Pos(Module.M_Type), Data => Tmp_Data,
               Data_2 => Tmp_Data_2);
            Index := Index + 1;
         end loop Convert_Modules_Loop;
         Get_Ada_Ship_Modules
           (N_Modules => Nim_Modules,
            Is_Player_Ship => (if S = Player_Ship then 1 else 0));
      end Get_Ada_Modules;
      procedure Get_Ada_Crew
        (Ship_Crew: Crew_Container.Vector := Player_Ship.Crew) is
         use Ships.Crew_Container;

      --## rule off TYPE_INITIAL_VALUES
         type Nim_Crew_Array is array(1 .. 128) of Nim_Member_Data;
      --## rule on TYPE_INITIAL_VALUES
         Nim_Crew: Nim_Crew_Array; --## rule line off IMPROPER_INITIALIZATION
         Index: Positive := 1;
         procedure Get_Ada_Ship_Crew
           (N_Crew: Nim_Crew_Array; Is_Player_Ship: Integer) with
            Import => True,
            Convention => C,
            External_Name => "getAdaShipCrew";
         procedure Get_Ada_Crew_Inventory
           (Inventory: Nim_Inventory_Array; Member_Index: Positive;
            Get_Player_Ship: Natural := 1) with
            Import => True,
            Convention => C,
            External_Name => "getAdaCrewInventory";
      begin
         Convert_Crew_Loop :
         for Member of Ship_Crew loop
            Nim_Crew(Index) := Member_To_Nim(Member => Member);
            Index := Index + 1;
         end loop Convert_Crew_Loop;
         Get_Ada_Ship_Crew
           (N_Crew => Nim_Crew,
            Is_Player_Ship => (if Ship_Crew = Player_Ship.Crew then 1 else 0));
         Get_Ada_Crew_Loop :
         for I in Ship_Crew.First_Index .. Ship_Crew.Last_Index loop
            Get_Ada_Crew_Inventory
              (Inventory =>
                 Inventory_To_Nim(Inventory => Ship_Crew(I).Inventory),
               Member_Index => I,
               Get_Player_Ship =>
                 (if Ship_Crew = Player_Ship.Crew then 1 else 0));
         end loop Get_Ada_Crew_Loop;
      end Get_Ada_Crew;
   begin
      Get_Ada_Map_Cell
        (X => Ship.Sky_X, Y => Ship.Sky_Y, Base_Index => Map_Cell.Base_Index,
         Visited => (if Map_Cell.Visited then 1 else 0),
         Event_Index => Map_Cell.Event_Index,
         Mission_Index => Map_Cell.Mission_Index);
      Get_Ada_Ship(Ada_Ship => Ship);
      Get_Ada_Modules(S => Ship);
      Get_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
   end Set_Ship_In_Nim;

end Ships;
