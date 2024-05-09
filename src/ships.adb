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

   function Generate_Ship_Name
     (Owner: Tiny_String.Bounded_String) return Tiny_String.Bounded_String is
      use Tiny_String;

      function Generate_Ada_Ship_Name(F_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "generateAdaShipName";
   begin
      return
        To_Bounded_String
          (Source =>
             Value
               (Item =>
                  Generate_Ada_Ship_Name
                    (F_Index =>
                       New_String(Str => To_String(Source => Owner)))));
   end Generate_Ship_Name;

   function Get_Cabin_Quality(Quality: Natural) return String is
      function Get_Cabin_Quality_Nim(Q: Natural) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCabinQuality";
   begin
      return Value(Item => Get_Cabin_Quality_Nim(Q => Quality));
   end Get_Cabin_Quality;

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
           (Inventory => Inventory_To_Nim(Inventory => Ship_Crew(I).Inventory),
            Member_Index => I,
            Get_Player_Ship =>
              (if Ship_Crew = Player_Ship.Crew then 1 else 0));
      end loop Get_Ada_Crew_Loop;
   end Get_Ada_Crew;

   procedure Set_Ada_Crew(Ship: in out Ship_Record) is
      use Interfaces.C;
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Crew_Array is array(1 .. 128) of Nim_Member_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_Crew: Nim_Crew_Array;
      Nim_Inventory: Nim_Inventory_Array;
      --## rule on IMPROPER_INITIALIZATION
      Index: Positive := 1;
      Add_Member: Boolean := False;
      Crew_Amount: Natural := 0;
      procedure Set_Ada_Ship_Crew
        (N_Crew: in out Nim_Crew_Array; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaShipCrew";
      procedure Set_Ada_Crew_Inventory
         (Inventory: out Nim_Inventory_Array; Member_Index: Positive;
         Get_Player_Ship: Natural := 1) with
         Import => True,
         Convention => C,
         External_Name => "setAdaCrewInventory";
   begin
      --## rule off IMPROPER_INITIALIZATION
      Set_Ada_Ship_Crew
        (N_Crew => Nim_Crew,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      --## rule on IMPROPER_INITIALIZATION
      Count_Crew_Loop :
      for Member of Nim_Crew loop
         exit Count_Crew_Loop when Strlen(Item => Member.Name) = 0;
         Crew_Amount := Crew_Amount + 1;
      end loop Count_Crew_Loop;
      if Crew_Amount /= Natural(Ship.Crew.Length) then
         Ship.Crew.Clear;
         Add_Member := True;
      end if;
      Convert_Crew_Loop :
      for Member of Nim_Crew loop
         exit Convert_Crew_Loop when Strlen(Item => Member.Name) = 0;
         Convert_Member_Block :
         declare
            Temp_Member: Member_Data :=
              Member_Data'
                (Amount_Of_Attributes => Attributes_Amount,
                 Amount_Of_Skills => Skills_Amount, others => <>);
         begin
            Member_From_Nim(Member => Member, Ada_Member => Temp_Member);
            if Add_Member then
               Ship.Crew.Append(New_Item => Temp_Member);
            else
               Ship.Crew(Index) := Temp_Member;
               Index := Index + 1;
            end if;
         end Convert_Member_Block;
      end loop Convert_Crew_Loop;
      Set_Ada_Crew_Loop :
      for I in Ship.Crew.First_Index .. Ship.Crew.Last_Index loop
         Set_Ada_Crew_Inventory
           (Inventory => Nim_Inventory, Member_Index => I,
            Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
         Ship.Crew(I).Inventory :=
           Inventory_From_Nim(Inventory => Nim_Inventory, Size => 32);
      end loop Set_Ada_Crew_Loop;
   end Set_Ada_Crew;

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
   type Nim_Modules_Array is array(1 .. 75) of Nim_Module_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Get_Ada_Modules(Ship: Ship_Record := Player_Ship) is
      use Tiny_String;

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
      for Module of Ship.Modules loop
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
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
   end Get_Ada_Modules;

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

   procedure Get_Ada_Ship(Ship: Ship_Record := Player_Ship) is
      Nim_Ship: constant Nim_Ship_Data :=
        (Name => New_String(Str => Tiny_String.To_String(Source => Ship.Name)),
         Sky_X => Ship.Sky_X, Sky_Y => Ship.Sky_Y,
         Speed => Ship_Speed'Pos(Ship.Speed),
         Upgrade_Module => Ship.Upgrade_Module,
         Destination_X => Ship.Destination_X,
         Destination_Y => Ship.Destination_Y,
         Repair_Module => Ship.Repair_Module,
         Description =>
           New_String
             (Str => Short_String.To_String(Source => Ship.Description)),
         Home_Base => Ship.Home_Base);
      procedure Get_Ada_Ship
        (Ship_Data: Nim_Ship_Data; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaShip";
   begin
      Get_Ada_Ship
        (Ship_Data => Nim_Ship,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
   end Get_Ada_Ship;

   procedure Set_Ada_Ship(Ship: in out Ship_Record) is
      Nim_Ship: Nim_Ship_Data; --## rule line off IMPROPER_INITIALIZATION
      procedure Set_Ada_Ship
        (N_Ship: in out Nim_Ship_Data; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaShip";
   begin
      --## rule off IMPROPER_INITIALIZATION
      Set_Ada_Ship
        (N_Ship => Nim_Ship,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Ship.Name :=
        Tiny_String.To_Bounded_String(Source => Value(Item => Nim_Ship.Name));
      Ship.Sky_X := Nim_Ship.Sky_X;
      Ship.Sky_Y := Nim_Ship.Sky_Y;
      Ship.Speed := Ship_Speed'Val(Nim_Ship.Speed);
      Ship.Upgrade_Module := Nim_Ship.Upgrade_Module;
      Ship.Destination_X := Nim_Ship.Destination_X;
      Ship.Destination_Y := Nim_Ship.Destination_Y;
      Ship.Repair_Module := Nim_Ship.Repair_Module;
      Ship.Description :=
        Short_String.To_Bounded_String
          (Source => Value(Item => Nim_Ship.Description));
      Ship.Home_Base := Nim_Ship.Home_Base;
      --## rule on IMPROPER_INITIALIZATION
   end Set_Ada_Ship;

   procedure Set_Ada_Modules(Ship: in out Ship_Record) is
      use Interfaces.C;

      Nim_Modules: Nim_Modules_Array :=
        (others => Nim_Module_Data'(others => <>));
      Modules_Amount: Natural := 0;
      procedure Set_Ada_Ship_Modules
        (N_Modules: in out Nim_Modules_Array; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaShipModules";
   begin
      Set_Ada_Ship_Modules
        (N_Modules => Nim_Modules,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Count_Modules_Loop :
      for Module of Nim_Modules loop
         exit Count_Modules_Loop when Strlen(Item => Module.Name) = 0;
         Modules_Amount := Modules_Amount + 1;
      end loop Count_Modules_Loop;
      Ship.Modules.Clear;
      Convert_Modules_Loop :
      for Module of Nim_Modules loop
         exit Convert_Modules_Loop when Strlen(Item => Module.Name) = 0;
         Convert_Module_Block :
         declare
            use Tiny_String;

            M_Type: constant Module_Type_2 := Module_Type_2'Val(Module.M_Type);
            --## rule off IMPROPER_INITIALIZATION
            Temp_Module: Module_Data;
            Owners: Natural_Container.Vector;
            --## rule on IMPROPER_INITIALIZATION
         begin
            Convert_Owners_Loop :
            for Owner of Module.Owner loop
               exit Convert_Owners_Loop when Owner = -1;
               Owners.Append(New_Item => Owner);
            end loop Convert_Owners_Loop;
            if Owners.Length = 0 then
               Owners.Append(New_Item => 0);
            end if;
            case M_Type is
               when ENGINE =>
                  Temp_Module :=
                    (M_Type => ENGINE,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Power => Module.Data(2),
                     Fuel_Usage => Module.Data(1),
                     Disabled => (if Module.Data(3) = 1 then True else False));
               when CABIN =>
                  Temp_Module :=
                    (M_Type => CABIN,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Cleanliness => Module.Data(1),
                     Quality => Module.Data(2));
               when TURRET =>
                  Temp_Module :=
                    (M_Type => TURRET,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Gun_Index => Module.Data(1));
               when GUN =>
                  Temp_Module :=
                    (M_Type => GUN,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Damage => Module.Data(1),
                     Ammo_Index => Module.Data(2));
               when HULL =>
                  Temp_Module :=
                    (M_Type => HULL,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Installed_Modules => Module.Data(1),
                     Max_Modules => Module.Data(2));
               when WORKSHOP =>
                  Temp_Module :=
                    (M_Type => WORKSHOP,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Crafting_Time => Module.Data(1),
                     Crafting_Amount => Module.Data(2),
                     Crafting_Index =>
                       To_Bounded_String
                         (Source => Value(Item => Module.Data_2)));
               when MEDICAL_ROOM =>
                  Temp_Module :=
                    (M_Type => MEDICAL_ROOM,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners);
               when COCKPIT =>
                  Temp_Module :=
                    (M_Type => COCKPIT,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners);
               when ARMOR =>
                  Temp_Module :=
                    (M_Type => ARMOR,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners);
               when CARGO_ROOM =>
                  Temp_Module :=
                    (M_Type => CARGO_ROOM,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners);
               when TRAINING_ROOM =>
                  Temp_Module :=
                    (M_Type => TRAINING_ROOM,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners,
                     Trained_Skill => Count_Type(Module.Data(1)));
               when BATTERING_RAM =>
                  Temp_Module :=
                    (M_Type => BATTERING_RAM,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Damage2 => Module.Data(1),
                     Cooling_Down =>
                       (if Module.Data(2) = 1 then True else False));
               when HARPOON_GUN =>
                  Temp_Module :=
                    (M_Type => HARPOON_GUN,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Module.Name)),
                     Proto_Index => Module.Proto_Index,
                     Weight => Module.Weight, Durability => Module.Durability,
                     Max_Durability => Module.Max_Durability,
                     Upgrade_Progress => Module.Upgrade_Progress,
                     Upgrade_Action => Ship_Upgrade'Val(Module.Upgrade_Action),
                     Owner => Owners, Duration => Module.Data(1),
                     Harpoon_Index => Module.Data(2));
               when others =>
                  null;
            end case;
            Ship.Modules.Append(New_Item => Temp_Module);
         end Convert_Module_Block;
      end loop Convert_Modules_Loop;
   end Set_Ada_Modules;

   procedure Set_Ship_In_Nim(Ship: Ship_Record := Player_Ship) is
      use Maps;

      Nim_Cargo: constant Nim_Inventory_Array :=
        Inventory_To_Nim(Inventory => Ship.Cargo);
      Map_Cell: constant Sky_Cell := Sky_Map(Ship.Sky_X, Ship.Sky_Y);
   begin
      Get_Ada_Map_Cell
        (X => Ship.Sky_X, Y => Ship.Sky_Y, Base_Index => Map_Cell.Base_Index,
         Visited => (if Map_Cell.Visited then 1 else 0),
         Event_Index => Map_Cell.Event_Index,
         Mission_Index => Map_Cell.Mission_Index);
      Get_Ada_Ship(Ship => Ship);
      Get_Ada_Modules(Ship => Ship);
      Get_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Get_Ada_Crew(Ship_Crew => Ship.Crew);
   end Set_Ship_In_Nim;

   procedure Get_Ship_From_Nim(Ship: in out Ship_Record) is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Cargo: Nim_Inventory_Array :=
        Inventory_To_Nim(Inventory => Ship.Cargo);
      --## rule on IMPROPER_INITIALIZATION
   begin
      Set_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Inventory_Container.Assign
        (Target => Ship.Cargo,
         Source => Inventory_From_Nim(Inventory => Nim_Cargo, Size => 128));
      Set_Ada_Crew(Ship => Ship);
      Set_Ada_Modules(Ship => Ship);
      Set_Ada_Ship(Ship => Ship);
   end Get_Ship_From_Nim;

   function Get_Proto_Ship(Proto_Index: Positive) return Proto_Ship_Data is
      use Interfaces.C;
      use Tiny_String;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Proto_Data_Array is array(0 .. 1) of Integer;
      type Nim_Proto_Ship_Data is record
         Name: chars_ptr;
         Accuracy: Nim_Proto_Data_Array;
         Combat_Ai: Integer;
         Evasion: Nim_Proto_Data_Array;
         Loot: Nim_Proto_Data_Array;
         Perception: Nim_Proto_Data_Array;
         Combat_Value: Integer;
         Description: chars_ptr;
         Owner: chars_ptr;
      end record;
      type Nim_Proto_Ship_Data_Array is array(0 .. 14, 0 .. 2) of Integer;
      type Nim_Proto_Ship_Modules_Array is array(0 .. 64) of Integer;
      type Nim_Proto_Ship_Recipes_Array is array(0 .. 14) of chars_ptr;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_Proto_Ship: Nim_Proto_Ship_Data;
      Temp_Record: Proto_Ship_Data;
      Nim_Proto_Data: Nim_Proto_Ship_Data_Array;
      Nim_Proto_Modules: Nim_Proto_Ship_Modules_Array;
      Nim_Proto_Recipes: Nim_Proto_Ship_Recipes_Array;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Proto_Ship
        (Index: Integer; Ada_Proto_Ship: out Nim_Proto_Ship_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShip";
      procedure Get_Ada_Proto_Ship_Data
        (Index, Get_Crew: Integer;
         Ada_Proto_Ship_Data: out Nim_Proto_Ship_Data_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShipData";
      procedure Get_Ada_Proto_Ship_Modules
        (Index: Integer;
         Ada_Proto_Ship_Modules: out Nim_Proto_Ship_Modules_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShipModules";
      procedure Get_Ada_Proto_Ship_Recipes
        (Index: Integer;
         Ada_Proto_Ship_Recipes: out Nim_Proto_Ship_Recipes_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShipRecipes";
   begin
      Get_Ada_Proto_Ship
        (Index => Proto_Index, Ada_Proto_Ship => Nim_Proto_Ship);
      if Nim_Proto_Ship.Combat_Value = -1 then
         return Empty_Proto_Ship;
      end if;
      Temp_Record.Name :=
        To_Bounded_String(Source => Value(Item => Nim_Proto_Ship.Name));
      Temp_Record.Accuracy :=
        (Min_Value => Nim_Proto_Ship.Accuracy(0),
         Max_Value => Nim_Proto_Ship.Accuracy(1));
      Temp_Record.Combat_Ai := Ship_Combat_Ai'Val(Nim_Proto_Ship.Combat_Ai);
      Temp_Record.Evasion :=
        (Min_Value => Nim_Proto_Ship.Evasion(0),
         Max_Value => Nim_Proto_Ship.Evasion(1));
      Temp_Record.Loot :=
        (Min_Value => Nim_Proto_Ship.Loot(0),
         Max_Value => Nim_Proto_Ship.Loot(1));
      Temp_Record.Perception :=
        (Min_Value => Nim_Proto_Ship.Perception(0),
         Max_Value => Nim_Proto_Ship.Perception(1));
      Temp_Record.Combat_Value := Nim_Proto_Ship.Combat_Value;
      Temp_Record.Description :=
        Short_String.To_Bounded_String
          (Source => Value(Item => Nim_Proto_Ship.Description));
      Temp_Record.Owner :=
        To_Bounded_String(Source => Value(Item => Nim_Proto_Ship.Owner));
      Temp_Record.Crew.Clear; --## rule line off IMPROPER_INITIALIZATION
      Get_Ada_Proto_Ship_Data
        (Index => Proto_Index, Get_Crew => 1,
         Ada_Proto_Ship_Data => Nim_Proto_Data);
      Load_Proto_Crew_Loop :
      for J in Nim_Proto_Ship_Data_Array'Range(1) loop
         exit Load_Proto_Crew_Loop when Nim_Proto_Data(J, 0) = 0;
         Temp_Record.Crew.Append
           (New_Item =>
              (Proto_Index => Nim_Proto_Data(J, 0),
               Min_Amount => Nim_Proto_Data(J, 1),
               Max_Amount => Nim_Proto_Data(J, 2)));
      end loop Load_Proto_Crew_Loop;
      MobInventory_Container.Clear(Container => Temp_Record.Cargo);
      Get_Ada_Proto_Ship_Data
        (Index => Proto_Index, Get_Crew => 0,
         Ada_Proto_Ship_Data => Nim_Proto_Data);
      Load_Proto_Cargo_Loop :
      for J in Nim_Proto_Ship_Data_Array'Range(1) loop
         exit Load_Proto_Cargo_Loop when Nim_Proto_Data(J, 0) = 0;
         MobInventory_Container.Append
           (Container => Temp_Record.Cargo,
            New_Item =>
              (Proto_Index => Nim_Proto_Data(J, 0),
               Min_Amount => Nim_Proto_Data(J, 1),
               Max_Amount => Nim_Proto_Data(J, 2)));
      end loop Load_Proto_Cargo_Loop;
      Temp_Record.Modules.Clear;
      Get_Ada_Proto_Ship_Modules
        (Index => Proto_Index, Ada_Proto_Ship_Modules => Nim_Proto_Modules);
      Load_Proto_Modules_Loop :
      for Module of Nim_Proto_Modules loop
         exit Load_Proto_Modules_Loop when Module = 0;
         Temp_Record.Modules.Append(New_Item => Module);
      end loop Load_Proto_Modules_Loop;
      TinyString_Formal_Container.Clear
        (Container => Temp_Record.Known_Recipes);
      Get_Ada_Proto_Ship_Recipes
        (Index => Proto_Index, Ada_Proto_Ship_Recipes => Nim_Proto_Recipes);
      Load_Proto_Recipes_Loop :
      for Recipe of Nim_Proto_Recipes loop
         exit Load_Proto_Recipes_Loop when Strlen(Item => Recipe) = 0;
         TinyString_Formal_Container.Append
           (Container => Temp_Record.Known_Recipes,
            New_Item => To_Bounded_String(Source => Value(Item => Recipe)));
      end loop Load_Proto_Recipes_Loop;
      return Temp_Record;
   end Get_Proto_Ship;

end Ships;
