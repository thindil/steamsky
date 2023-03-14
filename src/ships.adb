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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Bases;
with Crafts; use Crafts;
with Events;
with Factions;
with Log;
with Maps; use Maps;
with ShipModules; use ShipModules;
with Ships.Crew;
with Utils;

package body Ships is

   function Create_Ship
     (Proto_Index: Proto_Ships_Container.Extended_Index;
      Name: Tiny_String.Bounded_String; X: Map_X_Range; Y: Map_Y_Range;
      Speed: Ship_Speed; Random_Upgrades: Boolean := True)
      return Ship_Record is
      use Bases;
      use Tiny_String;
      use Utils;

      Tmp_Ship: Ship_Record := Empty_Ship;
      Ship_Modules: Modules_Container.Vector := Modules_Container.Empty_Vector;
      Ship_Crew: Crew_Container.Vector := Crew_Container.Empty_Vector;
      New_Name: Bounded_String := Null_Bounded_String;
      Hull_Index: Modules_Container.Extended_Index := 0;
      Amount: Natural := 0;
      Proto_Ship: constant Proto_Ship_Data := Proto_Ships_List(Proto_Index);
      --## rule off IMPROPER_INITIALIZATION
      Ship_Cargo: Inventory_Container.Vector (Capacity => 128);
      --## rule on IMPROPER_INITIALIZATION
      Owners: Natural_Container.Vector := Natural_Container.Empty_Vector;
   begin
      -- Set ship modules
      Set_Modules_Block :
      declare
         Weight_Gain: Natural := 0;
         Max_Upgrade_Value: Positive := 1;
         Temp_Module: Base_Module_Data := (others => <>);
         Roll: Positive range 1 .. 100 := 1;
         Upgrades_Amount: Natural :=
           (if Random_Upgrades then
              Get_Random(Min => 0, Max => Positive(Proto_Ship.Modules.Length))
            else 0);
      begin
         Set_Modules_Loop :
         for Module of Proto_Ship.Modules loop
            Temp_Module := Get_Module(Index => Module);
            if Upgrades_Amount = 0 or
              Get_Random(Min => 1, Max => 100) < 51 then
               goto End_Of_Setting_Upgrades;
            end if;
            Weight_Gain :=
              Get_Module(Index => Module).Weight /
              Get_Module(Index => Module).Durability;
            if Weight_Gain < 1 then
               Weight_Gain := 1;
            end if;
            Roll := Get_Random(Min => 1, Max => 100);
            case Roll is
               when 1 .. 50 => -- Upgrade durability of module
                  Max_Upgrade_Value :=
                    Positive
                      (Float(Get_Module(Index => Module).Durability) * 1.5);
                  Temp_Module.Durability :=
                    Get_Random
                      (Min => Get_Module(Index => Module).Durability,
                       Max => Max_Upgrade_Value);
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Temp_Module.Weight :=
                    Temp_Module.Weight +
                    (Weight_Gain *
                     (Temp_Module.Durability -
                      Get_Module(Index => Module).Durability));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               when 51 .. 75 => -- Upgrade value (depends on module) of module
                  if Get_Module(Index => Module).M_Type = ENGINE then
                     Weight_Gain := Weight_Gain * 10;
                     Max_Upgrade_Value :=
                       Positive
                         (Float(Get_Module(Index => Module).Value) / 2.0);
                     Temp_Module.Value :=
                       Get_Random
                         (Min => Max_Upgrade_Value,
                          Max => Get_Module(Index => Module).Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (Get_Module(Index => Module).Value -
                         Temp_Module.Value));
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                  end if;
               when 76 ..
                     100 => -- Upgrade max_value (depends on module) of module
                  case Get_Module(Index => Module).M_Type is
                     when HULL =>
                        Weight_Gain := Weight_Gain * 10;
                     when ENGINE =>
                        Weight_Gain := 1;
                     when others =>
                        null;
                  end case;
                  if Temp_Module.M_Type in ENGINE | CABIN | GUN |
                        BATTERING_RAM | HULL | HARPOON_GUN then
                     Max_Upgrade_Value :=
                       Positive
                         (Float(Get_Module(Index => Module).Max_Value) * 1.5);
                     Temp_Module.Max_Value :=
                       Get_Random
                         (Min => Get_Module(Index => Module).Max_Value,
                          Max => Max_Upgrade_Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (Temp_Module.Max_Value -
                         Get_Module(Index => Module).Max_Value));
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                  end if;
            end case;
            Upgrades_Amount := Upgrades_Amount - 1;
            <<End_Of_Setting_Upgrades>>
            Owners.Clear;
            if Temp_Module.Max_Owners > 0 then
               Set_Module_Owners_Loop :
               for I in 1 .. Temp_Module.Max_Owners loop
                  Owners.Append(New_Item => 0);
               end loop Set_Module_Owners_Loop;
            end if;
            case Temp_Module.M_Type is
               when ENGINE =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => ENGINE,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Fuel_Usage => Temp_Module.Value,
                        Power => Temp_Module.Max_Value, Disabled => False));
               when CABIN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => CABIN,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Cleanliness => Temp_Module.Value,
                        Quality => Temp_Module.Value));
               when ALCHEMY_LAB .. GREENHOUSE =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => WORKSHOP,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Crafting_Index => Null_Bounded_String,
                        Crafting_Time => 0, Crafting_Amount => 0));
               when MEDICAL_ROOM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => MEDICAL_ROOM,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when COCKPIT =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => COCKPIT,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TRAINING_ROOM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TRAINING_ROOM,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Trained_Skill => 0));
               when TURRET =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TURRET,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Gun_Index => 0));
               when GUN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => GUN,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage => Temp_Module.Max_Value, Ammo_Index => 0));
               when CARGO =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => CARGO_ROOM,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when HULL =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => HULL,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Installed_Modules => Temp_Module.Value,
                        Max_Modules => Temp_Module.Max_Value));
               when ARMOR =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => ARMOR,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when BATTERING_RAM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => BATTERING_RAM,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage2 => Temp_Module.Max_Value,
                        Cooling_Down => False));
               when HARPOON_GUN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => HARPOON_GUN,
                        Name => Get_Module(Index => Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Duration => Temp_Module.Max_Value,
                        Harpoon_Index => 0));
               when ANY =>
                  null;
            end case;
         end loop Set_Modules_Loop;
      end Set_Modules_Block;
      -- Set ship name
      New_Name :=
        (if Name = Tiny_String.Null_Bounded_String then Proto_Ship.Name
         else Name);
      -- Set ship crew
      Set_Ship_Crew_Block :
      declare
         Member: Member_Data :=
           Member_Data'
             (Amount_Of_Attributes => Attributes_Amount,
              Amount_Of_Skills => Skills_Amount, others => <>);
      begin
         Set_Crew_Loop :
         for ProtoMember of Proto_Ship.Crew loop
            Amount :=
              (if ProtoMember.Max_Amount = 0 then ProtoMember.Min_Amount
               else Get_Random
                   (Min => ProtoMember.Min_Amount,
                    Max => ProtoMember.Max_Amount));
            Add_Crew_Member_Loop :
            for I in 1 .. Amount loop
               Member :=
                 Generate_Mob
                   (Mob_Index => ProtoMember.Proto_Index,
                    Faction_Index => Proto_Ship.Owner);
               Ship_Crew.Append(New_Item => Member);
               Modules_Loop :
               for Module of Ship_Modules loop
                  if Module.M_Type = CABIN then
                     Set_Cabin_Name_Loop :
                     for J in Module.Owner.Iterate loop
                        if Module.Owner(J) = 0 then
                           Module.Owner(J) := Ship_Crew.Last_Index;
                           if Natural_Container.To_Index(Position => J) =
                             1 then
                              Module.Name :=
                                To_String(Source => Member.Name) &
                                To_Bounded_String(Source => "'s Cabin");
                           end if;
                           exit Modules_Loop;
                        end if;
                     end loop Set_Cabin_Name_Loop;
                  end if;
               end loop Modules_Loop;
               Set_Module_Owner_Loop :
               for Module of Ship_Modules loop
                  if Module.Owner.Length > 0 then
                     if Module.Owner(1) = 0 and
                       (Module.M_Type in GUN | HARPOON_GUN and
                        Member.Order = GUNNER) then
                        Module.Owner(1) := Ship_Crew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     elsif Module.M_Type = COCKPIT and
                       Member.Order = PILOT then
                        Module.Owner(1) := Ship_Crew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     end if;
                  end if;
               end loop Set_Module_Owner_Loop;
            end loop Add_Crew_Member_Loop;
         end loop Set_Crew_Loop;
      end Set_Ship_Crew_Block;
      -- Set ship cargo
      Set_Cargo_Loop :
      for I in
        MobInventory_Container.First_Index(Container => Proto_Ship.Cargo) ..
          MobInventory_Container.Last_Index(Container => Proto_Ship.Cargo) loop
         Set_Cargo_Block :
         declare
            Proto_Cargo: constant Mob_Inventory_Record :=
              MobInventory_Container.Element
                (Container => Proto_Ship.Cargo, Index => I);
         begin
            Amount :=
              (if Proto_Cargo.Max_Amount > 0 then
                 Get_Random
                   (Min => Proto_Cargo.Min_Amount,
                    Max => Proto_Cargo.Max_Amount)
               else Proto_Cargo.Min_Amount);
            Inventory_Container.Append
              (Container => Ship_Cargo,
               New_Item =>
                 (Proto_Index => Proto_Cargo.Proto_Index, Amount => Amount,
                  Name => Null_Bounded_String, Durability => 100, Price => 0));
         end Set_Cargo_Block;
      end loop Set_Cargo_Loop;
      Tmp_Ship :=
        (Name => New_Name, Sky_X => X, Sky_Y => Y, Speed => Speed,
         Modules => Ship_Modules, Cargo => Ship_Cargo, Crew => Ship_Crew,
         Upgrade_Module => 0, Destination_X => 0, Destination_Y => 0,
         Repair_Module => 0, Description => Proto_Ship.Description,
         Home_Base => 0);
      Assing_Gun_Block :
      declare
         Gun_Assigned: Boolean := False;
      begin
         Amount := 0;
         Count_Modules_Loop :
         for I in Tmp_Ship.Modules.Iterate loop
            if Tmp_Ship.Modules(I).M_Type = TURRET then
               Count_Guns_Loop :
               for J in Tmp_Ship.Modules.Iterate loop
                  if Tmp_Ship.Modules(J).M_Type in GUN | HARPOON_GUN then
                     Gun_Assigned := False;
                     Check_Assigned_Guns_Loop :
                     for K in Tmp_Ship.Modules.Iterate loop
                        if Tmp_Ship.Modules(K).M_Type = TURRET
                          and then Tmp_Ship.Modules(K).Gun_Index =
                            Modules_Container.To_Index(Position => J) then
                           Gun_Assigned := True;
                           exit Check_Assigned_Guns_Loop;
                        end if;
                     end loop Check_Assigned_Guns_Loop;
                     if not Gun_Assigned then
                        Tmp_Ship.Modules(I).Gun_Index :=
                          Modules_Container.To_Index(Position => J);
                     end if;
                  end if;
               end loop Count_Guns_Loop;
            elsif Tmp_Ship.Modules(I).M_Type = HULL then
               Hull_Index := Modules_Container.To_Index(Position => I);
            end if;
            if Get_Module(Index => Tmp_Ship.Modules(I).Proto_Index)
                .M_Type not in
                GUN | HARPOON_GUN | ARMOR | HULL then
               Amount :=
                 Amount +
                 Get_Module(Index => Tmp_Ship.Modules(I).Proto_Index).Size;
            end if;
         end loop Count_Modules_Loop;
         Tmp_Ship.Modules(Hull_Index).Installed_Modules := Amount;
      end Assing_Gun_Block;
      -- Set known crafting recipes
      Set_Known_Recipes_Loop :
      for Recipe of Proto_Ship.Known_Recipes loop
         Add_Known_Recipe(Recipe_Index => Recipe);
      end loop Set_Known_Recipes_Loop;
      -- Set home base for ship
      if Sky_Map(X, Y).Base_Index > 0 then
         Tmp_Ship.Home_Base := Sky_Map(X, Y).Base_Index;
      else
         Find_Home_Base_Block :
         declare
            Start_X, Start_Y, End_X, End_Y: Integer;
         begin
            Start_X := X - 100;
            Normalize_Coord(Coord => Start_X);
            Start_Y := Y - 100;
            Normalize_Coord(Coord => Start_Y, Is_X_Axis => False);
            End_X := X + 100;
            Normalize_Coord(Coord => End_X);
            End_Y := Y + 100;
            Normalize_Coord(Coord => End_Y, Is_X_Axis => False);
            Bases_X_Loop :
            for Sky_X in Start_X .. End_X loop
               Bases_Y_Loop :
               for Sky_Y in Start_Y .. End_Y loop
                  if Sky_Map(Sky_X, Sky_Y).Base_Index > 0 then
                     if Sky_Bases(Sky_Map(Sky_X, Sky_Y).Base_Index).Owner =
                       Proto_Ship.Owner then
                        Tmp_Ship.Home_Base := Sky_Map(Sky_X, Sky_Y).Base_Index;
                        exit Bases_X_Loop;
                     end if;
                  end if;
               end loop Bases_Y_Loop;
            end loop Bases_X_Loop;
            if Tmp_Ship.Home_Base = 0 then
               Set_Home_Base_Loop :
               for I in Sky_Bases'Range loop
                  if Sky_Bases(I).Owner = Proto_Ship.Owner then
                     Tmp_Ship.Home_Base := I;
                     exit Set_Home_Base_Loop;
                  end if;
               end loop Set_Home_Base_Loop;
            end if;
         end Find_Home_Base_Block;
      end if;
      -- Set home base for crew members
      Set_Home_For_Members_Loop :
      for Member of Tmp_Ship.Crew loop
         Member.Home_Base :=
           (if Get_Random(Min => 1, Max => 100) < 99 then Tmp_Ship.Home_Base
            else Get_Random(Min => Sky_Bases'First, Max => Sky_Bases'Last));
      end loop Set_Home_For_Members_Loop;
      return Tmp_Ship;
   end Create_Ship;

   procedure Load_Ships(File_Name: String) is
      use Interfaces.C;
      use Tiny_String;

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
      Result: chars_ptr;
      Nim_Proto_Ship: Nim_Proto_Ship_Data;
      Temp_Record: Proto_Ship_Data;
      Nim_Proto_Data: Nim_Proto_Ship_Data_Array;
      function Load_Ada_Ships(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaShips";
      procedure Get_Ada_Proto_Ship
        (Index: Integer; Ada_Proto_Ship: out Nim_Proto_Ship_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShip";
      procedure Get_Ada_Proto_Ship_Data
        (Index, Get_Crew: Integer; Ada_Proto_Ship_Data: out Nim_Proto_Ship_Data_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaProtoShipData";
   begin
      Result := Load_Ada_Ships(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Proto_Ships_Loop :
      for I in 1 .. 400 loop
         Get_Ada_Proto_Ship(Index => I, Ada_Proto_Ship => Nim_Proto_Ship);
         exit Load_Proto_Ships_Loop when Nim_Proto_Ship.Combat_Value = -1;
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
         Get_Ada_Proto_Ship_Data(Index => I, Get_Crew => 1, Ada_Proto_Ship_Data => Nim_Proto_Data);
         Proto_Ships_List.Append(New_Item => Temp_Record);
      end loop Load_Proto_Ships_Loop;
   end Load_Ships;

   function Count_Ship_Weight(Ship: Ship_Record) return Positive is
      Weight: Natural := 0;
      Cargo_Weight: Positive := 1;
   begin
      Count_Ship_Weight_Loop :
      for Module of Ship.Modules loop
         Weight := Weight + Module.Weight;
      end loop Count_Ship_Weight_Loop;
      Count_Cargo_Weight_Loop :
      for Item of Ship.Cargo loop
         Cargo_Weight :=
           Item.Amount * Get_Proto_Item(Index => Item.Proto_Index).Weight;
         Weight := Weight + Cargo_Weight;
      end loop Count_Cargo_Weight_Loop;
      return Weight;
   end Count_Ship_Weight;

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

   function Count_Combat_Value return Natural is
      Combat_Value: Natural := 0;
      procedure Count_Ammo_Value(Item_Type_Index, Multiple: Positive) is
         use Tiny_String;

      begin
         Count_Ammo_Value_Loop :
         for Item of Player_Ship.Cargo loop
            if Get_Proto_Item(Index => Item.Proto_Index).I_Type =
              Get_Ada_Item_Type(Item_Index => Item_Type_Index - 1) then
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value +
                 (Get_Proto_Item(Index => Item.Proto_Index).Value(1) *
                  Multiple);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            end if;
         end loop Count_Ammo_Value_Loop;
      end Count_Ammo_Value;
   begin
      Count_Combat_Value_Loop :
      for Module of Player_Ship.Modules loop
         case Get_Module(Index => Module.Proto_Index).M_Type is
            when BATTERING_RAM =>
               Combat_Value := Combat_Value + Module.Damage2;
            when GUN =>
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value + Module.Max_Durability + (Module.Damage * 10);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
               Count_Ammo_Value
                 (Item_Type_Index =>
                    Get_Module(Index => Module.Proto_Index).Value,
                  Multiple => 10);
            when ARMOR =>
               Combat_Value := Combat_Value + Module.Max_Durability;
            when HARPOON_GUN =>
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value + Module.Max_Durability + (Module.Duration * 5);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
               Count_Ammo_Value
                 (Item_Type_Index =>
                    Get_Module(Index => Module.Proto_Index).Value,
                  Multiple => 5);
            when HULL =>
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value + Module.Max_Durability +
                 (Module.Max_Modules * 10);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            when others =>
               null;
         end case;
      end loop Count_Combat_Value_Loop;
      return Combat_Value;
   end Count_Combat_Value;

   function Get_Cabin_Quality(Quality: Natural) return String is
      function Get_Cabin_Quality_Nim(Q: Natural) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getCabinQuality";
   begin
      return Value(Item => Get_Cabin_Quality_Nim(Q => Quality));
   end Get_Cabin_Quality;

   procedure Damage_Module
     (Ship: in out Ship_Record; Module_Index: Modules_Container.Extended_Index;
      Damage: Positive; Death_Reason: String) is
      use Ada.Strings.Unbounded;
      use Ships.Crew;

      Real_Damage: Natural := Damage;
      Weapon_Index: Natural := 0;
      procedure Remove_Gun(Module_Index2: Positive) is
      begin
         if Ship.Modules(Module_Index2).Owner(1) > 0 then
            Death
              (Member_Index => Ship.Modules(Module_Index2).Owner(1),
               Reason => To_Unbounded_String(Source => Death_Reason),
               Ship => Ship);
         end if;
      end Remove_Gun;
   begin
      if Damage > Ship.Modules(Module_Index).Durability then
         Real_Damage := Ship.Modules(Module_Index).Durability;
      end if;
      Ship.Modules(Module_Index).Durability :=
        Ship.Modules(Module_Index).Durability - Real_Damage;
      if Ship.Modules(Module_Index).Durability = 0 then
         case Get_Module(Index => Ship.Modules(Module_Index).Proto_Index)
           .M_Type is
            when HULL | ENGINE =>
               if Ship = Player_Ship then
                  Death
                    (Member_Index => 1,
                     Reason => To_Unbounded_String(Source => Death_Reason),
                     Ship => Player_Ship);
               end if;
            when TURRET =>
               Weapon_Index := Ship.Modules(Module_Index).Gun_Index;
               if Weapon_Index > 0 then
                  Ship.Modules(Weapon_Index).Durability := 0;
                  Remove_Gun(Module_Index2 => Weapon_Index);
               end if;
            when GUN =>
               Remove_Gun(Module_Index2 => Module_Index);
            when CABIN =>
               Kill_Owners_Loop :
               for Owner of Ship.Modules(Module_Index).Owner loop
                  if Owner > 0 and then Ship.Crew(Owner).Order = REST then
                     Death
                       (Member_Index => Owner,
                        Reason => To_Unbounded_String(Source => Death_Reason),
                        Ship => Ship);
                  end if;
               end loop Kill_Owners_Loop;
            when others =>
               if Ship.Modules(Module_Index).Owner.Length > 0 then
                  if Ship.Modules(Module_Index).Owner(1) > 0
                    and then
                      Ship.Crew(Ship.Modules(Module_Index).Owner(1)).Order /=
                      REST then
                     Death
                       (Member_Index => Ship.Modules(Module_Index).Owner(1),
                        Reason => To_Unbounded_String(Source => Death_Reason),
                        Ship => Ship);
                  end if;
               end if;
         end case;
      end if;
   end Damage_Module;

   procedure Get_Ada_Crew(Ship: Ship_Record := Player_Ship) is
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
      for Member of Ship.Crew loop
         Nim_Crew(Index) := Member_To_Nim(Member => Member);
         Index := Index + 1;
      end loop Convert_Crew_Loop;
      Get_Ada_Ship_Crew
        (N_Crew => Nim_Crew,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
   end Get_Ada_Crew;

   procedure Set_Ada_Crew(Ship: in out Ship_Record) is
      use Interfaces.C;
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Crew_Array is array(1 .. 128) of Nim_Member_Data;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Crew: Nim_Crew_Array; --## rule line off IMPROPER_INITIALIZATION
      Index: Positive := 1;
      procedure Set_Ada_Ship_Crew
        (N_Crew: in out Nim_Crew_Array; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaShipCrew";
   begin
      --## rule off IMPROPER_INITIALIZATION
      Set_Ada_Ship_Crew
        (N_Crew => Nim_Crew,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      --## rule on IMPROPER_INITIALIZATION
      Convert_Crew_Loop :
      for Member of Nim_Crew loop
         exit Convert_Crew_Loop when Strlen(Item => Member.Name) = 0;
         Member_From_Nim(Member => Member, Ada_Member => Ship.Crew(Index));
         Index := Index + 1;
      end loop Convert_Crew_Loop;
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
      procedure Set_Ada_Ship_Modules
        (N_Modules: in out Nim_Modules_Array; Is_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaShipModules";
   begin
      Set_Ada_Ship_Modules
        (N_Modules => Nim_Modules,
         Is_Player_Ship => (if Ship = Player_Ship then 1 else 0));
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
            for Index in Module.Owner'Range loop
               exit Convert_Owners_Loop when Index >
                 Get_Module(Index => Module.Proto_Index).Max_Owners;
               Owners.Append(New_Item => Module.Owner(Index));
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
      use Events;

      Nim_Cargo: constant Nim_Inventory_Array :=
        Inventory_To_Nim(Inventory => Ship.Cargo);
      Map_Cell: constant Sky_Cell := Sky_Map(Ship.Sky_X, Ship.Sky_Y);
   begin
      Get_Ada_Map_Cell
        (X => Ship.Sky_X, Y => Ship.Sky_Y, Base_Index => Map_Cell.Base_Index,
         Visited => (if Map_Cell.Visited then 1 else 0),
         Event_Index => Map_Cell.Event_Index,
         Mission_Index => Map_Cell.Mission_Index);
      if Map_Cell.Event_Index > 0 then
         Get_Ada_Event
           (Index => Map_Cell.Event_Index, X => Ship.Sky_X, Y => Ship.Sky_Y,
            Time => Events_List(Map_Cell.Event_Index).Time,
            E_Type =>
              Events_Types'Pos(Events_List(Map_Cell.Event_Index).E_Type),
            Data =>
              (case Events_List(Map_Cell.Event_Index).E_Type is
                 when DOUBLEPRICE =>
                   Events_List(Map_Cell.Event_Index).Item_Index,
                 when ATTACKONBASE | ENEMYSHIP | ENEMYPATROL | TRADER |
                   FRIENDLYSHIP =>
                   Events_List(Map_Cell.Event_Index).Ship_Index,
                 when others => Events_List(Map_Cell.Event_Index).Data));
      end if;
      Get_Ada_Ship(Ship => Ship);
      Get_Ada_Modules(Ship => Ship);
      Get_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Get_Ada_Crew(Ship => Ship);
      Get_Ada_Crew_Loop :
      for I in Ship.Crew.First_Index .. Ship.Crew.Last_Index loop
         Get_Ada_Crew_Inventory
           (Inventory => Inventory_To_Nim(Inventory => Ship.Crew(I).Inventory),
            Member_Index => I,
            Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      end loop Get_Ada_Crew_Loop;
   end Set_Ship_In_Nim;

   procedure Get_Ship_From_Nim(Ship: in out Ship_Record) is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Inventory: Nim_Inventory_Array;
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
      Set_Ada_Crew_Loop :
      for I in Ship.Crew.First_Index .. Ship.Crew.Last_Index loop
         Set_Ada_Crew_Inventory
           (Inventory => Nim_Inventory, Member_Index => I,
            Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
         Ship.Crew(I).Inventory :=
           Inventory_From_Nim(Inventory => Nim_Inventory, Size => 32);
      end loop Set_Ada_Crew_Loop;
      Set_Ada_Modules(Ship => Ship);
      Set_Ada_Ship(Ship => Ship);
   end Get_Ship_From_Nim;

end Ships;
