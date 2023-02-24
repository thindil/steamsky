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
with Utils; use Utils;
with Items; use Items;
with Factions;

package body Mobs is

   procedure Load_Mobs(Reader: Tree_Reader; File_Name: String) is
      pragma Unreferenced(Reader);
      use Interfaces.C;

      Temp_Record: Proto_Mob_Record
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Temp_Skills: Skills_Container.Vector (Capacity => Skills_Amount);
      Temp_Inventory: MobInventory_Container.Vector (Capacity => 32);
      Temp_Priorities: constant Natural_Array(1 .. 12) := (others => 0);
      Temp_Equipment: constant Equipment_Array := (others => 0);
      type Nim_Proto_Attributes_Array is array(0 .. 5, 0 .. 1) of Integer;
      type Nim_Proto_Skills_Array is array(0 .. 5, 0 .. 2) of Integer;
      type Nim_Proto_Inventory_Array is array(0 .. 19, 0 .. 2) of Integer;
      type Nim_Proto_Mob is record
         Attributes: Nim_Proto_Attributes_Array;
         Skills: Nim_Proto_Skills_Array;
         Order: Integer;
         Priorities: Natural_Array(1 .. 12);
         Inventory: Nim_Proto_Inventory_Array;
         Equipment: Nim_Equipment_Array;
      end record;
      --## rule off IMPROPER_INITIALIZATION
      Result: chars_ptr;
      Nim_Mob: Nim_Proto_Mob;
      --## rule on IMPROPER_INITIALIZATION
      function Load_Ada_Mobs(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaMobs";
      procedure Get_Ada_Mob(Index: Integer; Ada_Mob: out Nim_Proto_Mob) with
         Import => True,
         Convention => C,
         External_Name => "getAdaMob";
   begin
      Result := Load_Ada_Mobs(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Mobs_Loop :
      for I in 1 .. 126 loop
         Get_Ada_Mob(Index => I, Ada_Mob => Nim_Mob);
         exit Load_Mobs_Loop when Nim_Mob.Attributes(0, 0) = 0;
         Temp_Record :=
           (Amount_Of_Attributes => Attributes_Amount,
            Amount_Of_Skills => Skills_Amount, Skills => Temp_Skills,
            Attributes => (others => <>), Order => REST,
            Priorities => Temp_Priorities, Inventory => Temp_Inventory,
            Equipment => Temp_Equipment);
         Load_Attributes_Loop :
         for J in 0 .. 5 loop
            exit Load_Attributes_Loop when Nim_Mob.Attributes(J, 0) = 0;
            Temp_Record.Attributes(J + 1) :=
              (Level => Nim_Mob.Attributes(J, 0),
               Experience => Nim_Mob.Attributes(J, 1));
         end loop Load_Attributes_Loop;
         Load_Skills_Loop :
         for J in 0 .. 5 loop
            exit Load_Skills_Loop when Nim_Mob.Skills(J, 0) = 0;
            Skills_Container.Append
              (Container => Temp_Record.Skills,
               New_Item =>
                 (Index => Count_Type(Nim_Mob.Skills(J, 0)),
                  Level => Nim_Mob.Skills(J, 1),
                  Experience => Nim_Mob.Skills(J, 2)));
         end loop Load_Skills_Loop;
         Temp_Record.Order := Crew_Orders'Val(Nim_Mob.Order);
         Temp_Record.Priorities := Nim_Mob.Priorities;
         Load_Inventory_Loop :
         for J in 0 .. 19 loop
            exit Load_Inventory_Loop when Nim_Mob.Inventory(J, 0) = 0;
            MobInventory_Container.Append
              (Container => Temp_Record.Inventory,
               New_Item =>
                 (Proto_Index => Nim_Mob.Inventory(J, 0),
                  Min_Amount => Nim_Mob.Inventory(J, 1),
                  Max_Amount => Nim_Mob.Inventory(J, 2)));
         end loop Load_Inventory_Loop;
         Load_Equipment_Loop :
         for J in 0 .. 6 loop
            Temp_Record.Equipment(Equipment_Locations'Val(J)) :=
              Nim_Mob.Equipment(J);
         end loop Load_Equipment_Loop;
         ProtoMobs_Container.Append
           (Container => Proto_Mobs_List, New_Item => Temp_Record);
      end loop Load_Mobs_Loop;
   end Load_Mobs;

   function Generate_Mob
     (Mob_Index: ProtoMobs_Container.Extended_Index;
      Faction_Index: Tiny_String.Bounded_String) return Member_Data is
      use Factions;
      use Tiny_String;

      Mob: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Proto_Mob: constant Proto_Mob_Record :=
        ProtoMobs_Container.Element
          (Container => Proto_Mobs_List, Index => Mob_Index);
      Amount: Natural;
      Highest_Skill_Level, Weapon_Skill_Level: Skill_Range := 1;
      Skill_Index: Skills_Container.Extended_Index;
      Faction: Faction_Record;
   begin
      Mob.Faction :=
        (if Get_Random(Min => 1, Max => 100) < 99 then Faction_Index
         else Get_Random_Faction);
      Mob.Gender := 'M';
      Faction := Get_Faction(Index => Mob.Faction);
      if not Faction.Flags.Contains
          (Item => To_Unbounded_String(Source => "nogender"))
        and then Get_Random(Min => 1, Max => 100) > 50 then
         Mob.Gender := 'F';
      end if;
      Mob.Name :=
        Generate_Member_Name
          (Gender => Mob.Gender, Faction_Index => Mob.Faction);
      Skills_Loop :
      for Skill of Proto_Mob.Skills loop
         Skill_Index :=
           (if Skill.Index = Skills_Amount + 1 then Faction.Weapon_Skill
            else Skill.Index);
         if Skill.Experience = 0 then
            Skills_Container.Append
              (Container => Mob.Skills,
               New_Item =>
                 (Index => Skill_Index, Level => Skill.Level,
                  Experience => 0));
         else
            Skills_Container.Append
              (Container => Mob.Skills,
               New_Item =>
                 (Index => Skill_Index,
                  Level =>
                    Get_Random(Min => Skill.Level, Max => Skill.Experience),
                  Experience => 0));
         end if;
         if Skill_Index = Faction.Weapon_Skill then
            Weapon_Skill_Level :=
              Skills_Container.Element
                (Container => Mob.Skills,
                 Index => Skills_Container.Last_Index(Container => Mob.Skills))
                .Level;
         end if;
         if Skills_Container.Element
             (Container => Mob.Skills,
              Index => Skills_Container.Last_Index(Container => Mob.Skills))
             .Level >
           Highest_Skill_Level then
            Highest_Skill_Level :=
              Skills_Container.Element
                (Container => Mob.Skills,
                 Index => Skills_Container.Last_Index(Container => Mob.Skills))
                .Level;
         end if;
      end loop Skills_Loop;
      Attributes_Loop :
      for Attribute in Proto_Mob.Attributes'Range loop
         if Proto_Mob.Attributes(Attribute).Experience = 0 then
            Mob.Attributes(Attribute) := Proto_Mob.Attributes(Attribute);
         else
            Mob.Attributes(Attribute) :=
              (Level =>
                 Get_Random
                   (Min => Proto_Mob.Attributes(Attribute).Level,
                    Max => Proto_Mob.Attributes(Attribute).Experience),
               Experience => 0);
         end if;
      end loop Attributes_Loop;
      Inventory_Loop :
      for I in
        MobInventory_Container.First_Index(Container => Proto_Mob.Inventory) ..
          MobInventory_Container.Last_Index
            (Container => Proto_Mob.Inventory) loop
         Fill_Inventory_Block :
         declare
            Proto_Item: constant Mob_Inventory_Record :=
              MobInventory_Container.Element
                (Container => Proto_Mob.Inventory, Index => I);
         begin
            Amount :=
              (if Proto_Item.Max_Amount > 0 then
                 Get_Random
                   (Min => Proto_Item.Min_Amount, Max => Proto_Item.Max_Amount)
               else Proto_Item.Min_Amount);
            Inventory_Container.Append
              (Container => Mob.Inventory,
               New_Item =>
                 (Proto_Index => Proto_Item.Proto_Index, Amount => Amount,
                  Name => Null_Bounded_String, Durability => 100, Price => 0));
         end Fill_Inventory_Block;
      end loop Inventory_Loop;
      Mob.Equipment := Proto_Mob.Equipment;
      Equipment_Loop :
      for I in WEAPON .. LEGS loop
         Set_Equipment_Block :
         declare
            Equipment_Items_List: constant String :=
              (case I is when WEAPON => "weapon", when SHIELD => "shield",
                 when HELMET => "helmet", when TORSO => "torso",
                 when ARMS => "arms", when LEGS => "legs");
            Equipment_Item_Index: Natural;
         begin
            if Mob.Equipment(I) = 0 then
               Equipment_Item_Index := 0;
               if Get_Random(Min => 1, Max => 100) < 95 then
                  Equipment_Item_Index :=
                    Get_Random_Item
                      (Items_Indexes => Equipment_Items_List, Equip_Index => I,
                       Highest_Level => Highest_Skill_Level,
                       Weapon_Skill_Level => Weapon_Skill_Level,
                       Faction_Index => Mob.Faction, Highest_Skill => 1);
               end if;
               if Equipment_Item_Index > 0 then
                  Inventory_Container.Append
                    (Container => Mob.Inventory,
                     New_Item =>
                       (Proto_Index => Equipment_Item_Index, Amount => 1,
                        Name => Null_Bounded_String, Durability => 100,
                        Price => 0));
                  Mob.Equipment(I) :=
                    Inventory_Container.Last_Index(Container => Mob.Inventory);
               end if;
            end if;
         end Set_Equipment_Block;
      end loop Equipment_Loop;
      Mob.Orders := Proto_Mob.Priorities;
      Mob.Order := Proto_Mob.Order;
      Mob.Order_Time := 15;
      Mob.Previous_Order := REST;
      Mob.Health := 100;
      Mob.Tired := 0;
      Mob.Hunger := 0;
      Mob.Thirst := 0;
      Mob.Payment := (1 => 20, 2 => 0);
      Mob.Contract_Length := -1;
      Mob.Morale :=
        (1 =>
           (if
              Faction.Flags.Contains
                (Item => To_Unbounded_String(Source => "fanaticism"))
            then 100
            else 50),
         2 => 0);
      Mob.Loyalty := 100;
      Mob.Home_Base := 1;
      return Mob;
   end Generate_Mob;

   function Get_Random_Item
     (Items_Indexes: String; Equip_Index: Equipment_Locations;
      Highest_Level, Weapon_Skill_Level: Positive;
      Faction_Index: Tiny_String.Bounded_String; Highest_Skill: Positive)
      return Natural is
      function Get_Ada_Random_Item
        (Items: chars_ptr; E_Index, H_Level, W_Skill_Level: Integer;
         F_Index: chars_ptr; H_Skill: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "getAdaRandomItem";
   begin
      return
        Get_Ada_Random_Item
          (Items => New_String(Str => Items_Indexes),
           E_Index => Equipment_Locations'Pos(Equip_Index),
           H_Level => Highest_Level, W_Skill_Level => Weapon_Skill_Level,
           F_Index =>
             New_String(Str => Tiny_String.To_String(Source => Faction_Index)),
           H_Skill => Highest_Skill);
   end Get_Random_Item;

end Mobs;
