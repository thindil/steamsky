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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Trades; use Trades;
with Bases.Cargo; use Bases.Cargo;
with Config;
with BasesTypes;
with Maps; use Maps;
with ShipModules; use ShipModules;

package body Bases.Ship is

   procedure Repair_Ship(Module_Index: Integer) is
      use Tiny_String;

      Cost, Time: Natural := 0;
      Money_Index_2: constant Inventory_Container.Extended_Index :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
   begin
      Repair_Cost(Cost => Cost, Time => Time, Module_Index => Module_Index);
      if Cost = 0 then
         raise Bases_Ship_Nothing_To_Repair;
      end if;
      Count_Price(Price => Cost, Trader_Index => Trader_Index);
      if Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Money_Index_2)
          .Amount <
        Cost then
         raise Trade_Not_Enough_Money;
      end if;
      Give_Rest_Order_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order = REPAIR then
            Give_Orders
              (Ship => Player_Ship,
               Member_Index => Crew_Container.To_Index(Position => I),
               Given_Order => REST);
         end if;
      end loop Give_Rest_Order_Loop;
      if Module_Index > 0 then
         Player_Ship.Modules(Module_Index).Durability :=
           Player_Ship.Modules(Module_Index).Max_Durability;
         Add_Message
           (Message =>
              "You bought " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              " repair for" & Positive'Image(Cost) & " " &
              To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
      else
         Repair_Whole_Ship_Loop :
         for Module of Player_Ship.Modules loop
            if Module.Durability < Module.Max_Durability then
               Module.Durability := Module.Max_Durability;
            end if;
         end loop Repair_Whole_Ship_Loop;
         Add_Message
           (Message =>
              "You bought an entire ship repair for" & Positive'Image(Cost) &
              " " & To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
      end if;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Update_Cargo
        (Ship => Player_Ship, Cargo_Index => Money_Index_2, Amount => -(Cost));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
      Gain_Exp
        (Amount => 1, Skill_Number => Talking_Skill,
         Crew_Index => Trader_Index);
      Gain_Rep
        (Base_Index =>
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
         Points => 1);
      Update_Game(Minutes => Time);
   end Repair_Ship;

   procedure Upgrade_Ship(Install: Boolean; Module_Index: Positive) is
      use Tiny_String;

      Money_Index_2: constant Inventory_Container.Extended_Index :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Trader_Index: constant Crew_Container.Extended_Index :=
        Find_Member(Order => TALK);
      Hull_Index, Ship_Module_Index: Modules_Container.Extended_Index := 0;
      Free_Turret_Index: Modules_Container.Extended_Index := 0;
      Modules_Amount: Positive := 1;
      Price: Natural := 0;
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      --## rule off IMPROPER_INITIALIZATION
      Owners: Natural_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
   begin
      if Money_Index_2 = 0 then
         raise Trade_No_Money;
      end if;
      Find_Hull_And_Turrets_Loop :
      for I in Player_Ship.Modules.Iterate loop
         case Player_Ship.Modules(I).M_Type is
            when HULL =>
               Hull_Index := Modules_Container.To_Index(Position => I);
               Modules_Amount := Player_Ship.Modules(I).Installed_Modules;
            when TURRET =>
               if (Player_Ship.Modules(I).Gun_Index = 0 and Install)
                 and then
                   Get_Module(Index => Player_Ship.Modules(I).Proto_Index)
                     .Size >=
                   Get_Module(Index => Module_Index).Size then
                  Free_Turret_Index :=
                    Modules_Container.To_Index(Position => I);
               end if;
            when others =>
               null;
         end case;
      end loop Find_Hull_And_Turrets_Loop;
      if Install then
         Price := Get_Module(Index => Module_Index).Price;
         Count_Price(Price => Price, Trader_Index => Trader_Index);
         if Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Money_Index_2)
             .Amount <
           Price then
            raise Trade_Not_Enough_Money
              with To_String(Source => Get_Module(Index => Module_Index).Name);
         end if;
         Check_Unique_Module_Loop :
         for Module of Player_Ship.Modules loop
            if Get_Module(Index => Module.Proto_Index).M_Type =
              Get_Module(Index => Module_Index).M_Type and
              Get_Module(Index => Module_Index).Unique then
               raise Bases_Ship_Unique_Module
                 with To_String
                   (Source => Get_Module(Index => Module_Index).Name);
            end if;
         end loop Check_Unique_Module_Loop;
         if Get_Module(Index => Module_Index).M_Type = HULL then
            Check_Module_Size_Loop :
            for Module of Player_Ship.Modules loop
               if Get_Module(Index => Module.Proto_Index).Size >
                 Get_Module(Index => Module_Index).Value then
                  raise Bases_Ship_Installation_Error
                    with "This hull don't allow to have installed that big modules what you currently have.";
               end if;
            end loop Check_Module_Size_Loop;
            if Get_Module(Index => Module_Index).Max_Value <
              Modules_Amount then
               raise Bases_Ship_Installation_Error
                 with "This hull is too small for your ship. Remove some modules first.";
            end if;
            Player_Ship.Modules.Delete(Index => Hull_Index);
         else
            if Get_Module(Index => Module_Index).Size >
              Get_Module(Index => Player_Ship.Modules(Hull_Index).Proto_Index)
                .Value then
               raise Bases_Ship_Installation_Error
                 with "You can't install this module because it is too big for this hull.";
            end if;
            if Get_Module(Index => Module_Index).M_Type not in GUN |
                  HARPOON_GUN | ARMOR then
               Modules_Amount :=
                 Modules_Amount + Get_Module(Index => Module_Index).Size;
            end if;
            if Modules_Amount > Player_Ship.Modules(Hull_Index).Max_Modules and
              Get_Module(Index => Module_Index).M_Type not in GUN |
                  HARPOON_GUN | ARMOR then
               raise Bases_Ship_Installation_Error
                 with "You don't have free modules space for more modules.";
            end if;
            if
              (Get_Module(Index => Module_Index).M_Type = GUN or
               Get_Module(Index => Module_Index).M_Type = HARPOON_GUN) and
              Free_Turret_Index = 0 then
               raise Bases_Ship_Installation_Error
                 with "You don't have free turret with proprer size for this gun. Install new turret or remove old gun first.";
            end if;
         end if;
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => Money_Index_2,
            Amount => -(Price));
         --## rule on SIMPLIFIABLE_EXPRESSIONS
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Price);
         Gain_Exp
           (Amount => 1, Skill_Number => Talking_Skill,
            Crew_Index => Trader_Index);
         Gain_Rep
           (Base_Index =>
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
            Points => 1);
         Update_Game
           (Minutes => Get_Module(Index => Module_Index).Install_Time);
         if Get_Module(Index => Module_Index).M_Type = HULL then
            Player_Ship.Modules.Insert
              (Before => Hull_Index,
               New_Item =>
                 (M_Type => HULL,
                  Name => Get_Module(Index => Module_Index).Name,
                  Proto_Index => Module_Index,
                  Weight => Get_Module(Index => Module_Index).Weight,
                  Durability => Get_Module(Index => Module_Index).Durability,
                  Max_Durability =>
                    Get_Module(Index => Module_Index).Durability,
                  Owner => Owners, Upgrade_Progress => 0,
                  Upgrade_Action => NONE,
                  Installed_Modules => Get_Module(Index => Module_Index).Value,
                  Max_Modules => Get_Module(Index => Module_Index).Max_Value));
         else
            Set_Empty_Owners_Loop :
            for I in 1 .. Get_Module(Index => Module_Index).Max_Owners loop
               Owners.Append(New_Item => 0);
            end loop Set_Empty_Owners_Loop;
            case Get_Module(Index => Module_Index).M_Type is
               when ALCHEMY_LAB .. GREENHOUSE =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => WORKSHOP,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Crafting_Index => Null_Bounded_String,
                        Crafting_Time => 0, Crafting_Amount => 0));
               when MEDICAL_ROOM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => MEDICAL_ROOM,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TRAINING_ROOM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => TRAINING_ROOM,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Trained_Skill => 0));
               when COCKPIT =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => COCKPIT,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TURRET =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => TURRET,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Gun_Index => 0));
               when CABIN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => CABIN,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Cleanliness => Get_Module(Index => Module_Index).Value,
                        Quality =>
                          Get_Module(Index => Module_Index).Max_Value));
               when ShipModules.CARGO =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => CARGO_ROOM,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when ENGINE =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => ENGINE,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Fuel_Usage => Get_Module(Index => Module_Index).Value,
                        Power => Get_Module(Index => Module_Index).Max_Value,
                        Disabled => False));
               when ARMOR =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => ARMOR,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when BATTERING_RAM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => BATTERING_RAM,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage2 => Get_Module(Index => Module_Index).Max_Value,
                        Cooling_Down => False));
               when GUN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => GUN,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage => Get_Module(Index => Module_Index).Max_Value,
                        Ammo_Index => 0));
               when HARPOON_GUN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => HARPOON_GUN,
                        Name => Get_Module(Index => Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Get_Module(Index => Module_Index).Weight,
                        Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Max_Durability =>
                          Get_Module(Index => Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Duration =>
                          Get_Module(Index => Module_Index).Max_Value,
                        Harpoon_Index => 0));
               when ANY | HULL =>
                  null;
            end case;
         end if;
         case Get_Module(Index => Module_Index).M_Type is
            when GUN | HARPOON_GUN =>
               Player_Ship.Modules(Free_Turret_Index).Gun_Index :=
                 Player_Ship.Modules.Last_Index;
            when others =>
               Player_Ship.Modules(Hull_Index).Installed_Modules :=
                 Modules_Amount;
         end case;
         Add_Message
           (Message =>
              "You installed " &
              To_String(Source => Get_Module(Index => Module_Index).Name) &
              " on your ship for" & Positive'Image(Price) & " " &
              To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
      else
         Ship_Module_Index := Module_Index;
         Get_Price_Block :
         declare
            Damage: constant Damage_Factor :=
              1.0 -
              Damage_Factor
                (Float(Player_Ship.Modules(Ship_Module_Index).Durability) /
                 Float(Player_Ship.Modules(Ship_Module_Index).Max_Durability));
         begin
            Price :=
              Get_Module
                (Index => Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .Price -
              Integer
                (Float
                   (Get_Module
                      (Index =>
                         Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                      .Price) *
                 Float(Damage));
         end Get_Price_Block;
         Count_Price
           (Price => Price, Trader_Index => Trader_Index, Reduce => False);
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         if Free_Cargo(Amount => -(Price)) < 0 then
            raise Trade_No_Free_Cargo;
         end if;
         --## rule om SIMPLIFIABLE_EXPRESSIONS
         if Price >
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
             .Amount then
            raise Trade_No_Money_In_Base;
         end if;
         case Get_Module
           (Index => Player_Ship.Modules(Ship_Module_Index).Proto_Index)
           .M_Type is
            when TURRET =>
               if Player_Ship.Modules(Ship_Module_Index).Gun_Index > 0 then
                  raise Bases_Ship_Removing_Error
                    with "You have installed gun in this turret, remove it before you remove this turret.";
               end if;
            when GUN | HARPOON_GUN =>
               Find_Empty_Turret_Loop :
               for Module of Player_Ship.Modules loop
                  if Module.M_Type = TURRET
                    and then Module.Gun_Index = Ship_Module_Index then
                     Module.Gun_Index := 0;
                     exit Find_Empty_Turret_Loop;
                  end if;
               end loop Find_Empty_Turret_Loop;
            when ShipModules.CARGO =>
               if Free_Cargo
                   (Amount =>
                      0 -
                      Get_Module
                        (Index =>
                           Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                        .Max_Value) <
                 0 then
                  raise Bases_Ship_Removing_Error
                    with "You can't sell this cargo bay, because you have items in it.";
               end if;
            when others =>
               null;
         end case;
         if Get_Module
             (Index => Player_Ship.Modules(Ship_Module_Index).Proto_Index)
             .M_Type not in
             HULL | ARMOR | GUN | HARPOON_GUN then
            Modules_Amount :=
              Modules_Amount -
              Get_Module
                (Index => Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .Size;
            Player_Ship.Modules(Hull_Index).Installed_Modules :=
              Modules_Amount;
         end if;
         if Player_Ship.Upgrade_Module = Ship_Module_Index then
            Player_Ship.Upgrade_Module := 0;
            Remove_Upgrade_Order_Loop :
            for Crew_Index in Player_Ship.Crew.Iterate loop
               if Player_Ship.Crew(Crew_Index).Order = UPGRADING then
                  Give_Orders
                    (Ship => Player_Ship,
                     Member_Index =>
                       Crew_Container.To_Index(Position => Crew_Index),
                     Given_Order => REST);
                  exit Remove_Upgrade_Order_Loop;
               end if;
            end loop Remove_Upgrade_Order_Loop;
         end if;
         if Player_Ship.Modules(Ship_Module_Index).M_Type /= CABIN then
            Give_Rest_Order_Block :
            declare
               Owners: constant Natural_Container.Vector :=
                 Player_Ship.Modules(Ship_Module_Index).Owner;
            begin
               Give_Rest_Order_Loop :
               for Owner of Owners loop
                  if Owner > 0 then
                     Give_Orders
                       (Ship => Player_Ship, Member_Index => Owner,
                        Given_Order => REST, Check_Priorities => False);
                  end if;
               end loop Give_Rest_Order_Loop;
               Player_Ship.Modules(Ship_Module_Index).Owner := Owners;
            end Give_Rest_Order_Block;
         end if;
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => Money_Index_2,
            Amount => Price);
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Price);
         Gain_Exp
           (Amount => 1, Skill_Number => Talking_Skill,
            Crew_Index => Trader_Index);
         Gain_Rep
           (Base_Index =>
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index,
            Points => 1);
         Update_Game
           (Minutes =>
              Get_Module
                (Index => Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .Install_Time);
         Add_Message
           (Message =>
              "You removed " &
              To_String
                (Source => Player_Ship.Modules(Ship_Module_Index).Name) &
              " from your ship and received" & Positive'Image(Price) & " " &
              To_String(Source => Money_Name) & ".",
            M_Type => TRADEMESSAGE);
         Player_Ship.Modules.Delete(Index => Ship_Module_Index);
         if Player_Ship.Repair_Module > Ship_Module_Index then
            Player_Ship.Repair_Module := Player_Ship.Repair_Module - 1;
         elsif Player_Ship.Repair_Module = Ship_Module_Index then
            Player_Ship.Repair_Module := 0;
         end if;
         if Player_Ship.Upgrade_Module > Ship_Module_Index then
            Player_Ship.Upgrade_Module := Player_Ship.Upgrade_Module - 1;
         end if;
         Update_Turrets_Loop :
         for Module of Player_Ship.Modules loop
            if Module.M_Type = TURRET
              and then Module.Gun_Index > Ship_Module_Index then
               Module.Gun_Index := Module.Gun_Index - 1;
            end if;
         end loop Update_Turrets_Loop;
      end if;
   end Upgrade_Ship;

   procedure Pay_For_Dock is
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Pay_Ada_For_Dock with
         Import => True,
         Convention => C,
         External_Name => "payAdaForDock";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Pay_Ada_For_Dock;
      Set_Base_Cargo(Base_Index => Base_Index);
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Pay_For_Dock;

   procedure Repair_Cost(Cost, Time: in out Natural; Module_Index: Integer) is
      use BasesTypes;
      use Config;

      Proto_Index: Natural := 0;
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if Module_Index > 0 then
         Time :=
           Player_Ship.Modules(Module_Index).Max_Durability -
           Player_Ship.Modules(Module_Index).Durability;
         Proto_Index :=
           Find_Proto_Item
             (Item_Type =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Repair_Material);
         Cost :=
           Time *
           Get_Price
             (Base_Type => Sky_Bases(Base_Index).Base_Type,
              Item_Index => Proto_Index);
      else
         Count_Repair_Time_And_Cost_Loop :
         for Module of Player_Ship.Modules loop
            if Module.Durability < Module.Max_Durability then
               Time := Time + Module.Max_Durability - Module.Durability;
               Proto_Index :=
                 Find_Proto_Item
                   (Item_Type =>
                      Get_Module(Index => Module.Proto_Index).Repair_Material);
               Cost :=
                 Cost +
                 ((Module.Max_Durability - Module.Durability) *
                  Get_Price
                    (Base_Type => Sky_Bases(Base_Index).Base_Type,
                     Item_Index => Proto_Index));
            end if;
         end loop Count_Repair_Time_And_Cost_Loop;
         --## rule off SIMPLIFIABLE_STATEMENTS
         if Module_Index = -1 then
            Cost := Cost * 2;
            Time := Time / 2;
         elsif Module_Index = -2 then
            Cost := Cost * 4;
            Time := Time / 4;
         end if;
         --## rule on SIMPLIFIABLE_STATEMENTS
      end if;
      if Has_Flag
          (Base_Type => Sky_Bases(Base_Index).Base_Type,
           Flag => "shipyard") then
         Cost := Cost / 2;
      end if;
      Cost := Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
      if Cost = 0 then
         Cost := 1;
      end if;
      if Time = 0 then
         Time := 1;
      end if;
   end Repair_Cost;

end Bases.Ship;
