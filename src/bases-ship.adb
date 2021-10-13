--    Copyright 2017-2021 Bartek thindil Jasicki
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

with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Trades; use Trades;
with Bases.Cargo; use Bases.Cargo;
with Config; use Config;
with BasesTypes; use BasesTypes;
with Maps; use Maps;

package body Bases.Ship is

   procedure Repair_Ship(Module_Index: Integer) is
      Cost, Time: Natural := 0;
      Money_Index_2: constant Inventory_Container.Extended_Index :=
        FindItem(Inventory => Player_Ship.Cargo, ProtoIndex => Money_Index);
      Trader_Index: constant Crew_Container.Extended_Index :=
        FindMember(Order => Talk);
   begin
      Repair_Cost(Cost => Cost, Time => Time, Module_Index => Module_Index);
      if Cost = 0 then
         raise Bases_Ship_Nothing_To_Repair;
      end if;
      Count_Price(Price => Cost, Trader_Index => Trader_Index);
      if Player_Ship.Cargo(Money_Index_2).Amount < Cost then
         raise Trade_Not_Enough_Money;
      end if;
      Give_Rest_Order_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order = Repair then
            GiveOrders
              (Ship => Player_Ship,
               MemberIndex => Crew_Container.To_Index(Position => I),
               GivenOrder => Rest);
         end if;
      end loop Give_Rest_Order_Loop;
      if Module_Index > 0 then
         Player_Ship.Modules(Module_Index).Durability :=
           Player_Ship.Modules(Module_Index).Max_Durability;
         AddMessage
           (Message =>
              "You bought " &
              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
              " repair for" & Positive'Image(Cost) & " " &
              To_String(Source => Money_Name) & ".",
            MType => TradeMessage);
      else
         Repair_Whole_Ship_Loop :
         for Module of Player_Ship.Modules loop
            if Module.Durability < Module.Max_Durability then
               Module.Durability := Module.Max_Durability;
            end if;
         end loop Repair_Whole_Ship_Loop;
         AddMessage
           (Message =>
              "You bought an entire ship repair for" & Positive'Image(Cost) &
              " " & To_String(Source => Money_Name) & ".",
            MType => TradeMessage);
      end if;
      UpdateCargo
        (Ship => Player_Ship, CargoIndex => Money_Index_2, Amount => -(Cost));
      Update_Base_Cargo(Proto_Index => Money_Index, Amount => Cost);
      GainExp
        (Amount => 1, SkillNumber => Talking_Skill, CrewIndex => Trader_Index);
      Gain_Rep
        (Base_Index => SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex,
         Points => 1);
      Update_Game(Minutes => Time);
   end Repair_Ship;

   procedure Upgrade_Ship(Install: Boolean; Module_Index: Unbounded_String) is
      Money_Index_2: constant Inventory_Container.Extended_Index :=
        FindItem(Inventory => Player_Ship.Cargo, ProtoIndex => Money_Index);
      Trader_Index: constant Crew_Container.Extended_Index :=
        FindMember(Order => Talk);
      Hull_Index, Ship_Module_Index: Modules_Container.Extended_Index;
      Free_Turret_Index: Modules_Container.Extended_Index := 0;
      Modules_Amount: Positive;
      Price: Natural := 0;
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Owners: Natural_Container.Vector;
   begin
      if Money_Index_2 = 0 then
         raise Trade_No_Money;
      end if;
      Find_Hull_And_Turrets_Loop :
      for C in Player_Ship.Modules.Iterate loop
         case Player_Ship.Modules(C).M_Type is
            when HULL =>
               Hull_Index := Modules_Container.To_Index(Position => C);
               Modules_Amount := Player_Ship.Modules(C).Installed_Modules;
            when TURRET =>
               if (Player_Ship.Modules(C).Gun_Index = 0 and Install)
                 and then
                   Modules_List(Player_Ship.Modules(C).Proto_Index).Size >=
                   Modules_List(Module_Index).Size then
                  Free_Turret_Index :=
                    Modules_Container.To_Index(Position => C);
               end if;
            when others =>
               null;
         end case;
      end loop Find_Hull_And_Turrets_Loop;
      if Install then
         Price := Modules_List(Module_Index).Price;
         Count_Price(Price => Price, Trader_Index => Trader_Index);
         if Player_Ship.Cargo(Money_Index_2).Amount < Price then
            raise Trade_Not_Enough_Money
              with To_String(Source => Modules_List(Module_Index).Name);
         end if;
         Check_Unique_Module_Loop :
         for Module of Player_Ship.Modules loop
            if Modules_List(Module.Proto_Index).MType =
              Modules_List(Module_Index).MType and
              Modules_List(Module_Index).Unique then
               raise Bases_Ship_Unique_Module
                 with To_String(Source => Modules_List(Module_Index).Name);
            end if;
         end loop Check_Unique_Module_Loop;
         if Modules_List(Module_Index).MType /= HULL then
            if Modules_List(Module_Index).Size >
              Modules_List(Player_Ship.Modules(Hull_Index).Proto_Index)
                .Value then
               raise Bases_Ship_Installation_Error
                 with "You can't install this module because it is too big for this hull.";
            end if;
            Modules_Amount := Modules_Amount + Modules_List(Module_Index).Size;
            if Modules_Amount > Player_Ship.Modules(Hull_Index).Max_Modules and
              (Modules_List(Module_Index).MType /= GUN and
               Modules_List(Module_Index).MType /= HARPOON_GUN) then
               raise Bases_Ship_Installation_Error
                 with "You don't have free modules space for more modules.";
            end if;
            if
              (Modules_List(Module_Index).MType = GUN or
               Modules_List(Module_Index).MType = HARPOON_GUN) and
              Free_Turret_Index = 0 then
               raise Bases_Ship_Installation_Error
                 with "You don't have free turret with proprer size for this gun. Install new turret or remove old gun first.";
            end if;
         else
            Check_Module_Size_Loop :
            for Module of Player_Ship.Modules loop
               if Modules_List(Module.Proto_Index).Size >
                 Modules_List(Module_Index).Value then
                  raise Bases_Ship_Installation_Error
                    with "This hull don't allow to have installed that big modules what you currently have.";
               end if;
            end loop Check_Module_Size_Loop;
            if Modules_List(Module_Index).MaxValue < Modules_Amount then
               raise Bases_Ship_Installation_Error
                 with "This hull is too small for your ship. Remove some modules first.";
            end if;
            Player_Ship.Modules.Delete(Index => Hull_Index);
         end if;
         UpdateCargo
           (Ship => Player_Ship, CargoIndex => Money_Index_2,
            Amount => -(Price));
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Price);
         GainExp
           (Amount => 1, SkillNumber => Talking_Skill,
            CrewIndex => Trader_Index);
         Gain_Rep
           (Base_Index =>
              SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex,
            Points => 1);
         Update_Game(Minutes => Modules_List(Module_Index).InstallTime);
         if Modules_List(Module_Index).MType /= HULL then
            Set_Empty_Owners_Loop :
            for I in 1 .. Modules_List(Module_Index).MaxOwners loop
               Owners.Append(New_Item => 0);
            end loop Set_Empty_Owners_Loop;
            case Modules_List(Module_Index).MType is
               when ALCHEMY_LAB .. GREENHOUSE =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => WORKSHOP,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Crafting_Index => Null_Unbounded_String,
                        Crafting_Time => 0, Crafting_Amount => 0));
               when MEDICAL_ROOM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => MEDICAL_ROOM,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TRAINING_ROOM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => TRAINING_ROOM,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Trained_Skill => 0));
               when COCKPIT =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => COCKPIT,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TURRET =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => TURRET,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Gun_Index => 0));
               when CABIN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => CABIN,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Cleanliness => Modules_List(Module_Index).Value,
                        Quality => Modules_List(Module_Index).MaxValue));
               when ShipModules.CARGO =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => CARGO_ROOM,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when ENGINE =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => ENGINE,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Fuel_Usage => Modules_List(Module_Index).Value,
                        Power => Modules_List(Module_Index).MaxValue,
                        Disabled => False));
               when ARMOR =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => ARMOR,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when BATTERING_RAM =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => BATTERING_RAM,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage2 => Modules_List(Module_Index).MaxValue,
                        Cooling_Down => False));
               when GUN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => GUN, Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage => Modules_List(Module_Index).MaxValue,
                        Ammo_Index => 0));
               when HARPOON_GUN =>
                  Player_Ship.Modules.Append
                    (New_Item =>
                       (M_Type => HARPOON_GUN,
                        Name => Modules_List(Module_Index).Name,
                        Proto_Index => Module_Index,
                        Weight => Modules_List(Module_Index).Weight,
                        Durability => Modules_List(Module_Index).Durability,
                        Max_Durability =>
                          Modules_List(Module_Index).Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Duration => Modules_List(Module_Index).MaxValue,
                        Harpoon_Index => 0));
               when ANY | HULL =>
                  null;
            end case;
         else
            Player_Ship.Modules.Insert
              (Before => Hull_Index,
               New_Item =>
                 (M_Type => HULL, Name => Modules_List(Module_Index).Name,
                  Proto_Index => Module_Index,
                  Weight => Modules_List(Module_Index).Weight,
                  Durability => Modules_List(Module_Index).Durability,
                  Max_Durability => Modules_List(Module_Index).Durability,
                  Owner => Owners, Upgrade_Progress => 0,
                  Upgrade_Action => NONE,
                  Installed_Modules => Modules_List(Module_Index).Value,
                  Max_Modules => Modules_List(Module_Index).MaxValue));
         end if;
         case Modules_List(Module_Index).MType is
            when GUN | HARPOON_GUN =>
               Player_Ship.Modules(Free_Turret_Index).Gun_Index :=
                 Player_Ship.Modules.Last_Index;
            when others =>
               Player_Ship.Modules(Hull_Index).Installed_Modules :=
                 Modules_Amount;
         end case;
         AddMessage
           (Message =>
              "You installed " &
              To_String(Source => Modules_List(Module_Index).Name) &
              " on your ship for" & Positive'Image(Price) & " " &
              To_String(Source => Money_Name) & ".",
            MType => TradeMessage);
      else
         Ship_Module_Index := Integer'Value(To_String(Source => Module_Index));
         Get_Price_Block :
         declare
            Damage: Damage_Factor := 0.0;
         begin
            Damage :=
              1.0 -
              Damage_Factor
                (Float(Player_Ship.Modules(Ship_Module_Index).Durability) /
                 Float(Player_Ship.Modules(Ship_Module_Index).Max_Durability));
            Price :=
              Modules_List(Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .Price -
              Integer
                (Float
                   (Modules_List
                      (Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                      .Price) *
                 Float(Damage));
         end Get_Price_Block;
         Count_Price
           (Price => Price, Trader_Index => Trader_Index, Reduce => False);
         if FreeCargo(Amount => -(Price)) < 0 then
            raise Trade_No_Free_Cargo;
         end if;
         if Price > Sky_Bases(Base_Index).Cargo(1).Amount then
            raise Trade_No_Money_In_Base;
         end if;
         case Modules_List(Player_Ship.Modules(Ship_Module_Index).Proto_Index)
           .MType is
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
               if FreeCargo
                   (Amount =>
                      0 -
                      Modules_List
                        (Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                        .MaxValue) <
                 0 then
                  raise Bases_Ship_Removing_Error
                    with "You can't sell this cargo bay, because you have items in it.";
               end if;
            when others =>
               null;
         end case;
         if Modules_List(Player_Ship.Modules(Ship_Module_Index).Proto_Index)
             .MType not in
             HULL | ARMOR | GUN | HARPOON_GUN then
            Modules_Amount :=
              Modules_Amount -
              Modules_List(Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .Size;
            Player_Ship.Modules(Hull_Index).Installed_Modules :=
              Modules_Amount;
         end if;
         if Player_Ship.Upgrade_Module = Ship_Module_Index then
            Player_Ship.Upgrade_Module := 0;
            Remove_Upgrade_Order_Loop :
            for C in Player_Ship.Crew.Iterate loop
               if Player_Ship.Crew(C).Order = Upgrading then
                  GiveOrders
                    (Ship => Player_Ship,
                     MemberIndex => Crew_Container.To_Index(Position => C),
                     GivenOrder => Rest);
                  exit Remove_Upgrade_Order_Loop;
               end if;
            end loop Remove_Upgrade_Order_Loop;
         end if;
         if Player_Ship.Modules(Ship_Module_Index).M_Type /= CABIN then
            Give_Rest_Order_Loop :
            for Owner of Player_Ship.Modules(Ship_Module_Index).Owner loop
               if Owner > 0 then
                  GiveOrders
                    (Ship => Player_Ship, MemberIndex => Owner,
                     GivenOrder => Rest, CheckPriorities => False);
               end if;
            end loop Give_Rest_Order_Loop;
         end if;
         UpdateCargo
           (Ship => Player_Ship, CargoIndex => Money_Index_2, Amount => Price);
         Update_Base_Cargo(Proto_Index => Money_Index, Amount => Price);
         GainExp
           (Amount => 1, SkillNumber => Talking_Skill,
            CrewIndex => Trader_Index);
         Gain_Rep
           (Base_Index =>
              SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex,
            Points => 1);
         Update_Game
           (Minutes =>
              Modules_List(Player_Ship.Modules(Ship_Module_Index).Proto_Index)
                .InstallTime);
         AddMessage
           (Message =>
              "You removed " &
              To_String
                (Source => Player_Ship.Modules(Ship_Module_Index).Name) &
              " from your ship and received" & Positive'Image(Price) & " " &
              To_String(Source => Money_Name) & ".",
            MType => TradeMessage);
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
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Money_Index_2: constant Inventory_Container.Extended_Index :=
        FindItem(Inventory => Player_Ship.Cargo, ProtoIndex => Money_Index);
      Docking_Cost: Natural;
      Trader_Index: constant Crew_Container.Extended_Index :=
        FindMember(Order => Talk);
   begin
      if Sky_Bases(Base_Index).Population = 0 then
         return;
      end if;
      if Money_Index_2 = 0 then
         Gain_Rep(Base_Index => Base_Index, Points => -10);
         AddMessage
           (Message =>
              "You don't have " & To_String(Source => Money_Name) &
              " for pay for docking!",
            MType => OtherMessage, Color => RED);
         return;
      end if;
      Count_Docking_Cost_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = HULL then
            Docking_Cost := Module.Max_Modules;
            exit Count_Docking_Cost_Loop;
         end if;
      end loop Count_Docking_Cost_Loop;
      Docking_Cost :=
        Natural(Float(Docking_Cost) * Float(New_Game_Settings.Prices_Bonus));
      if Docking_Cost = 0 then
         Docking_Cost := 1;
      end if;
      Count_Price(Price => Docking_Cost, Trader_Index => Trader_Index);
      if Docking_Cost > Player_Ship.Cargo(Money_Index_2).Amount then
         Docking_Cost := Player_Ship.Cargo(Money_Index_2).Amount;
      end if;
      UpdateCargo
        (Ship => Player_Ship, CargoIndex => Money_Index_2,
         Amount => -(Docking_Cost));
      Update_Base_Cargo(Proto_Index => Money_Index, Amount => Docking_Cost);
      AddMessage
        (Message =>
           "You pay" & Positive'Image(Docking_Cost) & " " &
           To_String(Source => Money_Name) & " docking fee.",
         MType => OtherMessage);
      if Trader_Index > 0 then
         GainExp
           (Amount => 1, SkillNumber => Talking_Skill,
            CrewIndex => Trader_Index);
      end if;
   end Pay_For_Dock;

   procedure Repair_Cost(Cost, Time: in out Natural; Module_Index: Integer) is
      Proto_Index: Unbounded_String;
      Base_Index: constant Bases_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
   begin
      if Module_Index > 0 then
         Time :=
           Player_Ship.Modules(Module_Index).Max_Durability -
           Player_Ship.Modules(Module_Index).Durability;
         Proto_Index :=
           FindProtoItem
             (ItemType =>
                Modules_List(Player_Ship.Modules(Module_Index).Proto_Index)
                  .RepairMaterial);
         Cost :=
           Time *
           Get_Price
             (BaseType => Sky_Bases(Base_Index).Base_Type,
              ItemIndex => Proto_Index);
      else
         Count_Repair_Time_And_Cost_Loop :
         for Module of Player_Ship.Modules loop
            if Module.Durability < Module.Max_Durability then
               Time := Time + Module.Max_Durability - Module.Durability;
               Proto_Index :=
                 FindProtoItem
                   (ItemType =>
                      Modules_List(Module.Proto_Index).RepairMaterial);
               Cost :=
                 Cost +
                 ((Module.Max_Durability - Module.Durability) *
                  Get_Price
                    (BaseType => Sky_Bases(Base_Index).Base_Type,
                     ItemIndex => Proto_Index));
            end if;
         end loop Count_Repair_Time_And_Cost_Loop;
         if Module_Index = -1 then
            Cost := Cost * 2;
            Time := Time / 2;
         elsif Module_Index = -2 then
            Cost := Cost * 4;
            Time := Time / 4;
         end if;
      end if;
      if BasesTypes_List(Sky_Bases(Base_Index).Base_Type).Flags.Contains
          (Item => To_Unbounded_String(Source => "shipyard")) then
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
