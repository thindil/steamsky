--    Copyright 2016-2022 Bartek thindil Jasicki
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
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Bases;
with Crafts; use Crafts;
with Factions; use Factions;
with Log;
with Maps;
with Ships.Crew;
with Utils; use Utils;

package body Ships is

   function Create_Ship
     (Proto_Index: Proto_Ships_Container.Extended_Index;
      Name: Tiny_String.Bounded_String; X: Map_X_Range; Y: Map_Y_Range;
      Speed: Ship_Speed; Random_Upgrades: Boolean := True)
      return Ship_Record is
      use Bases;
      use Maps;
      use Tiny_String;

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
            Temp_Module :=
              BaseModules_Container.Element
                (Container => Modules_List, Index => Module);
            if Upgrades_Amount = 0 or
              Get_Random(Min => 1, Max => 100) < 51 then
               goto End_Of_Setting_Upgrades;
            end if;
            Weight_Gain :=
              BaseModules_Container.Element
                (Container => Modules_List, Index => Module)
                .Weight /
              BaseModules_Container.Element
                (Container => Modules_List, Index => Module)
                .Durability;
            if Weight_Gain < 1 then
               Weight_Gain := 1;
            end if;
            Roll := Get_Random(Min => 1, Max => 100);
            case Roll is
               when 1 .. 50 => -- Upgrade durability of module
                  Max_Upgrade_Value :=
                    Positive
                      (Float
                         (BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Durability) *
                       1.5);
                  Temp_Module.Durability :=
                    Get_Random
                      (Min =>
                         BaseModules_Container.Element
                           (Container => Modules_List, Index => Module)
                           .Durability,
                       Max => Max_Upgrade_Value);
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Temp_Module.Weight :=
                    Temp_Module.Weight +
                    (Weight_Gain *
                     (Temp_Module.Durability -
                      BaseModules_Container.Element
                        (Container => Modules_List, Index => Module)
                        .Durability));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               when 51 .. 75 => -- Upgrade value (depends on module) of module
                  if BaseModules_Container.Element
                      (Container => Modules_List, Index => Module)
                      .M_Type =
                    ENGINE then
                     Weight_Gain := Weight_Gain * 10;
                     Max_Upgrade_Value :=
                       Positive
                         (Float
                            (BaseModules_Container.Element
                               (Container => Modules_List, Index => Module)
                               .Value) /
                          2.0);
                     Temp_Module.Value :=
                       Get_Random
                         (Min => Max_Upgrade_Value,
                          Max =>
                            BaseModules_Container.Element
                              (Container => Modules_List, Index => Module)
                              .Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (BaseModules_Container.Element
                           (Container => Modules_List, Index => Module)
                           .Value -
                         Temp_Module.Value));
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                  end if;
               when 76 ..
                     100 => -- Upgrade max_value (depends on module) of module
                  case BaseModules_Container.Element
                    (Container => Modules_List, Index => Module)
                    .M_Type is
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
                         (Float
                            (BaseModules_Container.Element
                               (Container => Modules_List, Index => Module)
                               .Max_Value) *
                          1.5);
                     Temp_Module.Max_Value :=
                       Get_Random
                         (Min =>
                            BaseModules_Container.Element
                              (Container => Modules_List, Index => Module)
                              .Max_Value,
                          Max => Max_Upgrade_Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (Temp_Module.Max_Value -
                         BaseModules_Container.Element
                           (Container => Modules_List, Index => Module)
                           .Max_Value));
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when COCKPIT =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => COCKPIT,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TRAINING_ROOM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TRAINING_ROOM,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Trained_Skill => 0));
               when TURRET =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TURRET,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Gun_Index => 0));
               when GUN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => GUN,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when HULL =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => HULL,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when BATTERING_RAM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => BATTERING_RAM,
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
                        Name =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module)
                            .Name,
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
            if BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Tmp_Ship.Modules(I).Proto_Index)
                .M_Type not in
                GUN | HARPOON_GUN | ARMOR | HULL then
               Amount :=
                 Amount +
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Tmp_Ship.Modules(I).Proto_Index)
                   .Size;
            end if;
         end loop Count_Modules_Loop;
         Tmp_Ship.Modules(Hull_Index).Installed_Modules := Amount;
      end Assing_Gun_Block;
      -- Set known crafting recipes
      Set_Known_Recipes_Loop :
      for Recipe of Proto_Ship.Known_Recipes loop
         Known_Recipes.Append(New_Item => Recipe);
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

   procedure Load_Ships(Reader: Tree_Reader) is
      use Ada.Characters.Handling;
      use DOM.Core;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Log;
      use Short_String;
      use Tiny_String;

      Nodes_List: constant Node_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Get_Tree(Read => Reader), Tag_Name => "ship");
      Child_Nodes: Node_List; --## rule line off IMPROPER_INITIALIZATION
      Temp_Record: Proto_Ship_Data := Empty_Proto_Ship;
      Module_Amount, Delete_Index, Mob_Index: Positive := 1;
      Action, Sub_Action: Data_Action := Default_Data_Action;
      Ship_Node, Child_Node: Node;
      Module_Index: BaseModules_Container.Extended_Index := 0;
      Item_Index: Objects_Container.Extended_Index := 0;
      Recipe_Index: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Ship_Index: Proto_Ships_Container.Extended_Index := 0;
      --## rule off IMPROPER_INITIALIZATION
      Empty_Cargo: MobInventory_Container.Vector (Capacity => 32);
      Empty_Known_Recipes: TinyString_Formal_Container.Vector (Capacity => 16);
      --## rule on IMPROPER_INITIALIZATION
      procedure Count_Ammo_Value(Item_Type_Index, Multiple: Positive) is
      begin
         Count_Ammo_Value_Loop :
         for I in
           MobInventory_Container.First_Index
             (Container => Temp_Record.Cargo) ..
             MobInventory_Container.Last_Index
               (Container => Temp_Record.Cargo) loop
            Count_Ammo_Value_Block :
            declare
               Temp_Cargo: constant Mob_Inventory_Record :=
                 MobInventory_Container.Element
                   (Container => Temp_Record.Cargo, Index => I);
            begin
               if Objects_Container.Element
                   (Container => Items_List, Index => Temp_Cargo.Proto_Index)
                   .I_Type =
                 TinyString_Formal_Container.Element
                   (Container => Items_Types, Index => Item_Type_Index) then
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Temp_Record.Combat_Value :=
                    Temp_Record.Combat_Value +
                    (Objects_Container.Element
                       (Container => Items_List,
                        Index => Temp_Cargo.Proto_Index)
                       .Value
                       (1) *
                     Multiple);
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
               end if;
            end Count_Ammo_Value_Block;
         end loop Count_Ammo_Value_Loop;
      end Count_Ammo_Value;
   begin
      Load_Proto_Ships_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Tiny_String.Null_Bounded_String,
            Modules => Positive_Container.Empty_Vector,
            Accuracy => No_Ship_Bonus, Combat_Ai => NONE,
            Evasion => No_Ship_Bonus, Loot => No_Ship_Bonus,
            Perception => No_Ship_Bonus, Cargo => Empty_Cargo,
            Combat_Value => 1, Crew => Proto_Crew_Container.Empty_Vector,
            Description => Short_String.Null_Bounded_String,
            Owner => Factions_Container.Key(Position => Factions_List.First),
            Known_Recipes => Empty_Known_Recipes);
         Ship_Node := Item(List => Nodes_List, Index => I);
         Ship_Index :=
           Proto_Ships_Container.Extended_Index'Value
             (Get_Attribute(Elem => Ship_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Ship_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Ship_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if Ship_Index > Proto_Ships_List.Last_Index then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " ship '" & Ship_Index'Img &
                 "', there is no ship with that index.";
            end if;
         elsif Ship_Index <= Proto_Ships_List.Last_Index then
            raise Data_Loading_Error
              with "Can't add ship '" & Ship_Index'Img &
              "', there is already a ship with that index.";
         end if;
         if Action = REMOVE then
            Proto_Ships_List.Delete(Index => Ship_Index);
            Log_Message
              (Message => "Ship removed: " & Ship_Index'Img,
               Message_Type => EVERYTHING);
         else
            if Action = UPDATE then
               Temp_Record := Proto_Ships_List(Ship_Index);
            end if;
            if Get_Attribute(Elem => Ship_Node, Name => "name")'Length > 0 then
               Temp_Record.Name :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Ship_Node, Name => "name"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Ship_Node, Name => "module");
            Load_Modules_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Module_Amount :=
                 (if Get_Attribute(Elem => Child_Node, Name => "amount") /= ""
                  then
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "amount"))
                  else 1);
               Module_Index :=
                 BaseModules_Container.Extended_Index'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               if Module_Index not in
                   BaseModules_Container.First_Index
                         (Container => Modules_List) ..
                         BaseModules_Container.Last_Index
                           (Container => Modules_List) then
                  raise Ships_Invalid_Data
                    with "Invalid module index: |" &
                    Get_Attribute(Elem => Child_Node, Name => "index") &
                    "| in " & To_String(Source => Temp_Record.Name) & ".";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               if Sub_Action = ADD then
                  Temp_Record.Modules.Append
                    (New_Item => Module_Index,
                     Count => Count_Type(Module_Amount));
               else
                  Find_Delete_Module_Loop :
                  for K in Temp_Record.Modules.Iterate loop
                     if Temp_Record.Modules(K) = Module_Index then
                        Delete_Index :=
                          Positive_Container.To_Index(Position => K);
                        exit Find_Delete_Module_Loop;
                     end if;
                  end loop Find_Delete_Module_Loop;
                  Temp_Record.Modules.Delete
                    (Index => Delete_Index,
                     Count => Count_Type(Module_Amount));
               end if;
            end loop Load_Modules_Loop;
            if Get_Attribute(Elem => Ship_Node, Name => "accuracy") /= "" then
               Temp_Record.Accuracy.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "accuracy"));
               Temp_Record.Accuracy.Max_Value := 0;
            elsif Get_Attribute(Elem => Ship_Node, Name => "minaccuracy") /=
              "" then
               Temp_Record.Accuracy.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "minaccuracy"));
               Temp_Record.Accuracy.Max_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "maxaccuracy"));
               if Temp_Record.Accuracy.Max_Value <
                 Temp_Record.Accuracy.Min_Value then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & Ship_Index'Img &
                    "', invalid range for accuracy.";
               end if;
            end if;
            if Get_Attribute(Elem => Ship_Node, Name => "combatai") /= "" then
               Temp_Record.Combat_Ai :=
                 Ship_Combat_Ai'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "combatai"));
            end if;
            if Get_Attribute(Elem => Ship_Node, Name => "evasion") /= "" then
               Temp_Record.Evasion.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "evasion"));
               Temp_Record.Evasion.Max_Value := 0;
            elsif Get_Attribute(Elem => Ship_Node, Name => "minevasion") /=
              "" then
               Temp_Record.Evasion.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "minevasion"));
               Temp_Record.Evasion.Max_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "maxevasion"));
               if Temp_Record.Evasion.Max_Value <
                 Temp_Record.Evasion.Min_Value then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & Ship_Index'Img &
                    "', invalid range for evasion.";
               end if;
            end if;
            if Get_Attribute(Elem => Ship_Node, Name => "loot") /= "" then
               Temp_Record.Loot.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "loot"));
               Temp_Record.Loot.Max_Value := 0;
            elsif Get_Attribute(Elem => Ship_Node, Name => "minloot") /=
              "" then
               Temp_Record.Loot.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "minloot"));
               Temp_Record.Loot.Max_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "maxloot"));
               if Temp_Record.Loot.Max_Value < Temp_Record.Loot.Min_Value then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & Ship_Index'Img &
                    "', invalid range for loot.";
               end if;
            end if;
            if Get_Attribute(Elem => Ship_Node, Name => "perception") /=
              "" then
               Temp_Record.Perception.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "perception"));
               Temp_Record.Perception.Max_Value := 0;
            elsif Get_Attribute(Elem => Ship_Node, Name => "minperception") /=
              "" then
               Temp_Record.Perception.Min_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "minperception"));
               Temp_Record.Perception.Max_Value :=
                 Integer'Value
                   (Get_Attribute(Elem => Ship_Node, Name => "maxperception"));
               if Temp_Record.Perception.Max_Value <
                 Temp_Record.Perception.Min_Value then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & Ship_Index'Img &
                    "', invalid range for perception.";
               end if;
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Ship_Node, Name => "cargo");
            Load_Cargo_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Item_Index :=
                 Objects_Container.Extended_Index'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               if Item_Index not in
                   Objects_Container.First_Index(Container => Items_List) ..
                         Objects_Container.Last_Index
                           (Container => Items_List) then
                  raise Ships_Invalid_Data
                    with "Invalid item index: |" &
                    Get_Attribute(Elem => Child_Node, Name => "index") &
                    "| in " & To_String(Source => Temp_Record.Name) & ".";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     if Get_Attribute(Elem => Child_Node, Name => "amount")'
                         Length =
                       0 then
                        if Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "maxamount")) <
                          Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "minamount")) then
                           raise Ships_Invalid_Data
                             with "Invalid amount range for item : |" &
                             Get_Attribute
                               (Elem => Child_Node, Name => "index") &
                             "| in " & To_String(Source => Temp_Record.Name) &
                             ".";
                        end if;
                        MobInventory_Container.Append
                          (Container => Temp_Record.Cargo,
                           New_Item =>
                             (Proto_Index => Item_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "minamount")),
                              Max_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxamount"))));
                     else
                        MobInventory_Container.Append
                          (Container => Temp_Record.Cargo,
                           New_Item =>
                             (Proto_Index => Item_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "amount")),
                              Max_Amount => 0));
                     end if;
                  when UPDATE =>
                     Update_Cargo_Loop :
                     for K in
                       MobInventory_Container.First_Index
                         (Container => Temp_Record.Cargo) ..
                         MobInventory_Container.Last_Index
                           (Container => Temp_Record.Cargo) loop
                        Update_Proto_Cargo_Block :
                        declare
                           Item: Mob_Inventory_Record :=
                             MobInventory_Container.Element
                               (Container => Temp_Record.Cargo, Index => K);
                        begin
                           if Item.Proto_Index = Item_Index then
                              if Get_Attribute
                                  (Elem => Child_Node, Name => "amount")'
                                  Length =
                                0 then
                                 if Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "maxamount")) <
                                   Integer'Value
                                     (Get_Attribute
                                        (Elem => Child_Node,
                                         Name => "minamount")) then
                                    raise Ships_Invalid_Data
                                      with "Invalid amount range for item : |" &
                                      Get_Attribute
                                        (Elem => Child_Node, Name => "index") &
                                      "| in " &
                                      To_String(Source => Temp_Record.Name) &
                                      ".";
                                 end if;
                                 Item :=
                                   (Proto_Index => Item_Index,
                                    Min_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "minamount")),
                                    Max_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "maxamount")));
                              else
                                 Item :=
                                   (Proto_Index => Item_Index,
                                    Min_Amount =>
                                      Integer'Value
                                        (Get_Attribute
                                           (Elem => Child_Node,
                                            Name => "amount")),
                                    Max_Amount => 0);
                              end if;
                              MobInventory_Container.Replace_Element
                                (Container => Temp_Record.Cargo, Index => K,
                                 New_Item => Item);
                              exit Update_Cargo_Loop;
                           end if;
                        end Update_Proto_Cargo_Block;
                     end loop Update_Cargo_Loop;
                  when REMOVE =>
                     Remove_Cargo_Block :
                     declare
                        Cargo_Index: Inventory_Amount_Range := 1;
                     begin
                        --## rule off SIMPLIFIABLE_EXPRESSIONS
                        Delete_Cargo_Loop :
                        while Cargo_Index <=
                          MobInventory_Container.Last_Index
                            (Container => Temp_Record.Cargo) loop
                           if MobInventory_Container.Element
                               (Container => Temp_Record.Cargo,
                                Index => Cargo_Index)
                               .Proto_Index =
                             Item_Index then
                              MobInventory_Container.Delete
                                (Container => Temp_Record.Cargo,
                                 Index => Cargo_Index);
                              exit Delete_Cargo_Loop;
                           end if;
                           Cargo_Index := Cargo_Index + 1;
                        end loop Delete_Cargo_Loop;
                     end Remove_Cargo_Block;
                        --## rule on SIMPLIFIABLE_EXPRESSIONS
               end case;
            end loop Load_Cargo_Loop;
            if Get_Attribute(Elem => Ship_Node, Name => "owner") /= "" then
               Temp_Record.Owner :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute(Elem => Ship_Node, Name => "owner"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Ship_Node, Name => "recipe");
            Load_Known_Recipes_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Recipe_Index :=
                 To_Bounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Item(List => Child_Nodes, Index => J),
                         Name => "index"));
               if not Recipes_List.Contains(Key => Recipe_Index) then
                  raise Ships_Invalid_Data
                    with "Invalid recipe index: |" &
                    Get_Attribute
                      (Elem => Item(List => Child_Nodes, Index => J),
                       Name => "index") &
                    "| in " & To_String(Source => Temp_Record.Name) & ".";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               if Sub_Action = ADD then
                  TinyString_Formal_Container.Append
                    (Container => Temp_Record.Known_Recipes,
                     New_Item => Recipe_Index);
               else
                  Find_Delete_Recipe_Loop :
                  for K in
                    TinyString_Formal_Container.First_Index
                      (Container => Temp_Record.Known_Recipes) ..
                      TinyString_Formal_Container.Last_Index
                        (Container => Temp_Record.Known_Recipes) loop
                     if To_String
                         (Source =>
                            TinyString_Formal_Container.Element
                              (Container => Temp_Record.Known_Recipes,
                               Index => K)) =
                       To_String(Source => Recipe_Index) then
                        Delete_Index := K;
                        exit Find_Delete_Recipe_Loop;
                     end if;
                  end loop Find_Delete_Recipe_Loop;
                  TinyString_Formal_Container.Delete
                    (Container => Temp_Record.Known_Recipes,
                     Index => Delete_Index);
               end if;
            end loop Load_Known_Recipes_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Ship_Node, Name => "member");
            Load_Crew_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Child_Node := Item(List => Child_Nodes, Index => J);
               Mob_Index :=
                 Positive'Value
                   (Get_Attribute(Elem => Child_Node, Name => "index"));
               if Mob_Index not in
                   ProtoMobs_Container.First_Index
                         (Container => Proto_Mobs_List) ..
                         ProtoMobs_Container.Last_Index
                           (Container => Proto_Mobs_List) then
                  raise Ships_Invalid_Data
                    with "Invalid mob index: |" &
                    Get_Attribute(Elem => Child_Node, Name => "index") &
                    "| in " & To_String(Source => Temp_Record.Name) & ".";
               end if;
               Sub_Action :=
                 (if
                    Get_Attribute(Elem => Child_Node, Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute(Elem => Child_Node, Name => "action"))
                  else ADD);
               case Sub_Action is
                  when ADD =>
                     if Get_Attribute(Elem => Child_Node, Name => "amount") /=
                       "" then
                        Temp_Record.Crew.Append
                          (New_Item =>
                             (Proto_Index => Mob_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "amount")),
                              Max_Amount => 0));
                     elsif Get_Attribute
                         (Elem => Child_Node, Name => "minamount") /=
                       "" then
                        if Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "maxamount")) <
                          Integer'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "minamount")) then
                           raise Ships_Invalid_Data
                             with "Invalid amount range for member : |" &
                             Get_Attribute
                               (Elem => Child_Node, Name => "index") &
                             "| in " & To_String(Source => Temp_Record.Name) &
                             ".";
                        end if;
                        Temp_Record.Crew.Append
                          (New_Item =>
                             (Proto_Index => Mob_Index,
                              Min_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "minamount")),
                              Max_Amount =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxamount"))));
                     else
                        Temp_Record.Crew.Append
                          (New_Item =>
                             (Proto_Index => Mob_Index, Min_Amount => 1,
                              Max_Amount => 0));
                     end if;
                  when UPDATE =>
                     Update_Crew_Loop :
                     for Member of Temp_Record.Crew loop
                        if Member.Proto_Index = Mob_Index then
                           if Get_Attribute
                               (Elem => Child_Node, Name => "amount") /=
                             "" then
                              Member.Min_Amount :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node, Name => "amount"));
                              Member.Max_Amount := 0;
                           elsif Get_Attribute
                               (Elem => Child_Node, Name => "minamount") /=
                             "" then
                              if Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxamount")) <
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "minamount")) then
                                 raise Ships_Invalid_Data
                                   with "Invalid amount range for member : |" &
                                   Get_Attribute
                                     (Elem => Child_Node, Name => "index") &
                                   "| in " &
                                   To_String(Source => Temp_Record.Name) & ".";
                              end if;
                              Member.Min_Amount :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "minamount"));
                              Member.Max_Amount :=
                                Integer'Value
                                  (Get_Attribute
                                     (Elem => Child_Node,
                                      Name => "maxamount"));
                           else
                              Member.Min_Amount := 1;
                              Member.Max_Amount := 0;
                           end if;
                           exit Update_Crew_Loop;
                        end if;
                     end loop Update_Crew_Loop;
                  when REMOVE =>
                     Find_Delete_Crew_Loop :
                     for K in Temp_Record.Crew.Iterate loop
                        if Temp_Record.Crew(K).Proto_Index = Mob_Index then
                           Delete_Index :=
                             Proto_Crew_Container.To_Index(Position => K);
                           exit Find_Delete_Crew_Loop;
                        end if;
                     end loop Find_Delete_Crew_Loop;
                     Temp_Record.Crew.Delete(Index => Delete_Index);
               end case;
            end loop Load_Crew_Loop;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Ship_Node, Name => "description");
            if Length(List => Child_Nodes) > 0 then
               Temp_Record.Description :=
                 To_Bounded_String
                   (Source =>
                      Node_Value
                        (N =>
                           First_Child
                             (N => Item(List => Child_Nodes, Index => 0))));
            end if;
            Count_Combat_Value_Loop :
            for Module_Index2 of Temp_Record.Modules loop
               case BaseModules_Container.Element
                 (Container => Modules_List, Index => Module_Index2)
                 .M_Type is
                  when HULL | GUN | BATTERING_RAM =>
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Record.Combat_Value :=
                       Temp_Record.Combat_Value +
                       BaseModules_Container.Element
                         (Container => Modules_List, Index => Module_Index2)
                         .Durability +
                       (BaseModules_Container.Element
                          (Container => Modules_List, Index => Module_Index2)
                          .Max_Value *
                        10);
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                     if BaseModules_Container.Element
                         (Container => Modules_List, Index => Module_Index2)
                         .M_Type =
                       GUN then
                        Count_Ammo_Value
                          (Item_Type_Index =>
                             BaseModules_Container.Element
                               (Container => Modules_List,
                                Index => Module_Index2)
                               .Value,
                           Multiple => 10);
                     end if;
                  when ARMOR =>
                     Temp_Record.Combat_Value :=
                       Temp_Record.Combat_Value +
                       BaseModules_Container.Element
                         (Container => Modules_List, Index => Module_Index2)
                         .Durability;
                  when HARPOON_GUN =>
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Record.Combat_Value :=
                       Temp_Record.Combat_Value +
                       BaseModules_Container.Element
                         (Container => Modules_List, Index => Module_Index2)
                         .Durability +
                       (BaseModules_Container.Element
                          (Container => Modules_List, Index => Module_Index2)
                          .Max_Value *
                        5);
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                     Count_Ammo_Value
                       (Item_Type_Index =>
                          BaseModules_Container.Element
                            (Container => Modules_List, Index => Module_Index2)
                            .Value,
                        Multiple => 5);
                  when others =>
                     null;
               end case;
            end loop Count_Combat_Value_Loop;
            Temp_Record.Combat_Value := Temp_Record.Combat_Value - 1;
            if Action = UPDATE then
               Proto_Ships_List(Ship_Index) := Temp_Record;
            else
               Proto_Ships_List.Append(New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Ship added: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            end if;
         end if;
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
           Item.Amount *
           Objects_Container.Element
             (Container => Items_List, Index => Item.Proto_Index)
             .Weight;
         Weight := Weight + Cargo_Weight;
      end loop Count_Cargo_Weight_Loop;
      return Weight;
   end Count_Ship_Weight;

   function Generate_Ship_Name
     (Owner: Tiny_String.Bounded_String) return Tiny_String.Bounded_String is
      use Tiny_String;
      use Syllable_String;

      New_Name: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
   begin
      Generate_Ship_Name_Loop :
      for I in Factions_List.Iterate loop
         if Factions_Container.Key(Position => I) /= Owner then
            goto End_Of_Generate_Name_Loop;
         end if;
         if Factions_List(I).Names_Type = ROBOTIC then
            New_Name :=
              To_Bounded_String
                (Source => To_String(Source => Generate_Robotic_Name));
         else
            New_Name :=
              To_Bounded_String
                (Source =>
                   To_String
                     (Source =>
                        SyllableString_Container.Element
                          (Container => Ship_Syllables_Start,
                           Index =>
                             Get_Random
                               (Min =>
                                  SyllableString_Container.First_Index
                                    (Container => Ship_Syllables_Start),
                                Max =>
                                  SyllableString_Container.Last_Index
                                    (Container => Ship_Syllables_Start)))));
            if Get_Random(Min => 1, Max => 100) < 51 then
               Append
                 (Source => New_Name,
                  New_Item =>
                    To_String
                      (Source =>
                         SyllableString_Container.Element
                           (Container => Ship_Syllables_Middle,
                            Index =>
                              Get_Random
                                (Min =>
                                   SyllableString_Container.First_Index
                                     (Container => Ship_Syllables_Middle),
                                 Max =>
                                   SyllableString_Container.Last_Index
                                     (Container => Ship_Syllables_Middle)))));
            end if;
            Append
              (Source => New_Name,
               New_Item =>
                 To_String
                   (Source =>
                      SyllableString_Container.Element
                        (Container => Ship_Syllables_End,
                         Index =>
                           Get_Random
                             (Min =>
                                SyllableString_Container.First_Index
                                  (Container => Ship_Syllables_End),
                              Max =>
                                SyllableString_Container.Last_Index
                                  (Container => Ship_Syllables_End)))));
         end if;
         exit Generate_Ship_Name_Loop;
         <<End_Of_Generate_Name_Loop>>
      end loop Generate_Ship_Name_Loop;
      return New_Name;
   end Generate_Ship_Name;

   function Count_Combat_Value return Natural is
      Combat_Value: Natural := 0;
      procedure Count_Ammo_Value(Item_Type_Index, Multiple: Positive) is
         use Tiny_String;

      begin
         Count_Ammo_Value_Loop :
         for Item of Player_Ship.Cargo loop
            if Objects_Container.Element
                (Container => Items_List, Index => Item.Proto_Index)
                .I_Type =
              TinyString_Formal_Container.Element
                (Container => Items_Types, Index => Item_Type_Index) then
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value +
                 (Objects_Container.Element
                    (Container => Items_List, Index => Item.Proto_Index)
                    .Value
                    (1) *
                  Multiple);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            end if;
         end loop Count_Ammo_Value_Loop;
      end Count_Ammo_Value;
   begin
      Count_Combat_Value_Loop :
      for Module of Player_Ship.Modules loop
         case BaseModules_Container.Element
           (Container => Modules_List, Index => Module.Proto_Index)
           .M_Type is
            when BATTERING_RAM =>
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value + Module.Max_Durability + (Module.Damage2 * 10);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            when GUN =>
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Combat_Value :=
                 Combat_Value + Module.Max_Durability + (Module.Damage * 10);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
               Count_Ammo_Value
                 (Item_Type_Index =>
                    BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Value,
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
                    BaseModules_Container.Element
                      (Container => Modules_List, Index => Module.Proto_Index)
                      .Value,
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
   begin
      case Quality is
         when 0 .. 10 =>
            return "Empty room";
         when 11 .. 20 =>
            return "Minimal quality";
         when 21 .. 30 =>
            return "Basic quality";
         when 31 .. 40 =>
            return "Second class";
         when 41 .. 50 =>
            return "Medium quality";
         when 51 .. 60 =>
            return "First class";
         when 61 .. 70 =>
            return "Extended quality";
         when 71 .. 80 =>
            return "Encrusted room";
         when 81 .. 90 =>
            return "Luxury quality";
         when others =>
            return "Palace room";
      end case;
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
         case BaseModules_Container.Element
           (Container => Modules_List,
            Index => Ship.Modules(Module_Index).Proto_Index)
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

end Ships;
