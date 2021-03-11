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
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;

package body Crew.Inventory is

   procedure UpdateInventory
     (MemberIndex: Positive; Amount: Integer;
      ProtoIndex: Unbounded_String := Null_Unbounded_String;
      Durability: Items_Durability := 0;
      InventoryIndex, Price: Natural := 0) is
      ItemIndex: Inventory_Container.Extended_Index := 0;
   begin
      if InventoryIndex = 0 then
         ItemIndex :=
           (if Durability > 0 then
              FindItem
                (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
                 ProtoIndex => ProtoIndex, Durability => Durability)
            else FindItem(PlayerShip.Crew(MemberIndex).Inventory, ProtoIndex));
      else
         ItemIndex := InventoryIndex;
      end if;
      if Amount > 0 then
         declare
            Weight: constant Positive :=
              (if ItemIndex > 0 then
                 Items_List
                   (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)
                      .ProtoIndex)
                   .Weight *
                 Amount
               else Items_List(ProtoIndex).Weight * Amount);
         begin
            if FreeInventory(MemberIndex, -(Weight)) < 0 then
               raise Crew_No_Space_Error
                 with To_String(PlayerShip.Crew(MemberIndex).Name) &
                 " doesn't have any free space in their inventory.";
            end if;
         end;
      else
         if ItemIsUsed(MemberIndex, ItemIndex) then
            TakeOffItem(MemberIndex, ItemIndex);
         end if;
      end if;
      if ItemIndex = 0 then
         PlayerShip.Crew(MemberIndex).Inventory.Append
           (New_Item =>
              (ProtoIndex => ProtoIndex, Amount => Amount,
               Name => Items_List(ProtoIndex).Name, Durability => Durability,
               Price => Price));
      else
         declare
            NewAmount: constant Natural :=
              PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount +
              Amount;
         begin
            if NewAmount = 0 then
               PlayerShip.Crew(MemberIndex).Inventory.Delete
                 (Index => ItemIndex);
               Update_Item_Index_Loop :
               for Item of PlayerShip.Crew(MemberIndex).Equipment loop
                  if Item = ItemIndex then
                     Item := 0;
                  elsif Item > ItemIndex then
                     Item := Item - 1;
                  end if;
               end loop Update_Item_Index_Loop;
            else
               PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount :=
                 NewAmount;
            end if;
         end;
      end if;
   end UpdateInventory;

   function FreeInventory
     (MemberIndex: Positive; Amount: Integer) return Integer is
      FreeSpace: Integer :=
        50 + PlayerShip.Crew(MemberIndex).Attributes(Strength_Index)(1);
   begin
      Count_Free_Inventory_Space_Loop :
      for Item of PlayerShip.Crew(MemberIndex).Inventory loop
         FreeSpace :=
           FreeSpace - (Items_List(Item.ProtoIndex).Weight * Item.Amount);
      end loop Count_Free_Inventory_Space_Loop;
      return FreeSpace + Amount;
   end FreeInventory;

   procedure TakeOffItem(MemberIndex, ItemIndex: Positive) is
   begin
      Take_Off_Item_Loop :
      for I in PlayerShip.Crew(MemberIndex).Equipment'Range loop
         if PlayerShip.Crew(MemberIndex).Equipment(I) = ItemIndex then
            PlayerShip.Crew(MemberIndex).Equipment(I) := 0;
            exit Take_Off_Item_Loop;
         end if;
      end loop Take_Off_Item_Loop;
   end TakeOffItem;

   function ItemIsUsed(MemberIndex, ItemIndex: Positive) return Boolean is
   begin
      Check_Item_Usage_Loop :
      for I in PlayerShip.Crew(MemberIndex).Equipment'Range loop
         if PlayerShip.Crew(MemberIndex).Equipment(I) = ItemIndex then
            return True;
         end if;
      end loop Check_Item_Usage_Loop;
      return False;
   end ItemIsUsed;

   function FindTools
     (MemberIndex: Positive; ItemType: Unbounded_String; Order: Crew_Orders;
      ToolQuality: Positive := 100) return Natural is
      ToolsIndex: Inventory_Container.Extended_Index :=
        PlayerShip.Crew(MemberIndex).Equipment(7);
   begin
      -- If the crew member has equiped tool, check if it is a proper tool.
      -- If not, remove it and put to the ship cargo
      if ToolsIndex > 0 then
         declare
            ProtoIndex: constant Unbounded_String :=
              PlayerShip.Crew(MemberIndex).Inventory(ToolsIndex).ProtoIndex;
         begin
            if Items_List(ProtoIndex).IType /= ItemType or
              (Items_List(ProtoIndex).Value.Length > 0
               and then Items_List(ProtoIndex).Value(1) < ToolQuality) then
               UpdateCargo
                 (PlayerShip, ProtoIndex, 1,
                  PlayerShip.Crew(MemberIndex).Inventory(ToolsIndex)
                    .Durability);
               UpdateInventory
                 (MemberIndex => MemberIndex, Amount => -1,
                  InventoryIndex => ToolsIndex);
               ToolsIndex := 0;
            end if;
         end;
      end if;
      ToolsIndex :=
        FindItem
          (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
           ItemType => ItemType, Quality => ToolQuality);
      if ToolsIndex = 0 then
         ToolsIndex :=
           FindItem
             (Inventory => PlayerShip.Cargo, ItemType => ItemType,
              Quality => ToolQuality);
         if ToolsIndex > 0 then
            begin
               UpdateInventory
                 (MemberIndex, 1, PlayerShip.Cargo(ToolsIndex).ProtoIndex,
                  PlayerShip.Cargo(ToolsIndex).Durability);
               UpdateCargo
                 (Ship => PlayerShip, Amount => -1, CargoIndex => ToolsIndex);
               ToolsIndex :=
                 FindItem
                   (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
                    ItemType => ItemType, Quality => ToolQuality);
            exception
               when Crew_No_Space_Error =>
                  case Order is
                     when Repair =>
                        AddMessage
                          (To_String(PlayerShip.Crew(MemberIndex).Name) &
                           " can't continue repairs because they don't have free space in their inventory for repair tools.",
                           OrderMessage, RED);
                     when Upgrading =>
                        AddMessage
                          (To_String(PlayerShip.Crew(MemberIndex).Name) &
                           " can't continue upgrading module because they don't have free space in their inventory for repair tools.",
                           OrderMessage, RED);
                     when Clean =>
                        AddMessage
                          (To_String(PlayerShip.Crew(MemberIndex).Name) &
                           " can't continue cleaning ship because they don't have free space in their inventory for cleaning tools.",
                           OrderMessage, RED);
                     when Craft =>
                        AddMessage
                          (To_String(PlayerShip.Crew(MemberIndex).Name) &
                           " can't continue manufacturing because they don't have space in their inventory for the proper tools.",
                           OrderMessage, RED);
                     when Train =>
                        AddMessage
                          (To_String(PlayerShip.Crew(MemberIndex).Name) &
                           " can't continue training because they don't have space in their inventory for the proper tools.",
                           OrderMessage, RED);
                     when others =>
                        null;
                  end case;
                  GiveOrders(PlayerShip, MemberIndex, Rest);
                  return 0;
            end;
         end if;
      end if;
      PlayerShip.Crew(MemberIndex).Equipment(7) := ToolsIndex;
      return ToolsIndex;
   end FindTools;

end Crew.Inventory;
