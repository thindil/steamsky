--    Copyright 2017-2022 Bartek thindil Jasicki
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
      ProtoIndex: Tiny_String.Bounded_String :=
        Tiny_String.Null_Bounded_String;
      Durability: Items_Durability := 0; InventoryIndex, Price: Natural := 0;
      Ship: in out Ship_Record) is
      use Tiny_String;

      ItemIndex: Inventory_Container.Extended_Index := 0;
   begin
      if InventoryIndex = 0 then
         ItemIndex :=
           (if Durability > 0 then
              Find_Item
                (Inventory => Ship.Crew(MemberIndex).Inventory,
                 Proto_Index => ProtoIndex, Durability => Durability)
            else Find_Item(Ship.Crew(MemberIndex).Inventory, ProtoIndex));
      else
         ItemIndex := InventoryIndex;
      end if;
      if Amount > 0 then
         declare
            Weight: constant Positive :=
              (if ItemIndex > 0 then
                 Items_List
                   (Inventory_Container.Element
                      (Container => Ship.Crew(MemberIndex).Inventory,
                       Index => ItemIndex)
                      .Proto_Index)
                   .Weight *
                 Amount
               else Items_List(ProtoIndex).Weight * Amount);
         begin
            if FreeInventory(MemberIndex, -(Weight)) < 0 then
               raise Crew_No_Space_Error
                 with To_String(Ship.Crew(MemberIndex).Name) &
                 " doesn't have any free space in their inventory.";
            end if;
         end;
      else
         if ItemIsUsed(MemberIndex, ItemIndex) then
            TakeOffItem(MemberIndex, ItemIndex);
         end if;
      end if;
      if ItemIndex = 0 then
         Inventory_Container.Append
           (Container => Ship.Crew(MemberIndex).Inventory,
            New_Item =>
              (Proto_Index => ProtoIndex, Amount => Amount,
               Name =>
                 To_Bounded_String
                   (Source =>
                      To_String(Source => Items_List(ProtoIndex).Name)),
               Durability => Durability, Price => Price));
      else
         declare
            Item: Inventory_Data :=
              Inventory_Container.Element
                (Container => Ship.Crew(MemberIndex).Inventory,
                 Index => ItemIndex);
            NewAmount: constant Natural := Item.Amount + Amount;
         begin
            if NewAmount = 0 then
               Inventory_Container.Delete
                 (Container => Ship.Crew(MemberIndex).Inventory,
                  Index => ItemIndex);
               Update_Item_Index_Loop :
               for Item of Ship.Crew(MemberIndex).Equipment loop
                  if Item = ItemIndex then
                     Item := 0;
                  elsif Item > ItemIndex then
                     Item := Item - 1;
                  end if;
               end loop Update_Item_Index_Loop;
            else
               Item.Amount := NewAmount;
               Inventory_Container.Replace_Element
                 (Container => Ship.Crew(MemberIndex).Inventory,
                  Index => ItemIndex, New_Item => Item);
            end if;
         end;
      end if;
   end UpdateInventory;

   function FreeInventory
     (MemberIndex: Positive; Amount: Integer) return Integer is
      FreeSpace: Integer :=
        50 +
        Player_Ship.Crew(MemberIndex).Attributes(Positive(Strength_Index))
          .Level;
   begin
      Count_Free_Inventory_Space_Loop :
      for Item of Player_Ship.Crew(MemberIndex).Inventory loop
         FreeSpace :=
           FreeSpace - (Items_List(Item.Proto_Index).Weight * Item.Amount);
      end loop Count_Free_Inventory_Space_Loop;
      return FreeSpace + Amount;
   end FreeInventory;

   procedure TakeOffItem(MemberIndex, ItemIndex: Positive) is
   begin
      Take_Off_Item_Loop :
      for I in Player_Ship.Crew(MemberIndex).Equipment'Range loop
         if Player_Ship.Crew(MemberIndex).Equipment(I) = ItemIndex then
            Player_Ship.Crew(MemberIndex).Equipment(I) := 0;
            exit Take_Off_Item_Loop;
         end if;
      end loop Take_Off_Item_Loop;
   end TakeOffItem;

   function ItemIsUsed(MemberIndex, ItemIndex: Positive) return Boolean is
   begin
      Check_Item_Usage_Loop :
      for I in Player_Ship.Crew(MemberIndex).Equipment'Range loop
         if Player_Ship.Crew(MemberIndex).Equipment(I) = ItemIndex then
            return True;
         end if;
      end loop Check_Item_Usage_Loop;
      return False;
   end ItemIsUsed;

   function FindTools
     (MemberIndex: Positive; ItemType: Tiny_String.Bounded_String;
      Order: Crew_Orders; ToolQuality: Positive := 100) return Natural is
      use Tiny_String;

      ToolsIndex: Inventory_Container.Extended_Index :=
        Player_Ship.Crew(MemberIndex).Equipment(TOOL);
   begin
      -- If the crew member has equiped tool, check if it is a proper tool.
      -- If not, remove it and put to the ship cargo
      if ToolsIndex > 0 then
         declare
            ProtoIndex: constant Tiny_String.Bounded_String :=
              Inventory_Container.Element
                (Container => Player_Ship.Crew(MemberIndex).Inventory,
                 Index => ToolsIndex)
                .Proto_Index;
         begin
            if Items_List(ProtoIndex).I_Type /= ItemType or
              (Items_List(ProtoIndex).Value(1) < ToolQuality) then
               Update_Cargo
                 (Player_Ship, ProtoIndex, 1,
                  Inventory_Container.Element
                    (Container => Player_Ship.Crew(MemberIndex).Inventory,
                     Index => ToolsIndex)
                    .Durability);
               UpdateInventory
                 (MemberIndex => MemberIndex, Amount => -1,
                  InventoryIndex => ToolsIndex, Ship => Player_Ship);
               ToolsIndex := 0;
            end if;
         end;
      end if;
      ToolsIndex :=
        Find_Item
          (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
           Item_Type => ItemType, Quality => ToolQuality);
      if ToolsIndex = 0 then
         ToolsIndex :=
           Find_Item
             (Inventory => Player_Ship.Cargo, Item_Type => ItemType,
              Quality => ToolQuality);
         if ToolsIndex > 0 then
            begin
               UpdateInventory
                 (MemberIndex, 1,
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => ToolsIndex)
                    .Proto_Index,
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => ToolsIndex)
                    .Durability,
                  Ship => Player_Ship);
               Update_Cargo
                 (Ship => Player_Ship, Amount => -1,
                  Cargo_Index => ToolsIndex);
               ToolsIndex :=
                 Find_Item
                   (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
                    Item_Type => ItemType, Quality => ToolQuality);
            exception
               when Crew_No_Space_Error =>
                  case Order is
                     when REPAIR =>
                        Add_Message
                          (To_String(Player_Ship.Crew(MemberIndex).Name) &
                           " can't continue repairs because they don't have free space in their inventory for repair tools.",
                           ORDERMESSAGE, RED);
                     when UPGRADING =>
                        Add_Message
                          (To_String(Player_Ship.Crew(MemberIndex).Name) &
                           " can't continue upgrading module because they don't have free space in their inventory for repair tools.",
                           ORDERMESSAGE, RED);
                     when CLEAN =>
                        Add_Message
                          (To_String(Player_Ship.Crew(MemberIndex).Name) &
                           " can't continue cleaning ship because they don't have free space in their inventory for cleaning tools.",
                           ORDERMESSAGE, RED);
                     when CRAFT =>
                        Add_Message
                          (To_String(Player_Ship.Crew(MemberIndex).Name) &
                           " can't continue manufacturing because they don't have space in their inventory for the proper tools.",
                           ORDERMESSAGE, RED);
                     when TRAIN =>
                        Add_Message
                          (To_String(Player_Ship.Crew(MemberIndex).Name) &
                           " can't continue training because they don't have space in their inventory for the proper tools.",
                           ORDERMESSAGE, RED);
                     when others =>
                        null;
                  end case;
                  Give_Orders(Player_Ship, MemberIndex, REST);
                  return 0;
            end;
         end if;
      end if;
      Player_Ship.Crew(MemberIndex).Equipment(TOOL) := ToolsIndex;
      return ToolsIndex;
   end FindTools;

end Crew.Inventory;
