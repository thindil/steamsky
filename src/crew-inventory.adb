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

   procedure Update_Inventory
     (Member_Index: Positive; Amount: Integer;
      Proto_Index: Objects_Container.Extended_Index := 0;
      Durability: Items_Durability := 0; Inventory_Index, Price: Natural := 0;
      Ship: in out Ship_Record) is
      use Tiny_String;

      Item_Index: Inventory_Container.Extended_Index := 0;
   begin
      if Inventory_Index = 0 then
         Item_Index :=
           (if Durability > 0 then
              Find_Item
                (Inventory => Ship.Crew(Member_Index).Inventory,
                 Proto_Index => Proto_Index, Durability => Durability)
            else Find_Item
                (Inventory => Ship.Crew(Member_Index).Inventory,
                 Proto_Index => Proto_Index));
      else
         Item_Index := Inventory_Index;
      end if;
      if Amount > 0 then
         Check_Free_Inventory_Space_Block :
         declare
            Weight: constant Positive :=
              (if Item_Index > 0 then
                 Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Ship.Crew(Member_Index).Inventory,
                         Index => Item_Index)
                        .Proto_Index)
                   .Weight *
                 Amount
               else Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .Weight *
                 Amount);
         begin
            if Free_Inventory
                (Member_Index => Member_Index, Amount => -(Weight)) <
              0 then
               raise Crew_No_Space_Error
                 with To_String(Source => Ship.Crew(Member_Index).Name) &
                 " doesn't have any free space in their inventory.";
            end if;
         end Check_Free_Inventory_Space_Block;
      else
         if Item_Is_Used
             (Member_Index => Member_Index, Item_Index => Item_Index) then
            Take_Off_Item
              (Member_Index => Member_Index, Item_Index => Item_Index);
         end if;
      end if;
      if Item_Index = 0 then
         Inventory_Container.Append
           (Container => Ship.Crew(Member_Index).Inventory,
            New_Item =>
              (Proto_Index => Proto_Index, Amount => Amount,
               Name =>
                 To_Bounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Objects_Container.Element
                             (Container => Items_List, Index => Proto_Index)
                             .Name)),
               Durability => Durability, Price => Price));
      else
         Update_Inventory_Block :
         declare
            Item: Inventory_Data :=
              Inventory_Container.Element
                (Container => Ship.Crew(Member_Index).Inventory,
                 Index => Item_Index);
            New_Amount: constant Natural := Item.Amount + Amount;
         begin
            if New_Amount = 0 then
               Inventory_Container.Delete
                 (Container => Ship.Crew(Member_Index).Inventory,
                  Index => Item_Index);
               Update_Item_Index_Loop :
               for Item of Ship.Crew(Member_Index).Equipment loop
                  if Item = Item_Index then
                     Item := 0;
                  elsif Item > Item_Index then
                     Item := Item - 1;
                  end if;
               end loop Update_Item_Index_Loop;
            else
               Item.Amount := New_Amount;
               Inventory_Container.Replace_Element
                 (Container => Ship.Crew(Member_Index).Inventory,
                  Index => Item_Index, New_Item => Item);
            end if;
         end Update_Inventory_Block;
      end if;
   end Update_Inventory;

   function Free_Inventory
     (Member_Index: Positive; Amount: Integer; Update_Nim: Boolean := True)
      return Integer is
      function Free_Ada_Inventory(M_Index, Amnt: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "freeAdaInventory";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      return Free_Ada_Inventory(M_Index => Member_Index, Amnt => Amount);
   end Free_Inventory;

   procedure Take_Off_Item
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True) is
      procedure Take_Ada_Off_Item(M_Index, I_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "takeAdaOffItem";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      Take_Ada_Off_Item(M_Index => Member_Index, I_Index => Item_Index);
   end Take_Off_Item;

   function Item_Is_Used
     (Member_Index, Item_Index: Positive; Update_Nim: Boolean := True)
      return Boolean is
      function Item_Ada_Is_Used(M_Index, I_Index: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "itemAdaIsUsed";
   begin
      if Update_Nim then
         Get_Ada_Crew;
         Get_Ada_Crew_Inventory
           (Inventory =>
              Inventory_To_Nim
                (Inventory => Player_Ship.Crew(Member_Index).Inventory),
            Member_Index => Member_Index);
      end if;
      if Item_Ada_Is_Used(M_Index => Member_Index, I_Index => Item_Index) =
        1 then
         return True;
      end if;
      return False;
   end Item_Is_Used;

   function Find_Tools
     (Member_Index: Positive; Item_Type: Tiny_String.Bounded_String;
      Order: Crew_Orders; Tool_Quality: Positive := 100) return Natural is
      use Tiny_String;

      Tools_Index: Inventory_Container.Extended_Index :=
        Player_Ship.Crew(Member_Index).Equipment(TOOL);
   begin
      -- If the crew member has equiped tool, check if it is a proper tool.
      -- If not, remove it and put to the ship cargo
      if Tools_Index > 0 then
         Update_Cargo_Block :
         declare
            Proto_Index: constant Objects_Container.Extended_Index :=
              Inventory_Container.Element
                (Container => Player_Ship.Crew(Member_Index).Inventory,
                 Index => Tools_Index)
                .Proto_Index;
         begin
            if Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type /=
              Item_Type or
              (Objects_Container.Element
                 (Container => Items_List, Index => Proto_Index)
                 .Value
                 (1) <
               Tool_Quality) then
               Update_Cargo
                 (Ship => Player_Ship, Proto_Index => Proto_Index, Amount => 1,
                  Durability =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Crew(Member_Index).Inventory,
                       Index => Tools_Index)
                      .Durability);
               Update_Inventory
                 (Member_Index => Member_Index, Amount => -1,
                  Inventory_Index => Tools_Index, Ship => Player_Ship);
               Tools_Index := 0;
            end if;
         end Update_Cargo_Block;
      end if;
      Tools_Index :=
        Find_Item
          (Inventory => Player_Ship.Crew(Member_Index).Inventory,
           Item_Type => Item_Type, Quality => Tool_Quality);
      if Tools_Index = 0 then
         Tools_Index :=
           Find_Item
             (Inventory => Player_Ship.Cargo, Item_Type => Item_Type,
              Quality => Tool_Quality);
         if Tools_Index > 0 then
            Find_Tool_Block :
            begin
               Update_Inventory
                 (Member_Index => Member_Index, Amount => 1,
                  Proto_Index =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Tools_Index)
                      .Proto_Index,
                  Durability =>
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Tools_Index)
                      .Durability,
                  Ship => Player_Ship);
               Update_Cargo
                 (Ship => Player_Ship, Amount => -1,
                  Cargo_Index => Tools_Index);
               Tools_Index :=
                 Find_Item
                   (Inventory => Player_Ship.Crew(Member_Index).Inventory,
                    Item_Type => Item_Type, Quality => Tool_Quality);
            exception
               when Crew_No_Space_Error =>
                  case Order is
                     when REPAIR =>
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Member_Index).Name) &
                             " can't continue repairs because they don't have free space in their inventory for repair tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                     when UPGRADING =>
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Member_Index).Name) &
                             " can't continue upgrading module because they don't have free space in their inventory for repair tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                     when CLEAN =>
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Member_Index).Name) &
                             " can't continue cleaning ship because they don't have free space in their inventory for cleaning tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                     when CRAFT =>
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Member_Index).Name) &
                             " can't continue manufacturing because they don't have space in their inventory for the proper tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                     when TRAIN =>
                        Add_Message
                          (Message =>
                             To_String
                               (Source =>
                                  Player_Ship.Crew(Member_Index).Name) &
                             " can't continue training because they don't have space in their inventory for the proper tools.",
                           M_Type => ORDERMESSAGE, Color => RED);
                     when others =>
                        null;
                  end case;
                  Give_Orders
                    (Ship => Player_Ship, Member_Index => Member_Index,
                     Given_Order => REST);
                  return 0;
            end Find_Tool_Block;
         end if;
      end if;
      Player_Ship.Crew(Member_Index).Equipment(TOOL) := Tools_Index;
      return Tools_Index;
   end Find_Tools;

end Crew.Inventory;
