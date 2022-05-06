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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew.Inventory; use Crew.Inventory;

package body Ships.Repairs is

   procedure Repair_Ship(Minutes: Positive) is
      use Tiny_String;

      Order_Time, Current_Minutes, Repair_Points: Integer;
      Repair_Needed, Repair_Stopped: Boolean := False;
      Crew_Repair_Points: Natural_Container.Vector;
      procedure Repair_Module(Module_Index: Positive) is
         Points_Index, Points_Bonus, Repair_Value: Natural;
         Repair_Material, Tools_Index: Inventory_Container.Extended_Index;
      begin
         Points_Index := 0;
         Repair_Needed := True;
         Repair_Stopped := False;
         Repair_Module_Loop :
         for J in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(J).Order /= REPAIR then
               goto End_Of_Loop;
            end if;
            Points_Index := Points_Index + 1;
            if Crew_Repair_Points(Points_Index) > 0 then
               Points_Bonus :=
                 (Get_Skill_Level
                    (Member => Player_Ship.Crew(J),
                     Skill_Index =>
                       BaseModules_Container.Element
                         (Container => Modules_List,
                          Index =>
                            Player_Ship.Modules(Module_Index).Proto_Index)
                         .Repair_Skill) /
                  10) *
                 Crew_Repair_Points(Points_Index);
               Repair_Points :=
                 Crew_Repair_Points(Points_Index) + Points_Bonus;
               Tools_Index :=
                 FindTools
                   (MemberIndex => Crew_Container.To_Index(Position => J),
                    ItemType => Repair_Tools, Order => REPAIR);
               if Tools_Index = 0 then
                  if Points_Index = 1 then
                     Add_Message
                       (Message =>
                          "You don't have the proper repair tools to continue repairs of " &
                          To_String
                            (Source =>
                               Player_Ship.Modules(Module_Index).Name) &
                          ".",
                        M_Type => ORDERMESSAGE, Color => RED);
                  else
                     Add_Message
                       (Message =>
                          To_String(Source => Player_Ship.Crew(J).Name) &
                          " can't continue repairs due to a lack of repair tools.",
                        M_Type => ORDERMESSAGE, Color => RED);
                  end if;
                  Repair_Stopped := True;
                  return;
               end if;
               Repair_Material :=
                 Find_Item
                   (Inventory => Player_Ship.Cargo,
                    Item_Type =>
                      BaseModules_Container.Element
                        (Container => Modules_List,
                         Index =>
                           Player_Ship.Modules(Module_Index).Proto_Index)
                        .Repair_Material);
               if Repair_Material > 0
                 and then
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Repair_Material)
                     .Amount <
                   Repair_Points then
                  Repair_Points :=
                    Inventory_Container.Element
                      (Container => Player_Ship.Cargo,
                       Index => Repair_Material)
                      .Amount;
               end if;
               if Repair_Material = 0 then
                  Add_Message
                    (Message =>
                       "You don't have the proper repair materials to continue repairs of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".",
                     M_Type => ORDERMESSAGE, Color => RED);
                  Repair_Stopped := True;
                  return;
               end if;
               -- Repair module
               if Player_Ship.Modules(Module_Index).Durability +
                 Repair_Points >=
                 Player_Ship.Modules(Module_Index).Max_Durability then
                  Repair_Value :=
                    Player_Ship.Modules(Module_Index).Max_Durability -
                    Player_Ship.Modules(Module_Index).Durability;
                  Repair_Needed := False;
               else
                  Repair_Value := Repair_Points;
               end if;
               if Repair_Value =
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Repair_Material)
                   .Amount and
                 Tools_Index > Repair_Material then
                  Tools_Index := Tools_Index - 1;
               end if;
               Update_Cargo
                 (Ship => Player_Ship, Cargo_Index => Repair_Material,
                  Amount => (0 - Repair_Value));
               Player_Ship.Modules(Module_Index).Durability :=
                 Player_Ship.Modules(Module_Index).Durability + Repair_Value;
               if Repair_Value > Crew_Repair_Points(Points_Index) then
                  Repair_Value := Crew_Repair_Points(Points_Index);
                  Repair_Points := 0;
               else
                  Repair_Points :=
                    Crew_Repair_Points(Points_Index) - Repair_Value;
               end if;
               Gain_Exp
                 (Amount => Repair_Value,
                  Skill_Number =>
                    BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Repair_Skill,
                  Crew_Index => Crew_Container.To_Index(Position => J));
               Crew_Repair_Points(Points_Index) := Repair_Points;
               Damage_Item
                 (Inventory => Player_Ship.Crew(J).Inventory,
                  Item_Index => Tools_Index,
                  Skill_Level =>
                    Get_Skill_Level
                      (Member => Player_Ship.Crew(J),
                       Skill_Index =>
                         BaseModules_Container.Element
                           (Container => Modules_List,
                            Index =>
                              Player_Ship.Modules(Module_Index).Proto_Index)
                           .Repair_Skill),
                  Member_Index => Crew_Container.To_Index(Position => J),
                  Ship => Player_Ship);
               exit Repair_Module_Loop when not Repair_Needed;
            end if;
            <<End_Of_Loop>>
         end loop Repair_Module_Loop;
      end Repair_Module;
   begin
      Count_Repair_Workers_Loop :
      for Member of Player_Ship.Crew loop
         if Member.Order = REPAIR then
            Current_Minutes := Minutes;
            Order_Time := Member.Order_Time;
            Repair_Points := 0;
            Count_Repair_Points_Loop :
            while Current_Minutes > 0 loop
               if Current_Minutes >= Order_Time then
                  Current_Minutes := Current_Minutes - Order_Time;
                  Repair_Points := Repair_Points + 1;
                  Order_Time := 15;
               else
                  Order_Time := Order_Time - Current_Minutes;
                  Current_Minutes := 0;
               end if;
            end loop Count_Repair_Points_Loop;
            Crew_Repair_Points.Append(New_Item => Repair_Points);
            Member.Order_Time := Order_Time;
         end if;
      end loop Count_Repair_Workers_Loop;
      if Crew_Repair_Points.Length = 0 then
         return;
      end if;
      if Player_Ship.Repair_Module > 0
        and then Player_Ship.Modules(Player_Ship.Repair_Module).Durability <
          Player_Ship.Modules(Player_Ship.Repair_Module).Max_Durability then
         Repair_Module(Module_Index => Player_Ship.Repair_Module);
      end if;
      Repair_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).Durability <
           Player_Ship.Modules(I).Max_Durability then
            Repair_Module
              (Module_Index => Modules_Container.To_Index(Position => I));
         end if;
      end loop Repair_Loop;
      -- Send repair team on break if all is ok
      if not Repair_Needed or Repair_Stopped then
         if not Repair_Needed then
            Add_Message
              (Message => "All repairs have been finished.",
               M_Type => ORDERMESSAGE, Color => GREEN);
         end if;
         Give_Orders_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order = REPAIR then
               Give_Orders
                 (Ship => Player_Ship,
                  Member_Index => Crew_Container.To_Index(Position => I),
                  Given_Order => REST);
            end if;
         end loop Give_Orders_Loop;
      end if;
   end Repair_Ship;

end Ships.Repairs;
