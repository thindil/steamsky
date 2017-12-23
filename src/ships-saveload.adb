--    Copyright 2017 Bartek thindil Jasicki
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

with ShipModules; use ShipModules;
with Maps; use Maps;
with Game.SaveLoad; use Game.SaveLoad;

package body Ships.SaveLoad is

   procedure SavePlayerShip(SaveGame: in out File_Type) is
      RawValue: Unbounded_String;
   begin
      Put(SaveGame, To_String(PlayerShip.Name) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue :=
        To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      RawValue := To_Unbounded_String(PlayerShip.Modules.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Module of PlayerShip.Modules loop
         Put(SaveGame, To_String(Module.Name) & ";");
         Put(SaveGame, To_String(Modules_List(Module.ProtoIndex).Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Weight));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Durability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.MaxDurability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Module.Owner));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(Module.UpgradeProgress));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String
             (Integer'Image(ShipUpgrade'Pos(Module.UpgradeAction)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         for I in Module.Data'Range loop
            RawValue := To_Unbounded_String(Integer'Image(Module.Data(I)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Cargo.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Item of PlayerShip.Cargo loop
         Put(SaveGame, To_String(Items_List(Item.ProtoIndex).Index) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         Put(SaveGame, To_String(Item.Name) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Crew.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      for Member of PlayerShip.Crew loop
         Put(SaveGame, To_String(Member.Name) & ";");
         Put(SaveGame, Member.Gender & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Health));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Tired));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Hunger));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.Thirst));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String(Integer'Image(Crew_Orders'Pos(Member.Order)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Crew_Orders'Pos(Member.PreviousOrder)));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Integer'Image(Member.OrderTime));
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         RawValue := To_Unbounded_String(Member.Skills.Length'Img);
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         for Skill of Member.Skills loop
            RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
         for J in Member.Orders'Range loop
            RawValue := To_Unbounded_String(Integer'Image(Member.Orders(J)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
         RawValue := To_Unbounded_String(Member.Attributes.Length'Img);
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         for Attribute of Member.Attributes loop
            RawValue := To_Unbounded_String(Integer'Image(Attribute(1)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Attribute(2)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
         RawValue := To_Unbounded_String(Member.Inventory.Length'Img);
         Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         for Item of Member.Inventory loop
            Put(SaveGame, To_String(Items_List(Item.ProtoIndex).Index) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            Put(SaveGame, To_String(Item.Name) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
         for I in Member.Equipment'Range loop
            RawValue :=
              To_Unbounded_String(Integer'Image(Member.Equipment(I)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
         end loop;
      end loop;
      RawValue := To_Unbounded_String(PlayerShip.Missions.Length'Img);
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
      if PlayerShip.Missions.Length > 0 then
         for Mission of PlayerShip.Missions loop
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Missions_Types'Pos(Mission.MType)));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Target));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetX));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetY));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.Reward));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            RawValue := To_Unbounded_String(Integer'Image(Mission.StartBase));
            Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
            if Mission.Finished then
               Put(SaveGame, "Y;");
            else
               Put(SaveGame, "N;");
            end if;
         end loop;
      end if;
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.HomeBase));
      Put(SaveGame, To_String(Trim(RawValue, Ada.Strings.Left)) & ";");
   end SavePlayerShip;

   procedure LoadPlayerShip(SaveGame: File_Type) is
      VectorLength, SkillsLength: Natural;
      Skills: Skills_Container.Vector;
      ShipModules: Modules_Container.Vector;
      ShipCargo, Inventory: Inventory_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      TmpOrders: Orders_Array;
      Attributes: Attributes_Container.Vector;
   begin
      PlayerShip.Name := ReadData(SaveGame);
      PlayerShip.SkyX := Integer'Value(To_String(ReadData(SaveGame)));
      PlayerShip.SkyY := Integer'Value(To_String(ReadData(SaveGame)));
      PlayerShip.Speed :=
        ShipSpeed'Val(Integer'Value(To_String(ReadData(SaveGame))));
      PlayerShip.UpgradeModule := Integer'Value(To_String(ReadData(SaveGame)));
      PlayerShip.DestinationX := Integer'Value(To_String(ReadData(SaveGame)));
      PlayerShip.DestinationY := Integer'Value(To_String(ReadData(SaveGame)));
      PlayerShip.RepairModule := Integer'Value(To_String(ReadData(SaveGame)));
      VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         ShipModules.Append
         (New_Item =>
            (Name => ReadData(SaveGame),
             ProtoIndex => FindProtoModule(ReadData(SaveGame)),
             Weight => Natural'Value(To_String(ReadData(SaveGame))),
             Durability => Integer'Value(To_String(ReadData(SaveGame))),
             MaxDurability => Integer'Value(To_String(ReadData(SaveGame))),
             Owner => Integer'Value(To_String(ReadData(SaveGame))),
             UpgradeProgress => Integer'Value(To_String(ReadData(SaveGame))),
             UpgradeAction =>
               ShipUpgrade'Val(Integer'Value(To_String(ReadData(SaveGame)))),
             Data => (0, 0, 0)));
         for J in
           ShipModules(Modules_Container.Last_Index(ShipModules)).Data'
             Range loop
            ShipModules(Modules_Container.Last_Index(ShipModules)).Data(J) :=
              Integer'Value(To_String(ReadData(SaveGame)));
         end loop;
      end loop;
      PlayerShip.Modules := ShipModules;
      VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         ShipCargo.Append
         (New_Item =>
            (ProtoIndex => FindProtoItem(ReadData(SaveGame)),
             Amount => Positive'Value(To_String(ReadData(SaveGame))),
             Name => ReadData(SaveGame),
             Durability => Positive'Value(To_String(ReadData(SaveGame)))));
      end loop;
      PlayerShip.Cargo := ShipCargo;
      VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
      for I in 1 .. VectorLength loop
         Skills.Clear;
         Attributes.Clear;
         Inventory.Clear;
         ShipCrew.Append
         (New_Item =>
            (Name => ReadData(SaveGame),
             Gender => Element(ReadData(SaveGame), 1),
             Health => Natural'Value(To_String(ReadData(SaveGame))),
             Tired => Natural'Value(To_String(ReadData(SaveGame))),
             Skills => Skills,
             Hunger => Natural'Value(To_String(ReadData(SaveGame))),
             Thirst => Natural'Value(To_String(ReadData(SaveGame))),
             Order =>
               Crew_Orders'Val(Integer'Value(To_String(ReadData(SaveGame)))),
             PreviousOrder =>
               Crew_Orders'Val(Integer'Value(To_String(ReadData(SaveGame)))),
             OrderTime => Integer'Value(To_String(ReadData(SaveGame))),
             Orders => (others => 0),
             Attributes => Attributes,
             Inventory => Inventory,
             Equipment => (others => 0)));
         SkillsLength := Positive'Value(To_String(ReadData(SaveGame)));
         for J in 1 .. SkillsLength loop
            Skills.Append
            (New_Item =>
               (Natural'Value(To_String(ReadData(SaveGame))),
                Natural'Value(To_String(ReadData(SaveGame))),
                Natural'Value(To_String(ReadData(SaveGame)))));
         end loop;
         for J in TmpOrders'Range loop
            TmpOrders(J) := Natural'Value(To_String(ReadData(SaveGame)));
         end loop;
         SkillsLength := Positive'Value(To_String(ReadData(SaveGame)));
         for J in 1 .. SkillsLength loop
            Attributes.Append
            (New_Item =>
               (Natural'Value(To_String(ReadData(SaveGame))),
                Natural'Value(To_String(ReadData(SaveGame)))));
         end loop;
         VectorLength := Positive'Value(To_String(ReadData(SaveGame)));
         for I in 1 .. VectorLength loop
            Inventory.Append
            (New_Item =>
               (ProtoIndex => FindProtoItem(ReadData(SaveGame)),
                Amount => Positive'Value(To_String(ReadData(SaveGame))),
                Name => ReadData(SaveGame),
                Durability => Positive'Value(To_String(ReadData(SaveGame)))));
         end loop;
         ShipCrew(ShipCrew.Last_Index).Skills := Skills;
         ShipCrew(ShipCrew.Last_Index).Attributes := Attributes;
         ShipCrew(ShipCrew.Last_Index).Orders := TmpOrders;
         ShipCrew(ShipCrew.Last_Index).Inventory := Inventory;
         for I in ShipCrew(ShipCrew.Last_Index).Equipment'Range loop
            ShipCrew(ShipCrew.Last_Index).Equipment(I) :=
              Natural'Value(To_String(ReadData(SaveGame)));
         end loop;
      end loop;
      PlayerShip.Crew := ShipCrew;
      VectorLength := Natural'Value(To_String(ReadData(SaveGame)));
      if VectorLength > 0 then
         for I in 1 .. VectorLength loop
            BaseMissions.Append
            (New_Item =>
               (MType =>
                  Missions_Types'Val
                    (Integer'Value(To_String(ReadData(SaveGame)))),
                Target => Natural'Value(To_String(ReadData(SaveGame))),
                Time => Integer'Value(To_String(ReadData(SaveGame))),
                TargetX => Integer'Value(To_String(ReadData(SaveGame))),
                TargetY => Integer'Value(To_String(ReadData(SaveGame))),
                Reward => Integer'Value(To_String(ReadData(SaveGame))),
                StartBase => Integer'Value(To_String(ReadData(SaveGame))),
                Finished => False));
            if To_String(ReadData(SaveGame)) = "Y" then
               BaseMissions(BaseMissions.Last_Index).Finished := True;
            else
               BaseMissions(BaseMissions.Last_Index).Finished := False;
            end if;
            if not BaseMissions(I).Finished then
               SkyMap(BaseMissions(I).TargetX, BaseMissions(I).TargetY)
                 .MissionIndex :=
                 I;
            else
               SkyMap
                 (SkyBases(BaseMissions(I).StartBase).SkyX,
                  SkyBases(BaseMissions(I).StartBase).SkyY)
                 .MissionIndex :=
                 I;
            end if;
         end loop;
         PlayerShip.Missions := BaseMissions;
      end if;
      PlayerShip.HomeBase := Integer'Value(To_String(ReadData(SaveGame)));
   end LoadPlayerShip;

end Ships.SaveLoad;
