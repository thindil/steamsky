--    Copyright 2017-2018 Bartek thindil Jasicki
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

with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with ShipModules; use ShipModules;
with Maps; use Maps;
with Game.SaveLoad; use Game.SaveLoad;

package body Ships.SaveLoad is

   procedure SavePlayerShip(SaveData: Document; MainNode: DOM.Core.Element) is
      RawValue: Unbounded_String;
      CategoryNode, SubNode, DataNode: DOM.Core.Element;
   begin
      CategoryNode := Create_Element(SaveData, "playership");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      AddData("name", To_String(PlayerShip.Name), CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyX));
      AddData("x", To_String(Trim(RawValue, Ada.Strings.Left)), CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.SkyY));
      AddData("y", To_String(Trim(RawValue, Ada.Strings.Left)), CategoryNode);
      RawValue :=
        To_Unbounded_String(Integer'Image(ShipSpeed'Pos(PlayerShip.Speed)));
      AddData
        ("speed",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.UpgradeModule));
      AddData
        ("upgrademodule",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationX));
      AddData
        ("destinationx",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.DestinationY));
      AddData
        ("destinationy",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.RepairModule));
      AddData
        ("repairpriority",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
      SubNode := Create_Element(SaveData, "modules");
      SubNode := Append_Child(CategoryNode, SubNode);
      declare
         ModuleDataNode: DOM.Core.Element;
      begin
         for Module of PlayerShip.Modules loop
            DataNode := Create_Element(SaveData, "module");
            DataNode := Append_Child(SubNode, DataNode);
            AddData("name", To_String(Module.Name), DataNode);
            AddData
              ("index",
               To_String(Modules_List(Module.ProtoIndex).Index),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Weight));
            AddData
              ("weight",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Durability));
            AddData
              ("durability",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.MaxDurability));
            AddData
              ("maxdurability",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Module.Owner));
            AddData
              ("owner",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(Module.UpgradeProgress));
            AddData
              ("upgradeprogress",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(ShipUpgrade'Pos(Module.UpgradeAction)));
            AddData
              ("upgradeaction",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            ModuleDataNode := Create_Element(SaveData, "moduledata");
            ModuleDataNode := Append_Child(DataNode, ModuleDataNode);
            for I in Module.Data'Range loop
               RawValue := To_Unbounded_String(Integer'Image(Module.Data(I)));
               AddData
                 ("data",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  ModuleDataNode);
            end loop;
         end loop;
      end;
      SubNode := Create_Element(SaveData, "cargo");
      SubNode := Append_Child(CategoryNode, SubNode);
      for Item of PlayerShip.Cargo loop
         DataNode := Create_Element(SaveData, "item");
         DataNode := Append_Child(SubNode, DataNode);
         AddData
           ("index",
            To_String(Items_List(Item.ProtoIndex).Index),
            DataNode);
         RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
         AddData
           ("amount",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            DataNode);
         AddData("name", To_String(Item.Name), DataNode);
         RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
         AddData
           ("durability",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            DataNode);
      end loop;
      SubNode := Create_Element(SaveData, "crew");
      SubNode := Append_Child(CategoryNode, SubNode);
      declare
         CrewDataNode, StatNode: DOM.Core.Element;
      begin
         for Member of PlayerShip.Crew loop
            DataNode := Create_Element(SaveData, "member");
            DataNode := Append_Child(SubNode, DataNode);
            AddData("name", To_String(Member.Name), DataNode);
            AddData("gender", Member.Gender & "", DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Health));
            AddData
              ("health",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Tired));
            AddData
              ("tired",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Hunger));
            AddData
              ("hunger",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.Thirst));
            AddData
              ("thirst",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Crew_Orders'Pos(Member.Order)));
            AddData
              ("order",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Crew_Orders'Pos(Member.PreviousOrder)));
            AddData
              ("previousorder",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Member.OrderTime));
            AddData
              ("ordertime",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            CrewDataNode := Create_Element(SaveData, "skills");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Skill of Member.Skills loop
               StatNode := Create_Element(SaveData, "skill");
               StatNode := Append_Child(CrewDataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(1)));
               AddData
                 ("index",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(2)));
               AddData
                 ("level",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Skill(3)));
               AddData
                 ("experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "orderspriorities");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for J in Member.Orders'Range loop
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Orders(J)));
               AddData
                 ("priority",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  CrewDataNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "attributes");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Attribute of Member.Attributes loop
               StatNode := Create_Element(SaveData, "attribute");
               StatNode := Append_Child(CrewDataNode, StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Attribute(1)));
               AddData
                 ("level",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Attribute(2)));
               AddData
                 ("experience",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "inventory");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for Item of Member.Inventory loop
               StatNode := Create_Element(SaveData, "item");
               StatNode := Append_Child(CrewDataNode, StatNode);
               AddData
                 ("index",
                  To_String(Items_List(Item.ProtoIndex).Index),
                  StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Item.Amount));
               AddData
                 ("amount",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
               AddData("name", To_String(Item.Name), StatNode);
               RawValue := To_Unbounded_String(Integer'Image(Item.Durability));
               AddData
                 ("durability",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  StatNode);
            end loop;
            CrewDataNode := Create_Element(SaveData, "equipment");
            CrewDataNode := Append_Child(DataNode, CrewDataNode);
            for I in Member.Equipment'Range loop
               RawValue :=
                 To_Unbounded_String(Integer'Image(Member.Equipment(I)));
               AddData
                 ("index",
                  To_String(Trim(RawValue, Ada.Strings.Left)),
                  CrewDataNode);
            end loop;
         end loop;
      end;
      SubNode := Create_Element(SaveData, "missions");
      SubNode := Append_Child(CategoryNode, SubNode);
      if PlayerShip.Missions.Length > 0 then
         for Mission of PlayerShip.Missions loop
            DataNode := Create_Element(SaveData, "mission");
            DataNode := Append_Child(SubNode, DataNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(Missions_Types'Pos(Mission.MType)));
            AddData
              ("type",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Target));
            AddData
              ("target",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Time));
            AddData
              ("time",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetX));
            AddData
              ("targetx",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.TargetY));
            AddData
              ("targety",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.Reward));
            AddData
              ("reward",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            RawValue := To_Unbounded_String(Integer'Image(Mission.StartBase));
            AddData
              ("startbase",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               DataNode);
            if Mission.Finished then
               AddData("finished", "Y", DataNode);
            else
               AddData("finished", "N", DataNode);
            end if;
         end loop;
      end if;
      RawValue := To_Unbounded_String(Integer'Image(PlayerShip.HomeBase));
      AddData
        ("homebase",
         To_String(Trim(RawValue, Ada.Strings.Left)),
         CategoryNode);
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
             Gender => Ada.Strings.Unbounded.Element(ReadData(SaveGame), 1),
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
