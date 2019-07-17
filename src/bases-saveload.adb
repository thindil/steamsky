--    Copyright 2017-2019 Bartek thindil Jasicki
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
with DOM.Core.Elements; use DOM.Core.Elements;
with Maps; use Maps;

package body Bases.SaveLoad is

-- ****if* Bases.SaveLoad/SaveBases
-- SOURCE
   procedure SaveBases(SaveData: Document; MainNode: DOM.Core.Element) is
-- ****
      RawValue: Unbounded_String;
      BaseNode, SubNode: DOM.Core.Element;
   begin
      for I in SkyBases'Range loop
         BaseNode := Create_Element(SaveData, "base");
         BaseNode := Append_Child(MainNode, BaseNode);
         Set_Attribute(BaseNode, "name", To_String(SkyBases(I).Name));
         if SkyBases(I).Visited.Year > 0 then
            SubNode := Create_Element(SaveData, "visiteddate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Year));
            Set_Attribute
              (SubNode, "year", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Month));
            Set_Attribute
              (SubNode, "month", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Day));
            Set_Attribute
              (SubNode, "day", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Hour));
            Set_Attribute
              (SubNode, "hour", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Minutes));
            Set_Attribute
              (SubNode, "minutes",
               To_String(Trim(RawValue, Ada.Strings.Left)));
         end if;
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyX));
         Set_Attribute
           (BaseNode, "x", To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyY));
         Set_Attribute
           (BaseNode, "y", To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Bases_Types'Pos(SkyBases(I).BaseType)));
         Set_Attribute
           (BaseNode, "type", To_String(Trim(RawValue, Ada.Strings.Left)));
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Population));
         Set_Attribute
           (BaseNode, "population",
            To_String(Trim(RawValue, Ada.Strings.Left)));
         if SkyBases(I).Visited.Year > 0 then
            SubNode := Create_Element(SaveData, "recruitdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Year));
            Set_Attribute
              (SubNode, "year", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).RecruitDate.Month));
            Set_Attribute
              (SubNode, "month", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Day));
            Set_Attribute
              (SubNode, "day", To_String(Trim(RawValue, Ada.Strings.Left)));
            if SkyBases(I).Recruits.Length > 0 then
               declare
                  RecruitNode, RecruitDataNode: DOM.Core.Element;
               begin
                  for Recruit of SkyBases(I).Recruits loop
                     RecruitNode := Create_Element(SaveData, "recruit");
                     RecruitNode := Append_Child(BaseNode, RecruitNode);
                     Set_Attribute
                       (RecruitNode, "name", To_String(Recruit.Name));
                     Set_Attribute(RecruitNode, "gender", Recruit.Gender & "");
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Recruit.Price));
                     Set_Attribute
                       (RecruitNode, "price",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     for Skill of Recruit.Skills loop
                        RecruitDataNode := Create_Element(SaveData, "skill");
                        RecruitDataNode :=
                          Append_Child(RecruitNode, RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(1)));
                        Set_Attribute
                          (RecruitDataNode, "index",
                           To_String(Trim(RawValue, Ada.Strings.Left)));
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(2)));
                        Set_Attribute
                          (RecruitDataNode, "level",
                           To_String(Trim(RawValue, Ada.Strings.Left)));
                     end loop;
                     for Attribute of Recruit.Attributes loop
                        RecruitDataNode :=
                          Create_Element(SaveData, "attribute");
                        RecruitDataNode :=
                          Append_Child(RecruitNode, RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Attribute(1)));
                        Set_Attribute
                          (RecruitDataNode, "level",
                           To_String(Trim(RawValue, Ada.Strings.Left)));
                     end loop;
                     for Item of Recruit.Inventory loop
                        RecruitDataNode := Create_Element(SaveData, "item");
                        RecruitDataNode :=
                          Append_Child(RecruitNode, RecruitDataNode);
                        RawValue := Item;
                        Set_Attribute
                          (RecruitDataNode, "index",
                           To_String(Trim(RawValue, Ada.Strings.Left)));
                     end loop;
                     for J in Recruit.Equipment'Range loop
                        if Recruit.Equipment(J) > 0 then
                           RecruitDataNode :=
                             Create_Element(SaveData, "equipment");
                           RecruitDataNode :=
                             Append_Child(RecruitNode, RecruitDataNode);
                           RawValue := To_Unbounded_String(Integer'Image(J));
                           Set_Attribute
                             (RecruitDataNode, "slot",
                              To_String(Trim(RawValue, Ada.Strings.Left)));
                           RawValue :=
                             To_Unbounded_String
                               (Integer'Image(Recruit.Equipment(J)));
                           Set_Attribute
                             (RecruitDataNode, "index",
                              To_String(Trim(RawValue, Ada.Strings.Left)));
                        end if;
                     end loop;
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Recruit.Payment));
                     Set_Attribute
                       (RecruitNode, "payment",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Recruit.HomeBase));
                     Set_Attribute
                       (RecruitNode, "homebase",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     Set_Attribute
                       (RecruitNode, "faction", To_String(Recruit.Faction));
                  end loop;
               end;
            end if;
            if SkyBases(I).AskedForBases then
               Set_Attribute(BaseNode, "askedforbases", "Y");
            else
               Set_Attribute(BaseNode, "askedforbases", "N");
            end if;
            SubNode := Create_Element(SaveData, "askedforeventsdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Year));
            Set_Attribute
              (SubNode, "year", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Month));
            Set_Attribute
              (SubNode, "month", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Day));
            Set_Attribute
              (SubNode, "day", To_String(Trim(RawValue, Ada.Strings.Left)));
         end if;
         if SkyBases(I).Reputation(1) /= 0 then
            SubNode := Create_Element(SaveData, "reputation");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(1)));
            Set_Attribute
              (SubNode, "level", To_String(Trim(RawValue, Ada.Strings.Left)));
            if SkyBases(I).Reputation(2) > 0 then
               RawValue :=
                 To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(2)));
               Set_Attribute
                 (SubNode, "progress",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
            end if;
         end if;
         if SkyBases(I).Visited.Year > 0 then
            SubNode := Create_Element(SaveData, "missionsdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Year));
            Set_Attribute
              (SubNode, "year", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Month));
            Set_Attribute
              (SubNode, "month", To_String(Trim(RawValue, Ada.Strings.Left)));
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Day));
            Set_Attribute
              (SubNode, "day", To_String(Trim(RawValue, Ada.Strings.Left)));
            if SkyBases(I).Missions.Length > 0 then
               declare
                  MissionNode: DOM.Core.Element;
               begin
                  for Mission of SkyBases(I).Missions loop
                     MissionNode := Create_Element(SaveData, "mission");
                     MissionNode := Append_Child(BaseNode, MissionNode);
                     RawValue :=
                       To_Unbounded_String
                         (Integer'Image(Missions_Types'Pos(Mission.MType)));
                     Set_Attribute
                       (MissionNode, "type",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     if Mission.MType = Deliver then
                        RawValue := Mission.ItemIndex;
                     elsif Mission.MType = Passenger then
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Mission.Data));
                     elsif Mission.MType = Destroy then
                        RawValue := Mission.ShipIndex;
                     else
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Mission.Target));
                     end if;
                     Set_Attribute
                       (MissionNode, "target",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Time));
                     Set_Attribute
                       (MissionNode, "time",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.TargetX));
                     Set_Attribute
                       (MissionNode, "targetx",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.TargetY));
                     Set_Attribute
                       (MissionNode, "targety",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Reward));
                     Set_Attribute
                       (MissionNode, "reward",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                  end loop;
               end;
            end if;
            if SkyBases(I).Cargo.Length > 0 then
               declare
                  ItemNode: DOM.Core.Element;
               begin
                  for Item of SkyBases(I).Cargo loop
                     ItemNode := Create_Element(SaveData, "item");
                     ItemNode := Append_Child(BaseNode, ItemNode);
                     Set_Attribute
                       (ItemNode, "index", To_String(Item.ProtoIndex));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Amount));
                     Set_Attribute
                       (ItemNode, "amount",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Durability));
                     Set_Attribute
                       (ItemNode, "durability",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Price));
                     Set_Attribute
                       (ItemNode, "price",
                        To_String(Trim(RawValue, Ada.Strings.Left)));
                  end loop;
               end;
            end if;
         end if;
         if SkyBases(I).Known then
            Set_Attribute(BaseNode, "known", "Y");
         else
            Set_Attribute(BaseNode, "known", "N");
         end if;
         Set_Attribute(BaseNode, "owner", To_String(SkyBases(I).Owner));
         Set_Attribute(BaseNode, "size", Bases_Size'Image(SkyBases(I).Size));
      end loop;
   end SaveBases;

-- ****if* Bases.SaveLoad/LoadBases
-- SOURCE
   procedure LoadBases(SaveData: Document) is
-- ****
      BaseRecruits: Recruit_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      NodesList, BaseData: Node_List;
      BaseIndex: Positive;
      NodeName: Unbounded_String;
      BaseNode, ChildNode: Node;
   begin
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "base");
      BaseIndex := 1;
      for I in 0 .. Length(NodesList) - 1 loop
         BaseNode := Item(NodesList, I);
         SkyBases(BaseIndex) :=
           (Name => To_Unbounded_String(Get_Attribute(BaseNode, "name")),
            Visited => (0, 0, 0, 0, 0),
            SkyX => Integer'Value(Get_Attribute(BaseNode, "x")),
            SkyY => Integer'Value(Get_Attribute(BaseNode, "y")),
            BaseType =>
              Bases_Types'Val(Integer'Value(Get_Attribute(BaseNode, "type"))),
            Population => Integer'Value(Get_Attribute(BaseNode, "population")),
            RecruitDate => (0, 0, 0, 0, 0), Recruits => BaseRecruits,
            Known => False, AskedForBases => False,
            AskedForEvents => (0, 0, 0, 0, 0), Reputation => (0, 0),
            MissionsDate => (0, 0, 0, 0, 0), Missions => BaseMissions,
            Owner => Factions_Container.Key(Factions_List.First),
            Cargo => BaseCargo,
            Size => Bases_Size'Value(Get_Attribute(BaseNode, "size")));
         SkyBases(BaseIndex).Owner :=
           To_Unbounded_String(Get_Attribute(BaseNode, "owner"));
         if Get_Attribute(BaseNode, "known") = "Y" then
            SkyBases(BaseIndex).Known := True;
         end if;
         if Get_Attribute(BaseNode, "askedforbases") = "Y" then
            SkyBases(BaseIndex).AskedForBases := True;
         end if;
         BaseData := Child_Nodes(BaseNode);
         for J in 0 .. Length(BaseData) - 1 loop
            ChildNode := Item(BaseData, J);
            NodeName := To_Unbounded_String(Node_Name(ChildNode));
            if NodeName = To_Unbounded_String("visiteddate") then
               SkyBases(BaseIndex).Visited.Year :=
                 Natural'Value(Get_Attribute(ChildNode, "year"));
               SkyBases(BaseIndex).Visited.Month :=
                 Natural'Value(Get_Attribute(ChildNode, "month"));
               SkyBases(BaseIndex).Visited.Day :=
                 Natural'Value(Get_Attribute(ChildNode, "day"));
               SkyBases(BaseIndex).Visited.Hour :=
                 Natural'Value(Get_Attribute(ChildNode, "hour"));
               SkyBases(BaseIndex).Visited.Minutes :=
                 Natural'Value(Get_Attribute(ChildNode, "minutes"));
            elsif NodeName = To_Unbounded_String("recruitdate") then
               SkyBases(BaseIndex).RecruitDate.Year :=
                 Natural'Value(Get_Attribute(ChildNode, "year"));
               SkyBases(BaseIndex).RecruitDate.Month :=
                 Natural'Value(Get_Attribute(ChildNode, "month"));
               SkyBases(BaseIndex).RecruitDate.Day :=
                 Natural'Value(Get_Attribute(ChildNode, "day"));
            elsif NodeName = To_Unbounded_String("recruit") then
               declare
                  RecruitData: Node_List;
                  RecruitName, RecruitFaction: Unbounded_String;
                  Gender: String(1 .. 1);
                  Price, Payment, HomeBase: Positive;
                  Skills: Skills_Container.Vector;
                  Attributes: Attributes_Container.Vector;
                  Index, Level: Natural;
                  Inventory: UnboundedString_Container.Vector;
                  Equipment: Equipment_Array;
                  RecruitNode: Node;
               begin
                  Skills.Clear;
                  Attributes.Clear;
                  Inventory.Clear;
                  Equipment := (others => 0);
                  RecruitName :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "name"));
                  Gender := Get_Attribute(ChildNode, "gender");
                  Price := Positive'Value(Get_Attribute(ChildNode, "price"));
                  Payment := 20;
                  RecruitData := Child_Nodes(ChildNode);
                  for L in 0 .. Length(RecruitData) - 1 loop
                     RecruitNode := Item(RecruitData, L);
                     NodeName := To_Unbounded_String(Node_Name(RecruitNode));
                     if NodeName = To_Unbounded_String("skill") then
                        Index :=
                          Natural'Value(Get_Attribute(RecruitNode, "index"));
                        Level :=
                          Natural'Value(Get_Attribute(RecruitNode, "level"));
                        Skills.Append(New_Item => (Index, Level, 0));
                     elsif NodeName = To_Unbounded_String("attribute") then
                        Level :=
                          Natural'Value(Get_Attribute(RecruitNode, "level"));
                        Attributes.Append(New_Item => (Level, 0));
                     elsif NodeName = To_Unbounded_String("item") then
                        Inventory.Append
                          (New_Item =>
                             To_Unbounded_String
                               (Get_Attribute(RecruitNode, "index")));
                     elsif NodeName = To_Unbounded_String("equipment") then
                        Equipment
                          (Positive'Value
                             (Get_Attribute(RecruitNode, "slot"))) :=
                          Natural'Value(Get_Attribute(RecruitNode, "index"));
                     end if;
                     if Get_Attribute(ChildNode, "payment") /= "" then
                        Payment :=
                          Natural'Value(Get_Attribute(ChildNode, "payment"));
                     end if;
                     if Get_Attribute(ChildNode, "homebase") /= "" then
                        HomeBase :=
                          Positive'Value(Get_Attribute(ChildNode, "homebase"));
                     end if;
                     if Get_Attribute(ChildNode, "faction") /= "" then
                        RecruitFaction :=
                          To_Unbounded_String
                            (Get_Attribute(ChildNode, "faction"));
                     end if;
                  end loop;
                  SkyBases(BaseIndex).Recruits.Append
                    (New_Item =>
                       (Name => RecruitName, Gender => Gender(1),
                        Price => Price, Skills => Skills,
                        Attributes => Attributes, Inventory => Inventory,
                        Equipment => Equipment, Payment => Payment,
                        HomeBase => HomeBase, Faction => RecruitFaction));
               end;
            elsif NodeName = To_Unbounded_String("askedforeventsdate") then
               SkyBases(BaseIndex).AskedForEvents.Year :=
                 Natural'Value(Get_Attribute(ChildNode, "year"));
               SkyBases(BaseIndex).AskedForEvents.Month :=
                 Natural'Value(Get_Attribute(ChildNode, "month"));
               SkyBases(BaseIndex).AskedForEvents.Day :=
                 Natural'Value(Get_Attribute(ChildNode, "day"));
            elsif NodeName = To_Unbounded_String("reputation") then
               SkyBases(BaseIndex).Reputation(1) :=
                 Natural'Value(Get_Attribute(ChildNode, "level"));
               if Get_Attribute(ChildNode, "progress") /= "" then
                  SkyBases(BaseIndex).Reputation(2) :=
                    Natural'Value(Get_Attribute(ChildNode, "progress"));
               end if;
            elsif NodeName = To_Unbounded_String("missionsdate") then
               SkyBases(BaseIndex).MissionsDate.Year :=
                 Natural'Value(Get_Attribute(ChildNode, "year"));
               SkyBases(BaseIndex).MissionsDate.Month :=
                 Natural'Value(Get_Attribute(ChildNode, "month"));
               SkyBases(BaseIndex).MissionsDate.Day :=
                 Natural'Value(Get_Attribute(ChildNode, "day"));
            elsif NodeName = To_Unbounded_String("mission") then
               declare
                  MType: Missions_Types;
                  TargetX, TargetY: Natural;
                  Time, Reward: Positive;
                  Target: Integer;
                  Index: Unbounded_String;
               begin
                  MType :=
                    Missions_Types'Val
                      (Integer'Value(Get_Attribute(ChildNode, "type")));
                  if MType = Deliver or MType = Destroy then
                     Index :=
                       To_Unbounded_String(Get_Attribute(ChildNode, "target"));
                  else
                     Target :=
                       Integer'Value(Get_Attribute(ChildNode, "target"));
                  end if;
                  Time := Positive'Value(Get_Attribute(ChildNode, "time"));
                  TargetX :=
                    Natural'Value(Get_Attribute(ChildNode, "targetx"));
                  TargetY :=
                    Natural'Value(Get_Attribute(ChildNode, "targety"));
                  Reward := Positive'Value(Get_Attribute(ChildNode, "reward"));
                  case MType is
                     when Deliver =>
                        SkyBases(BaseIndex).Missions.Append
                          (New_Item =>
                             (MType => Deliver, ItemIndex => Index,
                              Time => Time, TargetX => TargetX,
                              TargetY => TargetY, Reward => Reward,
                              StartBase => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                     when Destroy =>
                        SkyBases(BaseIndex).Missions.Append
                          (New_Item =>
                             (MType => Destroy, ShipIndex => Index,
                              Time => Time, TargetX => TargetX,
                              TargetY => TargetY, Reward => Reward,
                              StartBase => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                     when Patrol =>
                        SkyBases(BaseIndex).Missions.Append
                          (New_Item =>
                             (MType => Patrol, Target => Target, Time => Time,
                              TargetX => TargetX, TargetY => TargetY,
                              Reward => Reward, StartBase => BaseIndex,
                              Finished => False, Multiplier => 1.0));
                     when Explore =>
                        SkyBases(BaseIndex).Missions.Append
                          (New_Item =>
                             (MType => Explore, Target => Target, Time => Time,
                              TargetX => TargetX, TargetY => TargetY,
                              Reward => Reward, StartBase => BaseIndex,
                              Finished => False, Multiplier => 1.0));
                     when Passenger =>
                        if Target > 91 then
                           Target := 91;
                        end if;
                        SkyBases(BaseIndex).Missions.Append
                          (New_Item =>
                             (MType => Passenger, Data => Target, Time => Time,
                              TargetX => TargetX, TargetY => TargetY,
                              Reward => Reward, StartBase => BaseIndex,
                              Finished => False, Multiplier => 1.0));
                  end case;
               end;
            elsif NodeName = To_Unbounded_String("item") then
               declare
                  Durability: Positive;
                  Amount, Price: Natural;
                  ProtoIndex: Unbounded_String;
               begin
                  ProtoIndex :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "index"));
                  Durability :=
                    Positive'Value(Get_Attribute(ChildNode, "durability"));
                  Amount := Natural'Value(Get_Attribute(ChildNode, "amount"));
                  Price := Natural'Value(Get_Attribute(ChildNode, "price"));
                  SkyBases(BaseIndex).Cargo.Append
                    (New_Item =>
                       (ProtoIndex => ProtoIndex, Amount => Amount,
                        Durability => Durability, Price => Price));
               end;
            end if;
         end loop;
         SkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY)
           .BaseIndex :=
           BaseIndex;
         BaseIndex := BaseIndex + 1;
      end loop;
   end LoadBases;

end Bases.SaveLoad;
