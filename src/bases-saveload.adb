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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Maps; use Maps;

package body Bases.SaveLoad is

   procedure SaveBases(SaveData: Document; MainNode: DOM.Core.Element) is
      RawValue: Unbounded_String;
      BaseNode, SubNode: DOM.Core.Element;
      procedure SaveNumber
        (Value: Integer; Name: String; Node: DOM.Core.Element := SubNode) is
         RawValue: constant String :=
           Trim(Integer'Image(Value), Ada.Strings.Left);
      begin
         Set_Attribute(Node, Name, RawValue);
      end SaveNumber;
   begin
      Save_Bases_Loop :
      for SkyBase of SkyBases loop
         BaseNode := Create_Element(SaveData, "base");
         BaseNode := Append_Child(MainNode, BaseNode);
         Set_Attribute(BaseNode, "name", To_String(SkyBase.Name));
         if SkyBase.Visited.Year = 0 then
            goto Save_Location;
         end if;
         SubNode := Create_Element(SaveData, "visiteddate");
         SubNode := Append_Child(BaseNode, SubNode);
         SaveNumber(SkyBase.Visited.Year, "year");
         SaveNumber(SkyBase.Visited.Month, "month");
         SaveNumber(SkyBase.Visited.Day, "day");
         SaveNumber(SkyBase.Visited.Hour, "hour");
         SaveNumber(SkyBase.Visited.Minutes, "minutes");
         <<Save_Location>>
         SaveNumber(SkyBase.SkyX, "x", BaseNode);
         SaveNumber(SkyBase.SkyY, "y", BaseNode);
         Set_Attribute(BaseNode, "type", To_String(SkyBase.BaseType));
         SaveNumber(SkyBase.Population, "population", BaseNode);
         if SkyBase.Visited.Year = 0 then
            goto Save_Reputation;
         end if;
         SubNode := Create_Element(SaveData, "recruitdate");
         SubNode := Append_Child(BaseNode, SubNode);
         SaveNumber(SkyBase.RecruitDate.Year, "year");
         SaveNumber(SkyBase.RecruitDate.Month, "month");
         SaveNumber(SkyBase.RecruitDate.Day, "day");
         if SkyBase.Recruits.Is_Empty then
            goto Save_AskForBases;
         end if;
         declare
            RecruitNode, RecruitDataNode: DOM.Core.Element;
         begin
            Save_Recruits_Loop :
            for Recruit of SkyBase.Recruits loop
               RecruitNode := Create_Element(SaveData, "recruit");
               RecruitNode := Append_Child(BaseNode, RecruitNode);
               Set_Attribute(RecruitNode, "name", To_String(Recruit.Name));
               Set_Attribute(RecruitNode, "gender", Recruit.Gender & "");
               RawValue := To_Unbounded_String(Integer'Image(Recruit.Price));
               Set_Attribute
                 (RecruitNode, "price",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               Save_Skills_Loop :
               for Skill of Recruit.Skills loop
                  RecruitDataNode := Create_Element(SaveData, "skill");
                  RecruitDataNode :=
                    Append_Child(RecruitNode, RecruitDataNode);
                  SaveNumber(Skill(1), "index", RecruitDataNode);
                  SaveNumber(Skill(2), "level", RecruitDataNode);
               end loop Save_Skills_Loop;
               Save_Attributes_Loop :
               for Attribute of Recruit.Attributes loop
                  RecruitDataNode := Create_Element(SaveData, "attribute");
                  RecruitDataNode :=
                    Append_Child(RecruitNode, RecruitDataNode);
                  SaveNumber(Attribute(1), "level", RecruitDataNode);
               end loop Save_Attributes_Loop;
               Save_Inventory_Loop :
               for Item of Recruit.Inventory loop
                  RecruitDataNode := Create_Element(SaveData, "item");
                  RecruitDataNode :=
                    Append_Child(RecruitNode, RecruitDataNode);
                  Set_Attribute(RecruitDataNode, "index", To_String(Item));
               end loop Save_Inventory_Loop;
               Save_Equipment_Loop :
               for J in Recruit.Equipment'Range loop
                  if Recruit.Equipment(J) > 0 then
                     RecruitDataNode := Create_Element(SaveData, "equipment");
                     RecruitDataNode :=
                       Append_Child(RecruitNode, RecruitDataNode);
                     SaveNumber(J, "slot", RecruitDataNode);
                     SaveNumber
                       (Recruit.Equipment(J), "index", RecruitDataNode);
                  end if;
               end loop Save_Equipment_Loop;
               SaveNumber(Recruit.Payment, "payment", RecruitNode);
               SaveNumber(Recruit.HomeBase, "homebase", RecruitNode);
               Set_Attribute
                 (RecruitNode, "faction", To_String(Recruit.Faction));
            end loop Save_Recruits_Loop;
         end;
         <<Save_AskForBases>>
         if SkyBase.AskedForBases then
            Set_Attribute(BaseNode, "askedforbases", "Y");
         else
            Set_Attribute(BaseNode, "askedforbases", "N");
         end if;
         SubNode := Create_Element(SaveData, "askedforeventsdate");
         SubNode := Append_Child(BaseNode, SubNode);
         SaveNumber(SkyBase.AskedForEvents.Year, "year");
         SaveNumber(SkyBase.AskedForEvents.Month, "month");
         SaveNumber(SkyBase.AskedForEvents.Day, "day");
         <<Save_Reputation>>
         if SkyBase.Reputation(1) /= 0 then
            SubNode := Create_Element(SaveData, "reputation");
            SubNode := Append_Child(BaseNode, SubNode);
            SaveNumber(SkyBase.Reputation(1), "level");
            if SkyBase.Reputation(2) > 0 then
               SaveNumber(SkyBase.Reputation(2), "progress");
            end if;
         end if;
         if SkyBase.Visited.Year = 0 then
            goto Save_Cargo;
         end if;
         SubNode := Create_Element(SaveData, "missionsdate");
         SubNode := Append_Child(BaseNode, SubNode);
         SaveNumber(SkyBase.MissionsDate.Year, "year");
         SaveNumber(SkyBase.MissionsDate.Month, "month");
         SaveNumber(SkyBase.MissionsDate.Day, "day");
         declare
            MissionNode: DOM.Core.Element;
         begin
            Save_Missions_Loop :
            for Mission of SkyBase.Missions loop
               MissionNode := Create_Element(SaveData, "mission");
               MissionNode := Append_Child(BaseNode, MissionNode);
               SaveNumber
                 (Missions_Types'Pos(Mission.MType), "type", MissionNode);
               RawValue :=
                 (case Mission.MType is when Deliver => Mission.ItemIndex,
                    when Passenger =>
                      To_Unbounded_String(Integer'Image(Mission.Data)),
                    when Destroy => Mission.ShipIndex,
                    when others =>
                      To_Unbounded_String(Integer'Image(Mission.Target)));
               Set_Attribute
                 (MissionNode, "target",
                  To_String(Trim(RawValue, Ada.Strings.Left)));
               SaveNumber(Mission.Time, "time", MissionNode);
               SaveNumber(Mission.TargetX, "targetx", MissionNode);
               SaveNumber(Mission.TargetY, "targety", MissionNode);
               SaveNumber(Mission.Reward, "reward", MissionNode);
            end loop Save_Missions_Loop;
         end;
         <<Save_Cargo>>
         if SkyBase.Cargo.Is_Empty then
            goto Save_Known;
         end if;
         declare
            ItemNode: DOM.Core.Element;
         begin
            Save_Cargo_Loop :
            for Item of SkyBase.Cargo loop
               ItemNode := Create_Element(SaveData, "item");
               ItemNode := Append_Child(BaseNode, ItemNode);
               Set_Attribute(ItemNode, "index", To_String(Item.ProtoIndex));
               SaveNumber(Item.Amount, "amount", ItemNode);
               SaveNumber(Item.Durability, "durability", ItemNode);
               SaveNumber(Item.Price, "price", ItemNode);
            end loop Save_Cargo_Loop;
         end;
         <<Save_Known>>
         if SkyBase.Known then
            Set_Attribute(BaseNode, "known", "Y");
         else
            Set_Attribute(BaseNode, "known", "N");
         end if;
         Set_Attribute(BaseNode, "owner", To_String(SkyBase.Owner));
         Set_Attribute(BaseNode, "size", Bases_Size'Image(SkyBase.Size));
      end loop Save_Bases_Loop;
   end SaveBases;

   procedure LoadBases(SaveData: Document) is
      BaseRecruits: Recruit_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      NodesList, BaseData: Node_List;
      BaseIndex: Bases_Range;
      NodeName: Unbounded_String;
      BaseNode, ChildNode: Node;
   begin
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "base");
      Load_Bases_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         BaseIndex := I + 1;
         BaseNode := Item(NodesList, I);
         SkyBases(BaseIndex) :=
           (Name => To_Unbounded_String(Get_Attribute(BaseNode, "name")),
            Visited => (others => 0),
            SkyX => Integer'Value(Get_Attribute(BaseNode, "x")),
            SkyY => Integer'Value(Get_Attribute(BaseNode, "y")),
            BaseType => To_Unbounded_String(Get_Attribute(BaseNode, "type")),
            Population => Integer'Value(Get_Attribute(BaseNode, "population")),
            RecruitDate => (others => 0), Recruits => BaseRecruits,
            Known => False, AskedForBases => False,
            AskedForEvents => (others => 0), Reputation => (0, 0),
            MissionsDate => (others => 0), Missions => BaseMissions,
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
         Load_Base_Loop :
         for J in 0 .. Length(BaseData) - 1 loop
            ChildNode := Item(BaseData, J);
            NodeName := To_Unbounded_String(Node_Name(ChildNode));
            if NodeName = To_Unbounded_String("visiteddate") then
               SkyBases(BaseIndex).Visited :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => Natural'Value(Get_Attribute(ChildNode, "hour")),
                  Minutes =>
                    Natural'Value(Get_Attribute(ChildNode, "minutes")));
            elsif NodeName = To_Unbounded_String("recruitdate") then
               SkyBases(BaseIndex).RecruitDate :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("recruit") then
               declare
                  RecruitData: Node_List;
                  RecruitName, RecruitFaction: Unbounded_String;
                  Gender: String(1 .. 1);
                  HomeBase: Bases_Range;
                  Price, Payment: Positive;
                  Skills: Skills_Container.Vector;
                  Attributes: Attributes_Container.Vector;
                  Index: SkillsData_Container.Extended_Index;
                  Inventory: UnboundedString_Container.Vector;
                  Equipment: Equipment_Array;
                  RecruitNode: Node;
                  Level: Skill_Range;
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
                  Load_Recruits_Loop :
                  for L in 0 .. Length(RecruitData) - 1 loop
                     RecruitNode := Item(RecruitData, L);
                     NodeName := To_Unbounded_String(Node_Name(RecruitNode));
                     if NodeName = To_Unbounded_String("skill") then
                        Index :=
                          SkillsData_Container.Extended_Index'Value
                            (Get_Attribute(RecruitNode, "index"));
                        Level :=
                          Skill_Range'Value
                            (Get_Attribute(RecruitNode, "level"));
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
                          Bases_Range'Value
                            (Get_Attribute(ChildNode, "homebase"));
                     end if;
                     if Get_Attribute(ChildNode, "faction") /= "" then
                        RecruitFaction :=
                          To_Unbounded_String
                            (Get_Attribute(ChildNode, "faction"));
                     end if;
                  end loop Load_Recruits_Loop;
                  SkyBases(BaseIndex).Recruits.Append
                    (New_Item =>
                       (Name => RecruitName, Gender => Gender(1),
                        Price => Price, Skills => Skills,
                        Attributes => Attributes, Inventory => Inventory,
                        Equipment => Equipment, Payment => Payment,
                        HomeBase => HomeBase, Faction => RecruitFaction));
               end;
            elsif NodeName = To_Unbounded_String("askedforeventsdate") then
               SkyBases(BaseIndex).AskedForEvents :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("reputation") then
               SkyBases(BaseIndex).Reputation(1) :=
                 Natural'Value(Get_Attribute(ChildNode, "level"));
               if Get_Attribute(ChildNode, "progress") /= "" then
                  SkyBases(BaseIndex).Reputation(2) :=
                    Natural'Value(Get_Attribute(ChildNode, "progress"));
               end if;
            elsif NodeName = To_Unbounded_String("missionsdate") then
               SkyBases(BaseIndex).MissionsDate :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("mission") then
               declare
                  MType: Missions_Types;
                  TargetX, TargetY: Natural range 0 .. 1024;
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
                  Durability: Items_Durability;
                  Amount, Price: Natural;
                  ProtoIndex: Unbounded_String;
               begin
                  ProtoIndex :=
                    To_Unbounded_String(Get_Attribute(ChildNode, "index"));
                  Durability :=
                    Items_Durability'Value
                      (Get_Attribute(ChildNode, "durability"));
                  Amount := Natural'Value(Get_Attribute(ChildNode, "amount"));
                  Price := Natural'Value(Get_Attribute(ChildNode, "price"));
                  SkyBases(BaseIndex).Cargo.Append
                    (New_Item =>
                       (ProtoIndex => ProtoIndex, Amount => Amount,
                        Durability => Durability, Price => Price));
               end;
            end if;
         end loop Load_Base_Loop;
         SkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY)
           .BaseIndex :=
           BaseIndex;
      end loop Load_Bases_Loop;
   end LoadBases;

end Bases.SaveLoad;
