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
with DOM.Core.Elements; use DOM.Core.Elements;
with Maps; use Maps;
with Items; use Items;
with Factions; use Factions;

package body Bases.SaveLoad is

   procedure SaveBases(SaveData: Document; MainNode: DOM.Core.Element) is
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
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(3)));
                        Set_Attribute
                          (RecruitDataNode, "experience",
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
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Attribute(2)));
                        Set_Attribute
                          (RecruitDataNode, "experience",
                           To_String(Trim(RawValue, Ada.Strings.Left)));
                     end loop;
                     for Item of Recruit.Inventory loop
                        RecruitDataNode := Create_Element(SaveData, "item");
                        RecruitDataNode :=
                          Append_Child(RecruitNode, RecruitDataNode);
                        RawValue := To_Unbounded_String(Integer'Image(Item));
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
                       (RecruitNode, "faction",
                        To_String(Factions_List(Recruit.Faction).Index));
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
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(2)));
            Set_Attribute
              (SubNode, "progress",
               To_String(Trim(RawValue, Ada.Strings.Left)));
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
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Target));
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
                       (ItemNode, "index",
                        To_String(Items_List(Item.ProtoIndex).Index));
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
         Set_Attribute
           (BaseNode, "owner",
            To_String(Factions_List(SkyBases(I).Owner).Index));
      end loop;
   end SaveBases;

   procedure LoadBases(SaveData: Document) is
      BaseRecruits: Recruit_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      NodesList, BaseData: Node_List;
      BaseIndex: Positive;
   begin
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(SaveData, "base");
      BaseIndex := 1;
      for I in 0 .. Length(NodesList) - 1 loop
         SkyBases(BaseIndex) :=
           (Name =>
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name")),
            Visited => (0, 0, 0, 0, 0),
            SkyX => Integer'Value(Get_Attribute(Item(NodesList, I), "x")),
            SkyY => Integer'Value(Get_Attribute(Item(NodesList, I), "y")),
            BaseType =>
              Bases_Types'Val
                (Integer'Value(Get_Attribute(Item(NodesList, I), "type"))),
            Population =>
              Integer'Value(Get_Attribute(Item(NodesList, I), "y")),
            RecruitDate => (0, 0, 0, 0, 0), Recruits => BaseRecruits,
            Known => False, AskedForBases => False,
            AskedForEvents => (0, 0, 0, 0, 0), Reputation => (0, 0),
            MissionsDate => (0, 0, 0, 0, 0), Missions => BaseMissions,
            Owner => 1, Cargo => BaseCargo);
         for J in Factions_List.Iterate loop
            if Factions_List(J).Index =
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "owner")) then
               SkyBases(BaseIndex).Owner := Factions_Container.To_Index(J);
               exit;
            end if;
         end loop;
         if Get_Attribute(Item(NodesList, I), "known") = "Y" then
            SkyBases(BaseIndex).Known := True;
         end if;
         if Get_Attribute(Item(NodesList, I), "askedforbases") = "Y" then
            SkyBases(BaseIndex).AskedForBases := True;
         end if;
         BaseData := Child_Nodes(Item(NodesList, I));
         for J in 0 .. Length(BaseData) - 1 loop
            if Node_Name(Item(BaseData, J)) = "visiteddate" then
               SkyBases(BaseIndex).Visited.Year :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "year"));
               SkyBases(BaseIndex).Visited.Month :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "month"));
               SkyBases(BaseIndex).Visited.Day :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "day"));
               SkyBases(BaseIndex).Visited.Hour :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "hour"));
               SkyBases(BaseIndex).Visited.Minutes :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "minutes"));
            elsif Node_Name(Item(BaseData, J)) = "recruitdate" then
               SkyBases(BaseIndex).RecruitDate.Year :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "year"));
               SkyBases(BaseIndex).RecruitDate.Month :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "month"));
               SkyBases(BaseIndex).RecruitDate.Day :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "day"));
            elsif Node_Name(Item(BaseData, J)) = "recruit" then
               declare
                  RecruitData: Node_List;
                  RecruitName: Unbounded_String;
                  Gender: String(1 .. 1);
                  Price, Payment, HomeBase, RecruitFaction: Positive;
                  Skills: Skills_Container.Vector;
                  Attributes: Attributes_Container.Vector;
                  Index, Level, Experience: Natural;
                  Inventory: Positive_Container.Vector;
                  Equipment: Equipment_Array;
               begin
                  Skills.Clear;
                  Attributes.Clear;
                  Inventory.Clear;
                  Equipment := (others => 0);
                  RecruitName :=
                    To_Unbounded_String
                      (Get_Attribute(Item(BaseData, J), "name"));
                  Gender := Get_Attribute(Item(BaseData, J), "gender");
                  Price :=
                    Positive'Value(Get_Attribute(Item(BaseData, J), "price"));
                  Payment := 20;
                  RecruitData := Child_Nodes(Item(BaseData, J));
                  for L in 0 .. Length(RecruitData) - 1 loop
                     if Node_Name(Item(RecruitData, L)) = "skill" then
                        Index :=
                          Natural'Value
                            (Get_Attribute(Item(RecruitData, L), "index"));
                        Level :=
                          Natural'Value
                            (Get_Attribute(Item(RecruitData, L), "level"));
                        Experience :=
                          Natural'Value
                            (Get_Attribute
                               (Item(RecruitData, L), "experience"));
                        Skills.Append(New_Item => (Index, Level, Experience));
                     elsif Node_Name(Item(RecruitData, L)) = "attribute" then
                        Level :=
                          Natural'Value
                            (Get_Attribute(Item(RecruitData, L), "level"));
                        Experience :=
                          Natural'Value
                            (Get_Attribute
                               (Item(RecruitData, L), "experience"));
                        Attributes.Append(New_Item => (Level, Experience));
                     elsif Node_Name(Item(RecruitData, L)) = "item" then
                        Inventory.Append
                          (New_Item =>
                             Positive'Value
                               (Get_Attribute(Item(RecruitData, L), "index")));
                     elsif Node_Name(Item(RecruitData, L)) = "equipment" then
                        Equipment
                          (Positive'Value
                             (Get_Attribute(Item(RecruitData, L), "slot"))) :=
                          Natural'Value
                            (Get_Attribute(Item(RecruitData, L), "index"));
                     end if;
                     if Get_Attribute(Item(BaseData, J), "payment") /= "" then
                        Payment :=
                          Natural'Value
                            (Get_Attribute(Item(BaseData, J), "payment"));
                     end if;
                     if Get_Attribute(Item(BaseData, J), "homebase") /= "" then
                        HomeBase :=
                          Positive'Value
                            (Get_Attribute(Item(BaseData, J), "homebase"));
                     end if;
                     if Get_Attribute(Item(BaseData, J), "faction") /= "" then
                        for K in Factions_List.Iterate loop
                           if Factions_List(K).Index =
                             To_Unbounded_String
                               (Get_Attribute
                                  (Item(BaseData, J), "faction")) then
                              RecruitFaction := Factions_Container.To_Index(K);
                              exit;
                           end if;
                        end loop;
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
            elsif Node_Name(Item(BaseData, J)) = "askedforeventsdate" then
               SkyBases(BaseIndex).AskedForEvents.Year :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "year"));
               SkyBases(BaseIndex).AskedForEvents.Month :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "month"));
               SkyBases(BaseIndex).AskedForEvents.Day :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "day"));
            elsif Node_Name(Item(BaseData, J)) = "reputation" then
               SkyBases(BaseIndex).Reputation(1) :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "level"));
               SkyBases(BaseIndex).Reputation(2) :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "progress"));
            elsif Node_Name(Item(BaseData, J)) = "missionsdate" then
               SkyBases(BaseIndex).MissionsDate.Year :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "year"));
               SkyBases(BaseIndex).MissionsDate.Month :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "month"));
               SkyBases(BaseIndex).MissionsDate.Day :=
                 Natural'Value(Get_Attribute(Item(BaseData, J), "day"));
            elsif Node_Name(Item(BaseData, J)) = "mission" then
               declare
                  MType: Missions_Types;
                  Target, TargetX, TargetY: Natural;
                  Time, Reward: Positive;
               begin
                  MType :=
                    Missions_Types'Val
                      (Integer'Value
                         (Get_Attribute(Item(BaseData, J), "type")));
                  Target :=
                    Natural'Value(Get_Attribute(Item(BaseData, J), "target"));
                  Time :=
                    Positive'Value(Get_Attribute(Item(BaseData, J), "time"));
                  TargetX :=
                    Natural'Value(Get_Attribute(Item(BaseData, J), "targetx"));
                  TargetY :=
                    Natural'Value(Get_Attribute(Item(BaseData, J), "targety"));
                  Reward :=
                    Positive'Value(Get_Attribute(Item(BaseData, J), "reward"));
                  SkyBases(BaseIndex).Missions.Append
                    (New_Item =>
                       (MType => MType, Target => Target, Time => Time,
                        TargetX => TargetX, TargetY => TargetY,
                        Reward => Reward, StartBase => BaseIndex,
                        Finished => False));
               end;
            elsif Node_Name(Item(BaseData, J)) = "item" then
               declare
                  ProtoIndex, Durability: Positive;
                  Amount, Price: Natural;
               begin
                  ProtoIndex :=
                    Positive'Value(Get_Attribute(Item(BaseData, J), "index"));
                  Durability :=
                    Positive'Value
                      (Get_Attribute(Item(BaseData, J), "durability"));
                  Amount :=
                    Natural'Value(Get_Attribute(Item(BaseData, J), "amount"));
                  Price :=
                    Natural'Value(Get_Attribute(Item(BaseData, J), "price"));
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
