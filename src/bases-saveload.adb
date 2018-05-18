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
with Maps; use Maps;
with Items; use Items;
with Game.SaveLoad; use Game.SaveLoad;

package body Bases.SaveLoad is

   procedure SaveBases(SaveData: Document; MainNode: DOM.Core.Element) is
      RawValue: Unbounded_String;
      CategoryNode, BaseNode, SubNode: DOM.Core.Element;
   begin
      CategoryNode := Create_Element(SaveData, "bases");
      CategoryNode := Append_Child(MainNode, CategoryNode);
      for I in SkyBases'Range loop
         BaseNode := Create_Element(SaveData, "base");
         BaseNode := Append_Child(CategoryNode, BaseNode);
         AddData("name", To_String(SkyBases(I).Name), BaseNode);
         SubNode := Create_Element(SaveData, "visiteddate");
         SubNode := Append_Child(BaseNode, SubNode);
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Year));
         AddData("year", To_String(Trim(RawValue, Ada.Strings.Left)), SubNode);
         if SkyBases(I).Visited.Year > 0 then
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Month));
            AddData
              ("month",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Day));
            AddData
              ("day",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Hour));
            AddData
              ("hour",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).Visited.Minutes));
            AddData
              ("minutes",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
         end if;
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyX));
         AddData("x", To_String(Trim(RawValue, Ada.Strings.Left)), BaseNode);
         RawValue := To_Unbounded_String(Integer'Image(SkyBases(I).SkyY));
         AddData("y", To_String(Trim(RawValue, Ada.Strings.Left)), BaseNode);
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Bases_Types'Pos(SkyBases(I).BaseType)));
         AddData
           ("type",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            BaseNode);
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Population));
         AddData
           ("population",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            BaseNode);
         if SkyBases(I).Visited.Year > 0 then
            SubNode := Create_Element(SaveData, "recruitdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Year));
            AddData
              ("year",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).RecruitDate.Month));
            AddData
              ("month",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).RecruitDate.Day));
            AddData
              ("day",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            SubNode := Create_Element(SaveData, "recruits");
            SubNode := Append_Child(BaseNode, SubNode);
            if SkyBases(I).Recruits.Length > 0 then
               declare
                  RecruitNode,
                  RecruitStatsNode,
                  RecruitDataNode: DOM.Core.Element;
               begin
                  for Recruit of SkyBases(I).Recruits loop
                     RecruitNode := Create_Element(SaveData, "recruit");
                     RecruitNode := Append_Child(SubNode, RecruitNode);
                     AddData("name", To_String(Recruit.Name), RecruitNode);
                     AddData("gender", Recruit.Gender & "", RecruitNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Recruit.Price));
                     AddData
                       ("price",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        RecruitNode);
                     RecruitStatsNode := Create_Element(SaveData, "skills");
                     RecruitStatsNode :=
                       Append_Child(RecruitNode, RecruitStatsNode);
                     for Skill of Recruit.Skills loop
                        RecruitDataNode := Create_Element(SaveData, "skill");
                        RecruitDataNode :=
                          Append_Child(RecruitStatsNode, RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(1)));
                        AddData
                          ("index",
                           To_String(Trim(RawValue, Ada.Strings.Left)),
                           RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(2)));
                        AddData
                          ("level",
                           To_String(Trim(RawValue, Ada.Strings.Left)),
                           RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Skill(3)));
                        AddData
                          ("experience",
                           To_String(Trim(RawValue, Ada.Strings.Left)),
                           RecruitDataNode);
                     end loop;
                     RecruitStatsNode :=
                       Create_Element(SaveData, "attributes");
                     RecruitStatsNode :=
                       Append_Child(RecruitNode, RecruitStatsNode);
                     for Attribute of Recruit.Attributes loop
                        RecruitDataNode :=
                          Create_Element(SaveData, "attribute");
                        RecruitDataNode :=
                          Append_Child(RecruitStatsNode, RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Attribute(1)));
                        AddData
                          ("level",
                           To_String(Trim(RawValue, Ada.Strings.Left)),
                           RecruitDataNode);
                        RawValue :=
                          To_Unbounded_String(Integer'Image(Attribute(2)));
                        AddData
                          ("experience",
                           To_String(Trim(RawValue, Ada.Strings.Left)),
                           RecruitDataNode);
                     end loop;
                  end loop;
               end;
            end if;
            if SkyBases(I).AskedForBases then
               AddData("askedforbases", "Y", BaseNode);
            else
               AddData("askedforbases", "N", BaseNode);
            end if;
            SubNode := Create_Element(SaveData, "askedforeventsdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Year));
            AddData
              ("year",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Month));
            AddData
              ("month",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).AskedForEvents.Day));
            AddData
              ("day",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
         end if;
         SubNode := Create_Element(SaveData, "reputation");
         SubNode := Append_Child(BaseNode, SubNode);
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(1)));
         AddData
           ("level",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            SubNode);
         RawValue :=
           To_Unbounded_String(Integer'Image(SkyBases(I).Reputation(2)));
         AddData
           ("progress",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            SubNode);
         if SkyBases(I).Visited.Year > 0 then
            SubNode := Create_Element(SaveData, "missionsdate");
            SubNode := Append_Child(BaseNode, SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Year));
            AddData
              ("year",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String
                (Integer'Image(SkyBases(I).MissionsDate.Month));
            AddData
              ("month",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            RawValue :=
              To_Unbounded_String(Integer'Image(SkyBases(I).MissionsDate.Day));
            AddData
              ("day",
               To_String(Trim(RawValue, Ada.Strings.Left)),
               SubNode);
            SubNode := Create_Element(SaveData, "missions");
            SubNode := Append_Child(BaseNode, SubNode);
            if SkyBases(I).Missions.Length > 0 then
               declare
                  MissionNode: DOM.Core.Element;
               begin
                  for Mission of SkyBases(I).Missions loop
                     MissionNode := Create_Element(SaveData, "mission");
                     MissionNode := Append_Child(SubNode, MissionNode);
                     RawValue :=
                       To_Unbounded_String
                         (Integer'Image(Missions_Types'Pos(Mission.MType)));
                     AddData
                       ("type",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Target));
                     AddData
                       ("target",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Time));
                     AddData
                       ("time",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.TargetX));
                     AddData
                       ("targetx",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.TargetY));
                     AddData
                       ("targety",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Mission.Reward));
                     AddData
                       ("reward",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        MissionNode);
                  end loop;
               end;
            end if;
            SubNode := Create_Element(SaveData, "cargo");
            SubNode := Append_Child(BaseNode, SubNode);
            if SkyBases(I).Cargo.Length > 0 then
               declare
                  ItemNode: DOM.Core.Element;
               begin
                  for Item of SkyBases(I).Cargo loop
                     ItemNode := Create_Element(SaveData, "item");
                     ItemNode := Append_Child(SubNode, ItemNode);
                     AddData
                       ("index",
                        To_String(Items_List(Item.ProtoIndex).Index),
                        ItemNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Amount));
                     AddData
                       ("amount",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        ItemNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Durability));
                     AddData
                       ("durability",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        ItemNode);
                     RawValue :=
                       To_Unbounded_String(Integer'Image(Item.Price));
                     AddData
                       ("price",
                        To_String(Trim(RawValue, Ada.Strings.Left)),
                        ItemNode);
                  end loop;
               end;
            end if;
         end if;
         if SkyBases(I).Known then
            AddData("known", "Y", BaseNode);
         else
            AddData("known", "N", BaseNode);
         end if;
         RawValue :=
           To_Unbounded_String
             (Integer'Image(Bases_Owners'Pos(SkyBases(I).Owner)));
         AddData
           ("owner",
            To_String(Trim(RawValue, Ada.Strings.Left)),
            BaseNode);
      end loop;
   end SaveBases;

   procedure LoadBases(SaveData: Document) is
      BaseRecruits: Recruit_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      NodesList, ChildNodes, BaseData: Node_List;
      BaseIndex: Positive;
   begin
      NodesList := Get_Elements_By_Tag_Name(SaveData, "bases");
      ChildNodes := Child_Nodes(Item(NodesList, 0));
      BaseIndex := 1;
      for I in 0 .. Length(ChildNodes) - 1 loop
         if Node_Name(Item(ChildNodes, I)) = "base" then
            SkyBases(BaseIndex) :=
              (Name => Null_Unbounded_String,
               Visited => (0, 0, 0, 0, 0),
               SkyX => 0,
               SkyY => 0,
               BaseType => Industrial,
               Population => 0,
               RecruitDate => (0, 0, 0, 0, 0),
               Recruits => BaseRecruits,
               Known => False,
               AskedForBases => False,
               AskedForEvents => (0, 0, 0, 0, 0),
               Reputation => (0, 0),
               MissionsDate => (0, 0, 0, 0, 0),
               Missions => BaseMissions,
               Owner => Poleis,
               Cargo => BaseCargo);
            BaseData := Child_Nodes(Item(ChildNodes, I));
            for J in 0 .. Length(BaseData) - 1 loop
               if Node_Name(Item(BaseData, J)) = "name" then
                  SkyBases(BaseIndex).Name :=
                    To_Unbounded_String
                      (Node_Value(First_Child(Item(BaseData, J))));
               elsif Node_Name(Item(BaseData, J)) = "visiteddate" then
                  declare
                     DateNode: Node_List;
                  begin
                     DateNode := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(DateNode) - 1 loop
                        if Node_Name(Item(DateNode, K)) = "year" then
                           SkyBases(BaseIndex).Visited.Year :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "month" then
                           SkyBases(BaseIndex).Visited.Month :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "day" then
                           SkyBases(BaseIndex).Visited.Day :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "hour" then
                           SkyBases(BaseIndex).Visited.Hour :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "minutes" then
                           SkyBases(BaseIndex).Visited.Minutes :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        end if;
                     end loop;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "x" then
                  SkyBases(BaseIndex).SkyX :=
                    Integer'Value(Node_Value(First_Child(Item(BaseData, J))));
               elsif Node_Name(Item(BaseData, J)) = "y" then
                  SkyBases(BaseIndex).SkyY :=
                    Integer'Value(Node_Value(First_Child(Item(BaseData, J))));
               elsif Node_Name(Item(BaseData, J)) = "type" then
                  SkyBases(BaseIndex).BaseType :=
                    Bases_Types'Val
                      (Integer'Value
                         (Node_Value(First_Child(Item(BaseData, J)))));
               elsif Node_Name(Item(BaseData, J)) = "population" then
                  SkyBases(BaseIndex).Population :=
                    Natural'Value(Node_Value(First_Child(Item(BaseData, J))));
               elsif Node_Name(Item(BaseData, J)) = "recruitdate" then
                  declare
                     DateNode: Node_List;
                  begin
                     DateNode := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(DateNode) - 1 loop
                        if Node_Name(Item(DateNode, K)) = "year" then
                           SkyBases(BaseIndex).RecruitDate.Year :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "month" then
                           SkyBases(BaseIndex).RecruitDate.Month :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "day" then
                           SkyBases(BaseIndex).RecruitDate.Day :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "hour" then
                           SkyBases(BaseIndex).RecruitDate.Hour :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "minutes" then
                           SkyBases(BaseIndex).RecruitDate.Minutes :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        end if;
                     end loop;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "recruits" then
                  declare
                     RecruitsNodes, RecruitData, StatsData: Node_List;
                     Name: Unbounded_String;
                     Gender: Character;
                     Price: Positive;
                     Skills: Skills_Container.Vector;
                     Attributes: Attributes_Container.Vector;
                     Index, Level, Experience: Natural;
                  begin
                     RecruitsNodes := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(RecruitsNodes) - 1 loop
                        if Node_Name(Item(RecruitsNodes, K)) = "recruit" then
                           Skills.Clear;
                           Attributes.Clear;
                           RecruitData := Child_Nodes(Item(RecruitsNodes, K));
                           for L in 0 .. Length(RecruitData) - 1 loop
                              if Node_Name(Item(RecruitData, L)) = "name" then
                                 Name :=
                                   To_Unbounded_String
                                     (Node_Value
                                        (First_Child(Item(RecruitData, L))));
                              elsif Node_Name(Item(RecruitData, L)) =
                                "gender" then
                                 Gender :=
                                   Node_Value
                                     (First_Child(Item(RecruitData, L)))
                                     (1);
                              elsif Node_Name(Item(RecruitData, L)) =
                                "price" then
                                 Price :=
                                   Positive'Value
                                     (Node_Value
                                        (First_Child(Item(RecruitData, L))));
                              elsif Node_Name(Item(RecruitData, L)) =
                                "skills" then
                                 StatsData :=
                                   Child_Nodes(Item(RecruitData, L));
                                 Index := 1;
                                 Level := 1;
                                 Experience := 0;
                                 for M in 0 .. Length(StatsData) - 1 loop
                                    if Node_Name(Item(StatsData, M)) =
                                      "index" then
                                       Index :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatsData, M))));
                                    elsif Node_Name(Item(StatsData, M)) =
                                      "level" then
                                       Level :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatsData, M))));
                                    elsif Node_Name(Item(StatsData, M)) =
                                      "experience" then
                                       Experience :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatsData, M))));
                                    end if;
                                 end loop;
                                 Skills.Append
                                 (New_Item => (Index, Level, Experience));
                              elsif Node_Name(Item(RecruitData, L)) =
                                "attributes" then
                                 StatsData :=
                                   Child_Nodes(Item(RecruitData, L));
                                 Level := 1;
                                 Experience := 0;
                                 for M in 0 .. Length(StatsData) - 1 loop
                                    if Node_Name(Item(StatsData, M)) =
                                      "level" then
                                       Level :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatsData, M))));
                                    elsif Node_Name(Item(StatsData, M)) =
                                      "experience" then
                                       Experience :=
                                         Natural'Value
                                           (Node_Value
                                              (First_Child
                                                 (Item(StatsData, M))));
                                    end if;
                                 end loop;
                                 Attributes.Append
                                 (New_Item => (Level, Experience));
                              end if;
                           end loop;
                           BaseRecruits.Append
                           (New_Item =>
                              (Name => Name,
                               Gender => Gender,
                               Price => Price,
                               Skills => Skills,
                               Attributes => Attributes));
                        end if;
                     end loop;
                     SkyBases(BaseIndex).Recruits := BaseRecruits;
                     BaseRecruits.Clear;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "askedforbases" then
                  if Node_Value(First_Child(Item(BaseData, J))) = "Y" then
                     SkyBases(BaseIndex).AskedForBases := True;
                  end if;
               elsif Node_Name(Item(BaseData, J)) = "askedforeventsdate" then
                  declare
                     DateNode: Node_List;
                  begin
                     DateNode := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(DateNode) - 1 loop
                        if Node_Name(Item(DateNode, K)) = "year" then
                           SkyBases(BaseIndex).AskedForEvents.Year :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "month" then
                           SkyBases(BaseIndex).AskedForEvents.Month :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "day" then
                           SkyBases(BaseIndex).AskedForEvents.Day :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        end if;
                     end loop;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "reputation" then
                  declare
                     ReputationNode: Node_List;
                  begin
                     ReputationNode := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(ReputationNode) - 1 loop
                        if Node_Name(Item(ReputationNode, K)) = "level" then
                           SkyBases(BaseIndex).Reputation(1) :=
                             Integer'Value
                               (Node_Value
                                  (First_Child(Item(ReputationNode, K))));
                        elsif Node_Name(Item(ReputationNode, K)) =
                          "progress" then
                           SkyBases(BaseIndex).Reputation(2) :=
                             Integer'Value
                               (Node_Value
                                  (First_Child(Item(ReputationNode, K))));
                        end if;
                     end loop;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "missionsdate" then
                  declare
                     DateNode: Node_List;
                  begin
                     DateNode := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(DateNode) - 1 loop
                        if Node_Name(Item(DateNode, K)) = "year" then
                           SkyBases(BaseIndex).MissionsDate.Year :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "month" then
                           SkyBases(BaseIndex).MissionsDate.Month :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        elsif Node_Name(Item(DateNode, K)) = "day" then
                           SkyBases(BaseIndex).MissionsDate.Day :=
                             Natural'Value
                               (Node_Value(First_Child(Item(DateNode, K))));
                        end if;
                     end loop;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "missions" then
                  declare
                     MissionsNodes, MissionData: Node_List;
                     MType: Missions_Types;
                     Target, TargetX, TargetY: Natural;
                     Time, Reward: Positive;
                  begin
                     MissionsNodes := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(MissionsNodes) - 1 loop
                        if Node_Name(Item(MissionsNodes, K)) = "mission" then
                           MissionData := Child_Nodes(Item(MissionsNodes, K));
                           MType := Deliver;
                           Target := 0;
                           Time := 1;
                           TargetX := 0;
                           TargetY := 0;
                           Reward := 1;
                           for L in 0 .. Length(MissionData) - 1 loop
                              if Node_Name(Item(MissionData, L)) = "type" then
                                 MType :=
                                   Missions_Types'Val
                                     (Integer'Value
                                        (Node_Value
                                           (First_Child
                                              (Item(MissionData, L)))));
                              elsif Node_Name(Item(MissionData, L)) =
                                "target" then
                                 Target :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(MissionData, L))));
                              elsif Node_Name(Item(MissionData, L)) =
                                "time" then
                                 Time :=
                                   Positive'Value
                                     (Node_Value
                                        (First_Child(Item(MissionData, L))));
                              elsif Node_Name(Item(MissionData, L)) =
                                "targetx" then
                                 TargetX :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(MissionData, L))));
                              elsif Node_Name(Item(MissionData, L)) =
                                "targety" then
                                 TargetY :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(MissionData, L))));
                              elsif Node_Name(Item(MissionData, L)) =
                                "reward" then
                                 Reward :=
                                   Positive'Value
                                     (Node_Value
                                        (First_Child(Item(MissionData, L))));
                              end if;
                           end loop;
                           BaseMissions.Append
                           (New_Item =>
                              (MType => MType,
                               Target => Target,
                               Time => Time,
                               TargetX => TargetX,
                               TargetY => TargetY,
                               Reward => Reward,
                               StartBase => BaseIndex,
                               Finished => False));
                        end if;
                     end loop;
                     SkyBases(BaseIndex).Missions := BaseMissions;
                     BaseMissions.Clear;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "cargo" then
                  declare
                     CargoNodes, ItemData: Node_List;
                     ProtoIndex, Durability: Positive;
                     Amount, Price: Natural;
                  begin
                     CargoNodes := Child_Nodes(Item(BaseData, J));
                     for K in 0 .. Length(CargoNodes) - 1 loop
                        if Node_Name(Item(CargoNodes, K)) = "item" then
                           ItemData := Child_Nodes(Item(CargoNodes, K));
                           ProtoIndex := 1;
                           Durability := 1;
                           Amount := 0;
                           Price := 0;
                           for L in 0 .. Length(ItemData) - 1 loop
                              if Node_Name(Item(ItemData, L)) = "index" then
                                 ProtoIndex :=
                                   FindProtoItem
                                     (To_Unbounded_String
                                        ((Node_Value
                                            (First_Child
                                               (Item(ItemData, L))))));
                              elsif Node_Name(Item(ItemData, L)) =
                                "amount" then
                                 Amount :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(ItemData, L))));
                              elsif Node_Name(Item(ItemData, L)) =
                                "durability" then
                                 Durability :=
                                   Positive'Value
                                     (Node_Value
                                        (First_Child(Item(ItemData, L))));
                              elsif Node_Name(Item(ItemData, L)) = "price" then
                                 Price :=
                                   Natural'Value
                                     (Node_Value
                                        (First_Child(Item(ItemData, L))));
                              end if;
                           end loop;
                           BaseCargo.Append
                           (New_Item =>
                              (ProtoIndex => ProtoIndex,
                               Amount => Amount,
                               Durability => Durability,
                               Price => Price));
                        end if;
                     end loop;
                     SkyBases(BaseIndex).Cargo := BaseCargo;
                     BaseCargo.Clear;
                  end;
               elsif Node_Name(Item(BaseData, J)) = "known" then
                  if Node_Value(First_Child(Item(BaseData, J))) = "Y" then
                     SkyBases(BaseIndex).Known := True;
                  end if;
               elsif Node_Name(Item(BaseData, J)) = "owner" then
                  SkyBases(BaseIndex).Owner :=
                    Bases_Owners'Val
                      (Integer'Value
                         (Node_Value(First_Child(Item(BaseData, J)))));
               end if;
            end loop;
            SkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY)
              .BaseIndex :=
              BaseIndex;
            BaseIndex := BaseIndex + 1;
         end if;
      end loop;
   end LoadBases;

end Bases.SaveLoad;
