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

   procedure LoadBases(SaveGame: File_Type) is
      BaseRecruits: Recruit_Container.Vector;
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      VectorLength, SkillsLength: Natural;
      Skills: Skills_Container.Vector;
      Attributes: Attributes_Container.Vector;
   begin
      for I in SkyBases'Range loop
         SkyBases(I) :=
           (Name => ReadData(SaveGame),
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
         SkyBases(I).Visited.Year :=
           Natural'Value(To_String(ReadData(SaveGame)));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).Visited.Month :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).Visited.Day :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).Visited.Hour :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).Visited.Minutes :=
              Natural'Value(To_String(ReadData(SaveGame)));
         end if;
         SkyBases(I).SkyX := Integer'Value(To_String(ReadData(SaveGame)));
         SkyBases(I).SkyY := Integer'Value(To_String(ReadData(SaveGame)));
         SkyBases(I).BaseType :=
           Bases_Types'Val(Integer'Value(To_String(ReadData(SaveGame))));
         SkyBases(I).Population :=
           Natural'Value(To_String(ReadData(SaveGame)));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).RecruitDate.Year :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).RecruitDate.Month :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).RecruitDate.Day :=
              Natural'Value(To_String(ReadData(SaveGame)));
            VectorLength := Natural'Value(To_String(ReadData(SaveGame)));
            if VectorLength > 0 then
               for J in 1 .. VectorLength loop
                  Skills.Clear;
                  Attributes.Clear;
                  BaseRecruits.Append
                  (New_Item =>
                     (Name => ReadData(SaveGame),
                      Gender =>
                        Ada.Strings.Unbounded.Element(ReadData(SaveGame), 1),
                      Price => Positive'Value(To_String(ReadData(SaveGame))),
                      Skills => Skills,
                      Attributes => Attributes));
                  SkillsLength :=
                    Positive'Value(To_String(ReadData(SaveGame)));
                  for K in 1 .. SkillsLength loop
                     Skills.Append
                     (New_Item =>
                        (Natural'Value(To_String(ReadData(SaveGame))),
                         Natural'Value(To_String(ReadData(SaveGame))),
                         Natural'Value(To_String(ReadData(SaveGame)))));
                  end loop;
                  BaseRecruits(BaseRecruits.Last_Index).Skills := Skills;
                  SkillsLength :=
                    Positive'Value(To_String(ReadData(SaveGame)));
                  if SkillsLength /= Natural(Attributes_List.Length) then
                     raise SaveGame_Invalid_Data
                       with "Different amount of character statistics.";
                  end if;
                  for K in 1 .. SkillsLength loop
                     Attributes.Append
                     (New_Item =>
                        (Natural'Value(To_String(ReadData(SaveGame))),
                         Natural'Value(To_String(ReadData(SaveGame)))));
                  end loop;
                  BaseRecruits(BaseRecruits.Last_Index).Attributes :=
                    Attributes;
               end loop;
               SkyBases(I).Recruits := BaseRecruits;
               BaseRecruits.Clear;
            end if;
            if ReadData(SaveGame) = To_Unbounded_String("Y") then
               SkyBases(I).AskedForBases := True;
            end if;
            SkyBases(I).AskedForEvents.Year :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).AskedForEvents.Month :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).AskedForEvents.Day :=
              Natural'Value(To_String(ReadData(SaveGame)));
         end if;
         SkyBases(I).Reputation(1) :=
           Integer'Value(To_String(ReadData(SaveGame)));
         SkyBases(I).Reputation(2) :=
           Integer'Value(To_String(ReadData(SaveGame)));
         if SkyBases(I).Visited.Year > 0 then
            SkyBases(I).MissionsDate.Year :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).MissionsDate.Month :=
              Natural'Value(To_String(ReadData(SaveGame)));
            SkyBases(I).MissionsDate.Day :=
              Natural'Value(To_String(ReadData(SaveGame)));
            VectorLength := Natural'Value(To_String(ReadData(SaveGame)));
            if VectorLength > 0 then
               for J in 1 .. VectorLength loop
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
                      StartBase => I,
                      Finished => False));
               end loop;
               SkyBases(I).Missions := BaseMissions;
               BaseMissions.Clear;
            end if;
            VectorLength := Natural'Value(To_String(ReadData(SaveGame)));
            if VectorLength > 0 then
               for J in 1 .. VectorLength loop
                  BaseCargo.Append
                  (New_Item =>
                     (ProtoIndex => FindProtoItem(ReadData(SaveGame)),
                      Amount => Natural'Value(To_String(ReadData(SaveGame))),
                      Durability =>
                        Positive'Value(To_String(ReadData(SaveGame))),
                      Price => Positive'Value(To_String(ReadData(SaveGame)))));
               end loop;
               SkyBases(I).Cargo := BaseCargo;
               BaseCargo.Clear;
            end if;
         end if;
         if ReadData(SaveGame) = To_Unbounded_String("Y") then
            SkyBases(I).Known := True;
         end if;
         SkyBases(I).Owner :=
           Bases_Owners'Val(Integer'Value(To_String(ReadData(SaveGame))));
         SkyMap(SkyBases(I).SkyX, SkyBases(I).SkyY).BaseIndex := I;
      end loop;
   end LoadBases;

end Bases.SaveLoad;
