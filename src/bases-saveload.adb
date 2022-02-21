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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Maps; use Maps;

package body Bases.SaveLoad is

   procedure Save_Bases
     (Save_Data: not null Document; Main_Node: not null DOM.Core.Element) is
      use Tiny_String;

      Raw_Value: Unbounded_String;
      Base_Node, Sub_Node: DOM.Core.Element;
      procedure Save_Number
        (Value: Integer; Name: String; Node: DOM.Core.Element := Sub_Node) is
         Number_Raw_Value: constant String :=
           Trim(Source => Integer'Image(Value), Side => Ada.Strings.Left);
      begin
         Set_Attribute(Elem => Node, Name => Name, Value => Number_Raw_Value);
      end Save_Number;
   begin
      Save_Bases_Loop :
      for SkyBase of Sky_Bases loop
         Base_Node := Create_Element(Doc => Save_Data, Tag_Name => "base");
         Base_Node := Append_Child(N => Main_Node, New_Child => Base_Node);
         Set_Attribute
           (Elem => Base_Node, Name => "name",
            Value => To_String(Source => SkyBase.Name));
         if SkyBase.Visited.Year = 0 then
            goto Save_Location;
         end if;
         Sub_Node :=
           Create_Element(Doc => Save_Data, Tag_Name => "visiteddate");
         Sub_Node := Append_Child(N => Base_Node, New_Child => Sub_Node);
         Save_Number(Value => SkyBase.Visited.Year, Name => "year");
         Save_Number(Value => SkyBase.Visited.Month, Name => "month");
         Save_Number(Value => SkyBase.Visited.Day, Name => "day");
         Save_Number(Value => SkyBase.Visited.Hour, Name => "hour");
         Save_Number(Value => SkyBase.Visited.Minutes, Name => "minutes");
         <<Save_Location>>
         Save_Number(Value => SkyBase.Sky_X, Name => "x", Node => Base_Node);
         Save_Number(Value => SkyBase.Sky_Y, Name => "y", Node => Base_Node);
         Set_Attribute
           (Elem => Base_Node, Name => "type",
            Value => To_String(Source => SkyBase.Base_Type));
         Save_Number
           (Value => SkyBase.Population, Name => "population",
            Node => Base_Node);
         if SkyBase.Visited.Year = 0 then
            goto Save_Reputation;
         end if;
         Sub_Node :=
           Create_Element(Doc => Save_Data, Tag_Name => "recruitdate");
         Sub_Node := Append_Child(N => Base_Node, New_Child => Sub_Node);
         Save_Number(Value => SkyBase.Recruit_Date.Year, Name => "year");
         Save_Number(Value => SkyBase.Recruit_Date.Month, Name => "month");
         Save_Number(Value => SkyBase.Recruit_Date.Day, Name => "day");
         if Recruit_Container.Is_Empty(Container => SkyBase.Recruits) then
            goto Save_Ask_For_Bases;
         end if;
         Save_Recruits_Block :
         declare
            Recruit_Node, Recruit_Data_Node: DOM.Core.Element;
            Recruit: Recruit_Data :=
              Recruit_Container.Element
                (Container => SkyBase.Recruits, Index => 1);
         begin
            Save_Recruits_Loop :
            for I in
              Recruit_Container.First_Index(Container => SkyBase.Recruits) ..
                Recruit_Container.Last_Index
                  (Container => SkyBase.Recruits) loop
               Recruit :=
                 Recruit_Container.Element
                   (Container => SkyBase.Recruits, Index => I);
               Recruit_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "recruit");
               Recruit_Node :=
                 Append_Child(N => Base_Node, New_Child => Recruit_Node);
               Set_Attribute
                 (Elem => Recruit_Node, Name => "name",
                  Value => To_String(Source => Recruit.Name));
               Set_Attribute
                 (Elem => Recruit_Node, Name => "gender",
                  Value => Recruit.Gender & "");
               Raw_Value :=
                 To_Unbounded_String(Source => Integer'Image(Recruit.Price));
               Set_Attribute
                 (Elem => Recruit_Node, Name => "price",
                  Value =>
                    To_String
                      (Source =>
                         Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
               Save_Skills_Loop :
               for Skill of Recruit.Skills loop
                  Recruit_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "skill");
                  Recruit_Data_Node :=
                    Append_Child
                      (N => Recruit_Node, New_Child => Recruit_Data_Node);
                  Save_Number
                    (Value => Natural(Skill.Index), Name => "index",
                     Node => Recruit_Data_Node);
                  Save_Number
                    (Value => Skill.Level, Name => "level",
                     Node => Recruit_Data_Node);
               end loop Save_Skills_Loop;
               Save_Attributes_Loop :
               for Attribute of Recruit.Attributes loop
                  Recruit_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "attribute");
                  Recruit_Data_Node :=
                    Append_Child
                      (N => Recruit_Node, New_Child => Recruit_Data_Node);
                  Save_Number
                    (Value => Attribute.Level, Name => "level",
                     Node => Recruit_Data_Node);
               end loop Save_Attributes_Loop;
               Save_Inventory_Loop :
               for Item of Recruit.Inventory loop
                  Recruit_Data_Node :=
                    Create_Element(Doc => Save_Data, Tag_Name => "item");
                  Recruit_Data_Node :=
                    Append_Child
                      (N => Recruit_Node, New_Child => Recruit_Data_Node);
                  Set_Attribute
                    (Elem => Recruit_Data_Node, Name => "index",
                     Value => To_String(Source => Item));
               end loop Save_Inventory_Loop;
               Save_Equipment_Loop :
               for J in Recruit.Equipment'Range loop
                  if Recruit.Equipment(J) > 0 then
                     Recruit_Data_Node :=
                       Create_Element
                         (Doc => Save_Data, Tag_Name => "equipment");
                     Recruit_Data_Node :=
                       Append_Child
                         (N => Recruit_Node, New_Child => Recruit_Data_Node);
                     Save_Number
                       (Value => Equipment_Locations'Pos(J) + 1,
                        Name => "slot", Node => Recruit_Data_Node);
                     Save_Number
                       (Value => Recruit.Equipment(J), Name => "index",
                        Node => Recruit_Data_Node);
                  end if;
               end loop Save_Equipment_Loop;
               Save_Number
                 (Value => Recruit.Payment, Name => "payment",
                  Node => Recruit_Node);
               Save_Number
                 (Value => Recruit.Home_Base, Name => "homebase",
                  Node => Recruit_Node);
               Set_Attribute
                 (Elem => Recruit_Node, Name => "faction",
                  Value => To_String(Source => Recruit.Faction));
            end loop Save_Recruits_Loop;
         end Save_Recruits_Block;
         <<Save_Ask_For_Bases>>
         if SkyBase.Asked_For_Bases then
            Set_Attribute
              (Elem => Base_Node, Name => "askedforbases", Value => "Y");
         else
            Set_Attribute
              (Elem => Base_Node, Name => "askedforbases", Value => "N");
         end if;
         Sub_Node :=
           Create_Element(Doc => Save_Data, Tag_Name => "askedforeventsdate");
         Sub_Node := Append_Child(N => Base_Node, New_Child => Sub_Node);
         Save_Number(Value => SkyBase.Asked_For_Events.Year, Name => "year");
         Save_Number(Value => SkyBase.Asked_For_Events.Month, Name => "month");
         Save_Number(Value => SkyBase.Asked_For_Events.Day, Name => "day");
         <<Save_Reputation>>
         if SkyBase.Reputation.Level /= 0 then
            Sub_Node :=
              Create_Element(Doc => Save_Data, Tag_Name => "reputation");
            Sub_Node := Append_Child(N => Base_Node, New_Child => Sub_Node);
            Save_Number(SkyBase.Reputation.Level, "level");
            if SkyBase.Reputation.Experience > 0 then
               Save_Number(SkyBase.Reputation.Experience, "progress");
            end if;
         end if;
         if SkyBase.Visited.Year = 0 then
            goto Save_Cargo;
         end if;
         Sub_Node := Create_Element(Save_Data, "missionsdate");
         Sub_Node := Append_Child(Base_Node, Sub_Node);
         Save_Number(SkyBase.Missions_Date.Year, "year");
         Save_Number(SkyBase.Missions_Date.Month, "month");
         Save_Number(SkyBase.Missions_Date.Day, "day");
         declare
            MissionNode: DOM.Core.Element;
         begin
            Save_Missions_Loop :
            for Mission of SkyBase.Missions loop
               MissionNode := Create_Element(Save_Data, "mission");
               MissionNode := Append_Child(Base_Node, MissionNode);
               Save_Number
                 (Missions_Types'Pos(Mission.M_Type), "type", MissionNode);
               Raw_Value :=
                 (case Mission.M_Type is
                    when DELIVER =>
                      To_Unbounded_String
                        (Source => To_String(Source => Mission.Item_Index)),
                    when PASSENGER =>
                      To_Unbounded_String(Integer'Image(Mission.Data)),
                    when DESTROY => Mission.Ship_Index,
                    when others =>
                      To_Unbounded_String(Integer'Image(Mission.Target)));
               Set_Attribute
                 (MissionNode, "target",
                  To_String(Trim(Raw_Value, Ada.Strings.Left)));
               Save_Number(Mission.Time, "time", MissionNode);
               Save_Number(Mission.Target_X, "targetx", MissionNode);
               Save_Number(Mission.Target_Y, "targety", MissionNode);
               Save_Number(Mission.Reward, "reward", MissionNode);
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
               ItemNode := Create_Element(Save_Data, "item");
               ItemNode := Append_Child(Base_Node, ItemNode);
               Set_Attribute(ItemNode, "index", To_String(Item.Proto_Index));
               Save_Number(Item.Amount, "amount", ItemNode);
               Save_Number(Item.Durability, "durability", ItemNode);
               Save_Number(Item.Price, "price", ItemNode);
            end loop Save_Cargo_Loop;
         end;
         <<Save_Known>>
         if SkyBase.Known then
            Set_Attribute(Base_Node, "known", "Y");
         else
            Set_Attribute(Base_Node, "known", "N");
         end if;
         Set_Attribute(Base_Node, "owner", To_String(SkyBase.Owner));
         Set_Attribute(Base_Node, "size", Bases_Size'Image(SkyBase.Size));
      end loop Save_Bases_Loop;
   end Save_Bases;

   procedure Load_Bases(Save_Data: not null Document) is
      use Tiny_String;

      BaseRecruits: Recruit_Container.Vector (Capacity => 30);
      BaseMissions: Mission_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      NodesList, BaseData: Node_List;
      BaseIndex: Bases_Range;
      NodeName: Unbounded_String;
      BaseNode, ChildNode: Node;
   begin
      Recruit_Container.Clear(Container => BaseRecruits);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(Save_Data, "base");
      Load_Bases_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         BaseIndex := I + 1;
         BaseNode := Item(NodesList, I);
         Sky_Bases(BaseIndex).Name :=
           To_Unbounded_String(Get_Attribute(BaseNode, "name"));
         Sky_Bases(BaseIndex).Visited := (others => 0);
         Sky_Bases(BaseIndex).Sky_X :=
           Integer'Value(Get_Attribute(BaseNode, "x"));
         Sky_Bases(BaseIndex).Sky_Y :=
           Integer'Value(Get_Attribute(BaseNode, "y"));
         Sky_Bases(BaseIndex).Base_Type :=
           To_Unbounded_String(Get_Attribute(BaseNode, "type"));
         Sky_Bases(BaseIndex).Population :=
           Integer'Value(Get_Attribute(BaseNode, "population"));
         Sky_Bases(BaseIndex).Recruit_Date := (others => 0);
         Recruit_Container.Assign
           (Target => Sky_Bases(BaseIndex).Recruits, Source => BaseRecruits);
         Sky_Bases(BaseIndex).Known := False;
         Sky_Bases(BaseIndex).Asked_For_Bases := False;
         Sky_Bases(BaseIndex).Asked_For_Events := (others => 0);
         Sky_Bases(BaseIndex).Reputation := (0, 0);
         Sky_Bases(BaseIndex).Missions_Date := (others => 0);
         Sky_Bases(BaseIndex).Missions := BaseMissions;
         Sky_Bases(BaseIndex).Owner :=
           Factions_Container.Key(Factions_List.First);
         Sky_Bases(BaseIndex).Cargo := BaseCargo;
         Sky_Bases(BaseIndex).Size :=
           Bases_Size'Value(Get_Attribute(BaseNode, "size"));
         Sky_Bases(BaseIndex).Owner :=
           To_Bounded_String(Get_Attribute(BaseNode, "owner"));
         if Get_Attribute(BaseNode, "known") = "Y" then
            Sky_Bases(BaseIndex).Known := True;
         end if;
         if Get_Attribute(BaseNode, "askedforbases") = "Y" then
            Sky_Bases(BaseIndex).Asked_For_Bases := True;
         end if;
         BaseData := Child_Nodes(BaseNode);
         Load_Base_Loop :
         for J in 0 .. Length(BaseData) - 1 loop
            ChildNode := Item(BaseData, J);
            NodeName := To_Unbounded_String(Node_Name(ChildNode));
            if NodeName = To_Unbounded_String("visiteddate") then
               Sky_Bases(BaseIndex).Visited :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => Natural'Value(Get_Attribute(ChildNode, "hour")),
                  Minutes =>
                    Natural'Value(Get_Attribute(ChildNode, "minutes")));
            elsif NodeName = To_Unbounded_String("recruitdate") then
               Sky_Bases(BaseIndex).Recruit_Date :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("recruit") then
               declare
                  RecruitData: Node_List;
                  RecruitName: Unbounded_String;
                  RecruitFaction: Bounded_String;
                  Gender: String(1 .. 1);
                  HomeBase: Bases_Range;
                  Price, Payment: Positive;
                  Skills: Skills_Container.Vector (Capacity => Skills_Amount);
                  Index: SkillsData_Container.Extended_Index;
                  Inventory: TinyString_Formal_Container.Vector
                    (Capacity => Equipment_Array'Length);
                  Equipment: Equipment_Array;
                  RecruitNode: Node;
                  Level: Skill_Range;
                  Attributes: Mob_Attributes
                    (1 ..
                         Positive
                           (AttributesData_Container.Length
                              (Container => Attributes_List)));
                  Attribute_Index: Positive := 1;
               begin
                  Skills_Container.Clear(Container => Skills);
                  Attributes := (others => <>);
                  TinyString_Formal_Container.Clear(Container => Inventory);
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
                        Skills_Container.Append
                          (Container => Skills, New_Item => (Index, Level, 0));
                     elsif NodeName = To_Unbounded_String("attribute") then
                        Level :=
                          Natural'Value(Get_Attribute(RecruitNode, "level"));
                        Attributes(Attribute_Index) := (Level, 0);
                        Attribute_Index := Attribute_Index + 1;
                     elsif NodeName = To_Unbounded_String("item") then
                        TinyString_Formal_Container.Append
                          (Container => Inventory,
                           New_Item =>
                             To_Bounded_String
                               (Get_Attribute(RecruitNode, "index")));
                     elsif NodeName = To_Unbounded_String("equipment") then
                        Equipment
                          (Equipment_Locations'Val
                             (Natural'Value
                                (Get_Attribute(RecruitNode, "slot")) -
                              1)) :=
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
                          To_Bounded_String
                            (Get_Attribute(ChildNode, "faction"));
                     end if;
                  end loop Load_Recruits_Loop;
                  Recruit_Container.Append
                    (Container => Sky_Bases(BaseIndex).Recruits,
                     New_Item =>
                       (Amount_Of_Attributes => Attributes_Amount,
                        Amount_Of_Skills => Skills_Amount,
                        Name =>
                          Tiny_String.To_Bounded_String
                            (Source => To_String(Source => RecruitName)),
                        Gender => Gender(1), Price => Price, Skills => Skills,
                        Attributes => Attributes, Inventory => Inventory,
                        Equipment => Equipment, Payment => Payment,
                        Home_Base => HomeBase, Faction => RecruitFaction));
               end;
            elsif NodeName = To_Unbounded_String("askedforeventsdate") then
               Sky_Bases(BaseIndex).Asked_For_Events :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("reputation") then
               Sky_Bases(BaseIndex).Reputation.Level :=
                 Natural'Value(Get_Attribute(ChildNode, "level"));
               if Get_Attribute(ChildNode, "progress") /= "" then
                  Sky_Bases(BaseIndex).Reputation.Experience :=
                    Natural'Value(Get_Attribute(ChildNode, "progress"));
               end if;
            elsif NodeName = To_Unbounded_String("missionsdate") then
               Sky_Bases(BaseIndex).Missions_Date :=
                 (Year => Natural'Value(Get_Attribute(ChildNode, "year")),
                  Month => Natural'Value(Get_Attribute(ChildNode, "month")),
                  Day => Natural'Value(Get_Attribute(ChildNode, "day")),
                  Hour => 0, Minutes => 0);
            elsif NodeName = To_Unbounded_String("mission") then
               declare
                  MType: Missions_Types;
                  TargetX, TargetY: Natural range 0 .. 1_024;
                  Time, Reward: Positive;
                  Target: Integer;
                  Index: Unbounded_String;
               begin
                  MType :=
                    Missions_Types'Val
                      (Integer'Value(Get_Attribute(ChildNode, "type")));
                  if MType = DELIVER or MType = DESTROY then
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
                     when DELIVER =>
                        Sky_Bases(BaseIndex).Missions.Append
                          (New_Item =>
                             (M_Type => DELIVER,
                              Item_Index =>
                                To_Bounded_String
                                  (Source => To_String(Source => Index)),
                              Time => Time, Target_X => TargetX,
                              Target_Y => TargetY, Reward => Reward,
                              Start_Base => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                     when DESTROY =>
                        Sky_Bases(BaseIndex).Missions.Append
                          (New_Item =>
                             (M_Type => DESTROY, Ship_Index => Index,
                              Time => Time, Target_X => TargetX,
                              Target_Y => TargetY, Reward => Reward,
                              Start_Base => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                     when PATROL =>
                        Sky_Bases(BaseIndex).Missions.Append
                          (New_Item =>
                             (M_Type => PATROL, Target => Target, Time => Time,
                              Target_X => TargetX, Target_Y => TargetY,
                              Reward => Reward, Start_Base => BaseIndex,
                              Finished => False, Multiplier => 1.0));
                     when EXPLORE =>
                        Sky_Bases(BaseIndex).Missions.Append
                          (New_Item =>
                             (M_Type => EXPLORE, Target => Target,
                              Time => Time, Target_X => TargetX,
                              Target_Y => TargetY, Reward => Reward,
                              Start_Base => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                     when PASSENGER =>
                        if Target > 91 then
                           Target := 91;
                        end if;
                        Sky_Bases(BaseIndex).Missions.Append
                          (New_Item =>
                             (M_Type => PASSENGER, Data => Target,
                              Time => Time, Target_X => TargetX,
                              Target_Y => TargetY, Reward => Reward,
                              Start_Base => BaseIndex, Finished => False,
                              Multiplier => 1.0));
                  end case;
               end;
            elsif NodeName = To_Unbounded_String("item") then
               declare
                  Durability: Items_Durability;
                  Amount, Price: Natural;
                  ProtoIndex: Bounded_String;
               begin
                  ProtoIndex :=
                    To_Bounded_String(Get_Attribute(ChildNode, "index"));
                  Durability :=
                    Items_Durability'Value
                      (Get_Attribute(ChildNode, "durability"));
                  Amount := Natural'Value(Get_Attribute(ChildNode, "amount"));
                  Price := Natural'Value(Get_Attribute(ChildNode, "price"));
                  Sky_Bases(BaseIndex).Cargo.Append
                    (New_Item =>
                       (Proto_Index => ProtoIndex, Amount => Amount,
                        Durability => Durability, Price => Price));
               end;
            end if;
         end loop Load_Base_Loop;
         Sky_Map(Sky_Bases(BaseIndex).Sky_X, Sky_Bases(BaseIndex).Sky_Y)
           .Base_Index :=
           BaseIndex;
      end loop Load_Bases_Loop;
   end Load_Bases;

end Bases.SaveLoad;
