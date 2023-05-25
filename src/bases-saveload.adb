--    Copyright 2017-2023 Bartek thindil Jasicki
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
with Factions;
with Maps; use Maps;

package body Bases.SaveLoad is

   procedure Load_Bases(Save_Data: not null Document) is
      use Factions;
      use Tiny_String;

      Base_Recruits: Recruit_Container.Vector (Capacity => 5);
      --## rule off IMPROPER_INITIALIZATION
      Base_Missions: Mission_Container.Vector;
      Nodes_List, Base_Data: Node_List;
      --## rule on IMPROPER_INITIALIZATION
      Base_Index: Bases_Range := 1;
      Base_Node_Name: Unbounded_String := Null_Unbounded_String;
      Base_Node, Child_Node: Node;
   begin
      Recruit_Container.Clear(Container => Base_Recruits);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Save_Data, Tag_Name => "base");
      Load_Bases_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Base_Index := I + 1;
         Base_Node := Item(List => Nodes_List, Index => I);
         Sky_Bases(Base_Index).Name :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "name"));
         Sky_Bases(Base_Index).Visited := (others => 0);
         Sky_Bases(Base_Index).Sky_X :=
           Integer'Value(Get_Attribute(Elem => Base_Node, Name => "x"));
         Sky_Bases(Base_Index).Sky_Y :=
           Integer'Value(Get_Attribute(Elem => Base_Node, Name => "y"));
         Sky_Bases(Base_Index).Base_Type :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "type"));
         Sky_Bases(Base_Index).Population :=
           Integer'Value
             (Get_Attribute(Elem => Base_Node, Name => "population"));
         Sky_Bases(Base_Index).Recruit_Date := (others => 0);
         Recruit_Container.Assign
           (Target => Sky_Bases(Base_Index).Recruits, Source => Base_Recruits);
         Sky_Bases(Base_Index).Known := False;
         Sky_Bases(Base_Index).Asked_For_Bases := False;
         Sky_Bases(Base_Index).Asked_For_Events := (others => 0);
         Sky_Bases(Base_Index).Reputation := (Level => 0, Experience => 0);
         Sky_Bases(Base_Index).Missions_Date := (others => 0);
         Sky_Bases(Base_Index).Missions := Base_Missions;
         Sky_Bases(Base_Index).Owner := Get_Faction_Index(Number => 1);
         Sky_Bases(Base_Index).Size :=
           Bases_Size'Value(Get_Attribute(Elem => Base_Node, Name => "size"));
         Sky_Bases(Base_Index).Owner :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "owner"));
         Get_Base_Owner(Base_Index => Base_Index);
         if Get_Attribute(Elem => Base_Node, Name => "known") = "Y" then
            Sky_Bases(Base_Index).Known := True;
         end if;
         if Get_Attribute(Elem => Base_Node, Name => "askedforbases") =
           "Y" then
            Sky_Bases(Base_Index).Asked_For_Bases := True;
         end if;
         Base_Data := Child_Nodes(N => Base_Node);
         Load_Base_Loop :
         for J in 0 .. Length(List => Base_Data) - 1 loop
            Child_Node := Item(List => Base_Data, Index => J);
            Base_Node_Name :=
              To_Unbounded_String(Source => Node_Name(N => Child_Node));
            if Base_Node_Name =
              To_Unbounded_String(Source => "visiteddate") then
               Sky_Bases(Base_Index).Visited :=
                 (Year =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "year")),
                  Month =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "month")),
                  Day =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "day")),
                  Hour =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "hour")),
                  Minutes =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "minutes")));
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "recruitdate") then
               Sky_Bases(Base_Index).Recruit_Date :=
                 (Year =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "year")),
                  Month =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "month")),
                  Day =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "day")),
                  Hour => 0, Minutes => 0);
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "recruit") then
               Load_Recruits_Block :
               declare
                  Recruit_Save_Data: Node_List;
                  Recruit_Name: Bounded_String;
                  Recruit_Faction: Bounded_String := Null_Bounded_String;
                  Gender: String(1 .. 1);
                  Home_Base: Bases_Range := 1;
                  Price, Payment: Positive;
                  Skill_Index: SkillsData_Container.Extended_Index := 0;
                  --## rule off IMPROPER_INITIALIZATION
                  Skills: Skills_Container.Vector (Capacity => Skills_Amount);
                  Inventory: Positive_Formal_Container.Vector (Capacity => 7);
                  --## rule on IMPROPER_INITIALIZATION
                  Equipment: Equipment_Array;
                  Recruit_Node: Node;
                  Level: Skill_Range := 0;
                  Attributes: Mob_Attributes
                    (1 ..
                         Positive
                           (AttributesData_Container.Length
                              (Container => Attributes_List)));
                  Attribute_Index: Positive := 1;
               begin
                  --## rule off IMPROPER_INITIALIZATION
                  Skills_Container.Clear(Container => Skills);
                  Attributes := (others => <>);
                  Positive_Formal_Container.Clear(Container => Inventory);
                  --## rule on IMPROPER_INITIALIZATION
                  Equipment := (others => 0);
                  Recruit_Name :=
                    To_Bounded_String
                      (Source =>
                         Get_Attribute(Elem => Child_Node, Name => "name"));
                  Gender :=
                    Get_Attribute(Elem => Child_Node, Name => "gender");
                  Price :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "price"));
                  Payment := 20;
                  Recruit_Save_Data := Child_Nodes(N => Child_Node);
                  Load_Recruits_Loop :
                  for L in 0 .. Length(List => Recruit_Save_Data) - 1 loop
                     Recruit_Node :=
                       Item(List => Recruit_Save_Data, Index => L);
                     Base_Node_Name :=
                       To_Unbounded_String
                         (Source => Node_Name(N => Recruit_Node));
                     if Base_Node_Name =
                       To_Unbounded_String(Source => "skill") then
                        Skill_Index :=
                          SkillsData_Container.Extended_Index'Value
                            (Get_Attribute
                               (Elem => Recruit_Node, Name => "index"));
                        Level :=
                          Skill_Range'Value
                            (Get_Attribute
                               (Elem => Recruit_Node, Name => "level"));
                        Skills_Container.Append
                          (Container => Skills,
                           New_Item =>
                             (Index => Skill_Index, Level => Level,
                              Experience => 0));
                     elsif Base_Node_Name =
                       To_Unbounded_String(Source => "attribute") then
                        Level :=
                          Natural'Value
                            (Get_Attribute
                               (Elem => Recruit_Node, Name => "level"));
                        Attributes(Attribute_Index) :=
                          (Level => Level, Experience => 0);
                        Attribute_Index := Attribute_Index + 1;
                     elsif Base_Node_Name =
                       To_Unbounded_String(Source => "item") then
                        Positive_Formal_Container.Append
                          (Container => Inventory,
                           New_Item =>
                             Positive'Value
                               (Get_Attribute
                                  (Elem => Recruit_Node, Name => "index")));
                     elsif Base_Node_Name =
                       To_Unbounded_String(Source => "equipment") then
                        Equipment
                          (Equipment_Locations'Val
                             (Natural'Value
                                (Get_Attribute
                                   (Elem => Recruit_Node, Name => "slot")) -
                              1)) :=
                          Natural'Value
                            (Get_Attribute
                               (Elem => Recruit_Node, Name => "index"));
                     end if;
                     if Get_Attribute(Elem => Child_Node, Name => "payment") /=
                       "" then
                        Payment :=
                          Natural'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "payment"));
                     end if;
                     if Get_Attribute
                         (Elem => Child_Node, Name => "homebase") /=
                       "" then
                        Home_Base :=
                          Bases_Range'Value
                            (Get_Attribute
                               (Elem => Child_Node, Name => "homebase"));
                     end if;
                     if Get_Attribute(Elem => Child_Node, Name => "faction") /=
                       "" then
                        Recruit_Faction :=
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Child_Node, Name => "faction"));
                     end if;
                  end loop Load_Recruits_Loop;
                  Recruit_Container.Append
                    (Container => Sky_Bases(Base_Index).Recruits,
                     New_Item =>
                       (Amount_Of_Attributes => Attributes_Amount,
                        Amount_Of_Skills => Skills_Amount,
                        Name => Recruit_Name, Gender => Gender(1),
                        Price => Price, Skills => Skills,
                        Attributes => Attributes, Inventory => Inventory,
                        Equipment => Equipment, Payment => Payment,
                        Home_Base => Home_Base, Faction => Recruit_Faction));
               end Load_Recruits_Block;
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "askedforeventsdate") then
               Sky_Bases(Base_Index).Asked_For_Events :=
                 (Year =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "year")),
                  Month =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "month")),
                  Day =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "day")),
                  Hour => 0, Minutes => 0);
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "reputation") then
               Sky_Bases(Base_Index).Reputation.Level :=
                 Natural'Value
                   (Get_Attribute(Elem => Child_Node, Name => "level"));
               if Get_Attribute(Elem => Child_Node, Name => "progress") /=
                 "" then
                  Sky_Bases(Base_Index).Reputation.Experience :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "progress"));
               end if;
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "missionsdate") then
               Sky_Bases(Base_Index).Missions_Date :=
                 (Year =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "year")),
                  Month =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "month")),
                  Day =>
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "day")),
                  Hour => 0, Minutes => 0);
            elsif Base_Node_Name =
              To_Unbounded_String(Source => "mission") then
               Load_Missions_Block :
               declare
                  M_Type: Missions_Types;
                  Target_X, Target_Y: Natural range 0 .. 1_024;
                  Time, Reward: Positive;
                  Target: Integer := 0;
                  Target_Index: Unbounded_String := Null_Unbounded_String;
               begin
                  M_Type :=
                    Missions_Types'Val
                      (Integer'Value
                         (Get_Attribute(Elem => Child_Node, Name => "type")));
                  if M_Type in DELIVER | DESTROY then
                     Target_Index :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute
                              (Elem => Child_Node, Name => "target"));
                  else
                     Target :=
                       Integer'Value
                         (Get_Attribute(Elem => Child_Node, Name => "target"));
                  end if;
                  Time :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "time"));
                  Target_X :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "targetx"));
                  Target_Y :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "targety"));
                  Reward :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "reward"));
                  case M_Type is
                     when DELIVER =>
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => DELIVER,
                              Item_Index =>
                                Positive'Value
                                  (To_String(Source => Target_Index)),
                              Time => Time, Target_X => Target_X,
                              Target_Y => Target_Y, Reward => Reward,
                              Start_Base => Base_Index, Finished => False,
                              Multiplier => 1.0));
                     when DESTROY =>
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => DESTROY,
                              Ship_Index =>
                                Positive'Value
                                  (To_String(Source => Target_Index)),
                              Time => Time, Target_X => Target_X,
                              Target_Y => Target_Y, Reward => Reward,
                              Start_Base => Base_Index, Finished => False,
                              Multiplier => 1.0));
                     when PATROL =>
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => PATROL, Target => Target, Time => Time,
                              Target_X => Target_X, Target_Y => Target_Y,
                              Reward => Reward, Start_Base => Base_Index,
                              Finished => False, Multiplier => 1.0));
                     when EXPLORE =>
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => EXPLORE, Target => Target,
                              Time => Time, Target_X => Target_X,
                              Target_Y => Target_Y, Reward => Reward,
                              Start_Base => Base_Index, Finished => False,
                              Multiplier => 1.0));
                     when PASSENGER =>
                        if Target > 91 then
                           Target := 91;
                        end if;
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => PASSENGER, Data => Target,
                              Time => Time, Target_X => Target_X,
                              Target_Y => Target_Y, Reward => Reward,
                              Start_Base => Base_Index, Finished => False,
                              Multiplier => 1.0));
                  end case;
               end Load_Missions_Block;
            elsif Base_Node_Name = To_Unbounded_String(Source => "item") then
               Load_Base_Cargo_Block :
               declare
                  Durability: Items_Durability;
                  Amount, Price: Natural;
                  Proto_Index: Natural;
               begin
                  Proto_Index :=
                    Positive'Value
                      (Get_Attribute(Elem => Child_Node, Name => "index"));
                  Durability :=
                    Items_Durability'Value
                      (Get_Attribute
                         (Elem => Child_Node, Name => "durability"));
                  Amount :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "amount"));
                  Price :=
                    Natural'Value
                      (Get_Attribute(Elem => Child_Node, Name => "price"));
                  BaseCargo_Container.Append
                    (Container => Sky_Bases(Base_Index).Cargo,
                     New_Item =>
                       (Proto_Index => Proto_Index, Amount => Amount,
                        Durability => Durability, Price => Price));
               end Load_Base_Cargo_Block;
            end if;
         end loop Load_Base_Loop;
         Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
           .Base_Index :=
           Base_Index;
      end loop Load_Bases_Loop;
   end Load_Bases;

end Bases.SaveLoad;
