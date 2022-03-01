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
            Save_Number(Value => SkyBase.Reputation.Level, Name => "level");
            if SkyBase.Reputation.Experience > 0 then
               Save_Number
                 (Value => SkyBase.Reputation.Experience, Name => "progress");
            end if;
         end if;
         if SkyBase.Visited.Year = 0 then
            goto Save_Cargo;
         end if;
         Sub_Node :=
           Create_Element(Doc => Save_Data, Tag_Name => "missionsdate");
         Sub_Node := Append_Child(N => Base_Node, New_Child => Sub_Node);
         Save_Number(Value => SkyBase.Missions_Date.Year, Name => "year");
         Save_Number(Value => SkyBase.Missions_Date.Month, Name => "month");
         Save_Number(Value => SkyBase.Missions_Date.Day, Name => "day");
         Save_Missions_Block :
         declare
            Mission_Node: DOM.Core.Element;
         begin
            Save_Missions_Loop :
            for Mission of SkyBase.Missions loop
               Mission_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "mission");
               Mission_Node :=
                 Append_Child(N => Base_Node, New_Child => Mission_Node);
               Save_Number
                 (Value => Missions_Types'Pos(Mission.M_Type), Name => "type",
                  Node => Mission_Node);
               Raw_Value :=
                 (case Mission.M_Type is
                    when DELIVER =>
                      To_Unbounded_String
                        (Source => To_String(Source => Mission.Item_Index)),
                    when PASSENGER =>
                      To_Unbounded_String
                        (Source => Integer'Image(Mission.Data)),
                    when DESTROY => Mission.Ship_Index,
                    when others =>
                      To_Unbounded_String
                        (Source => Integer'Image(Mission.Target)));
               Set_Attribute
                 (Elem => Mission_Node, Name => "target",
                  Value =>
                    To_String
                      (Source =>
                         Trim(Source => Raw_Value, Side => Ada.Strings.Left)));
               Save_Number
                 (Value => Mission.Time, Name => "time", Node => Mission_Node);
               Save_Number
                 (Value => Mission.Target_X, Name => "targetx",
                  Node => Mission_Node);
               Save_Number
                 (Value => Mission.Target_Y, Name => "targety",
                  Node => Mission_Node);
               Save_Number
                 (Value => Mission.Reward, Name => "reward",
                  Node => Mission_Node);
            end loop Save_Missions_Loop;
         end Save_Missions_Block;
         <<Save_Cargo>>
         if SkyBase.Cargo.Is_Empty then
            goto Save_Known;
         end if;
         Save_Base_Cargo_Block :
         declare
            Item_Node: DOM.Core.Element;
         begin
            Save_Cargo_Loop :
            for Item of SkyBase.Cargo loop
               Item_Node :=
                 Create_Element(Doc => Save_Data, Tag_Name => "item");
               Item_Node :=
                 Append_Child(N => Base_Node, New_Child => Item_Node);
               Set_Attribute
                 (Elem => Item_Node, Name => "index",
                  Value => To_String(Source => Item.Proto_Index));
               Save_Number
                 (Value => Item.Amount, Name => "amount", Node => Item_Node);
               Save_Number
                 (Value => Item.Durability, Name => "durability",
                  Node => Item_Node);
               Save_Number
                 (Value => Item.Price, Name => "price", Node => Item_Node);
            end loop Save_Cargo_Loop;
         end Save_Base_Cargo_Block;
         <<Save_Known>>
         if SkyBase.Known then
            Set_Attribute(Elem => Base_Node, Name => "known", Value => "Y");
         else
            Set_Attribute(Elem => Base_Node, Name => "known", Value => "N");
         end if;
         Set_Attribute
           (Elem => Base_Node, Name => "owner",
            Value => To_String(Source => SkyBase.Owner));
         Set_Attribute
           (Elem => Base_Node, Name => "size",
            Value => Bases_Size'Image(SkyBase.Size));
      end loop Save_Bases_Loop;
   end Save_Bases;

   procedure Load_Bases(Save_Data: not null Document) is
      use Tiny_String;

      Base_Recruits: Recruit_Container.Vector (Capacity => 30);
      Base_Missions: Mission_Container.Vector;
      Base_Cargo: BaseCargo_Container.Vector;
      Nodes_List, Base_Data: Node_List;
      Base_Index: Bases_Range;
      Base_Node_Name: Unbounded_String;
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
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "name"));
         Sky_Bases(Base_Index).Visited := (others => 0);
         Sky_Bases(Base_Index).Sky_X :=
           Integer'Value(Get_Attribute(Elem => Base_Node, Name => "x"));
         Sky_Bases(Base_Index).Sky_Y :=
           Integer'Value(Get_Attribute(Elem => Base_Node, Name => "y"));
         Sky_Bases(Base_Index).Base_Type :=
           To_Unbounded_String
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
         Sky_Bases(Base_Index).Owner :=
           Factions_Container.Key(Position => Factions_List.First);
         Sky_Bases(Base_Index).Cargo := Base_Cargo;
         Sky_Bases(Base_Index).Size :=
           Bases_Size'Value(Get_Attribute(Elem => Base_Node, Name => "size"));
         Sky_Bases(Base_Index).Owner :=
           To_Bounded_String
             (Source => Get_Attribute(Elem => Base_Node, Name => "owner"));
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
                  Recruit_Data: Node_List;
                  Recruit_Name, Recruit_Faction: Bounded_String;
                  Gender: String(1 .. 1);
                  Home_Base: Bases_Range;
                  Price, Payment: Positive;
                  Skills: Skills_Container.Vector (Capacity => Skills_Amount);
                  Index: SkillsData_Container.Extended_Index;
                  Inventory: TinyString_Formal_Container.Vector
                    (Capacity => Equipment_Array'Length);
                  Equipment: Equipment_Array;
                  Recruit_Node: Node;
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
                  Recruit_Data := Child_Nodes(N => Child_Node);
                  Load_Recruits_Loop :
                  for L in 0 .. Length(List => Recruit_Data) - 1 loop
                     Recruit_Node := Item(List => Recruit_Data, Index => L);
                     Base_Node_Name :=
                       To_Unbounded_String
                         (Source => Node_Name(N => Recruit_Node));
                     if Base_Node_Name =
                       To_Unbounded_String(Source => "skill") then
                        Index :=
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
                             (Index => Index, Level => Level,
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
                        TinyString_Formal_Container.Append
                          (Container => Inventory,
                           New_Item =>
                             To_Bounded_String
                               (Source =>
                                  Get_Attribute
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
                  Target: Integer;
                  Index: Unbounded_String;
               begin
                  M_Type :=
                    Missions_Types'Val
                      (Integer'Value
                         (Get_Attribute(Elem => Child_Node, Name => "type")));
                  if M_Type in DELIVER | DESTROY then
                     Index :=
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
                                To_Bounded_String
                                  (Source => To_String(Source => Index)),
                              Time => Time, Target_X => Target_X,
                              Target_Y => Target_Y, Reward => Reward,
                              Start_Base => Base_Index, Finished => False,
                              Multiplier => 1.0));
                     when DESTROY =>
                        Sky_Bases(Base_Index).Missions.Append
                          (New_Item =>
                             (M_Type => DESTROY, Ship_Index => Index,
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
                  Proto_Index: Bounded_String;
               begin
                  Proto_Index :=
                    To_Bounded_String
                      (Source =>
                         Get_Attribute(Elem => Child_Node, Name => "index"));
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
                  Sky_Bases(Base_Index).Cargo.Append
                    (New_Item =>
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
