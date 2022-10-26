--    Copyright 2018-2022 Bartek thindil Jasicki
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

with Interfaces.C.Strings;
with Utils; use Utils;

package body Factions is

   procedure Load_Factions(File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Tiny_String;

      Temp_Record: Faction_Record;
      type Faction_Nim_Data is record
         Name: chars_ptr;
         Member_Name: chars_ptr;
         Plural_Member_Name: chars_ptr;
         Spawn_Chance: Natural := 0;
         Population: Attributes_Array;
         Names_Type: Integer;
         Description: chars_ptr;
         Healing_Tools: chars_ptr;
         Healing_Skill: SkillsData_Container.Extended_Index;
         Base_Icon: Integer;
         Weapon_Skill: SkillsData_Container.Extended_Index;
      end record;
      type Faction_Nim_Relation is array(0 .. 3) of Integer;
      type Career_Nim_Record is record
         Ship_Index: Positive;
         Player_Index: chars_ptr;
         Description: chars_ptr;
         Name: chars_ptr;
      end record;
      Faction_Index: Tiny_String.Bounded_String;
      Temp_Nim_Record: Faction_Nim_Data;
      Index: Positive := 1;
      Index2: Natural := 0;
      Faction_Data: Unbounded_String;
      Faction_Relation: Faction_Nim_Relation;
      Faction_Career: Career_Nim_Record;
      Faction_Base: Positive;
      procedure Load_Ada_Factions(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaFactions";
      function Get_Ada_Faction
        (Faction_Index: Integer; Ada_Faction: out Faction_Nim_Data)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFaction";
      function Get_Ada_Faction_Data
        (Faction_Index: chars_ptr; Item_Index: Integer; Data_Type: chars_ptr)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionData";
      function Get_Ada_Faction_Relation
        (Faction_Index: chars_ptr; Relation_Index: Integer;
         Relation: out Faction_Nim_Relation) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionRelation";
      function Get_Ada_Faction_Career
        (Faction_Index: chars_ptr; Career_Index: Integer;
         Career: out Career_Nim_Record) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionCareer";
      function Get_Ada_Faction_Base
        (Faction_Index: chars_ptr; Base_Index: Integer; Base: out Positive)
         return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFactionBase";
   begin
      Load_Ada_Factions(Name => New_String(Str => File_Name));
      Load_Factions_Data_Loop :
      loop
         Faction_Index :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item =>
                     Get_Ada_Faction
                       (Faction_Index => Index,
                        Ada_Faction => Temp_Nim_Record)));
         exit Load_Factions_Data_Loop when Length(Source => Faction_Index) = 0;
         Temp_Record.Name :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value(Item => Temp_Nim_Record.Name));
         Temp_Record.Member_Name :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Member_Name));
         Temp_Record.Plural_Member_Name :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Plural_Member_Name));
         Temp_Record.Spawn_Chance := Temp_Nim_Record.Spawn_Chance;
         Temp_Record.Population := Temp_Nim_Record.Population;
         Temp_Record.Names_Type := Names_Types'Val(Temp_Nim_Record.Names_Type);
         Temp_Record.Description :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Description));
         Temp_Record.Healing_Tools :=
           To_Bounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item => Temp_Nim_Record.Healing_Tools));
         Temp_Record.Healing_Skill := Temp_Nim_Record.Healing_Skill;
         Temp_Record.Base_Icon :=
           Wide_Character'Val(Temp_Nim_Record.Base_Icon);
         Temp_Record.Weapon_Skill := Temp_Nim_Record.Weapon_Skill;
         Index2 := 0;
         Temp_Record.Food_Types.Clear;
         Load_Faction_Food_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "foodType")))));
            exit Load_Faction_Food_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Food_Types.Append
              (New_Item =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Food_Loop;
         Index2 := 0;
         Temp_Record.Drinks_Types.Clear;
         Load_Faction_Drinks_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "drinkType")))));
            exit Load_Faction_Drinks_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Drinks_Types.Append
              (New_Item =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Drinks_Loop;
         Index2 := 0;
         Temp_Record.Flags.Clear;
         Load_Faction_Flags_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Data
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Item_Index => Index2,
                            Data_Type => New_String(Str => "flag")))));
            exit Load_Faction_Flags_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Flags.Append(New_Item => Faction_Data);
            Index2 := Index2 + 1;
         end loop Load_Faction_Flags_Loop;
         Index2 := 1;
         Temp_Record.Relations.Clear;
         Load_Faction_Relation_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Relation
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Relation_Index => Index2,
                            Relation => Faction_Relation))));
            exit Load_Faction_Relation_Loop when Length
                (Source => Faction_Data) =
              0;
            Temp_Record.Relations.Include
              (Key =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)),
               New_Item =>
                 (Reputation =>
                    (Min => Faction_Relation(0), Max => Faction_Relation(1)),
                  Friendly =>
                    (if Faction_Relation(2) = 0 then False else True)));
            Index2 := Index2 + 1;
         end loop Load_Faction_Relation_Loop;
         Index2 := 1;
         Temp_Record.Careers.Clear;
         Load_Faction_Career_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Career
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Career_Index => Index2,
                            Career => Faction_Career))));
            exit Load_Faction_Career_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Careers.Include
              (Key => Faction_Data,
               New_Item =>
                 (Ship_Index => Faction_Career.Ship_Index,
                  Player_Index =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Player_Index)),
                  Description =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Description)),
                  Name =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value
                           (Item => Faction_Career.Name))));
            Index2 := Index2 + 1;
         end loop Load_Faction_Career_Loop;
         Index2 := 1;
         Temp_Record.Bases_Types.Clear;
         Load_Faction_Bases_Loop :
         loop
            Faction_Data :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Faction_Base
                           (Faction_Index =>
                              New_String
                                (Str => To_String(Source => Faction_Index)),
                            Base_Index => Index2, Base => Faction_Base))));
            exit Load_Faction_Bases_Loop when Length(Source => Faction_Data) =
              0;
            Temp_Record.Bases_Types.Include
              (Key =>
                 To_Bounded_String
                   (Source => To_String(Source => Faction_Data)),
               New_Item => Faction_Base);
            Index2 := Index2 + 1;
         end loop Load_Faction_Bases_Loop;
         Factions_List.Include(Key => Faction_Index, New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Factions_Data_Loop;
   end Load_Factions;

   function Get_Reputation
     (Source_Faction, Target_Faction: Tiny_String.Bounded_String)
      return Integer is
   begin
      return
        (if
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             .Max =
           0
         then
           Factions_List(Source_Faction).Relations(Target_Faction).Reputation
             .Min
         else Get_Random
             (Min =>
                Factions_List(Source_Faction).Relations(Target_Faction)
                  .Reputation
                  .Min,
              Max =>
                Factions_List(Source_Faction).Relations(Target_Faction)
                  .Reputation
                  .Max));
   end Get_Reputation;

   function Is_Friendly
     (Source_Faction, Target_Faction: Tiny_String.Bounded_String)
      return Boolean is
   begin
      return Factions_List(Source_Faction).Relations(Target_Faction).Friendly;
   end Is_Friendly;

   function Get_Random_Faction return Tiny_String.Bounded_String is
      Faction_Index,
      Current_Index: Positive range 1 .. Positive(Factions_List.Length);
   begin
      Faction_Index :=
        Get_Random(Min => 1, Max => Positive(Factions_List.Length));
      Current_Index := 1;
      Get_Random_Faction_Loop :
      for J in Factions_List.Iterate loop
         if Current_Index = Faction_Index then
            return Factions_Container.Key(Position => J);
         end if;
         Current_Index := Current_Index + 1;
      end loop Get_Random_Faction_Loop;
      return Tiny_String.Null_Bounded_String;
   end Get_Random_Faction;

end Factions;
