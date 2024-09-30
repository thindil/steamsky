--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Trades; use Trades;

package body Bases is

   function Generate_Base_Name
     (Faction_Index: Tiny_String.Bounded_String)
      return Tiny_String.Bounded_String is
      use Tiny_String;
      function Generate_Ada_Base_Name(F_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "generateAdaBaseName";
   begin
      return
        To_Bounded_String
          (Source =>
             Value
               (Item =>
                  Generate_Ada_Base_Name
                    (F_Index =>
                       New_String
                         (Str => To_String(Source => Faction_Index)))));
   end Generate_Base_Name;

   function Recruit_To_Nim(Recruit: Recruit_Data) return Nim_Recruit_Data is
      use Tiny_String;
      Nim_Recruit: Nim_Recruit_Data :=
        (Attributes => (others => (others => 0)),
         Skills => (others => (others => 0)),
         Name => New_String(Str => To_String(Source => Recruit.Name)),
         Gender => Recruit.Gender, Equipment => (others => 0),
         Payment => Recruit.Payment, Inventory => (others => 0),
         Home_Base => Recruit.Home_Base, Price => Recruit.Price,
         Faction => New_String(Str => To_String(Source => Recruit.Faction)));
   begin
      Convert_Inventory_Loop :
      for I in Nim_Recruit.Inventory'Range loop
         exit Convert_Inventory_Loop when I =
           Positive_Formal_Container.Last_Index
             (Container => Recruit.Inventory);
         Nim_Recruit.Inventory(I) :=
           Positive_Formal_Container.Element
             (Container => Recruit.Inventory, Index => I + 1);
      end loop Convert_Inventory_Loop;
      Convert_Equipment_Loop :
      for I in Recruit.Equipment'Range loop
         Nim_Recruit.Equipment(Equipment_Locations'Pos(I)) :=
           Recruit.Equipment(I);
      end loop Convert_Equipment_Loop;
      Convert_Atrributes_Loop :
      for I in Recruit.Attributes'Range loop
         Nim_Recruit.Attributes(I, 1) := Recruit.Attributes(I).Level;
         Nim_Recruit.Attributes(I, 2) := Recruit.Attributes(I).Experience;
      end loop Convert_Atrributes_Loop;
      Convert_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Recruit.Skills) ..
          Skills_Container.Last_Index(Container => Recruit.Skills) loop
         Convert_Skill_Block :
         declare
            Skill: constant Skill_Info :=
              Skills_Container.Element
                (Container => Recruit.Skills, Index => I);
         begin
            Nim_Recruit.Skills(Integer(I), 1) := Integer(Skill.Index);
            Nim_Recruit.Skills(Integer(I), 2) := Skill.Level;
            Nim_Recruit.Skills(Integer(I), 3) := Skill.Experience;
         end Convert_Skill_Block;
      end loop Convert_Skills_Loop;
      return Nim_Recruit;
   end Recruit_To_Nim;

   procedure Recruit_From_Nim
     (Recruit: Nim_Recruit_Data; Ada_Recruit: in out Recruit_Data) is
      use Tiny_String;
   begin
      Ada_Recruit.Name :=
        To_Bounded_String(Source => Value(Item => Recruit.Name));
      Ada_Recruit.Gender := Recruit.Gender;
      Ada_Recruit.Payment := Recruit.Payment;
      Ada_Recruit.Price := Recruit.Price;
      Convert_Inventory_Loop :
      for Item of Recruit.Inventory loop
         exit Convert_Inventory_Loop when Item = 0;
         Positive_Formal_Container.Append
           (Container => Ada_Recruit.Inventory, New_Item => Item);
      end loop Convert_Inventory_Loop;
      Convert_Equipment_Loop :
      for I in Recruit.Equipment'Range loop
         Ada_Recruit.Equipment(Equipment_Locations'Val(I)) :=
           Recruit.Equipment(I) + 1;
      end loop Convert_Equipment_Loop;
      Convert_Atrributes_Loop :
      for I in Recruit.Attributes'Range(1) loop
         exit Convert_Atrributes_Loop when I > Attributes_Amount;
         Ada_Recruit.Attributes(I).Level := Recruit.Attributes(I, 1);
         Ada_Recruit.Attributes(I).Experience := Recruit.Attributes(I, 2);
      end loop Convert_Atrributes_Loop;
      Skills_Container.Clear(Container => Ada_Recruit.Skills);
      Convert_Skills_Loop :
      for I in Recruit.Skills'Range(1) loop
         exit Convert_Skills_Loop when Recruit.Skills(I, 1) = 0;
         Skills_Container.Append
           (Container => Ada_Recruit.Skills,
            New_Item =>
              Skill_Info'
                (Index => Skills_Amount_Range(Recruit.Skills(I, 1)),
                 Level => Recruit.Skills(I, 2),
                 Experience => Recruit.Skills(I, 3)));
      end loop Convert_Skills_Loop;
      Ada_Recruit.Faction :=
        To_Bounded_String(Source => Value(Item => Recruit.Faction));
      Ada_Recruit.Home_Base := Recruit.Home_Base;
   end Recruit_From_Nim;

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Base_Cargo is record
      Proto_Index: Natural;
      Amount: Natural;
      Durability: Items_Durability;
      Price: Natural := 0;
   end record;
   type Nim_Cargo_Array is array(0 .. 127) of Nim_Base_Cargo;
   --## rule on TYPE_INITIAL_VALUES

   procedure Set_Base_In_Nim(Base_Index: Bases_Range) is
      procedure Get_Base_Cargo(B_I: Natural) is
         procedure Get_Ada_Base_Cargo
           (B_Index: Integer; Cargo: Nim_Cargo_Array) with
            Import => True,
            Convention => C,
            External_Name => "getAdaBaseCargo";
         function Cargo_To_Nim
           (Cargo: BaseCargo_Container.Vector) return Nim_Cargo_Array is
            Nim_Cargo: Nim_Cargo_Array :=
              (others =>
                 (Proto_Index => 0, Amount => 1, Durability => 0, Price => 0));
         begin
            Fill_Nim_Array_Loop :
            for I in
              BaseCargo_Container.First_Index(Container => Cargo) ..
                BaseCargo_Container.Last_Index(Container => Cargo) loop
               Set_Item_Block :
               declare
                  Item: constant Base_Cargo :=
                    BaseCargo_Container.Element
                      (Container => Cargo, Index => I);
               begin
                  Nim_Cargo(I - 1) :=
                    (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
                     Durability => Item.Durability, Price => Item.Price);
               end Set_Item_Block;
            end loop Fill_Nim_Array_Loop;
            return Nim_Cargo;
         end Cargo_To_Nim;
      begin
         Get_Ada_Base_Cargo
           (B_Index => B_I,
            Cargo =>
              Cargo_To_Nim
                (Cargo =>
                   (if B_I > 0 then Sky_Bases(B_I).Cargo else Trader_Cargo)));
      end Get_Base_Cargo;
      procedure Get_Ada_Base_Name(B_Index: Integer; B_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseName";
      procedure Get_Ada_Base_Asked_For_Bases
        (B_Index, Asked_For_Bases: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseAskedForBases";
      procedure Get_Ada_Base_Size(B_Index, Size: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseSize";
      procedure Get_Ada_Base_Known(B_Index, Known: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseKnown";
      procedure Get_Base_Owner(B_Index: Bases_Range) is
         procedure Get_Ada_Base_Owner(Ba_Index: Integer; Owner: chars_ptr) with
            Import => True,
            Convention => C,
            External_Name => "getAdaBaseOwner";
      begin
         Get_Ada_Base_Owner
           (Ba_Index => B_Index,
            Owner =>
              New_String
                (Str =>
                   Tiny_String.To_String(Source => Sky_Bases(B_Index).Owner)));
      end Get_Base_Owner;
      procedure Get_Base_Type
        (B_Index: Bases_Range; Base_Type: Tiny_String.Bounded_String) is
         procedure Get_Ada_Base_Type(Ba_Index: Integer; B_Type: chars_ptr) with
            Import => True,
            Convention => C,
            External_Name => "getAdaBaseType";
      begin
         Get_Ada_Base_Type
           (Ba_Index => B_Index,
            B_Type =>
              New_String(Str => Tiny_String.To_String(Source => Base_Type)));
      end Get_Base_Type;
      procedure Get_Ada_Base_Date
        (B_Index: Bases_Range;
         Year, Month, Day, Hour, Minutes, Date_Type: Natural) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseDate";
      procedure Get_Ada_Base_Population
        (B_Index: Bases_Range; Population: Natural) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBasePopulation";
      procedure Get_Ada_Recruits
        (Recruits: Recruit_Container.Vector; B_Index: Bases_Range) is

         --## rule off TYPE_INITIAL_VALUES
         type Nim_Recruits_Array is array(1 .. 20) of Nim_Recruit_Data;
         --## rule on TYPE_INITIAL_VALUES
         --## rule off IMPROPER_INITIALIZATION
         Nim_Recruits: Nim_Recruits_Array;
         --## rule on IMPROPER_INITIALIZATION
         procedure Get_Ada_Base_Recruits
           (N_Recruits: Nim_Recruits_Array; B_I: Integer) with
            Import => True,
            Convention => C,
            External_Name => "getAdaRecruits";
      begin
         Convert_Recruits_Loop :
         for I in
           Recruit_Container.First_Index(Container => Recruits) ..
             Recruit_Container.Last_Index(Container => Recruits) loop
            Nim_Recruits(I) :=
              Recruit_To_Nim
                (Recruit =>
                   Recruit_Container.Element
                     (Container => Recruits, Index => I));
         end loop Convert_Recruits_Loop;
         Get_Ada_Base_Recruits(N_Recruits => Nim_Recruits, B_I => B_Index);
      end Get_Ada_Recruits;
      procedure Get_Base_Reputation(B_Index: Bases_Range) is
         procedure Get_Ada_Base_Reputation
           (B_I, Level, Experience: Integer) with
            Import => True,
            Convention => C,
            External_Name => "getAdaBaseReputation";
      begin
         Get_Ada_Base_Reputation
           (B_I => B_Index,
            Level => Sky_Bases(B_Index).Reputation.Level,
            Experience => Sky_Bases(B_Index).Reputation.Experience);
      end Get_Base_Reputation;
      procedure Get_Ada_Base_Location
         (B_Index: Bases_Range; X: Map_X_Range; Y: Map_Y_Range) with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseLocation";
   begin
      Get_Ada_Base_Name
        (B_Index => Base_Index,
         B_Name =>
           New_String
             (Str =>
                Tiny_String.To_String(Source => Sky_Bases(Base_Index).Name)));
      Get_Ada_Base_Date
        (B_Index => Base_Index, Year => Sky_Bases(Base_Index).Visited.Year,
         Month => Sky_Bases(Base_Index).Visited.Month,
         Day => Sky_Bases(Base_Index).Visited.Day,
         Hour => Sky_Bases(Base_Index).Visited.Hour,
         Minutes => Sky_Bases(Base_Index).Visited.Minutes, Date_Type => 0);
      Get_Ada_Base_Location
        (B_Index => Base_Index, X => Sky_Bases(Base_Index).Sky_X,
         Y => Sky_Bases(Base_Index).Sky_Y);
      Get_Base_Type
        (B_Index => Base_Index, Base_Type => Sky_Bases(Base_Index).Base_Type);
      Get_Ada_Base_Population
        (B_Index => Base_Index,
         Population => Sky_Bases(Base_Index).Population);
      Get_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Recruit_Date.Year,
         Month => Sky_Bases(Base_Index).Recruit_Date.Month,
         Day => Sky_Bases(Base_Index).Recruit_Date.Day,
         Hour => Sky_Bases(Base_Index).Recruit_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Recruit_Date.Minutes,
         Date_Type => 2);
      Get_Ada_Recruits
        (Recruits => Sky_Bases(Base_Index).Recruits, B_Index => Base_Index);
      Get_Ada_Base_Known
        (B_Index => Base_Index,
         Known => (if Sky_Bases(Base_Index).Known then 1 else 0));
      Get_Ada_Base_Asked_For_Bases
        (B_Index => Base_Index,
         Asked_For_Bases =>
           (if Sky_Bases(Base_Index).Asked_For_Bases then 1 else 0));
      Get_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Asked_For_Events.Year,
         Month => Sky_Bases(Base_Index).Asked_For_Events.Month,
         Day => Sky_Bases(Base_Index).Asked_For_Events.Day,
         Hour => Sky_Bases(Base_Index).Asked_For_Events.Hour,
         Minutes => Sky_Bases(Base_Index).Asked_For_Events.Minutes,
         Date_Type => 3);
      Get_Base_Reputation(B_Index => Base_Index);
      Get_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Missions_Date.Year,
         Month => Sky_Bases(Base_Index).Missions_Date.Month,
         Day => Sky_Bases(Base_Index).Missions_Date.Day,
         Hour => Sky_Bases(Base_Index).Missions_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Missions_Date.Minutes,
         Date_Type => 1);
      Get_Missions(Base_Index => Base_Index);
      Get_Base_Owner(B_Index => Base_Index);
      Get_Base_Cargo(B_I => Base_Index);
      Get_Ada_Base_Size
        (B_Index => Base_Index,
         Size => Bases_Size'Pos(Sky_Bases(Base_Index).Size));
   end Set_Base_In_Nim;

   procedure Get_Base_From_Nim(Base_Index: Bases_Range) is
      use Tiny_String;
      Name: chars_ptr;
      Known: Integer;
      procedure Set_Base_Cargo(B_Index: Natural) is
         Nim_Cargo: Nim_Cargo_Array;
         procedure Set_Ada_Base_Cargo
           (B_I: Integer; Cargo: out Nim_Cargo_Array) with
            Import => True,
            Convention => C,
            External_Name => "setAdaBaseCargo";
         procedure Cargo_From_Nim(Cargo: Nim_Cargo_Array; B_I: Natural) is
         --## rule off IMPROPER_INITIALIZATION
            Ada_Cargo: BaseCargo_Container.Vector (Capacity => 32);
         --## rule on IMPROPER_INITIALIZATION
         begin
            Fill_Ada_Inventory_Loop :
            for Item of Cargo loop
               exit Fill_Ada_Inventory_Loop when Item.Proto_Index = 0;
               BaseCargo_Container.Append
                 (Container => Ada_Cargo,
                  New_Item =>
                    (Proto_Index => Item.Proto_Index, Amount => Item.Amount,
                     Durability => Item.Durability, Price => Item.Price));
            end loop Fill_Ada_Inventory_Loop;
            if B_I > 0 then
               BaseCargo_Container.Assign
                 (Target => Sky_Bases(B_I).Cargo, Source => Ada_Cargo);
            else
               BaseCargo_Container.Assign
                 (Target => Trader_Cargo, Source => Ada_Cargo);
            end if;
         end Cargo_From_Nim;

      begin
         Set_Ada_Base_Cargo(B_I => B_Index, Cargo => Nim_Cargo);
         Cargo_From_Nim(Cargo => Nim_Cargo, B_I => B_Index);
      end Set_Base_Cargo;
      procedure Set_Ada_Base_Name(B_Index: Integer; B_Name: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseName";
      procedure Set_Ada_Base_Location
        (B_Index: Bases_Range; X: out Map_X_Range; Y: out Map_Y_Range) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseLocation";
      procedure Set_Ada_Base_Type(B_Index: Integer; B_Type: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseType";
      procedure Set_Ada_Base_Asked_For_Bases
        (B_Index: Integer; Asked_For_Bases: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseAskedForBases";
      procedure Set_Ada_Base_Owner
        (B_Index: Integer; B_Owner: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseOwner";
      procedure Set_Ada_Base_Size(B_Index: Integer; Size: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseSize";
      procedure Set_Base_Population(B_Index: Bases_Range) is
         procedure Set_Ada_Base_Population
           (B_Ind: Integer; Population: out Integer) with
            Import => True,
            Convention => C,
            External_Name => "setAdaBasePopulation";
      begin
         Set_Ada_Base_Population
           (B_Ind => B_Index, Population => Sky_Bases(B_Index).Population);
      end Set_Base_Population;
      procedure Set_Ada_Base_Date
        (B_Index: Bases_Range; Date_Type: Natural;
         Year, Month, Day, Hour, Minutes: out Natural) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseDate";
      procedure Set_Ada_Base_Known(B_Index: Integer; B_Known: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaBaseKnown";
      procedure Set_Base_Reputation(Ba_Index: Bases_Range) is
         procedure Set_Ada_Base_Reputation
           (B_Index: Integer; Level, Experience: out Integer) with
            Import => True,
            Convention => C,
            External_Name => "setAdaBaseReputation";
      begin
         Set_Ada_Base_Reputation
           (B_Index => Ba_Index, Level => Sky_Bases(Ba_Index).Reputation.Level,
            Experience => Sky_Bases(Ba_Index).Reputation.Experience);
      end Set_Base_Reputation;
      procedure Set_Ada_Recruits
        (Recruits: in out Recruit_Container.Vector; B_Index: Bases_Range) is
         use Interfaces.C;
         --## rule off TYPE_INITIAL_VALUES
         type Nim_Recruits_Array is array(1 .. 20) of Nim_Recruit_Data;
         --## rule on TYPE_INITIAL_VALUES
         --## rule off IMPROPER_INITIALIZATION
         Nim_Recruits: Nim_Recruits_Array;
         --## rule on IMPROPER_INITIALIZATION
         procedure Set_Ada_Base_Recruits
           (N_Recruits: in out Nim_Recruits_Array; B_I: Integer) with
            Import => True,
            Convention => C,
            External_Name => "setAdaRecruits";
      begin
         --## rule off IMPROPER_INITIALIZATION
         Set_Ada_Base_Recruits(N_Recruits => Nim_Recruits, B_I => B_Index);
         Recruit_Container.Clear(Container => Recruits);
         --## rule on IMPROPER_INITIALIZATION
         Convert_Crew_Loop :
         for Recruit of Nim_Recruits loop
            exit Convert_Crew_Loop when Strlen(Item => Recruit.Name) = 0;
            Convert_Recruit_Block :
            declare
               Temp_Recruit: Recruit_Data :=
                 Recruit_Data'
                   (Amount_Of_Attributes => Attributes_Amount,
                    Amount_Of_Skills => Skills_Amount, others => <>);
            begin
               Recruit_From_Nim
                 (Recruit => Recruit, Ada_Recruit => Temp_Recruit);
               Recruit_Container.Append
                 (Container => Recruits, New_Item => Temp_Recruit);
            end Convert_Recruit_Block;
         end loop Convert_Crew_Loop;
      end Set_Ada_Recruits;
   begin
      Set_Ada_Base_Name(B_Index => Base_Index, B_Name => Name);
      Sky_Bases(Base_Index).Name :=
        To_Bounded_String(Source => Value(Item => Name));
      Set_Ada_Base_Date
        (B_Index => Base_Index, Year => Sky_Bases(Base_Index).Visited.Year,
         Month => Sky_Bases(Base_Index).Visited.Month,
         Day => Sky_Bases(Base_Index).Visited.Day,
         Hour => Sky_Bases(Base_Index).Visited.Hour,
         Minutes => Sky_Bases(Base_Index).Visited.Minutes, Date_Type => 0);
      Set_Ada_Base_Location
        (B_Index => Base_Index, X => Sky_Bases(Base_Index).Sky_X,
         Y => Sky_Bases(Base_Index).Sky_Y);
      Set_Ada_Base_Type(B_Index => Base_Index, B_Type => Name);
      Sky_Bases(Base_Index).Base_Type :=
        To_Bounded_String(Source => Value(Item => Name));
      Set_Base_Population(B_Index => Base_Index);
      Set_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Recruit_Date.Year,
         Month => Sky_Bases(Base_Index).Recruit_Date.Month,
         Day => Sky_Bases(Base_Index).Recruit_Date.Day,
         Hour => Sky_Bases(Base_Index).Recruit_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Recruit_Date.Minutes,
         Date_Type => 2);
      Set_Ada_Recruits
        (Recruits => Sky_Bases(Base_Index).Recruits, B_Index => Base_Index);
      Set_Ada_Base_Known(B_Index => Base_Index, B_Known => Known);
      if Known = 1 then
         Sky_Bases(Base_Index).Known := True;
      else
         Sky_Bases(Base_Index).Known := False;
      end if;
      Set_Ada_Base_Asked_For_Bases
        (B_Index => Base_Index, Asked_For_Bases => Known);
      if Known = 1 then
         Sky_Bases(Base_Index).Asked_For_Bases := True;
      else
         Sky_Bases(Base_Index).Asked_For_Bases := False;
      end if;
      Set_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Asked_For_Events.Year,
         Month => Sky_Bases(Base_Index).Asked_For_Events.Month,
         Day => Sky_Bases(Base_Index).Asked_For_Events.Day,
         Hour => Sky_Bases(Base_Index).Asked_For_Events.Hour,
         Minutes => Sky_Bases(Base_Index).Asked_For_Events.Minutes,
         Date_Type => 3);
      Set_Base_Reputation(Ba_Index => Base_Index);
      Set_Ada_Base_Date
        (B_Index => Base_Index,
         Year => Sky_Bases(Base_Index).Missions_Date.Year,
         Month => Sky_Bases(Base_Index).Missions_Date.Month,
         Day => Sky_Bases(Base_Index).Missions_Date.Day,
         Hour => Sky_Bases(Base_Index).Missions_Date.Hour,
         Minutes => Sky_Bases(Base_Index).Missions_Date.Minutes,
         Date_Type => 1);
      Set_Missions(Base_Index => Base_Index);
      Set_Ada_Base_Owner(B_Index => Base_Index, B_Owner => Name);
      Sky_Bases(Base_Index).Owner :=
        To_Bounded_String(Source => Value(Item => Name));
      Set_Base_Cargo(B_Index => Base_Index);
      Set_Ada_Base_Size(B_Index => Base_Index, Size => Known);
      Sky_Bases(Base_Index).Size := Bases_Size'Val(Known);
   end Get_Base_From_Nim;

end Bases;
