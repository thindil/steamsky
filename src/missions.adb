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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ships; use Ships;
with Maps; use Maps;
with Bases; use Bases;

package body Missions is

   function Get_Accepted_Mission(Mission_Index: Positive) return Mission_Data;
   function Get_Accepted_Missions_Amount return Natural with
      Import => True,
      Convention => C,
      External_Name => "getAdaAcceptedMissionsAmount";

   procedure Accept_Mission(Mission_Index: Positive) is
      use Interfaces.C;

      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Mission: Mission_Data;
      Message: chars_ptr;
      function Accept_Ada_Mission(M_Index: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "acceptAdaMission";
   begin
      Set_Base_In_Nim(Base_Index => Base_Index);
      Set_Ship_In_Nim;
      Get_Missions(Base_Index => Base_Index);
      Message := Accept_Ada_Mission(M_Index => Mission_Index);
      if Strlen(Item => Message) > 0 then
         raise Missions_Accepting_Error with Value(Item => Message);
      end if;
      Set_Missions(Base_Index => Base_Index);
      Get_Ship_From_Nim(Ship => Player_Ship);
      Get_Base_From_Nim(Base_Index => Base_Index);
      Mission :=
        Get_Accepted_Mission(Mission_Index => Get_Accepted_Missions_Amount);
      Sky_Map(Mission.Target_X, Mission.Target_Y).Mission_Index :=
        Get_Accepted_Missions_Amount;
   end Accept_Mission;

   function Auto_Finish_Missions return String is
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Message: chars_ptr;
      function Auto_Ada_Finish_Mission return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "autoAdaFinishMissions";
   begin
      if Base_Index = 0 then
         return "";
      end if;
      Set_Ship_In_Nim;
      Get_Ada_Base_Location
        (Base_Index => Base_Index, X => Sky_Bases(Base_Index).Sky_X,
         Y => Sky_Bases(Base_Index).Sky_Y);
      Message := Auto_Ada_Finish_Mission;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Sky_Map(Sky_Bases(Base_Index).Sky_X, Sky_Bases(Base_Index).Sky_Y)
        .Mission_Index :=
        0;
      Update_Map_Loop :
      for I in 1 .. Get_Accepted_Missions_Amount loop
         if Get_Accepted_Mission(Mission_Index => I).Finished then
            Sky_Map
              (Sky_Bases(Get_Accepted_Mission(Mission_Index => I).Start_Base)
                 .Sky_X,
               Sky_Bases(Get_Accepted_Mission(Mission_Index => I).Start_Base)
                 .Sky_Y)
              .Mission_Index :=
              I;
         else
            Sky_Map
              (Get_Accepted_Mission(Mission_Index => I).Target_X,
               Get_Accepted_Mission(Mission_Index => I).Target_Y)
              .Mission_Index :=
              I;
         end if;
      end loop Update_Map_Loop;
      return Value(Item => Message);
   end Auto_Finish_Missions;

   function Get_Mission_Type(M_Type: Missions_Types) return String is
      function Get_Ada_Mission_Type(M_T: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaMissionType";
   begin
      return
        Value(Item => Get_Ada_Mission_Type(M_T => Missions_Types'Pos(M_Type)));
   end Get_Mission_Type;

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Mission_Data is record
      Time: Natural;
      Target_X: Natural;
      Target_Y: Natural;
      Reward: Natural;
      Start_Base: Natural;
      Finished: Natural;
      Multiplier: Reward_Multiplier := 0.0;
      M_Type: Natural;
      Data: Natural;
   end record;

   type Nim_Missions_Array is array(0 .. 49) of Nim_Mission_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Get_Missions(Base_Index: Positive) is
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      Missions_List: constant Mission_Container.Vector :=
        Sky_Bases(Base_Index).Missions;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Missions
        (N_Missions: Nim_Missions_Array; B_Index: Natural) with
         Import => True,
         Convention => C,
         External_Name => "getAdaMissions";
   begin
      Convert_Missions_Loop :
      for I in Nim_Missions'Range loop
         if I <= Integer(Missions_List.Length - 1) then
            Nim_Missions(I) :=
              (Time => Missions_List(I + 1).Time,
               Target_X => Missions_List(I + 1).Target_X,
               Target_Y => Missions_List(I + 1).Target_Y,
               Reward => Missions_List(I + 1).Reward,
               Start_Base => Missions_List(I + 1).Start_Base,
               Finished => (if Missions_List(I + 1).Finished then 1 else 0),
               Multiplier => Missions_List(I + 1).Multiplier,
               M_Type => Missions_Types'Pos(Missions_List(I + 1).M_Type),
               Data =>
                 (case Missions_List(I + 1).M_Type is
                    when DELIVER => Missions_List(I + 1).Item_Index,
                    when DESTROY => Missions_List(I + 1).Ship_Index,
                    when PASSENGER => Missions_List(I + 1).Data,
                    when others => Missions_List(I + 1).Target));
         else
            Nim_Missions(I) :=
              (Time => 0, Target_X => 0, Target_Y => 0, Reward => 0,
               Start_Base => 0, Finished => 0, Multiplier => 0.0, M_Type => 0,
               Data => 0);
         end if;
      end loop Convert_Missions_Loop;
      Get_Ada_Missions(N_Missions => Nim_Missions, B_Index => Base_Index);
   end Get_Missions;

   -- ****if* Missions/Missions.Set_Missions_List
   -- FUNCTION
   -- Get the list of missions from Nim
   -- PARAMETERS
   -- Base_Index - The index of the base which missions will be get. If set to
   --              zero, get the list of accepted missions
   -- RESULT
   -- The list with the selected missions
   -- SOURCE
   function Set_Missions_List
     (Base_Index: Natural) return Mission_Container.Vector is
     -- ****
      --## rule off IMPROPER_INITIALIZATION
      Nim_Missions: Nim_Missions_Array;
      Missions_List: Mission_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Missions
        (N_Missions: out Nim_Missions_Array; B_Index: Natural) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMissions";
   begin
      Set_Ada_Missions(N_Missions => Nim_Missions, B_Index => Base_Index);
      Convert_Missions_Loop :
      for Nim_Mission of Nim_Missions loop
         exit Convert_Missions_Loop when Nim_Mission.Time = 0;
         case Nim_Mission.M_Type is
            when 0 =>
               Missions_List.Append
                 (New_Item =>
                    (M_Type => DELIVER, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Item_Index => Nim_Mission.Data));
            when 1 =>
               Missions_List.Append
                 (New_Item =>
                    (M_Type => DESTROY, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Ship_Index => Nim_Mission.Data));
            when 2 =>
               Missions_List.Append
                 (New_Item =>
                    (M_Type => PATROL, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Target => Nim_Mission.Data));
            when 3 =>
               Missions_List.Append
                 (New_Item =>
                    (M_Type => EXPLORE, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Target => Nim_Mission.Data));
            when 4 =>
               Missions_List.Append
                 (New_Item =>
                    (M_Type => PASSENGER, Time => Nim_Mission.Time,
                     Target_X => Nim_Mission.Target_X,
                     Target_Y => Nim_Mission.Target_Y,
                     Reward => Nim_Mission.Reward,
                     Start_Base => Nim_Mission.Start_Base,
                     Finished =>
                       (if Nim_Mission.Finished = 1 then True else False),
                     Multiplier => Nim_Mission.Multiplier,
                     Data => Nim_Mission.Data));
            when others =>
               null;
         end case;
      end loop Convert_Missions_Loop;
      return Missions_List;
   end Set_Missions_List;

   procedure Set_Missions(Base_Index: Positive) is
   begin
      Sky_Bases(Base_Index).Missions :=
        Set_Missions_List(Base_Index => Base_Index);
   end Set_Missions;

   function Get_Accepted_Mission
     (Mission_Index: Positive) return Mission_Data is
   begin
      return Set_Missions_List(Base_Index => 0)(Mission_Index);
   end Get_Accepted_Mission;

end Missions;
