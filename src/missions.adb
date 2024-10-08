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

with Interfaces.C.Strings;
with Bases;

package body Missions is

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
   --## rule on TYPE_INITIAL_VALUES

   function Get_Mission_Type(M_Type: Missions_Types) return String is
      use Interfaces.C.Strings;

      function Get_Ada_Mission_Type(M_T: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaMissionType";
   begin
      return
        Value(Item => Get_Ada_Mission_Type(M_T => Missions_Types'Pos(M_Type)));
   end Get_Mission_Type;

   procedure Get_Missions(Base_Index: Positive) is
      use Bases;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Missions_Array is array(0 .. 49) of Nim_Mission_Data;
      --## rule on TYPE_INITIAL_VALUES
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

end Missions;
