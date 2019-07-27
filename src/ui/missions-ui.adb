--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Button; use Gtk.Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Adjustment; use Gtk.Adjustment;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Game; use Game;
with Ships; use Ships;
with Bases; use Bases;
with Messages; use Messages;
with Items; use Items;
with Utils.UI; use Utils.UI;

package body Missions.UI is

-- ****iv* Missions.UI/Builder
-- SOURCE
   Builder: Gtkada_Builder;
-- ****
-- ****iv* Missions.UI/MissionIndex
-- SOURCE
   MissionIndex: Natural := 0;
-- ****
-- ****iv* Missions.UI/Cleaning
-- SOURCE
   Cleaning: Boolean;
-- ****

-- ****if* Missions.UI/ShowMissionInfo
-- SOURCE
   procedure ShowMissionInfo(User_Data: access GObject_Record'Class) is
-- ****
      MissionsIter: Gtk_Tree_Iter;
      MissionsModel: Gtk_Tree_Model;
      MissionInfo: Unbounded_String;
      Mission: Mission_Data;
      CabinTaken: Boolean := False;
      CanAccept: Boolean := True;
      MissionsLimit: Natural;
      OldIndex: constant Natural := MissionIndex;
   begin
      if Cleaning then
         return;
      end if;
      Get_Selected
        (Gtk.Tree_View.Get_Selection(Gtk_Tree_View(User_Data)), MissionsModel,
         MissionsIter);
      if MissionsIter = Null_Iter then
         return;
      end if;
      MissionIndex := Positive(Get_Int(MissionsModel, MissionsIter, 1));
      if User_Data = Get_Object(Builder, "treemissions") then
         if MissionIndex >
           Positive
             (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                .Missions
                .Length) then
            return;
         end if;
         Mission :=
           SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
             .Missions
             (MissionIndex);
         if OldIndex /= MissionIndex then
            Set_Value(Gtk_Adjustment(Get_Object(Builder, "adjmission")), 1.00);
         end if;
      else
         if MissionIndex > Positive(AcceptedMissions.Length) then
            return;
         end if;
         Mission := AcceptedMissions(MissionIndex);
      end if;
      case Mission.MType is
         when Deliver =>
            MissionInfo :=
              To_Unbounded_String("Item: ") &
              Items_List(Mission.ItemIndex).Name;
            Append
              (MissionInfo,
               LF & "Weight:" &
               Positive'Image(Items_List(Mission.ItemIndex).Weight) & " kg");
            Append
              (MissionInfo,
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name));
         when Patrol =>
            MissionInfo := To_Unbounded_String("Patrol selected area");
         when Destroy =>
            MissionInfo :=
              To_Unbounded_String
                ("Target: " &
                 To_String(ProtoShips_List(Mission.ShipIndex).Name));
         when Explore =>
            MissionInfo := To_Unbounded_String("Explore selected area");
         when Passenger =>
            CanAccept := False;
            Modules_Loop :
            for Module of PlayerShip.Modules loop
               if (Module.MType = CABIN and not CanAccept)
                 and then Module.Quality >= Mission.Data then
                  CanAccept := True;
                  CabinTaken := False;
                  for Owner of Module.Owner loop
                     if Owner > 0 then
                        CabinTaken := True;
                        CanAccept := False;
                        exit;
                     end if;
                  end loop;
                  exit when CanAccept;
               end if;
            end loop Modules_Loop;
            if User_Data = Get_Object(Builder, "treemissions1") then
               CanAccept := True;
            end if;
            MissionInfo := To_Unbounded_String("Needed quality of cabin: ");
            if CanAccept then
               Append(MissionInfo, GetCabinQuality(Mission.Data));
            elsif CabinTaken then
               Append
                 (MissionInfo,
                  To_Unbounded_String("<span foreground=""yellow"">") &
                  GetCabinQuality(Mission.Data) &
                  To_Unbounded_String("</span>"));
            else
               Append
                 (MissionInfo,
                  To_Unbounded_String("<span foreground=""red"">") &
                  GetCabinQuality(Mission.Data) &
                  To_Unbounded_String("</span>"));
            end if;
            Append
              (MissionInfo,
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name));
      end case;
      Append(MissionInfo, LF & "Time limit:");
      MinutesToDate(Mission.Time, MissionInfo);
      if User_Data = Get_Object(Builder, "treemissions") then
         Mission.Multiplier :=
           RewardMultiplier
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjmission"))));
         SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Missions
           (MissionIndex)
           .Multiplier :=
           Mission.Multiplier;
      end if;
      Append
        (MissionInfo,
         LF & "Base reward:" &
         Natural'Image
           (Natural(Float(Mission.Reward) * Float(Mission.Multiplier))) &
         " " & To_String(MoneyName));
      if User_Data = Get_Object(Builder, "treemissions") then
         declare
            Distance: Positive;
         begin
            if Mission.MType = Deliver or Mission.MType = Passenger then
               Distance := Positive(Get_Int(MissionsModel, MissionsIter, 2));
            else
               Distance :=
                 Positive(Get_Int(MissionsModel, MissionsIter, 2)) * 2;
            end if;
            TravelInfo(MissionInfo, Distance, True);
         end;
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblavailablemissioninfo")),
            To_String(MissionInfo));
         case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
           .Reputation
           (1) is
            when 0 .. 25 =>
               MissionsLimit := 1;
            when 26 .. 50 =>
               MissionsLimit := 3;
            when 51 .. 75 =>
               MissionsLimit := 5;
            when 76 .. 100 =>
               MissionsLimit := 10;
            when others =>
               MissionsLimit := 0;
         end case;
         for Mission of AcceptedMissions loop
            if Mission.StartBase =
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex then
               MissionsLimit := MissionsLimit - 1;
            end if;
         end loop;
         if MissionsLimit > 0 then
            Set_Label
              (Gtk_Label(Get_Object(Builder, "lblavailablemissions")),
               "You can take" & Natural'Image(MissionsLimit) &
               " more missions in from base.");
         else
            Set_Label
              (Gtk_Label(Get_Object(Builder, "lblavailablemissions")),
               "You can't take any more missions from this base.");
            CanAccept := False;
         end if;
         if not CanAccept then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Builder, "btnacceptmission")), False);
         else
            Set_Sensitive
              (Gtk_Widget(Get_Object(Builder, "btnacceptmission")), True);
         end if;
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblmissioninfo")),
            To_String(MissionInfo));
         if Mission.Finished then
            Show_All(Gtk_Widget(Get_Object(Builder, "lblfinished")));
            Set_Label
              (Gtk_Button(Get_Object(Builder, "btnmissiondestination")),
               "S_et starting base as destination for ship");
         else
            Hide(Gtk_Widget(Get_Object(Builder, "lblfinished")));
            Set_Label
              (Gtk_Button(Get_Object(Builder, "btnmissiondestination")),
               "S_et mission as destination for the ship");
         end if;
      end if;
   end ShowMissionInfo;

-- ****if* Missions.UI/RefreshMissionsList
-- SOURCE
   procedure RefreshMissionsList is
-- ****
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsIter: Gtk_Tree_Iter;
      MissionsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "missionslist"));
   begin
      Cleaning := True;
      Clear(MissionsList);
      Cleaning := False;
      for I in SkyBases(BaseIndex).Missions.Iterate loop
         Append(MissionsList, MissionsIter);
         case SkyBases(BaseIndex).Missions(I).MType is
            when Deliver =>
               Set(MissionsList, MissionsIter, 0, "Deliver item to base");
            when Patrol =>
               Set(MissionsList, MissionsIter, 0, "Patrol area");
            when Destroy =>
               Set(MissionsList, MissionsIter, 0, "Destroy ship");
            when Explore =>
               Set(MissionsList, MissionsIter, 0, "Explore area");
            when Passenger =>
               Set
                 (MissionsList, MissionsIter, 0,
                  "Transport passenger to base");
         end case;
         Set
           (MissionsList, MissionsIter, 1,
            Gint(Mission_Container.To_Index(I)));
         Set
           (MissionsList, MissionsIter, 2,
            Gint
              (CountDistance
                 (SkyBases(BaseIndex).Missions(I).TargetX,
                  SkyBases(BaseIndex).Missions(I).TargetY)));
      end loop;
   end RefreshMissionsList;

-- ****if* Missions.UI/AcceptSelectedMission
-- SOURCE
   procedure AcceptSelectedMission
     (Object: access Gtkada_Builder_Record'Class) is
-- ****
      MissionsIter: Gtk_Tree_Iter;
      MissionsModel: Gtk_Tree_Model;
   begin
      AcceptMission(MissionIndex);
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treemissions"))),
         MissionsModel, MissionsIter);
      if MissionsIter = Null_Iter then
         return;
      end if;
      if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex).Missions
          .Length =
        0 then
         CloseMessages(Object);
         return;
      end if;
      RefreshMissionsList;
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemissions")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   exception
      when An_Exception : Missions_Accepting_Error =>
         ShowDialog(Exception_Message(An_Exception));
   end AcceptSelectedMission;

-- ****if* Missions.UI/ButtonMission
-- SOURCE
   procedure ButtonMission(User_Data: access GObject_Record'Class) is
-- ****
      X, Y: Integer;
   begin
      if User_Data = Get_Object(Builder, "btnmissioncenter") then
         ShowSkyMap
           (AcceptedMissions(MissionIndex).TargetX,
            AcceptedMissions(MissionIndex).TargetY);
      else
         if not AcceptedMissions(MissionIndex).Finished then
            X := AcceptedMissions(MissionIndex).TargetX;
            Y := AcceptedMissions(MissionIndex).TargetY;
         else
            X := SkyBases(AcceptedMissions(MissionIndex).StartBase).SkyX;
            Y := SkyBases(AcceptedMissions(MissionIndex).StartBase).SkyY;
         end if;
         if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
            ShowDialog("You are at this target now.");
            return;
         end if;
         PlayerShip.DestinationX := X;
         PlayerShip.DestinationY := Y;
         AddMessage
           ("You set the travel destination for your ship.", OrderMessage);
         ShowSkyMap;
      end if;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "skymap");
   end ButtonMission;

-- ****if* Missions.UI/ShowAvailableMission
-- SOURCE
   procedure ShowAvailableMission
     (Object: access Gtkada_Builder_Record'Class) is
-- ****
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      ShowSkyMap
        (SkyBases(BaseIndex).Missions(MissionIndex).TargetX,
         SkyBases(BaseIndex).Missions(MissionIndex).TargetY);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end ShowAvailableMission;

   procedure CreateMissionsUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Mission_Info", ShowMissionInfo'Access);
      Register_Handler(Builder, "Button_Mission", ButtonMission'Access);
      Register_Handler
        (Builder, "Accept_Mission", AcceptSelectedMission'Access);
      Register_Handler
        (Builder, "Show_Available_Mission", ShowAvailableMission'Access);
      On_Button_Release_Event
        (Gtk_Widget(Get_Object(Builder, "treemissions1")),
         ShowPopupMenuButton'Access);
      On_Button_Release_Event
        (Gtk_Widget(Get_Object(Builder, "treemissions")),
         ShowPopupMenuButton'Access);
   end CreateMissionsUI;

   procedure ShowMissionsUI is
   begin
      RefreshMissionsList;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "availablemissions");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemissions")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      UpdateMessages;
   end ShowMissionsUI;

   procedure ShowAcceptedMissions is
      MissionsIter: Gtk_Tree_Iter;
      MissionsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "missionslist"));
   begin
      Cleaning := True;
      Clear(MissionsList);
      Cleaning := False;
      for I in AcceptedMissions.Iterate loop
         Append(MissionsList, MissionsIter);
         case AcceptedMissions(I).MType is
            when Deliver =>
               Set(MissionsList, MissionsIter, 0, "Deliver item to base");
            when Patrol =>
               Set(MissionsList, MissionsIter, 0, "Patrol area");
            when Destroy =>
               Set(MissionsList, MissionsIter, 0, "Destroy ship");
            when Explore =>
               Set(MissionsList, MissionsIter, 0, "Explore area");
            when Passenger =>
               Set
                 (MissionsList, MissionsIter, 0,
                  "Transport passenger to base");
         end case;
         Set
           (MissionsList, MissionsIter, 1,
            Gint(Mission_Container.To_Index(I)));
         Set
           (MissionsList, MissionsIter, 2,
            Gint
              (CountDistance
                 (AcceptedMissions(I).TargetX, AcceptedMissions(I).TargetY)));
      end loop;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "acceptedmissions");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treemissions1")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end ShowAcceptedMissions;

end Missions.UI;
