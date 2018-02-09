--    Copyright 2018 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Label; use Gtk.Label;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Container; use Gtk.Container;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Button; use Gtk.Button;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types; use Glib.Types;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Utils.UI; use Utils.UI;
with Bases; use Bases;
with ShipModules; use ShipModules;
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with Ships.Crew; use Ships.Crew;
with Ships.UI; use Ships.UI;
with Ships.Cargo.UI; use Ships.Cargo.UI;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Help.UI; use Help.UI;

package body Combat.UI is

   Builder: Gtkada_Builder;
   PilotOrders: constant array(1 .. 4) of Unbounded_String :=
     (To_Unbounded_String("Go closer"),
      To_Unbounded_String("Keep distance"),
      To_Unbounded_String("Evade"),
      To_Unbounded_String("Escape"));
   EngineerOrders: constant array(1 .. 4) of Unbounded_String :=
     (To_Unbounded_String("All stop"),
      To_Unbounded_String("Quarter speed"),
      To_Unbounded_String("Half speed"),
      To_Unbounded_String("Full speed"));
   GunnerOrders: constant array(1 .. 6) of Unbounded_String :=
     (To_Unbounded_String("Don't shoot"),
      To_Unbounded_String("Precise fire"),
      To_Unbounded_String("Fire at will"),
      To_Unbounded_String("Aim for engine"),
      To_Unbounded_String("Aim in weapon"),
      To_Unbounded_String("Aim in hull"));

   procedure RefreshCombatUI is
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
      DamagePercent: Gint;
      IsDamaged: Boolean := False;
      EnemyInfo: Unbounded_String;
      MemberIndex: Natural;
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      MessagesIter: Gtk_Text_Iter;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"),
         To_Unbounded_String("green"),
         To_Unbounded_String("red"),
         To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
   begin
      if (HarpoonDuration > 0 or Enemy.HarpoonDuration > 0) and
        ProtoShips_List(EnemyShipIndex).Crew.Length > 0 then
         Show_All(Gtk_Widget(Get_Object(Builder, "btnboard")));
      else
         Hide(Gtk_Widget(Get_Object(Builder, "btnboard")));
      end if;
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmessages")), "");
      Get_Start_Iter(MessagesBuffer, MessagesIter);
      if LoopStart = 0 then
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      for I in reverse LoopStart .. -1 loop
         Message := GetMessage(I + 1);
         exit when Message.MessageIndex < MessagesStarts;
         if Message.Color = 0 then
            Insert(MessagesBuffer, MessagesIter, To_String(Message.Message));
         else
            Insert_With_Tags
              (MessagesBuffer,
               MessagesIter,
               To_String(Message.Message),
               Lookup
                 (Get_Tag_Table(MessagesBuffer),
                  To_String(TagNames(Message.Color))));
         end if;
         if I > LoopStart then
            Insert(MessagesBuffer, MessagesIter, "" & ASCII.LF);
         end if;
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "crewlist"));
      Clear(List);
      Append(List, Iter);
      MemberIndex := FindMember(Pilot);
      if MemberIndex = 0 then
         Set(List, Iter, 0, "Pilot:");
         Set(List, Iter, 2, "Nobody");
      else
         Set(List, Iter, 0, "Pilot:");
         Set(List, Iter, 1, To_String(PilotOrders(PilotOrder)));
         Set(List, Iter, 2, To_String(PlayerShip.Crew(MemberIndex).Name));
      end if;
      Append(List, Iter);
      MemberIndex := FindMember(Engineer);
      if MemberIndex = 0 then
         Set(List, Iter, 0, "Engineer:");
         Set(List, Iter, 2, "Nobody");
      else
         Set(List, Iter, 0, "Engineer:");
         Set(List, Iter, 1, To_String(EngineerOrders(EngineerOrder)));
         Set(List, Iter, 2, To_String(PlayerShip.Crew(MemberIndex).Name));
      end if;
      for I in Guns.First_Index .. Guns.Last_Index loop
         Append(List, Iter);
         Set
           (List,
            Iter,
            0,
            To_String(PlayerShip.Modules(Guns(I)(1)).Name) & ": ");
         if PlayerShip.Modules(Guns(I)(1)).Owner /= 0 then
            if PlayerShip.Crew(PlayerShip.Modules(Guns(I)(1)).Owner).Order =
              Gunner then
               Set(List, Iter, 1, To_String(GunnerOrders(Guns(I)(2))));
               Set
                 (List,
                  Iter,
                  2,
                  To_String
                    (PlayerShip.Crew(PlayerShip.Modules(Guns(I)(1)).Owner)
                       .Name));
            else
               Set(List, Iter, 2, "Nobody");
            end if;
         else
            Set(List, Iter, 2, "Nobody");
         end if;
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "damagelist"));
      Clear(List);
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Module.Name));
            DamagePercent :=
              100 -
              Gint
                ((Float(Module.Durability) / Float(Module.MaxDurability)) *
                 100.0);
            Set(List, Iter, 1, DamagePercent);
            IsDamaged := True;
         end if;
      end loop;
      if not IsDamaged then
         Hide(Gtk_Widget(Get_Object(Builder, "scrolldamage")));
      else
         Show_All(Gtk_Widget(Get_Object(Builder, "scrolldamage")));
      end if;
      Append(EnemyInfo, "Name: ");
      Append(EnemyInfo, EnemyName);
      Append(EnemyInfo, ASCII.LF);
      Append(EnemyInfo, "Type: ");
      Append(EnemyInfo, Enemy.Ship.Name);
      Append(EnemyInfo, ASCII.LF);
      Append(EnemyInfo, "Home: ");
      Append(EnemyInfo, SkyBases(Enemy.Ship.HomeBase).Name);
      Append(EnemyInfo, ASCII.LF);
      Append(EnemyInfo, "Distance: ");
      if Enemy.Distance >= 15000 then
         Append(EnemyInfo, "Escaped");
      elsif Enemy.Distance < 15000 and Enemy.Distance >= 10000 then
         Append(EnemyInfo, "Long");
      elsif Enemy.Distance < 10000 and Enemy.Distance >= 5000 then
         Append(EnemyInfo, "Medium");
      elsif Enemy.Distance < 5000 and Enemy.Distance >= 1000 then
         Append(EnemyInfo, "Short");
      else
         Append(EnemyInfo, "Close");
      end if;
      Append(EnemyInfo, ASCII.LF);
      Append(EnemyInfo, "Status: ");
      if Enemy.Distance < 15000 then
         if Enemy.Ship.Modules(1).Durability = 0 then
            Append(EnemyInfo, "Destroyed");
         else
            declare
               EnemyStatus: Unbounded_String := To_Unbounded_String("Ok");
            begin
               for Module of Enemy.Ship.Modules loop
                  if Module.Durability < Module.MaxDurability then
                     EnemyStatus := To_Unbounded_String("Damaged");
                     exit;
                  end if;
               end loop;
               Append(EnemyInfo, EnemyStatus);
            end;
            for Module of Enemy.Ship.Modules loop
               if Module.Durability > 0 then
                  case Modules_List(Module.ProtoIndex).MType is
                     when ARMOR =>
                        Append(EnemyInfo, " (armored)");
                     when GUN =>
                        Append(EnemyInfo, " (gun)");
                     when BATTERING_RAM =>
                        Append(EnemyInfo, " (battering ram)");
                     when HARPOON_GUN =>
                        Append(EnemyInfo, " (harpoon gun)");
                     when others =>
                        null;
                  end case;
               end if;
            end loop;
         end if;
      else
         Append(EnemyInfo, "Unknown");
      end if;
      Append(EnemyInfo, ASCII.LF);
      Append(EnemyInfo, "Speed: ");
      if Enemy.Distance < 15000 then
         case Enemy.Ship.Speed is
            when FULL_STOP =>
               Append(EnemyInfo, "Stopped");
            when QUARTER_SPEED =>
               Append(EnemyInfo, "Slow");
            when HALF_SPEED =>
               Append(EnemyInfo, "Medium");
            when FULL_SPEED =>
               Append(EnemyInfo, "Fast");
            when others =>
               null;
         end case;
      else
         Append(EnemyInfo, "Unknown");
      end if;
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblenemyinfo")),
         To_String(EnemyInfo));
   end RefreshCombatUI;

   procedure ShowCombatUI is
      CombatStarted: Boolean;
   begin
      if EnemyName /=
        ProtoShips_List
          (Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .Data)
          .Name then
         CombatStarted :=
           StartCombat
             (Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .Data,
              False);
         if not CombatStarted then
            return;
         end if;
      end if;
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lbldescription")),
         To_String(Enemy.Ship.Description));
      Show_All(Gtk_Widget(Get_Object(Builder, "combatwindow")));
      RefreshCombatUI;
   end ShowCombatUI;

   procedure SetOrdersList(Object: access Gtkada_Builder_Record'Class) is
      OrdersModel: Glib.Types.GType_Interface;
      OrdersList, CrewList: Gtk_List_Store;
      OrdersIter, CrewIter, NamesIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      Position: Natural;
      AssignedName, AssignedOrder: Unbounded_String;
      SkillIndex, SkillValue: Natural := 0;
      SkillString: Unbounded_String;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew"))),
         CrewModel,
         CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      Position := Natural'Value(To_String(Get_Path(CrewModel, CrewIter)));
      CrewList := Gtk_List_Store(Get_Object(Builder, "crewnames"));
      Clear(CrewList);
      AssignedName := To_Unbounded_String(Get_String(CrewModel, CrewIter, 2));
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         case Position is
            when 0 =>
               if GetSkillLevel(PlayerShip.Crew(I), PilotingSkill) >
                 SkillValue then
                  SkillIndex := I;
                  SkillValue :=
                    GetSkillLevel(PlayerShip.Crew(I), PilotingSkill);
               end if;
            when 1 =>
               if GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill) >
                 SkillValue then
                  SkillIndex := I;
                  SkillValue :=
                    GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill);
               end if;
            when others =>
               if GetSkillLevel(PlayerShip.Crew(I), GunnerySkill) >
                 SkillValue then
                  SkillIndex := I;
                  SkillValue :=
                    GetSkillLevel(PlayerShip.Crew(I), GunnerySkill);
               end if;
         end case;
      end loop;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Name /= AssignedName and
           PlayerShip.Crew(I).Skills.Length > 0 then
            SkillString := Null_Unbounded_String;
            case Position is
               when 0 =>
                  if GetSkillLevel(PlayerShip.Crew(I), PilotingSkill) > 0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
               when 1 =>
                  if GetSkillLevel(PlayerShip.Crew(I), EngineeringSkill) >
                    0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
               when others =>
                  if GetSkillLevel(PlayerShip.Crew(I), GunnerySkill) > 0 then
                     SkillString := To_Unbounded_String(" +");
                  end if;
            end case;
            if I = SkillIndex then
               SkillString := SkillString & To_Unbounded_String("+");
            end if;
            if PlayerShip.Crew(I).Order /= Rest then
               SkillString := SkillString & To_Unbounded_String(" -");
            end if;
            Append(CrewList, NamesIter);
            Set
              (CrewList,
               NamesIter,
               0,
               To_String(PlayerShip.Crew(I).Name & SkillString));
            Set(CrewList, NamesIter, 1, Gint(I));
         end if;
      end loop;
      OrdersModel :=
        Get_Property
          (Get_Object(Object, "renderorders"),
           Gtk.Cell_Renderer_Combo.Model_Property);
      OrdersList := -(Gtk_Tree_Model(OrdersModel));
      OrdersList.Clear;
      if AssignedName = To_Unbounded_String("Nobody") then
         return;
      end if;
      AssignedOrder := To_Unbounded_String(Get_String(CrewModel, CrewIter, 1));
      if Position = 0 then
         for I in PilotOrders'Range loop
            if AssignedOrder /= PilotOrders(I) then
               Append(OrdersList, OrdersIter);
               Set(OrdersList, OrdersIter, 0, To_String(PilotOrders(I)));
               Set(OrdersList, OrdersIter, 1, Gint(I));
            end if;
         end loop;
      elsif Position = 1 then
         for I in EngineerOrders'Range loop
            if AssignedOrder /= EngineerOrders(I) then
               Append(OrdersList, OrdersIter);
               Set(OrdersList, OrdersIter, 0, To_String(EngineerOrders(I)));
               Set(OrdersList, OrdersIter, 1, Gint(I));
            end if;
         end loop;
      else
         for I in GunnerOrders'Range loop
            if AssignedOrder /= GunnerOrders(I) then
               Append(OrdersList, OrdersIter);
               Set(OrdersList, OrdersIter, 0, To_String(GunnerOrders(I)));
               Set(OrdersList, OrdersIter, 1, Gint(I));
            end if;
         end loop;
      end if;
   end SetOrdersList;

   procedure GiveCombatOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String;
      New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter) is
      Model: Glib.Types.GType_Interface;
      List: Gtk_List_Store;
      ModuleIndex: Natural := 0;
   begin
      Model := Get_Property(Self, Gtk.Cell_Renderer_Combo.Model_Property);
      List := -(Gtk_Tree_Model(Model));
      if Self = Gtk_Cell_Renderer_Combo(Get_Object(Builder, "rendercrew")) then
         if Path_String = "0" then
            GiveOrders
              (PlayerShip,
               Positive(Get_Int(List, New_Iter, 1)),
               Pilot,
               ModuleIndex);
         elsif Path_String = "1" then
            GiveOrders
              (PlayerShip,
               Positive(Get_Int(List, New_Iter, 1)),
               Engineer,
               ModuleIndex);
         else
            ModuleIndex := Guns(Positive'Value(Path_String) - 1)(1);
            GiveOrders
              (PlayerShip,
               Positive(Get_Int(List, New_Iter, 1)),
               Gunner,
               ModuleIndex);
         end if;
      else
         if Path_String = "0" then
            PilotOrder := Positive(Get_Int(List, New_Iter, 1));
            AddMessage
              ("Order for " &
               To_String(PlayerShip.Crew(FindMember(Pilot)).Name) &
               " was set on: " &
               To_String(PilotOrders(PilotOrder)),
               CombatMessage);
         elsif Path_String = "1" then
            EngineerOrder := Positive(Get_Int(List, New_Iter, 1));
            AddMessage
              ("Order for " &
               To_String(PlayerShip.Crew(FindMember(Engineer)).Name) &
               " was set on: " &
               To_String(EngineerOrders(EngineerOrder)),
               CombatMessage);
         else
            Guns(Positive'Value(Path_String) - 1)(2) :=
              Positive(Get_Int(List, New_Iter, 1));
            AddMessage
              ("Order for " &
               To_String
                 (PlayerShip.Crew
                    (PlayerShip.Modules
                       (Guns(Positive'Value(Path_String) - 1)(1))
                       .Owner)
                    .Name) &
               " was set on: " &
               To_String
                 (GunnerOrders(Guns(Positive'Value(Path_String) - 1)(2))),
               CombatMessage);
         end if;
      end if;
      RefreshCombatUI;
   end GiveCombatOrders;

   procedure NextTurn(Object: access Gtkada_Builder_Record'Class) is
   begin
      CombatTurn;
      RefreshCombatUI;
      if EndCombat then
         Hide(Gtk_Widget(Get_Object(Object, "btnboxinfo")));
         Hide(Gtk_Widget(Get_Object(Object, "btnboxactions")));
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "treecrew")), False);
      end if;
   end NextTurn;

   function CloseWindow
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      if not EndCombat then
         return QuitGame(User_Data);
      end if;
      Set_Sensitive(Gtk_Widget(Get_Object(Builder, "treecrew")), True);
      Hide(Gtk_Widget(Get_Object(Builder, "combatwindow")));
      CreateSkyMap;
      return True;
   end CloseWindow;

   procedure ShowEnemyInfo(Object: access Gtkada_Builder_Record'Class) is
      EnemyList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "enemyinfolist"));
      DamagePercent, SpaceIndex: Natural;
      ModuleName: Unbounded_String;
      EnemyIter: Gtk_Tree_Iter;
   begin
      Clear(EnemyList);
      for I in Enemy.Ship.Modules.Iterate loop
         Append(EnemyList, EnemyIter);
         if Enemy.Distance > 1000 then
            ModuleName :=
              To_Unbounded_String
                (ModuleType'Image
                   (Modules_List(Enemy.Ship.Modules(I).ProtoIndex).MType));
            Replace_Slice
              (ModuleName,
               2,
               Length(ModuleName),
               To_Lower(Slice(ModuleName, 2, Length(ModuleName))));
            SpaceIndex := Index(ModuleName, "_");
            while SpaceIndex > 0 loop
               Replace_Element(ModuleName, SpaceIndex, ' ');
               SpaceIndex := Index(ModuleName, "_");
            end loop;
         else
            ModuleName := Modules_List(Enemy.Ship.Modules(I).ProtoIndex).Name;
         end if;
         Set(EnemyList, EnemyIter, 0, To_String(ModuleName));
         DamagePercent :=
           100 -
           Natural
             ((Float(Enemy.Ship.Modules(I).Durability) /
               Float(Enemy.Ship.Modules(I).MaxDurability)) *
              100.0);
         Set(EnemyList, EnemyIter, 1, Gint(DamagePercent));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Object, "enemyinfowindow")));
   end ShowEnemyInfo;

   procedure RemoveButton(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      Destroy(Widget);
   end RemoveButton;

   procedure ShowBoardOrder(Object: access Gtkada_Builder_Record'Class) is
      ButtonBox: constant Gtk_Container :=
        Gtk_Container(Get_Object(Object, "btnboxboard"));
   begin
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxboard")),
         RemoveButton'Access);
      for Member of PlayerShip.Crew loop
         Add
           (ButtonBox,
            Gtk_Check_Button_New_With_Label(To_String(Member.Name)));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Object, "boardwindow")));
   end ShowBoardOrder;

   procedure SetBoarding(Widget: not null access Gtk_Widget_Record'Class) is
      MemberName: Unbounded_String;
   begin
      if Get_Active(Gtk_Toggle_Button(Widget)) then
         MemberName := To_Unbounded_String(Get_Label(Gtk_Button(Widget)));
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Name = MemberName then
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Boarding, 0);
               exit;
            end if;
         end loop;
      end if;
   end SetBoarding;

   procedure SetBoardingParty(Object: access Gtkada_Builder_Record'Class) is
   begin
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxboard")),
         SetBoarding'Access);
      Hide(Gtk_Widget(Get_Object(Object, "boardwindow")));
      RefreshCombatUI;
   end SetBoardingParty;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(4);
   end ShowHelp;

   procedure ShowInfo(User_Data: access GObject_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "combatwindow")));
      if User_Data = Get_Object(Builder, "btnmessages") then
         ShowMessagesUI(Combat_View);
      elsif User_Data = Get_Object(Builder, "btncargoinfo") then
         ShowCargoUI(Combat_View);
      elsif User_Data = Get_Object(Builder, "btnshipinfo") then
         ShowShipUI(Combat_View);
      else
         ShowCrewUI(Combat_View);
      end if;
   end ShowInfo;

   procedure CreateCombatUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "combat.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "messagesview")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "messagesview")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Close_Window", CloseWindow'Access);
      Register_Handler(Builder, "Set_Orders_List", SetOrdersList'Access);
      Register_Handler(Builder, "Next_Turn", NextTurn'Access);
      Register_Handler(Builder, "Show_Enemy_Info", ShowEnemyInfo'Access);
      Register_Handler(Builder, "Show_Board_Order", ShowBoardOrder'Access);
      Register_Handler(Builder, "Set_Boarding_Party", SetBoardingParty'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Register_Handler(Builder, "Show_Info", ShowInfo'Access);
      Do_Connect(Builder);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "renderorders")),
         GiveCombatOrders'Access);
      On_Changed
        (Gtk_Cell_Renderer_Combo(Get_Object(Builder, "rendercrew")),
         GiveCombatOrders'Access);
   end CreateCombatUI;

end Combat.UI;
