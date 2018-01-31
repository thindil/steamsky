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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Utils.UI; use Utils.UI;
with Bases; use Bases;
with ShipModules; use ShipModules;
with Events; use Events;
with Maps; use Maps;
with Crew; use Crew;
with Ships.Crew; use Ships.Crew;

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
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnboard")));
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmessages")), "");
      List := Gtk_List_Store(Get_Object(Builder, "crewnames"));
      Clear(List);
      Append(List, Iter);
      Set(List, Iter, 0, "Empty");
      for Member of PlayerShip.Crew loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Member.Name));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "crewlist"));
      Clear(List);
      Append(List, Iter);
      MemberIndex := FindMember(Pilot);
      if MemberIndex = 0 then
         Set(List, Iter, 0, "Pilot:");
         Set(List, Iter, 2, "Empty");
      else
         Set(List, Iter, 0, "Pilot:");
         Set(List, Iter, 1, To_String(PilotOrders(PilotOrder)));
         Set(List, Iter, 2, To_String(PlayerShip.Crew(MemberIndex).Name));
      end if;
      Append(List, Iter);
      MemberIndex := FindMember(Engineer);
      if MemberIndex = 0 then
         Set(List, Iter, 0, "Engineer:");
         Set(List, Iter, 2, "Empty");
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
               Set(List, Iter, 2, "Empty");
            end if;
         else
            Set(List, Iter, 2, "Empty");
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
      Show_All(Gtk_Widget(Get_Object(Builder, "combatwindow")));
      RefreshCombatUI;
   end ShowCombatUI;

   procedure CreateCombatUI is
      Error: aliased GError;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
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
      List := Gtk_List_Store(Get_Object(Builder, "pilotorders"));
      for Order of PilotOrders loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Order));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "engineerorders"));
      for Order of EngineerOrders loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Order));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "gunnerorders"));
      for Order of GunnerOrders loop
         Append(List, Iter);
         Set(List, Iter, 0, To_String(Order));
      end loop;
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Quit_Game", QuitGame'Access);
      Do_Connect(Builder);
   end CreateCombatUI;

end Combat.UI;
