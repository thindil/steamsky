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
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Window; use Gtk.Window;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Container; use Gtk.Container;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Image; use Gtk.Image;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Pango.Font; use Pango.Font;
with Gdk; use Gdk;
with Gdk.Device; use Gdk.Device;
with Gdk.Window; use Gdk.Window;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Types; use Gdk.Types;
with Game; use Game;
with Utils; use Utils;
with Utils.UI; use Utils.UI;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with ShipModules; use ShipModules;
with Events; use Events;
with Events.UI; use Events.UI;
with Items; use Items;
with Config; use Config;
with Bases; use Bases;
with Bases.UI; use Bases.UI;
with Bases.LootUI; use Bases.LootUI;
with Bases.SchoolUI; use Bases.SchoolUI;
with Bases.ShipyardUI; use Bases.ShipyardUI;
with Statistics.UI; use Statistics.UI;
with MainMenu; use MainMenu;
with Maps.UI.Handlers; use Maps.UI.Handlers;
with Missions; use Missions;
with Missions.UI; use Missions.UI;
with BasesList; use BasesList;
with Combat.UI; use Combat.UI;
with Crafts.UI; use Crafts.UI;
with GameOptions; use GameOptions;
with Ships.UI; use Ships.UI;
with Ships.Cargo.UI; use Ships.Cargo.UI;
with Trades.UI; use Trades.UI;

package body Maps.UI is

   procedure DeathConfirm is
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
      if ShowConfirmDialog
          ("You are dead. Did you want to see your game statistics?",
           Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
         ShowStatsUI(Main_Menu);
      else
         EndGame(False);
         ShowMainMenu;
      end if;
   end DeathConfirm;

   procedure UpdateHeader is
      HaveWorker, HaveGunner: Boolean := True;
      NeedCleaning,
      NeedRepairs,
      NeedWorker,
      HavePilot,
      HaveEngineer,
      HaveTrader,
      HaveUpgrader,
      HaveCleaner,
      HaveRepairman: Boolean :=
        False;
      ItemIndex, ItemAmount: Natural := 0;
   begin
      if PlayerShip.Crew(1).Health = 0 then
         DeathConfirm;
      end if;
      Set_Text(Gtk_Label(Get_Object(Builder, "lbltime")), FormatedTime);
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnofuel"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnodrink"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnofood"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofood")));
      end if;
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
      if ItemIndex > 0 then
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = FuelType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowFuel;
         end loop;
         if ItemAmount < GameSettings.LowFuel then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnofuel")),
               "[<span foreground=""yellow"">Low Fuel</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnofuel")),
               "Low level of fuel on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
         end if;
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnofuel")),
            "[<span foreground=""red"">No Fuel</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnofuel")),
            "You can't travel anymore.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
      end if;
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => DrinksType);
      if ItemIndex = 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnodrink")),
            "[<span foreground=""red"">No Drinks</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnodrink")),
            "You don't have any drinks in ship.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = DrinksType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowDrinks;
         end loop;
         if ItemAmount < GameSettings.LowDrinks then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnodrink")),
               "[<span foreground=""yellow"">Low Drinks</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnodrink")),
               "Low level of drinks on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
         end if;
      end if;
      for FoodType of FoodTypes loop
         ItemIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => FoodType);
         exit when ItemIndex > 0;
      end loop;
      if ItemIndex = 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnofood")),
            "[<span foreground=""red"">No Food</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnofood")),
            "You don't have any food in ship.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnofood")));
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if FoodTypes.Find_Index
              (Item => Items_List(Item.ProtoIndex).IType) /=
              UnboundedString_Container.No_Index then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowFood;
         end loop;
         if ItemAmount < GameSettings.LowFood then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnofood")),
               "[<span foreground=""yellow"">Low Food</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnofood")),
               "Low level of food on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnofood")));
         end if;
      end if;
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when GUN | HARPOON_GUN =>
               if Module.Owner = 0 then
                  HaveGunner := False;
               elsif PlayerShip.Crew(Module.Owner).Order /= Gunner then
                  HaveGunner := False;
               end if;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Data(1) > 0 then
                  NeedWorker := True;
                  if Module.Owner = 0 then
                     HaveWorker := False;
                  elsif PlayerShip.Crew(Module.Owner).Order /= Craft then
                     HaveWorker := False;
                  end if;
               end if;
            when CABIN =>
               if Module.Data(1) /= Module.Data(2) then
                  NeedCleaning := True;
               end if;
            when others =>
               null;
         end case;
         if Module.Durability /= Module.MaxDurability then
            NeedRepairs := True;
         end if;
      end loop;
      for Member of PlayerShip.Crew loop
         case Member.Order is
            when Pilot =>
               HavePilot := True;
            when Engineer =>
               HaveEngineer := True;
            when Talk =>
               HaveTrader := True;
            when Upgrading =>
               HaveUpgrader := True;
            when Clean =>
               HaveCleaner := True;
            when Repair =>
               HaveRepairman := True;
            when others =>
               null;
         end case;
      end loop;
      if HavePilot then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblpilot")),
            "[<span foreground=""#4E9A06"">P</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblpilot")),
            "Pilot is in position.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblpilot")),
            "[<span foreground=""red"">P</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblpilot")),
            "No pilot assigned. Ship can't move.");
      end if;
      if HaveEngineer then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblengineer")),
            "[<span foreground=""#4E9A06"">E</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblengineer")),
            "Engineer is in position.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblengineer")),
            "[<span foreground=""red"">E</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblengineer")),
            "No engineer assigned. Ship can't move.");
      end if;
      if HaveGunner then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblgunners")),
            "[<span foreground=""#4E9A06"">G</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblgunners")),
            "All guns are manned.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblgunners")),
            "[<span foreground=""red"">G</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblgunners")),
            "One or more guns don't have a gunner.");
      end if;
      if NeedRepairs then
         if HaveRepairman then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblrepairs")),
               "[<span foreground=""#4E9A06"">R</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
               "Ship is being repaired.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblrepairs")),
               "[<span foreground=""red"">R</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
               "Ship needs repairs but no one is working on it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblrepairs")), "[R]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
            "Ship needs no repairs.");
      end if;
      if NeedWorker then
         if HaveWorker then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblcrafting")),
               "[<span foreground=""#4E9A06"">M</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
               "All crafting orders are executed.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblcrafting")),
               "[<span foreground=""red"">M</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
               "You need to assign crew members to manufacturing.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblcrafting")), "[M]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
            "No crafting orders were set.");
      end if;
      if PlayerShip.UpgradeModule > 0 then
         if HaveUpgrader then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblupgrade")),
               "[<span foreground=""#4E9A06"">U</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
               "Ship module upgrade in progress.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblupgrade")),
               "[<span foreground=""red"">U</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
               "Ship module upgrade in progress but no one is working on it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblupgrade")), "[U]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
            "No ship module upgrade was set.");
      end if;
      if HaveTrader then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lbltalk")),
            "[<span foreground=""#4E9A06"">T</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "Trader is in position.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lbltalk")),
            "[<span foreground=""red"">T</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "No trader assigned. You need one to talk/trade.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
         if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .EType =
           FriendlyShip then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lbltalk")),
               "[<span foreground=""red"">T</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lbltalk")),
               "No trader assigned. You need one to talk/trade.");
         else
            Set_Text(Gtk_Label(Get_Object(Builder, "lbltalk")), "[T]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lbltalk")),
               "No trader needed.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lbltalk")), "[T]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "No trader needed.");
      end if;
      if NeedCleaning then
         if HaveCleaner then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblclean")),
               "[<span foreground=""#4E9A06"">C</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblclean")),
               "Ship is cleaned.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblclean")),
               "[<span foreground=""red"">C</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblclean")),
               "Ship is dirty but no one is cleaning it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblclean")), "[C]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblclean")),
            "Ship needs no cleaning.");
      end if;
   end UpdateHeader;

   procedure UpdateMoveButtons is
   begin
      if PlayerShip.Speed = DOCKED then
         Hide(Gtk_Widget(Get_Object(Builder, "cmbspeed")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
         Set_Label(Gtk_Label(Get_Object(Builder, "lblmovewait")), "Wait");
         Set_Label(Gtk_Button(Get_Object(Builder, "btndock")), "Undock");
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupleft")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnup")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupright")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnleft")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnright")), False);
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "btnbottomleft")),
            False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottom")), False);
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "btnbottomright")),
            False);
      else
         Set_Active
           (Gtk_Combo_Box(Get_Object(Builder, "cmbspeed")),
            Gint(ShipSpeed'Pos(PlayerShip.Speed) - 1));
         Show_All(Gtk_Widget(Get_Object(Builder, "cmbspeed")));
         if PlayerShip.DestinationX > 0 and PlayerShip.DestinationY > 0 then
            Show_All(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Label(Gtk_Label(Get_Object(Builder, "lblmovewait")), "Move");
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Label(Gtk_Label(Get_Object(Builder, "lblmovewait")), "Wait");
         end if;
         Set_Label(Gtk_Button(Get_Object(Builder, "btndock")), "Dock");
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            Show_All(Gtk_Widget(Get_Object(Builder, "btndock")));
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btndock")));
         end if;
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnup")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupright")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnright")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottomleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottom")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottomright")));
      end if;
   end UpdateMoveButtons;

   procedure DrawMap is
      Iter: Gtk_Text_Iter;
      MapBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmap"));
      EndY, EndX: Integer;
      Tags: constant Gtk_Text_Tag_Table := Get_Tag_Table(MapBuffer);
      WhiteColor: constant Gtk_Text_Tag := Lookup(Tags, "white");
      GrayColor: constant Gtk_Text_Tag := Lookup(Tags, "gray");
      RedColor: constant Gtk_Text_Tag := Lookup(Tags, "red");
      GreenColor: constant Gtk_Text_Tag := Lookup(Tags, "green");
      YellowColor: constant Gtk_Text_Tag := Lookup(Tags, "yellow");
      CyanColor: constant Gtk_Text_Tag := Lookup(Tags, "cyan");
      LimeColor: constant Gtk_Text_Tag := Lookup(Tags, "lime");
      MapChar: Character;
      MapColor: Gtk_Text_Tag;
      TextWindow: constant Gdk_Window :=
        Get_Window
          (Gtk_Text_View(Get_Object(Builder, "mapview")),
           Text_Window_Text);
   begin
      StartY := CenterY - (MapHeight / 2);
      StartX := CenterX - (MapWidth / 2);
      EndY := CenterY + (MapHeight / 2);
      EndX := CenterX + (MapWidth / 2);
      if StartY < 1 then
         StartY := 1;
         EndY := MapHeight;
      end if;
      if StartX < 1 then
         StartX := 1;
         EndX := MapWidth;
      end if;
      if EndY > 1024 then
         EndY := 1024;
         StartY := 1024 - MapHeight;
      end if;
      if EndX > 1024 then
         EndX := 1024;
         StartX := 1024 - MapWidth;
      end if;
      Get_Start_Iter(MapBuffer, Iter);
      for Y in StartY .. EndY loop
         for X in StartX .. EndX loop
            if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
               MapChar := '+';
               MapColor := WhiteColor;
            else
               MapChar := ' ';
               if SkyMap(X, Y).Visited then
                  MapColor := WhiteColor;
               else
                  MapColor := GrayColor;
               end if;
               if X = PlayerShip.DestinationX and
                 Y = PlayerShip.DestinationY then
                  MapChar := 'X';
               elsif SkyMap(X, Y).MissionIndex > 0 then
                  MapChar := '!';
               elsif SkyMap(X, Y).EventIndex > 0 then
                  MapChar := '?';
               elsif SkyMap(X, Y).BaseIndex > 0 then
                  if SkyBases(SkyMap(X, Y).BaseIndex).Known then
                     MapChar := 'o';
                     if SkyBases(SkyMap(X, Y).BaseIndex).Visited.Year > 0 then
                        case SkyBases(SkyMap(X, Y).BaseIndex).BaseType is
                           when Industrial =>
                              MapColor := RedColor;
                           when Agricultural =>
                              MapColor := GreenColor;
                           when Refinery =>
                              MapColor := YellowColor;
                           when Shipyard =>
                              MapColor := CyanColor;
                           when Military =>
                              MapColor := LimeColor;
                           when others =>
                              null;
                        end case;
                     end if;
                  end if;
               end if;
            end if;
            Insert_With_Tags(MapBuffer, Iter, "" & MapChar, MapColor);
         end loop;
         if Y < EndY then
            Insert(MapBuffer, Iter, "" & ASCII.LF);
         end if;
      end loop;
      if TextWindow /= null then
         Set_Cursor(TextWindow, Gdk_Cursor_New(Cross));
      end if;
   end DrawMap;

   procedure UpdateMessages is
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      Iter: Gtk_Text_Iter;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"),
         To_Unbounded_String("green"),
         To_Unbounded_String("red"),
         To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
   begin
      Set_Text(MessagesBuffer, "");
      Get_Start_Iter(MessagesBuffer, Iter);
      if LoopStart = 0 then
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      for I in reverse LoopStart .. -1 loop
         Message := GetMessage(I + 1);
         if Message.Color = 0 then
            Insert(MessagesBuffer, Iter, To_String(Message.Message));
         else
            Insert_With_Tags
              (MessagesBuffer,
               Iter,
               To_String(Message.Message),
               Lookup
                 (Get_Tag_Table(MessagesBuffer),
                  To_String(TagNames(Message.Color))));
         end if;
         if I > LoopStart then
            Insert(MessagesBuffer, Iter, "" & ASCII.LF);
         end if;
      end loop;
      if LastMessage = Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
      else
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         Check_Resize(Gtk_Container(Get_Object(Builder, "skymapwindow")));
         LastMessage := Null_Unbounded_String;
      end if;
   end UpdateMessages;

   procedure HideButtons(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      Set_No_Show_All(Widget, True);
      Hide(Widget);
   end HideButtons;

   procedure CheckButtons(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      if not Get_No_Show_All(Widget) and not ButtonsVisible then
         ButtonsVisible := True;
      end if;
   end CheckButtons;

   procedure GetCurrentCellCoords is
      Mask: Gdk_Modifier_Type;
      MouseX, MouseY: Gint;
      DeviceManager: constant Gdk_Device_Manager :=
        Get_Device_Manager
          (Get_Display(Gtk_Widget(Get_Object(Builder, "mapview"))));
      Mouse: constant Gdk_Device := Get_Client_Pointer(DeviceManager);
      Window: Gdk_Window;
   begin
      Get_Device_Position
        (Get_Window(Gtk_Widget(Get_Object(Builder, "mapview"))),
         Mouse,
         MouseX,
         MouseY,
         Mask,
         Window);
      MapX := (Natural(MouseX) / MapCellWidth) + StartX;
      MapY := (Natural(MouseY) / MapCellHeight) + StartY;
   end GetCurrentCellCoords;

   procedure BuildMapInfo(MapInfoText: in out Unbounded_String) is
   begin
      if SkyMap(MapX, MapY).BaseIndex > 0 then
         declare
            BaseIndex: constant Positive := SkyMap(MapX, MapY).BaseIndex;
         begin
            if Length(MapInfoText) > 0 then
               Append(MapInfoText, ASCII.LF);
            end if;
            Append(MapInfoText, "Base info:");
            Append(MapInfoText, ASCII.LF);
            Append
              (MapInfoText,
               To_Unbounded_String("Name: ") & SkyBases(BaseIndex).Name);
            if SkyBases(SkyMap(MapX, MapY).BaseIndex).Visited.Year > 0 then
               Append(MapInfoText, ASCII.LF);
               Append
                 (MapInfoText,
                  "Type: " &
                  To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
               Append(MapInfoText, ASCII.LF);
               if SkyBases(BaseIndex).Population > 0 and
                 SkyBases(BaseIndex).Population < 150 then
                  Append(MapInfoText, "Population: small");
               elsif SkyBases(BaseIndex).Population > 149 and
                 SkyBases(BaseIndex).Population < 300 then
                  Append(MapInfoText, "Population: medium");
               elsif SkyBases(BaseIndex).Population > 299 then
                  Append(MapInfoText, "Population: large");
               end if;
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, ASCII.LF);
               end if;
               if SkyBases(BaseIndex).Owner = Abandoned then
                  Append(MapInfoText, "Base is abandoned");
               else
                  Append
                    (MapInfoText,
                     "Owner: " &
                     To_Lower(Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
               end if;
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, ASCII.LF);
                  case SkyBases(BaseIndex).Reputation(1) is
                     when -100 .. -75 =>
                        Append(MapInfoText, "You are hated here");
                     when -74 .. -50 =>
                        Append(MapInfoText, "You are outlaw here");
                     when -49 .. -25 =>
                        Append(MapInfoText, "You are hostile here");
                     when -24 .. -1 =>
                        Append(MapInfoText, "They are unfriendly to you");
                     when 0 =>
                        Append(MapInfoText, "You are unknown here");
                     when 1 .. 25 =>
                        Append(MapInfoText, "You are know here as visitor");
                     when 26 .. 50 =>
                        Append(MapInfoText, "You are know here as trader");
                     when 51 .. 75 =>
                        Append(MapInfoText, "You are know here as friend");
                     when 76 .. 100 =>
                        Append(MapInfoText, "You are well know here");
                     when others =>
                        null;
                  end case;
               end if;
               if BaseIndex = PlayerShip.HomeBase then
                  Append(MapInfoText, ASCII.LF);
                  Append(MapInfoText, "It is your home base");
               end if;
            end if;
         end;
      end if;
      if SkyMap(MapX, MapY).EventIndex > 0 then
         declare
            EventIndex: constant Positive := SkyMap(MapX, MapY).EventIndex;
         begin
            if Length(MapInfoText) > 0 then
               Append(MapInfoText, ASCII.LF);
            end if;
            if Events_List(EventIndex).EType /= BaseRecovery and
              SkyMap(MapX, MapY).BaseIndex > 0 then
               Append(MapInfoText, ASCII.LF & ASCII.LF);
            end if;
            case Events_List(EventIndex).EType is
               when EnemyShip | Trader | FriendlyShip =>
                  Append
                    (MapInfoText,
                     ProtoShips_List(Events_List(EventIndex).Data).Name);
               when FullDocks =>
                  Append(MapInfoText, "Full docks in base");
               when AttackOnBase =>
                  Append(MapInfoText, "Base is under attack");
               when Disease =>
                  Append(MapInfoText, "Disease in base");
               when EnemyPatrol =>
                  Append(MapInfoText, "Enemy patrol");
               when DoublePrice =>
                  Append
                    (MapInfoText,
                     "Double price for " &
                     To_String(Items_List(Events_List(EventIndex).Data).Name));
               when None | BaseRecovery =>
                  null;
            end case;
         end;
      end if;
      if SkyMap(MapX, MapY).MissionIndex > 0 then
         declare
            MissionIndex: constant Positive := SkyMap(MapX, MapY).MissionIndex;
         begin
            if Length(MapInfoText) > 0 then
               Append(MapInfoText, ASCII.LF);
            end if;
            if SkyMap(MapX, MapY).BaseIndex > 0 or
              SkyMap(MapX, MapY).EventIndex > 0 then
               Append(MapInfoText, ASCII.LF & ASCII.LF);
            end if;
            case PlayerShip.Missions(MissionIndex).MType is
               when Deliver =>
                  Append
                    (MapInfoText,
                     "Deliver " &
                     To_String
                       (Items_List(PlayerShip.Missions(MissionIndex).Target)
                          .Name));
               when Destroy =>
                  Append
                    (MapInfoText,
                     "Destroy " &
                     To_String
                       (ProtoShips_List
                          (PlayerShip.Missions(MissionIndex).Target)
                          .Name));
               when Patrol =>
                  Append(MapInfoText, "Patrol area");
               when Explore =>
                  Append(MapInfoText, "Explore area");
               when Passenger =>
                  Append(MapInfoText, "Transport passenger");
            end case;
         end;
      end if;
   end BuildMapInfo;

   procedure CreateSkyMap
     (X: Integer := PlayerShip.SkyX;
      Y: Integer := PlayerShip.SkyY) is
      Error: aliased GError;
      FontDescription: constant Pango_Font_Description :=
        Pango_Font_Description_New;
      Accelerators: Gtk_Accel_Group;
   begin
      CenterX := X;
      CenterY := Y;
      if Builder = null then
         Gtk_New(Builder);
         if Add_From_File
             (Builder,
              To_String(DataDirectory) & "ui" & Dir_Separator & "game.glade",
              Error'Access) =
           Guint(0) then
            Put_Line("Error : " & Get_Message(Error));
            return;
         end if;
         CreateMessagesUI(Builder);
         CreateEventsUI(Builder);
         CreateMissionsUI(Builder);
         CreateBasesListUI(Builder);
         CreateStatsUI(Builder);
         CreateBasesUI(Builder);
         CreateBasesLootUI(Builder);
         CreateBasesSchoolUI(Builder);
         CreateBasesShipyardUI(Builder);
         CreateCombatUI(Builder);
         CreateCraftsUI(Builder);
         CreateGameOptions(Builder);
         CreateCrewUI(Builder);
         CreateShipUI(Builder);
         CreateCargoUI(Builder);
         CreateTradeUI(Builder);
         Register_Handler(Builder, "Quit_Game", QuitGame'Access);
         Register_Handler(Builder, "Quit_Game_Menu", QuitGameMenu'Access);
         Register_Handler
           (Builder,
            "Hide_Last_Message",
            HideLastMessage'Access);
         Register_Handler(Builder, "Get_New_Size", GetMapSize'Access);
         Register_Handler(Builder, "Show_Map_Info", ShowMapCellInfo'Access);
         Register_Handler(Builder, "Hide_Map_Info", HideMapInfoWindow'Access);
         Register_Handler(Builder, "Show_Window", ShowWindow'Access);
         Register_Handler(Builder, "Set_Destination", SetDestination'Access);
         Register_Handler(Builder, "Hide_Window", HideWindow'Access);
         Register_Handler(Builder, "Move_Map", MoveMap'Access);
         Register_Handler(Builder, "Dock_Ship", BtnDockClicked'Access);
         Register_Handler(Builder, "Change_Speed", ChangeSpeed'Access);
         Register_Handler(Builder, "Move_Ship", MoveShip'Access);
         Register_Handler(Builder, "Show_Orders", ShowOrders'Access);
         Register_Handler(Builder, "Wait_Order", WaitOrder'Access);
         Register_Handler(Builder, "Attack_Order", AttackOrder'Access);
         Register_Handler(Builder, "Show_Help", ShowHelp'Access);
         Register_Handler(Builder, "Show_Info", ShowInfo'Access);
         Register_Handler(Builder, "Resign_From_Game", ResignFromGame'Access);
         Register_Handler(Builder, "Show_Missions", ShowMissions'Access);
         Register_Handler(Builder, "Start_Mission", StartMission'Access);
         Register_Handler(Builder, "Complete_Mission", CompleteMission'Access);
         Register_Handler(Builder, "Execute_Order", ExecuteOrder'Access);
         Register_Handler(Builder, "Show_Wait_Orders", ShowWaitOrders'Access);
         Register_Handler(Builder, "Update_Tooltip", UpdateTooltip'Access);
         Register_Handler
           (Builder,
            "Deliver_Medicines",
            DeliverMedicines'Access);
         Do_Connect(Builder);
         Set_Family(FontDescription, "monospace");
         Override_Font
           (Gtk_Widget(Get_Object(Builder, "mapview")),
            FontDescription);
         Add_Entry("<skymapwindow>/btnupleft", GDK_KP_7, 0);
         Add_Entry("<skymapwindow>/btnup", GDK_KP_8, 0);
         Add_Entry("<skymapwindow>/btnupright", GDK_KP_9, 0);
         Add_Entry("<skymapwindow>/btnleft", GDK_KP_4, 0);
         Add_Entry("<skymapwindow>/btnmovewait", GDK_KP_5, 0);
         Add_Entry("<skymapwindow>/btnright", GDK_KP_6, 0);
         Add_Entry("<skymapwindow>/btnbottomleft", GDK_KP_1, 0);
         Add_Entry("<skymapwindow>/btnbottom", GDK_KP_2, 0);
         Add_Entry("<skymapwindow>/btnbottomright", GDK_KP_3, 0);
         Add_Entry("<skymapwindow>/btnmoveto", GDK_KP_Divide, 0);
         Add_Entry("<skymapwindow>/Menu/ShipInfo", GDK_S, 0);
         Add_Entry("<skymapwindow>/Menu/ShipCargoInfo", GDK_A, 0);
         Add_Entry("<skymapwindow>/Menu/CrewInfo", GDK_C, 0);
         Add_Entry("<skymapwindow>/Menu/ShipOrders", GDK_O, 0);
         Add_Entry("<skymapwindow>/Menu/CraftInfo", GDK_R, 0);
         Add_Entry("<skymapwindow>/Menu/MessagesInfo", GDK_M, 0);
         Add_Entry("<skymapwindow>/Menu/BasesInfo", GDK_B, 0);
         Add_Entry("<skymapwindow>/Menu/EventsInfo", GDK_N, 0);
         Add_Entry("<skymapwindow>/Menu/MissionsInfo", GDK_I, 0);
         Add_Entry("<skymapwindow>/Menu/MoveMap", GDK_V, 0);
         Add_Entry("<skymapwindow>/Menu/GameStats", GDK_G, 0);
         Add_Entry("<skymapwindow>/Menu/Help", GDK_F1, 0);
         Add_Entry("<skymapwindow>/Menu/GameOptions", GDK_P, 0);
         Add_Entry("<skymapwindow>/Menu/QuitGame", GDK_Q, 0);
         Add_Entry("<skymapwindow>/Menu/ResignFromGame", GDK_X, 0);
         Add_Entry("<skymapwindow>/Menu", GDK_E, 0);
         Add_Entry("<skymapwindow>/Menu/WaitOrders", GDK_W, 0);
         if Exists(To_String(SaveDirectory) & "keys.cfg") then
            Load(To_String(SaveDirectory) & "keys.cfg");
         end if;
         Accelerators :=
           Gtk_Accel_Group(Get_Object(Builder, "movementaccels"));
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnupleft")),
            "<skymapwindow>/btnupleft",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnup")),
            "<skymapwindow>/btnup",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnupright")),
            "<skymapwindow>/btnupright",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnleft")),
            "<skymapwindow>/btnleft",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnmovewait")),
            "<skymapwindow>/btnmovewait",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnright")),
            "<skymapwindow>/btnright",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnbottomleft")),
            "<skymapwindow>/btnbottomleft",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnbottom")),
            "<skymapwindow>/btnbottom",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnbottomright")),
            "<skymapwindow>/btnbottomleft",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnmoveto")),
            "<skymapwindow>/btnmoveto",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnmenu")),
            "<skymapwindow>/Menu",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuship")),
            "<skymapwindow>/Menu/ShipInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menucargo")),
            "<skymapwindow>/Menu/ShipCargoInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menucrew")),
            "<skymapwindow>/Menu/CrewInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuorders")),
            "<skymapwindow>/Menu/ShipOrders",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menucrafting")),
            "<skymapwindow>/Menu/CraftInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menumessages")),
            "<skymapwindow>/Menu/MessagesInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menubaseslist")),
            "<skymapwindow>/Menu/BasesInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuevents")),
            "<skymapwindow>/Menu/EventsInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menumissions")),
            "<skymapwindow>/Menu/MissionsInfo",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menumovemap")),
            "<skymapwindow>/Menu/MoveMap",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menustats")),
            "<skymapwindow>/Menu/GameStats",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuhelp")),
            "<skymapwindow>/Menu/Help",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuoptions")),
            "<skymapwindow>/Menu/GameOptions",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuquit")),
            "<skymapwindow>/Menu/QuitGame",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuresign")),
            "<skymapwindow>/Menu/ResignFromGame",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "menuwait")),
            "<skymapwindow>/Menu/WaitOrders",
            Accelerators);
         Set_Accel_Path
           (Gtk_Widget(Get_Object(Builder, "btnshowhelp")),
            "<skymapwindow>/Menu/Help",
            Accelerators);
         declare
            Key: Gtk_Accel_Key;
            Found: Boolean;
         begin
            Lookup_Entry("<skymapwindow>/Menu/Help", Key, Found);
            Set_Label
              (Gtk_Button(Get_Object(Builder, "btnshowhelp")),
               "Help [" &
               Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods) &
               "]");
         end;
         On_Key_Release_Event
           (Gtk_Widget(Get_Object(Builder, "movemapwindow")),
            CloseWindow'Access);
         On_Key_Release_Event
           (Gtk_Widget(Get_Object(Builder, "orderswindow")),
            CloseWindow'Access);
         On_Key_Release_Event
           (Gtk_Widget(Get_Object(Builder, "waitwindow")),
            CloseWindow'Access);
         Set
           (Gtk_Image(Get_Object(Builder, "image1")),
            To_String(DataDirectory) &
            "ui" &
            Dir_Separator &
            "images" &
            Dir_Separator &
            "up.png");
         Set
           (Gtk_Image(Get_Object(Builder, "image2")),
            To_String(DataDirectory) &
            "ui" &
            Dir_Separator &
            "images" &
            Dir_Separator &
            "down.png");
         Set
           (Gtk_Image(Get_Object(Builder, "image3")),
            To_String(DataDirectory) &
            "ui" &
            Dir_Separator &
            "images" &
            Dir_Separator &
            "left.png");
         Set
           (Gtk_Image(Get_Object(Builder, "image4")),
            To_String(DataDirectory) &
            "ui" &
            Dir_Separator &
            "images" &
            Dir_Separator &
            "right.png");
         Set_Transition_Type
           (Gtk_Stack(Get_Object(Builder, "gamestack")),
            Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
         Set_Transition_Type
           (Gtk_Stack(Get_Object(Builder, "shipyardstack")),
            Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
         Set_Transition_Type
           (Gtk_Stack(Get_Object(Builder, "optionsstack")),
            Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
         Set_Transition_Type
           (Gtk_Stack(Get_Object(Builder, "combatstack")),
            Gtk_Stack_Transition_Type'Val(GameSettings.AnimationType));
         Set_Default_Size
           (Gtk_Window(Get_Object(Builder, "skymapwindow")),
            Gint(GameSettings.WindowWidth),
            Gint(GameSettings.WindowHeight));
      end if;
      UpdateMessages;
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "txtmap")),
         "X" & ASCII.LF & "X");
      Show_All(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
      UpdateHeader;
      UpdateMoveButtons;
      Hide(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnclose")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnshowhelp")));
   end CreateSkyMap;

end Maps.UI;
