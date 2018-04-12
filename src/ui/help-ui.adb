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
with Gtk.Label; use Gtk.Label;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Utils.UI; use Utils.UI;
with Items; use Items;

package body Help.UI is

   Builder: Gtkada_Builder;

   procedure SelectTopic(Self: access Gtk_Menu_Item_Record'Class) is
      TopicName: constant Unbounded_String :=
        To_Unbounded_String(Get_Label(Self));
   begin
      for I in Help_List.Iterate loop
         if TopicName = Help_List(I).Title then
            ShowHelpUI(Help_Container.To_Index(I));
            exit;
         end if;
      end loop;
   end SelectTopic;

   procedure CreateHelpUI is
      Error: aliased GError;
      MenuTopic: Gtk_Menu_Item;
      TopicList: Gtk_Menu_Shell;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "help.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      TopicList := Gtk_Menu_Shell(Get_Object(Builder, "helpmenu"));
      for Help of Help_List loop
         Gtk_New_With_Label(MenuTopic, To_String(Help.Title));
         Append(TopicList, MenuTopic);
         Show(MenuTopic);
         On_Activate(MenuTopic, SelectTopic'Access);
      end loop;
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Do_Connect(Builder);
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "helpwindow")),
         CloseWindow'Access);
   end CreateHelpUI;

   procedure ShowHelpUI(Topic: Positive) is
      NewText: Unbounded_String;
      VariableIndex: Natural;
      Key: Gtk_Accel_Key;
      Found: Boolean;
      VariablesNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("{MoneyName}"),
         To_Unbounded_String("{FuelName}"),
         To_Unbounded_String("{StrengthName}"),
         To_Unbounded_String("{HealingTools}"),
         To_Unbounded_String("{HealingSkill}"),
         To_Unbounded_String("{PilotingSkill}"),
         To_Unbounded_String("{EngineeringSkill}"),
         To_Unbounded_String("{GunnerySkill}"),
         To_Unbounded_String("{TalkingSkill}"),
         To_Unbounded_String("{PerceptionSkill}"),
         To_Unbounded_String("{ConditionName}"),
         To_Unbounded_String("{DodgeSkill}"));
      VariablesValues: constant array(Positive range <>) of Unbounded_String :=
        (MoneyName,
         Items_List(FindProtoItem(ItemType => FuelType)).Name,
         Attributes_List(StrengthIndex).Name,
         HealingTools,
         Skills_List(HealingSkill).Name,
         Skills_List(PilotingSkill).Name,
         Skills_List(EngineeringSkill).Name,
         Skills_List(GunnerySkill).Name,
         Skills_List(TalkingSkill).Name,
         Skills_List(PerceptionSkill).Name,
         Attributes_List(ConditionIndex).Name,
         Skills_List(DodgeSkill).Name);
      AccelNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("<skymapwindow>/btnupleft"),
         To_Unbounded_String("<skymapwindow>/btnup"),
         To_Unbounded_String("<skymapwindow>/btnupright"),
         To_Unbounded_String("<skymapwindow>/btnleft"),
         To_Unbounded_String("<skymapwindow>/btnmovewait"),
         To_Unbounded_String("<skymapwindow>/btnright"),
         To_Unbounded_String("<skymapwindow>/btnbottomleft"),
         To_Unbounded_String("<skymapwindow>/btnbottom"),
         To_Unbounded_String("<skymapwindow>/btnbottomright"),
         To_Unbounded_String("<skymapwindow>/btnmoveto"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipCargoInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/CrewInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipOrders"),
         To_Unbounded_String("<skymapwindow>/Menu/CraftInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MessagesInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/BasesInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/EventsInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MissionsInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MoveMap"),
         To_Unbounded_String("<skymapwindow>/Menu/GameStats"),
         To_Unbounded_String("<skymapwindow>/Menu/Help"),
         To_Unbounded_String("<skymapwindow>/Menu/GameOptions"),
         To_Unbounded_String("<skymapwindow>/Menu/QuitGame"),
         To_Unbounded_String("<skymapwindow>/Menu/ResignFromGame"),
         To_Unbounded_String("<skymapwindow>/Menu"),
         To_Unbounded_String("<skymapwindow>/Menu/WaitOrders"));
      HelpBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "helpbuffer"));
      StartIter, EndIter: Gtk_Text_Iter;
      Tags: constant Gtk_Text_Tag_Table := Get_Tag_Table(HelpBuffer);
      BoldText: constant Gtk_Text_Tag := Lookup(Tags, "bold");
   begin
      NewText := Help_List(Topic).Text;
      Set_Text(HelpBuffer, To_String(NewText));
      for I in VariablesNames'Range loop
         loop
            VariableIndex := Index(NewText, To_String(VariablesNames(I)));
            exit when VariableIndex = 0;
            Get_Iter_At_Offset(HelpBuffer, StartIter, Gint(VariableIndex - 1));
            Get_Iter_At_Offset
              (HelpBuffer,
               EndIter,
               Gint(VariableIndex + Length(VariablesNames(I)) - 1));
            Delete(HelpBuffer, StartIter, EndIter);
            Insert_With_Tags
              (HelpBuffer,
               StartIter,
               To_String(VariablesValues(I)),
               BoldText);
            Replace_Slice
              (NewText,
               VariableIndex,
               (VariableIndex + Length(VariablesNames(I)) - 1),
               To_String(VariablesValues(I)));
         end loop;
      end loop;
      for I in AccelNames'Range loop
         loop
            VariableIndex :=
              Index(NewText, "{GameKey" & Positive'Image(I) & "}");
            exit when VariableIndex = 0;
            Lookup_Entry(To_String(AccelNames(I)), Key, Found);
            Get_Iter_At_Offset(HelpBuffer, StartIter, Gint(VariableIndex - 1));
            Get_Iter_At_Offset
              (HelpBuffer,
               EndIter,
               Gint(VariableIndex + 8 + Positive'Image(I)'Length));
            Delete(HelpBuffer, StartIter, EndIter);
            Insert_With_Tags
              (HelpBuffer,
               StartIter,
               "'" &
               Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods) &
               "'",
               BoldText);
            Replace_Slice
              (NewText,
               VariableIndex,
               (VariableIndex + 8 + Positive'Image(I)'Length),
               "'" &
               Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods) &
               "'");
         end loop;
      end loop;
      Set_Text
        (Gtk_Label(Get_Object(Builder, "lblhelptopic")),
         To_String(Help_List(Topic).Title));
      Show_All(Gtk_Widget(Get_Object(Builder, "helpwindow")));
   end ShowHelpUI;

end Help.UI;
