--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Interfaces.C.Strings;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Game; use Game;
with Statistics;

package body HallOfFame is

   procedure Load_Hall_Of_Fame is
      use Interfaces.C;
      use Interfaces.C.Strings;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Hall_Of_Fame_Data is record
         Name: chars_ptr;
         Points: Natural := 0;
         Death_Reason: chars_ptr;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_Entry: Nim_Hall_Of_Fame_Data;
      Result: chars_ptr;
      --## rule on IMPROPER_INITIALIZATION
      function Load_Ada_Hall_Of_Fame return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHallOfFame";
      procedure Get_Ada_Hof_Entry
        (Index: Natural; N_Entry: out Nim_Hall_Of_Fame_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHofEntry";
   begin
      Result := Load_Ada_Hall_Of_Fame;
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Hof_Loop :
      for I in 1 .. 10 loop
         Get_Ada_Hof_Entry(Index => I, N_Entry => Nim_Entry);
         if Strlen(Item => Nim_Entry.Name) > 0 then
            Hall_Of_Fame_Array(I) :=
              (Name =>
                 To_Unbounded_String(Source => Value(Item => Nim_Entry.Name)),
               Points => Nim_Entry.Points,
               Death_Reason =>
                 To_Unbounded_String
                   (Source => Value(Item => Nim_Entry.Death_Reason)));
         end if;
      end loop Load_Hof_Loop;
   end Load_Hall_Of_Fame;

   procedure Update_Hall_Of_Fame
     (Player_Name, Death_Reason: Unbounded_String) is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;
      use Statistics;

      New_Index: Natural range 0 .. 10 := 0;
      Hof_File: File_Type;
      Hall_Of_Fame: DOM_Implementation; --## rule line off IMPROPER_INITIALIZATION
      Entry_Node, Main_Node: DOM.Core.Element;
      Hof_Data: Document;
   begin
      Find_New_Index_Loop :
      for I in Hall_Of_Fame_Array'Range loop
         if Hall_Of_Fame_Array(I).Points < Get_Game_Points then
            New_Index := I;
            exit Find_New_Index_Loop;
         end if;
      end loop Find_New_Index_Loop;
      if New_Index = 0 then
         return;
      end if;
      Hall_Of_Fame_Array(New_Index + 1 .. Hall_Of_Fame_Array'Last) :=
        Hall_Of_Fame_Array(New_Index .. Hall_Of_Fame_Array'Last - 1);
      Hall_Of_Fame_Array(New_Index) :=
        (Name => Player_Name, Points => Get_Game_Points,
         Death_Reason => Death_Reason);
      Hof_Data := Create_Document(Implementation => Hall_Of_Fame);
      Main_Node :=
        Append_Child
          (N => Hof_Data,
           New_Child =>
             Create_Element(Doc => Hof_Data, Tag_Name => "halloffame"));
      Update_Hall_Of_Fame_Loop :
      for Element of Hall_Of_Fame_Array loop
         if Element.Name = Null_Unbounded_String then
            exit Update_Hall_Of_Fame_Loop;
         end if;
         Entry_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element(Doc => Hof_Data, Tag_Name => "entry"));
         Set_Attribute
           (Elem => Entry_Node, Name => "name",
            Value => To_String(Source => Element.Name));
         Set_Attribute
           (Elem => Entry_Node, Name => "points",
            Value =>
              Trim
                (Source => Integer'Image(Element.Points),
                 Side => Ada.Strings.Left));
         Set_Attribute
           (Elem => Entry_Node, Name => "Death_Reason",
            Value => To_String(Source => Element.Death_Reason));
      end loop Update_Hall_Of_Fame_Loop;
      Create
        (File => Hof_File, Mode => Out_File,
         Name => To_String(Source => Save_Directory) & "halloffame.dat");
      Write
        (Stream => Stream(File => Hof_File), N => Hof_Data,
         Pretty_Print => True);
      Close(File => Hof_File);
   end Update_Hall_Of_Fame;

end HallOfFame;
