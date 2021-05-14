--    Copyright 2017-2021 Bartek thindil Jasicki
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
with Ada.Directories;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers;
with Input_Sources.File;
with Game; use Game;
with Statistics;

package body HallOfFame is

   procedure Load_Hall_Of_Fame is
      use DOM.Readers;
      use Input_Sources.File;

      Hof_File: File_Input;
      Reader: Tree_Reader; --## rule line off IMPROPER_INITIALIZATION
      Entries_List: Node_List;
      Entry_Node: Node;
      Hof_Data: Document;
   begin
      if Hall_Of_Fame_Array(1).Name /= Null_Unbounded_String then
         return;
      end if;
      Open
        (Filename => To_String(Source => Save_Directory) & "halloffame.dat",
         Input => Hof_File);
      --## rule off IMPROPER_INITIALIZATION
      Parse(Parser => Reader, Input => Hof_File);
      Close(Input => Hof_File);
      Hof_Data := Get_Tree(Read => Reader);
      --## rule on IMPROPER_INITIALIZATION
      Entries_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Hof_Data,
           Tag_Name => "entry");
      Load_Hall_Of_Fame_Loop:
      for I in 0 .. Length(List => Entries_List) - 1 loop
         Entry_Node := Item(List => Entries_List, Index => I);
         Hall_Of_Fame_Array(I + 1).Name :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Entry_Node, Name => "name"));
         Hall_Of_Fame_Array(I + 1).Points :=
           Natural'Value(Get_Attribute(Elem => Entry_Node, Name => "points"));
         Hall_Of_Fame_Array(I + 1).Death_Reason :=
           To_Unbounded_String
             (Source =>
                Get_Attribute(Elem => Entry_Node, Name => "Death_Reason"));
      end loop Load_Hall_Of_Fame_Loop;
      Free(Read => Reader);
   exception
      when Ada.Directories.Name_Error =>
         null;
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
      Find_New_Index_Loop:
      for I in Hall_Of_Fame_Array'Range loop
         if Hall_Of_Fame_Array(I).Points < GetGamePoints then
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
        (Name => Player_Name,
         Points => GetGamePoints,
         Death_Reason => Death_Reason);
      Hof_Data := Create_Document(Implementation => Hall_Of_Fame);
      Main_Node :=
        Append_Child
          (N => Hof_Data,
           New_Child =>
             Create_Element(Doc => Hof_Data, Tag_Name => "halloffame"));
      Update_Hall_Of_Fame_Loop:
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
           (Elem => Entry_Node,
            Name => "name",
            Value => To_String(Source => Element.Name));
         Set_Attribute
           (Elem => Entry_Node,
            Name => "points",
            Value =>
              Trim
                (Source => Integer'Image(Element.Points),
                 Side => Ada.Strings.Left));
         Set_Attribute
           (Elem => Entry_Node,
            Name => "Death_Reason",
            Value => To_String(Source => Element.Death_Reason));
      end loop Update_Hall_Of_Fame_Loop;
      Create
        (File => Hof_File,
         Mode => Out_File,
         Name => To_String(Source => Save_Directory) & "halloffame.dat");
      Write
        (Stream => Stream(File => Hof_File),
         N => Hof_Data,
         Pretty_Print => True);
      Close(File => Hof_File);
   end Update_Hall_Of_Fame;

end HallOfFame;
