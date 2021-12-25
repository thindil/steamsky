--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Items; use Items;

package body ShipModules is

   procedure Load_Ship_Modules(Reader: Tree_Reader) is
      Nodes_List: Node_List;
      Modules_Data: Document;
      Temp_Record: Base_Module_Data;
      Action: Data_Action;
      Module_Node: Node;
      Skill_Index: Natural;
      Material_Exists: Boolean;
      Module_Index: Unbounded_String;
   begin
      Modules_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Modules_Data, Tag_Name => "module");
      Load_Modules_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record :=
           (Name => Null_Unbounded_String, MType => ENGINE, Weight => 0,
            Value => 0, Max_Value => 0, Durability => 0,
            Repair_Material => Null_Unbounded_String, Repair_Skill => 2,
            Price => 0, Install_Time => 60, Unique => False, Size => 1,
            Description => Null_Unbounded_String, Max_Owners => 1, Speed => 4,
            Reputation => -100);
         Module_Node := Item(List => Nodes_List, Index => I);
         Module_Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Module_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Module_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Module_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not BaseModules_Container.Contains
                (Container => Modules_List, Key => Module_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " ship module '" & To_String(Module_Index) &
                 "', there is no ship module with that index.";
            end if;
         elsif BaseModules_Container.Contains(Modules_List, Module_Index) then
            raise Data_Loading_Error
              with "Can't add ship module '" & To_String(Module_Index) &
              "', there is already a ship with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Modules_List(Module_Index);
            end if;
            if Get_Attribute(Module_Node, "name")'Length > 0 then
               Temp_Record.Name :=
                 To_Unbounded_String(Get_Attribute(Module_Node, "name"));
            end if;
            if Get_Attribute(Module_Node, "type")'Length > 0 then
               Temp_Record.MType :=
                 Module_Type'Value(Get_Attribute(Module_Node, "type"));
            end if;
            if Get_Attribute(Module_Node, "weight")'Length > 0 then
               Temp_Record.Weight :=
                 Natural'Value(Get_Attribute(Module_Node, "weight"));
            end if;
            if Get_Attribute(Module_Node, "value")'Length > 0 then
               Temp_Record.Value :=
                 Integer'Value(Get_Attribute(Module_Node, "value"));
            end if;
            if Get_Attribute(Module_Node, "maxvalue")'Length > 0 then
               Temp_Record.Max_Value :=
                 Integer'Value(Get_Attribute(Module_Node, "maxvalue"));
            end if;
            if Get_Attribute(Module_Node, "durability")'Length > 0 then
               Temp_Record.Durability :=
                 Integer'Value(Get_Attribute(Module_Node, "durability"));
            end if;
            if Get_Attribute(Module_Node, "material")'Length > 0 then
               Temp_Record.Repair_Material :=
                 To_Unbounded_String(Get_Attribute(Module_Node, "material"));
               Material_Exists := False;
               Check_Materials_Loop :
               for Material of Items_Types loop
                  if Material = Temp_Record.Repair_Material then
                     Material_Exists := True;
                     exit Check_Materials_Loop;
                  end if;
               end loop Check_Materials_Loop;
               if not Material_Exists then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " ship module '" & To_String(Module_Index) &
                    "', there is no item type '" &
                    Get_Attribute(Module_Node, "material") & "'.";
               end if;
            end if;
            if Get_Attribute(Module_Node, "skill")'Length > 0 then
               Skill_Index :=
                 Find_Skill_Index(Get_Attribute(Module_Node, "skill"));
               if Skill_Index = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(Data_Action'Image(Action)) &
                    " ship module '" & To_String(Module_Index) &
                    "', there is no skill named '" &
                    Get_Attribute(Module_Node, "skill") & "'.";
               end if;
               Temp_Record.Repair_Skill := Skill_Index;
            end if;
            if Get_Attribute(Module_Node, "price")'Length > 0 then
               Temp_Record.Price :=
                 Integer'Value(Get_Attribute(Module_Node, "price"));
            end if;
            if Get_Attribute(Module_Node, "installtime")'Length > 0 then
               Temp_Record.Install_Time :=
                 Positive'Value(Get_Attribute(Module_Node, "installtime"));
            end if;
            if Get_Attribute(Module_Node, "unique") /= "" then
               Temp_Record.Unique := True;
            end if;
            if Get_Attribute(Module_Node, "size") /= "" then
               Temp_Record.Size :=
                 Integer'Value(Get_Attribute(Module_Node, "size"));
            end if;
            if Get_Attribute(Module_Node, "maxowners")'Length > 0 then
               Temp_Record.Max_Owners :=
                 Integer'Value(Get_Attribute(Module_Node, "maxowners"));
            end if;
            if Get_Attribute(Module_Node, "speed")'Length > 0 then
               Temp_Record.Speed :=
                 Integer'Value(Get_Attribute(Module_Node, "speed"));
            end if;
            if Get_Attribute(Module_Node, "reputation")'Length > 0 then
               Temp_Record.Reputation :=
                 Integer'Value(Get_Attribute(Module_Node, "reputation"));
            end if;
            if Has_Child_Nodes(Module_Node) then
               Temp_Record.Description :=
                 To_Unbounded_String(Node_Value(First_Child(Module_Node)));
            end if;
            if Action /= UPDATE then
               BaseModules_Container.Include
                 (Modules_List, Module_Index, Temp_Record);
               Log_Message
                 ("Module added: " & To_String(Temp_Record.Name), EVERYTHING);
            else
               Modules_List(Module_Index) := Temp_Record;
               Log_Message
                 ("Module updated: " & To_String(Temp_Record.Name),
                  EVERYTHING);
            end if;
         else
            BaseModules_Container.Exclude(Modules_List, Module_Index);
            Log_Message
              ("Module removed: " & To_String(Module_Index), EVERYTHING);
         end if;
      end loop Load_Modules_Loop;
   end Load_Ship_Modules;

   function Get_Module_Type(Module_Index: Unbounded_String) return String is
      ModuleTypeName: Unbounded_String :=
        To_Unbounded_String
          (To_Lower(Module_Type'Image(Modules_List(Module_Index).MType)));
   begin
      Replace_Element
        (ModuleTypeName, 1,
         To_Upper(Ada.Strings.Unbounded.Element(ModuleTypeName, 1)));
      Translate(ModuleTypeName, To_Mapping("_", " "));
      return To_String(ModuleTypeName);
   end Get_Module_Type;

end ShipModules;
