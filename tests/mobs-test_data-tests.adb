--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mobs.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Items; use Items;

--  begin read only
--  end read only
package body Mobs.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Generate_Mob_8a189b_d5341e
     (Mob_Index: ProtoMobs_Container.Extended_Index;
      Faction_Index: Unbounded_String) return Member_Data is
   begin
      begin
         pragma Assert
           ((Mob_Index > 0 and Mob_Index < Proto_Mobs_List.Last_Index and
             Factions_List.Contains(Key => Faction_Index)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(mobs.ads:0):Test_GenearateMob test requirement violated");
      end;
      declare
         Test_Generate_Mob_8a189b_d5341e_Result: constant Member_Data :=
           GNATtest_Generated.GNATtest_Standard.Mobs.Generate_Mob
             (Mob_Index, Faction_Index);
      begin
         begin
            pragma Assert
              (Test_Generate_Mob_8a189b_d5341e_Result.Name /=
               Null_Unbounded_String);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(mobs.ads:0:):Test_GenearateMob test commitment violated");
         end;
         return Test_Generate_Mob_8a189b_d5341e_Result;
      end;
   end Wrap_Test_Generate_Mob_8a189b_d5341e;
--  end read only

--  begin read only
   procedure Test_Generate_Mob_test_genearatemob(Gnattest_T: in out Test);
   procedure Test_Generate_Mob_8a189b_d5341e(Gnattest_T: in out Test) renames
     Test_Generate_Mob_test_genearatemob;
--  id:2.2/8a189b5f41e1ef8b/Generate_Mob/1/0/test_genearatemob/
   procedure Test_Generate_Mob_test_genearatemob(Gnattest_T: in out Test) is
      function Generate_Mob
        (Mob_Index: ProtoMobs_Container.Extended_Index;
         Faction_Index: Unbounded_String) return Member_Data renames
        Wrap_Test_Generate_Mob_8a189b_d5341e;
--  end read only

      pragma Unreferenced(Gnattest_T);
      NewMob: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);

   begin

      NewMob := Generate_Mob(5, To_Unbounded_String("POLEIS"));
      Assert(NewMob.Attributes(1).Level = 2, "Failed to generate mob.");
      Assert
        (NewMob.Order_Time = 15,
         "Failed to set order time for the generated mob.");

--  begin read only
   end Test_Generate_Mob_test_genearatemob;
--  end read only

--  begin read only
   function Wrap_Test_Get_Random_Item_4fe348_0ff879
     (Items_Indexes: TinyString_Container.Vector;
      Equip_Index: Equipment_Locations;
      Highest_Level, Weapon_Skill_Level: Positive;
      Faction_Index: Unbounded_String) return Tiny_String.Bounded_String is
   begin
      begin
         pragma Assert
           ((Highest_Level < 101 and Weapon_Skill_Level < 101 and
             Factions_List.Contains(Key => Faction_Index)));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(mobs.ads:0):Test_GetRandomItem test requirement violated");
      end;
      declare
         Test_Get_Random_Item_4fe348_0ff879_Result: constant Tiny_String
           .Bounded_String :=
           GNATtest_Generated.GNATtest_Standard.Mobs.Get_Random_Item
             (Items_Indexes, Equip_Index, Highest_Level, Weapon_Skill_Level,
              Faction_Index);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(mobs.ads:0:):Test_GetRandomItem test commitment violated");
         end;
         return Test_Get_Random_Item_4fe348_0ff879_Result;
      end;
   end Wrap_Test_Get_Random_Item_4fe348_0ff879;
--  end read only

--  begin read only
   procedure Test_Get_Random_Item_test_getrandomitem(Gnattest_T: in out Test);
   procedure Test_Get_Random_Item_4fe348_0ff879
     (Gnattest_T: in out Test) renames
     Test_Get_Random_Item_test_getrandomitem;
--  id:2.2/4fe3489072973ca7/Get_Random_Item/1/0/test_getrandomitem/
   procedure Test_Get_Random_Item_test_getrandomitem
     (Gnattest_T: in out Test) is
      function Get_Random_Item
        (Items_Indexes: TinyString_Container.Vector;
         Equip_Index: Equipment_Locations;
         Highest_Level, Weapon_Skill_Level: Positive;
         Faction_Index: Unbounded_String)
         return Tiny_String.Bounded_String renames
        Wrap_Test_Get_Random_Item_4fe348_0ff879;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

   begin

      Assert
        (Get_Random_Item
           (Weapons_List, WEAPON, 20, 20, To_Unbounded_String("POLEIS")) /=
         Null_Bounded_String,
         "Failed to get random item for mob.");

--  begin read only
   end Test_Get_Random_Item_test_getrandomitem;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mobs.Test_Data.Tests;
