--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Crafts.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ships.Cargo; use Ships.Cargo;

--  begin read only
--  end read only
package body Crafts.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_Manufacturing_dd583a_cf804c (Minutes: Positive) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Crafts.Manufacturing (Minutes);
   end Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

--  begin read only
   procedure Test_Manufacturing_test_manufacturing (Gnattest_T : in out Test);
   procedure Test_Manufacturing_dd583a_cf804c (Gnattest_T : in out Test) renames Test_Manufacturing_test_manufacturing;
--  id:2.2/dd583af67efcd5dc/Manufacturing/1/0/test_manufacturing/
   procedure Test_Manufacturing_test_manufacturing (Gnattest_T : in out Test) is
   procedure Manufacturing (Minutes: Positive) renames Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Manufacturing(15);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Manufacturing_test_manufacturing;
--  end read only

--  begin read only
   function Wrap_Test_CheckRecipe_6b22c5_37e1c4 (RecipeIndex: Unbounded_String)  return Positive
   is
   begin
      begin
         pragma Assert
           (RecipeIndex /= Null_Unbounded_String);
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(crafts.ads:0):Test_CheckRecipe test requirement violated");
      end;
      declare
         Test_CheckRecipe_6b22c5_37e1c4_Result : constant Positive := GNATtest_Generated.GNATtest_Standard.Crafts.CheckRecipe (RecipeIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crafts.ads:0:):Test_CheckRecipe test commitment violated");
         end;
         return Test_CheckRecipe_6b22c5_37e1c4_Result;
      end;
   end Wrap_Test_CheckRecipe_6b22c5_37e1c4;
--  end read only

--  begin read only
   procedure Test_CheckRecipe_test_checkrecipe (Gnattest_T : in out Test);
   procedure Test_CheckRecipe_6b22c5_37e1c4 (Gnattest_T : in out Test) renames Test_CheckRecipe_test_checkrecipe;
--  id:2.2/6b22c50e71f35d02/CheckRecipe/1/0/test_checkrecipe/
   procedure Test_CheckRecipe_test_checkrecipe (Gnattest_T : in out Test) is
      function CheckRecipe (RecipeIndex: Unbounded_String) return Positive renames Wrap_Test_CheckRecipe_6b22c5_37e1c4;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      UpdateCargo(PlayerShip, To_Unbounded_String("6"), 10);
      Assert(CheckRecipe(To_Unbounded_String("1")) > 0, "Failed to check crafting recipe requirements.");

--  begin read only
   end Test_CheckRecipe_test_checkrecipe;
--  end read only

--  begin read only
   procedure Wrap_Test_SetRecipe_d9013b_dcc889 (Workshop, Amount: Positive; RecipeIndex: Unbounded_String) 
   is
   begin
      begin
         pragma Assert
           ((Workshop <= PlayerShip.Modules.Last_Index and RecipeIndex /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crafts.ads:0):Test_SetRecipe test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crafts.SetRecipe (Workshop, Amount, RecipeIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crafts.ads:0:):Test_SetRecipe test commitment violated");
      end;
   end Wrap_Test_SetRecipe_d9013b_dcc889;
--  end read only

--  begin read only
   procedure Test_SetRecipe_test_setrecipe (Gnattest_T : in out Test);
   procedure Test_SetRecipe_d9013b_dcc889 (Gnattest_T : in out Test) renames Test_SetRecipe_test_setrecipe;
--  id:2.2/d9013bfcb0ae8d7e/SetRecipe/1/0/test_setrecipe/
   procedure Test_SetRecipe_test_setrecipe (Gnattest_T : in out Test) is
   procedure SetRecipe (Workshop, Amount: Positive; RecipeIndex: Unbounded_String) renames Wrap_Test_SetRecipe_d9013b_dcc889;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      UpdateCargo(PlayerShip, To_Unbounded_String("6"), 10);
      SetRecipe(9, 10, To_Unbounded_String("1"));
      Assert(PlayerShip.Modules(9).CraftingAmount = 10, "Failed to set crafting recipe.");

--  begin read only
   end Test_SetRecipe_test_setrecipe;
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
end Crafts.Test_Data.Tests;
