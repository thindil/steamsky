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
   procedure Wrap_Test_Manufacturing_dd583a_cf804c(Minutes: Positive) is
   begin
      GNATtest_Generated.GNATtest_Standard.Crafts.Manufacturing(Minutes);
   end Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

--  begin read only
   procedure Test_Manufacturing_test_manufacturing(Gnattest_T: in out Test);
   procedure Test_Manufacturing_dd583a_cf804c(Gnattest_T: in out Test) renames
     Test_Manufacturing_test_manufacturing;
--  id:2.2/dd583af67efcd5dc/Manufacturing/1/0/test_manufacturing/
   procedure Test_Manufacturing_test_manufacturing(Gnattest_T: in out Test) is
      procedure Manufacturing(Minutes: Positive) renames
        Wrap_Test_Manufacturing_dd583a_cf804c;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Manufacturing(15);
      Assert(True, "This test can only crash.");

--  begin read only
   end Test_Manufacturing_test_manufacturing;
--  end read only

--  begin read only
   function Wrap_Test_Check_Recipe_b182a8_b1abbf
     (Recipe_Index: Tiny_String.Bounded_String) return Positive is
   begin
      begin
         pragma Assert(Tiny_String.Length(Source => Recipe_Index) > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crafts.ads:0):Test_CheckRecipe test requirement violated");
      end;
      declare
         Test_Check_Recipe_b182a8_b1abbf_Result: constant Positive :=
           GNATtest_Generated.GNATtest_Standard.Crafts.Check_Recipe
             (Recipe_Index);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(crafts.ads:0:):Test_CheckRecipe test commitment violated");
         end;
         return Test_Check_Recipe_b182a8_b1abbf_Result;
      end;
   end Wrap_Test_Check_Recipe_b182a8_b1abbf;
--  end read only

--  begin read only
   procedure Test_Check_Recipe_test_checkrecipe(Gnattest_T: in out Test);
   procedure Test_Check_Recipe_b182a8_b1abbf(Gnattest_T: in out Test) renames
     Test_Check_Recipe_test_checkrecipe;
--  id:2.2/b182a8f48a461a4f/Check_Recipe/1/0/test_checkrecipe/
   procedure Test_Check_Recipe_test_checkrecipe(Gnattest_T: in out Test) is
      function Check_Recipe
        (Recipe_Index: Tiny_String.Bounded_String) return Positive renames
        Wrap_Test_Check_Recipe_b182a8_b1abbf;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Cargo(Player_Ship, 6, 10);
      Assert
        (Check_Recipe(Tiny_String.To_Bounded_String("1")) = 10,
         "Failed to check crafting recipe requirements.");

--  begin read only
   end Test_Check_Recipe_test_checkrecipe;
--  end read only

--  begin read only
   procedure Wrap_Test_Set_Recipe_06227a_d0c883
     (Workshop, Amount: Positive; Recipe_Index: Tiny_String.Bounded_String) is
   begin
      begin
         pragma Assert
           ((Workshop <= Player_Ship.Modules.Last_Index and
             Tiny_String.Length(Source => Recipe_Index) > 0));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(crafts.ads:0):Test_SetRecipe test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Crafts.Set_Recipe
        (Workshop, Amount, Recipe_Index);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(crafts.ads:0:):Test_SetRecipe test commitment violated");
      end;
   end Wrap_Test_Set_Recipe_06227a_d0c883;
--  end read only

--  begin read only
   procedure Test_Set_Recipe_test_setrecipe(Gnattest_T: in out Test);
   procedure Test_Set_Recipe_06227a_d0c883(Gnattest_T: in out Test) renames
     Test_Set_Recipe_test_setrecipe;
--  id:2.2/06227a2e531c4565/Set_Recipe/1/0/test_setrecipe/
   procedure Test_Set_Recipe_test_setrecipe(Gnattest_T: in out Test) is
      procedure Set_Recipe
        (Workshop, Amount: Positive;
         Recipe_Index: Tiny_String.Bounded_String) renames
        Wrap_Test_Set_Recipe_06227a_d0c883;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Update_Cargo(Player_Ship, 6, 10);
      Set_Recipe(9, 10, Tiny_String.To_Bounded_String("1"));
      Assert
        (Player_Ship.Modules(9).Crafting_Amount = 10,
         "Failed to set crafting recipe.");

--  begin read only
   end Test_Set_Recipe_test_setrecipe;
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
