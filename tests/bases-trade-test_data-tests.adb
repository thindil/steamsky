--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bases.Trade.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Crafts; use Crafts;
with Maps; use Maps;

--  begin read only
--  end read only
package body Bases.Trade.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   procedure Wrap_Test_HireRecruit_e2a034_6a9998 (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural; ContractLenght: Integer) 
   is
   begin
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.HireRecruit (RecruitIndex, Cost, DailyPayment, TradePayment, ContractLenght);
   end Wrap_Test_HireRecruit_e2a034_6a9998;
--  end read only

--  begin read only
   procedure Test_HireRecruit_test_hirerecruit (Gnattest_T : in out Test);
   procedure Test_HireRecruit_e2a034_6a9998 (Gnattest_T : in out Test) renames Test_HireRecruit_test_hirerecruit;
--  id:2.2/e2a03470a37e9b74/HireRecruit/1/0/test_hirerecruit/
   procedure Test_HireRecruit_test_hirerecruit (Gnattest_T : in out Test) is
   procedure HireRecruit (RecruitIndex, Cost: Positive; DailyPayment, TradePayment: Natural; ContractLenght: Integer) renames Wrap_Test_HireRecruit_e2a034_6a9998;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Amount: constant Positive := Positive(PlayerShip.Crew.Length);

   begin

      HireRecruit(1, 1, 0, 0, 1);
      Assert(Positive(PlayerShip.Crew.Length) = Amount + 1, "Failed to hire recruit to player ship crew.");

--  begin read only
   end Test_HireRecruit_test_hirerecruit;
--  end read only

--  begin read only
   procedure Wrap_Test_BuyRecipe_64b3a1_e0c4a8 (RecipeIndex: Unbounded_String) 
   is
   begin
      begin
         pragma Assert
           ((RecipeIndex /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(bases-trade.ads:0):Test_BuyRecipe test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Bases.Trade.BuyRecipe (RecipeIndex);
      begin
         pragma Assert
           (True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(bases-trade.ads:0:):Test_BuyRecipe test commitment violated");
      end;
   end Wrap_Test_BuyRecipe_64b3a1_e0c4a8;
--  end read only

--  begin read only
   procedure Test_BuyRecipe_test_buyrecipe (Gnattest_T : in out Test);
   procedure Test_BuyRecipe_64b3a1_e0c4a8 (Gnattest_T : in out Test) renames Test_BuyRecipe_test_buyrecipe;
--  id:2.2/64b3a1fdc448171c/BuyRecipe/1/0/test_buyrecipe/
   procedure Test_BuyRecipe_test_buyrecipe (Gnattest_T : in out Test) is
   procedure BuyRecipe (RecipeIndex: Unbounded_String) renames Wrap_Test_BuyRecipe_64b3a1_e0c4a8;
--  end read only

      pragma Unreferenced (Gnattest_T);
      Amount: Positive := Positive(Known_Recipes.Length);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Positive :=
        Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1;

   begin

      for I in Recipes_List.Iterate loop
         if Recipes_List(I).BaseType = BaseType and then Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
        Positive_Container.No_Index then
            BuyRecipe(Recipes_Container.Key(I));
            exit;
         end if;
      end loop;
      Assert(Positive(Known_Recipes.Length) = Amount + 1, "Failed to buy recipe from base.");

--  begin read only
   end Test_BuyRecipe_test_buyrecipe;
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
end Bases.Trade.Test_Data.Tests;
