module TypeInferSpec where

    import GlobalState
    import Parser
    import Type
    import TypeInferer

    import qualified TypeEnv

    import Test.Hspec

    spec :: Spec 
    spec = do 
        describe "instantiates" $ do
            context "forall (1,2,3) (1->(2->3))" $ 
                it "should be (4->(5->6))" $ do
                    let c = ForAll [TVar 1,TVar 2,TVar 3] (Scheme (Arr (TVar 1) (Arr (TVar 2) (TVar 3))))
                    let s = GlobalEnv { varName = 4 }
                    runTyInfer' (instantiate c) s
                    `shouldBe` Right (Arr (TVar 4) (Arr (TVar 5) (TVar 6))) 

            context "forall (1,2,3) (forall (4) (4->(1->3))->2" $ 
                it "should be (8->(5->7))->6" $ do
                    let c = ForAll [TVar 1,TVar 2,TVar 3] (ForAll [TVar 4] (Scheme (Arr (Arr (TVar 4) (Arr (TVar 1) (TVar 3))) (TVar 2))))
                    let s = GlobalEnv { varName = 5 }
                    runTyInfer' (instantiate c) s
                    `shouldBe` Right (Arr (Arr (TVar 8) (Arr (TVar 5) (TVar 7))) (TVar 6))

        describe "infers type for" $  
            context "\\x. 0" $ 
                it "should be (0->Nat)" $ do
                    let Right e = parseExpr "\\x. 0"
                    typeInfer e `shouldBe` Right (Arr (TVar 0) Nat)