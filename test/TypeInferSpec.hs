module TypeInferSpec where

    import GlobalState 
    import Parser
    import Substitution as Subs
    import Type
    import TypeInferer

    import qualified TypeEnv

    import Test.Hspec

    import qualified Data.Map as Map

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

        describe "unifies" $ do
            context "Bool and Nat" $
                it "should be error" $ 
                    unwrap (unify Bool Nat) 
                    `shouldBe` Left "Incompatible types"

            context "Bool and TVar 1" $
                it "should be 1->Bool" $
                    unwrap (unify Bool (TVar 1)) 
                    `shouldBe` Right (Subs (Map.fromList [(1,Bool)]))

            context "TVar 1->Bool and Nat->TVar 2" $
                it "should be 1->Nat, 2->Bool" $ 
                    unwrap (unify (Arr (TVar 1) Bool) (Arr Nat (TVar 2)))
                    `shouldBe` Right (Subs (Map.fromList [(1,Nat),(2,Bool)])) 

            context "Nat and Nat" $
                it "should be empty" $
                    unwrap (unify Nat Nat) 
                    `shouldBe` Right Subs.empty 

            context "TVar 1 and TVar 1" $
                it "should be empty" $
                    unwrap (unify (TVar 1) (TVar 1)) 
                    `shouldBe` Right Subs.empty 

            context "Nat and TVar 1->TVar 2" $
                it "should be error" $
                    unwrap (unify Nat (Arr (TVar 1) (TVar 2))) 
                    `shouldBe` Left "Incompatible types"

            context "TVar 1 and TVar 1->Nat" $
                it "should be error" $
                    unwrap (unify (TVar 1) (Arr (TVar 1) Nat)) 
                    `shouldBe` Left "Occur check"

            context "TVar 1->TVar 3 and TVar 1->TVar 2" $
                it "should be 2->3, 3->2" $
                    unwrap (unify (Arr (TVar 1) (TVar 3)) (Arr (TVar 1) (TVar 2)))  
                    `shouldBe` Right (Subs (Map.fromList [(2,TVar 3),(3,TVar 2)])) 

            context "TVar 1->TVar 1 and Nat->TVar 2" $
                it "should be 1->Nat, 2->Nat" $
                    unwrap (unify (Arr (TVar 1) (TVar 1)) (Arr Nat (TVar 2)))  
                    `shouldBe` Right (Subs (Map.fromList [(1,Nat),(2,Nat)])) 

        describe "infers type for" $ do 
            context "\\x. 0" $ 
                it "should be (0->Nat)" $ do
                    let Right e = parseExpr "\\x. 0"
                    typeInfer e `shouldBe` Right (Arr (TVar 0) Nat)

            context "(\\x. 0) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. 0) 0"
                    typeInfer e `shouldBe` Right Nat

            context "(\\x. x) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. x) 0"
                    typeInfer e `shouldBe` Right Nat

            context "(\\x. x) (\\x. 0)" $ 
                it "should be (1->Nat)" $ do
                    let Right e = parseExpr "(\\x. x) (\\x. 0)"
                    typeInfer e `shouldBe` Right (Arr (TVar 1) Nat)

            context "(\\x. x 0) (\\x. x)" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. x 0) (\\x. x)"
                    typeInfer e `shouldBe` Right Nat

            context "(\\m. (\\x. x 0) m) (\\x. x)" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\m. (\\x. x 0) m) (\\x. x)"
                    typeInfer e `shouldBe` Right Nat

            context "(\\m. (\\x. x) m) true" $ 
                it "should be Bool" $ do
                    let Right e = parseExpr "(\\m. (\\x. x) m) true"
                    typeInfer e `shouldBe` Right (TVar 1)

            context "(\\n. (\\m. (\\x. n (m 0)))))" $ 
                it "should be (3->4)->((Nat->3)->(2->4))" $ do
                    let Right e = parseExpr "(\\n. (\\m. (\\x. n (m 0)))))"
                    typeInfer e `shouldBe` Right (Arr (Arr (TVar 3) (TVar 4)) (Arr (Arr Nat (TVar 3)) (Arr (TVar 2) (TVar 4))))