module TypeInferSpec where

    import Error
    import GlobalState 
    import Parser
    import Substitution as Subs
    import Type
    import TypeEnv
    import TypeInferer
    import TypeScheme

    import qualified TypeEnv

    import Test.Hspec

    import qualified Data.Map as Map

    spec :: Spec 
    spec = do 
        describe "instantiates" $ do
            context "forall [1,2,3] (1->(2->3))" $ 
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

        describe "generalizes" $ do
            context "{x: forall [2,3] 1->2->3} and 1->4" $ 
                it "should be forall [4] 1->4" $ do
                    let r = TypeEnv $ Map.fromList [("x", ForAll [TVar 2,TVar 3] (Scheme (Arr (TVar 1) (Arr (TVar 2) (TVar 3)))))]
                    let t = Arr (TVar 1) (TVar 4)
                    generalize r t 
                    `shouldBe` ForAll [TVar 4] (Scheme (Arr (TVar 1) (TVar 4)))

            context "{x: forall [2,3] 1->2->3} and 1->4" $ 
                it "should be 1->4" $ do
                    let r = TypeEnv $ Map.fromList [("x", ForAll [TVar 2,TVar 3] (Scheme (Arr (TVar 1) (Arr (TVar 2) (TVar 3))))),("y", Scheme (TVar 4))]
                    let t = Arr (TVar 1) (TVar 4)
                    generalize r t 
                    `shouldBe` Scheme (Arr (TVar 1) (TVar 4))
                             
        describe "unifies" $ do
            context "Bool and Nat" $
                it "should be error" $ 
                    unwrap (unify Bool Nat) 
                    `shouldBe` Left (Mismatch Bool Nat)

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
                    `shouldBe` Left (Mismatch Nat (Arr (TVar 1) (TVar 2)))

            context "TVar 1 and TVar 1->Nat" $
                it "should be error" $
                    unwrap (unify (TVar 1) (Arr (TVar 1) Nat)) 
                    `shouldBe` Left (Occur 1 (Arr (TVar 1) Nat))

            context "TVar 1->TVar 3 and TVar 1->TVar 2" $
                it "should be 2->3, 3->2" $
                    unwrap (unify (Arr (TVar 1) (TVar 3)) (Arr (TVar 1) (TVar 2)))  
                    `shouldBe` Right (Subs (Map.fromList [(3,TVar 2)])) 

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

            context "succ 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "succ 0"
                    typeInfer e `shouldBe` Right Nat 

            context "(\\x. x) succ 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. x) succ 0"
                    typeInfer e `shouldBe` Right Nat 

            context "(\\x. if iszero x then 0 else succ 0) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. if iszero x then 0 else succ 0) 0"
                    typeInfer e `shouldBe` Right Nat 

            context "if true then true else 0" $ 
                it "should be error" $ do
                    let Right e = parseExpr "if true then true else 0"
                    typeInfer e `shouldBe` Left (Mismatch Bool Nat)

            context "(\\x. if x then 0 else succ 0)" $ 
                it "should be Bool->Nat" $ do
                    let Right e = parseExpr "(\\x. if x then 0 else succ 0)"
                    typeInfer e `shouldBe` Right (Arr Bool Nat)

            context "(\\m. (\\x. if x then m 0 else m (succ 0))))" $ 
                it "should be (Nat->4)->(Bool->4)" $ do
                    let Right e = parseExpr "(\\m. (\\x. if x then m 0 else m (succ 0))))"
                    typeInfer e `shouldBe` Right (Arr (Arr Nat (TVar 4)) (Arr Bool (TVar 4)))

            context "((\\m. (\\x. if true then x else m x)) (\\x. x)) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "((\\m. (\\x. if true then x else m x)) (\\x. x)) 0"
                    typeInfer e `shouldBe` Right Nat

            context "(\\m. (\\x. if true then x else m x))" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\m. (\\x. if true then x else m x))"
                    typeInfer e `shouldBe` Right (Arr (Arr (TVar 2) (TVar 2)) (Arr (TVar 2) (TVar 2)))

            context "((\\m. (\\x. if m then x else succ x)) true) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "((\\m. (\\x. if m then x else succ x)) true) 0"
                    typeInfer e `shouldBe` Right Nat

            context "(\\x. if true then x else succ x)" $ 
                it "should be Nat->Nat" $ do
                    let Right e = parseExpr "(\\x. if true then x else succ x)"
                    typeInfer e `shouldBe` Right (Arr Nat Nat)

            context "(\\x. x 0) (\\x. succ x)" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. x 0) (\\x. succ x)"
                    typeInfer e `shouldBe` Right Nat 

            context "(\\x. x true) (\\x. succ x)" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "(\\x. x true) (\\x. succ x)"
                    typeInfer e `shouldBe` Left (Mismatch Bool Nat)

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
                    typeInfer e `shouldBe` Right Bool

            context "(\\m. (\\x. x) m) (\\x. x)" $ 
                it "should be Bool" $ do
                    let Right e = parseExpr "(\\m. (\\x. x) m) (\\x. x)"
                    typeInfer e `shouldBe` Right (Arr (TVar 3) (TVar 3))

            context "(\\m. (\\x. x m) (\\x. x)) 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "((\\m. (\\x. x m)) 0) (\\x. x)"
                    typeInfer e `shouldBe` Right Nat

            context "(\\n. (\\m. (\\x. x) m) n) (\\x. x)" $ 
                it "should be Bool" $ do
                    let Right e = parseExpr "(\\n. (\\m. (\\x. x) m) n) (\\x. x)"
                    typeInfer e `shouldBe` Right (Arr (TVar 5) (TVar 5))

            context "(\\n. (\\m. (\\x. n (m 0)))))" $ 
                it "should be (3->4)->((Nat->3)->(2->4))" $ do
                    let Right e = parseExpr "(\\n. (\\m. (\\x. n (m 0)))))"
                    typeInfer e `shouldBe` Right (Arr (Arr (TVar 3) (TVar 4)) (Arr (Arr Nat (TVar 3)) (Arr (TVar 2) (TVar 4))))

            context "let m = \\x. x in m m" $ 
                it "should be 4->4" $ do
                    let Right e = parseExpr "let m = \\x. x in m m"
                    typeInfer e `shouldBe` Right (Arr (TVar 4) (TVar 4))

            context "let m = (\\x. let y = x in y) in m m" $ 
                it "should be 4->4" $ do
                    let Right e = parseExpr "let m = (\\x. let y = x in y) in m m"
                    typeInfer e `shouldBe` Right (Arr (TVar 4) (TVar 4))

            context "let m = (\\x. let y = x in y) in (m m) succ 0" $ 
                it "should be Nat" $ do
                    let Right e = parseExpr "let m = (\\x. let y = x in y) in (m m) succ 0"
                    typeInfer e `shouldBe` Right Nat

            context "\\x. x x" $ 
                it "should be occur error" $ do
                    let Right e = parseExpr "\\x. x x"
                    typeInfer e `shouldBe` Left (Occur 0 (Arr (TVar 0) (TVar 1)))

            context "\\m. let y = m in let x = y true in x" $ 
                it "should be Bool->1->1" $ do
                    let Right e = parseExpr "\\m. let y = m in let x = y true in x"
                    typeInfer e `shouldBe` Right (Arr (Arr Bool (TVar 1)) (TVar 1))