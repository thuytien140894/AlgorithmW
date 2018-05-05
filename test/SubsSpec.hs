module SubsSpec where 

    import Substitution 
    import Syntax
    import Type
    import TypeEnv

    import Test.Hspec 
    import qualified Data.Map as Map (fromList)

    spec :: Spec 
    spec = do
        describe "composes" $ do 
            context "{y/x, z/y} and {Bool/x, Nat/y, y/z}" $ 
                it "should be {Nat/x, Nat/y, y/z}" $ do
                    let s1 = Subs $ Map.fromList [(1, TVar 2), (2, TVar 3)]
                    let s2 = Subs $ Map.fromList [(1, Bool), (2, Nat), (3, TVar 2)]
                    let s3 = Subs (Map.fromList [(1, Nat), (2, Nat), (3, TVar 2)])
                    compose s1 s2 `shouldBe` s3

            context "(s1 s2) r" $ 
                it "should be s1 (s2 r)" $ do
                    let s1 = Subs $ Map.fromList [(1, TVar 2), (2, TVar 3)]
                    let s2 = Subs $ Map.fromList [(1, Bool), (2, Nat), (3, TVar 2)]
                    let r  = TypeEnv (Map.fromList [("m", Scheme (TVar 2)), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) (TVar 3))))])
                    let res1 = subsTEnv (compose s1 s2) r 
                    let res2 = subsTEnv s1 (subsTEnv s2 r)
                    res1 `shouldBe` res2

        describe "applies to type environment" $
            context "{Bool/x, Nat/y, Nat/z} to {m: y, n: forall [x,y] y->z}" $
                it "should be {m: Nat, n: forall [x,y] y->Nat}" $ do 
                    let r = TypeEnv (Map.fromList [("m", Scheme (TVar 2)), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) (TVar 3))))])
                    let s = Subs $ Map.fromList [(2, Nat), (1, Bool), (3, Nat)]
                    let res = TypeEnv (Map.fromList [("m", Scheme Nat), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) Nat)))])
                    subsTEnv s r `shouldBe` res