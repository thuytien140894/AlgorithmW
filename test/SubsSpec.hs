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
                    let s2 = Subs $ Map.fromList [(1, Boolean), (2, Nat), (3, TVar 2)]
                    let s3 = Subs $ Map.fromList [(1, Nat), (2, Nat), (3, TVar 2)]
                    compose s1 s2 `shouldBe` s3

            context "{4:Nat, 2:Nat, 5:Nat} and {0:(Nat->2), 1:(Nat->2), 2:3, 3:2}" $ 
                it "should be {0:(Nat->2), 1:(Nat->2), 2:Nat, 3:2, 4:Nat, 5:Nat}" $ do
                    let s1 = Subs $ Map.fromList [(4, Nat), (2, Nat), (5, Nat)]
                    let s2 = Subs $ Map.fromList [(0, Arr Nat (TVar 2)), (1, Arr Nat (TVar 2)), (2, TVar 3), (3,TVar 2)]
                    let s3 = Subs $ Map.fromList [(0,Arr Nat (TVar 2)),(1,Arr Nat (TVar 2)),(2,Nat),(3,TVar 2),(4,Nat),(5,Nat)]
                    compose s1 s2 `shouldBe` s3

            context "(s1 s2) r" $ 
                it "should be s1 (s2 r)" $ do
                    let s1 = Subs $ Map.fromList [(1, TVar 2), (2, TVar 3)]
                    let s2 = Subs $ Map.fromList [(1, Boolean), (2, Nat), (3, TVar 2)]
                    let r  = TypeEnv $ Map.fromList [("m", Scheme (TVar 2)), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) (TVar 3))))]
                    let res1 = subs (compose s1 s2) r 
                    let res2 = subs s1 (subs s2 r)
                    res1 `shouldBe` res2

        describe "applies to type environment" $
            context "{Boolean/x, Nat/y, Nat/z} to {m: y, n: forall [x,y] y->z}" $
                it "should be {m: Nat, n: forall [x,y] y->Nat}" $ do 
                    let r = TypeEnv $ Map.fromList [("m", Scheme (TVar 2)), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) (TVar 3))))]
                    let s = Subs $ Map.fromList [(2, Nat), (1, Boolean), (3, Nat)]
                    let res = TypeEnv $ Map.fromList [("m", Scheme Nat), ("n", ForAll [TVar 1, TVar 2] (Scheme (Arr (TVar 2) Nat)))]
                    subs s r `shouldBe` res