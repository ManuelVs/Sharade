module Sharade.Translator.Semantic.Substitutable where
  import qualified Data.Map as Map
  import qualified Data.Set as Set

  import Sharade.Translator.Semantic.Type
  import Sharade.Translator.Semantic.TypeEnv

  type Subst = Map.Map TVar Type

  class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar
  
  instance Substitutable Type where
    apply _ (TCon a)       = TCon a
    apply s t@(TVar a)     = Map.findWithDefault t a s
    apply s (TList t)      = TList (apply s t)
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

    ftv TCon{}         = Set.empty
    ftv (TVar a)       = Set.singleton a
    ftv (TList t)      = ftv t
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

  instance Substitutable Scheme where
    apply s (Forall as t)   = Forall as $ apply s' t
                              where s' = foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

  instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

  instance Substitutable TypeEnv where
    apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env


  nullSubst :: Subst
  nullSubst = Map.empty

  compose :: Subst -> Subst -> Subst
  s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1
