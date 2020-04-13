import ClassyPrelude
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified GameLogic as GL

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

prop_allSubsets :: Property
prop_allSubsets =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 16) Gen.alpha
    length (GL.allSubsets xs) === 2 ^ length xs

genCard :: (MonadGen m) => m GL.Card
genCard = do
  val <- Gen.enumBounded
  season <- Gen.enumBounded
  return $ GL.Card val season

prop_canHarvestWithNum_singleCard :: Property
prop_canHarvestWithNum_singleCard =
  property $ do
    card@(GL.Card val season) <- forAll genCard
    True === GL.canHarvestWithNum (GL.toNumberValWithFoolAsOne val) [GL.fromCard card]

hedgehogTests :: IO Bool
hedgehogTests =
  checkParallel $$(discover)

main :: IO ()
main = do
  _ <- hedgehogTests
  return ()
