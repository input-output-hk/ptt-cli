module Coverage.Spec where

import Cardano.PTT.CLI.Coverage
import Test.Tasty
import Test.Tasty.HUnit

import Data.Map qualified as Map

coverageTests :: TestTree
coverageTests =
  testGroup
    "flattenCoverage tests"
    [ testCase "covered with all the statuses" $
        let actual = flattenCoverage $ CoverageGroup covered [] []
             where
              covered =
                [ CoverageEntry 1 1 2 2 Nothing "f1"
                , CoverageEntry 3 3 4 4 (Just True) "f1"
                , CoverageEntry 5 5 6 6 (Just False) "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain HasBeenHere NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 3
                            , _covLocEndLine = 3
                            , _covLocStartCol = 4
                            , _covLocEndCol = 4
                            }
                        , OnChain HasBeenTrue NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 5
                            , _covLocEndLine = 5
                            , _covLocStartCol = 6
                            , _covLocEndCol = 6
                            }
                        , OnChain HasBeenFalse NotIgnored
                        )
                      ]
                }
         in actual @?= expected
    , testCase "uncovered with all the statuses" $
        let actual = flattenCoverage $ CoverageGroup [] uncovered []
             where
              uncovered =
                [ CoverageEntry 1 1 2 2 Nothing "f1"
                , CoverageEntry 3 3 4 4 (Just True) "f1"
                , CoverageEntry 5 5 6 6 (Just False) "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain NotCovered NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 3
                            , _covLocEndLine = 3
                            , _covLocStartCol = 4
                            , _covLocEndCol = 4
                            }
                        , OnChain NotCovered NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 5
                            , _covLocEndLine = 5
                            , _covLocStartCol = 6
                            , _covLocEndCol = 6
                            }
                        , OnChain NotCovered NotIgnored
                        )
                      ]
                }
         in actual @?= expected
    , testCase "non overlapped covered and uncovered" $
        let actual = flattenCoverage $ CoverageGroup covered uncovered []
             where
              uncovered =
                [ CoverageEntry 1 1 2 2 (Just True) "f1"
                ]
              covered =
                [ CoverageEntry 3 3 4 4 (Just True) "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain NotCovered NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 3
                            , _covLocEndLine = 3
                            , _covLocStartCol = 4
                            , _covLocEndCol = 4
                            }
                        , OnChain HasBeenTrue NotIgnored
                        )
                      ]
                }
         in actual @?= expected
    , testCase "ignored with all the statuses" $
        let actual = flattenCoverage $ CoverageGroup [] [] ignored
             where
              ignored =
                [ CoverageEntry 1 1 2 2 Nothing "f1"
                , CoverageEntry 3 3 4 4 (Just True) "f1"
                , CoverageEntry 5 5 6 6 (Just False) "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain HasBeenHere AlwaysIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 3
                            , _covLocEndLine = 3
                            , _covLocStartCol = 4
                            , _covLocEndCol = 4
                            }
                        , OnChain HasBeenTrue IgnoredIfTrue
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 5
                            , _covLocEndLine = 5
                            , _covLocStartCol = 6
                            , _covLocEndCol = 6
                            }
                        , OnChain HasBeenFalse IgnoredIfFalse
                        )
                      ]
                }
         in actual @?= expected
    , testCase "overlapped covered and uncovered" $
        let actual = flattenCoverage $ CoverageGroup covered uncovered []
             where
              covered =
                [ CoverageEntry 1 1 2 2 Nothing "f1"
                , CoverageEntry 3 3 4 4 (Just True) "f1"
                , CoverageEntry 5 5 6 6 (Just False) "f1"
                ]
              uncovered =
                [ CoverageEntry 1 1 2 2 (Just False) "f1"
                , CoverageEntry 3 3 4 4 Nothing "f1"
                , CoverageEntry 5 5 6 6 Nothing "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain HasBeenHere NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 3
                            , _covLocEndLine = 3
                            , _covLocStartCol = 4
                            , _covLocEndCol = 4
                            }
                        , OnChain HasBeenTrue NotIgnored
                        )
                      ,
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 5
                            , _covLocEndLine = 5
                            , _covLocStartCol = 6
                            , _covLocEndCol = 6
                            }
                        , OnChain HasBeenFalse NotIgnored
                        )
                      ]
                }
         in actual @?= expected
    , testCase "overlapped covered , uncovered, ignored" $
        let actual = flattenCoverage $ CoverageGroup covered uncovered ignored
             where
              covered =
                [ CoverageEntry 1 1 2 2 Nothing "f1"
                ]
              uncovered =
                [ CoverageEntry 1 1 2 2 (Just False) "f1"
                ]
              ignored =
                [ CoverageEntry 1 1 2 2 (Just False) "f1"
                ]
            expected =
              FlattenCoverage
                { unFlattenCoverage =
                    Map.fromList
                      [
                        ( CovLoc
                            { _covLocFile = "f1"
                            , _covLocStartLine = 1
                            , _covLocEndLine = 1
                            , _covLocStartCol = 2
                            , _covLocEndCol = 2
                            }
                        , OnChain HasBeenHere IgnoredIfFalse
                        )
                      ]
                }
         in actual @?= expected
    ]
