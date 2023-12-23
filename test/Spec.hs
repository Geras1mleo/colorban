import BoardObject (BoardObject (color, coordinate))
import Game (getSelectedRobot, isSolved, move, selectNextRobot)
import GameData (Button (Button, bcoordinate, isPressed), Coin (Coin, coinCoordinate, coinName, value), Crate (Crate, ccolor, ccoordinate, cname, weight), Direction, Door (Door, buttons, dname, doorCoordinate, isOpened), FColor (Blue, Purple, Red), IntF (Natural), Layout (Layout, dimentions, tiles), Level (..), Robot (Robot, rcolor, rcoordinate, rname, selected, strength), Spot (Spot, durability, spcolor, spcoordinate, spname), Storage (Storage, scolor, scoordinate, sname), Tile (Tile, tcoordinate, ttype), TileType (Normal, TileRight, Wall))
import Static (down, left, right, up)
import Test.Hspec (describe, hspec, it, shouldBe)

move' :: Level -> [Direction] -> Level
move' = foldl move

main :: IO ()
main = hspec $ do
  describe "Prelude" $ do
    it "isSolved should be False => not enough coins collected" $ do
      isSolved (move level1 right) `shouldBe` False

    it "isSolved should be False => coins collected but crates not in storages" $ do
      isSolved (move' level1 [down, left]) `shouldBe` False

    it "isSolved should be True => coins collected" $ do
      isSolved (move' level1 [right, down, left]) `shouldBe` True

    it "Cannot push heavy crate => robot strength = 2 & crate weight = 5" $ do
      coordinate (getSelectedRobot (move' level3 [right, right])) `shouldBe` (2, 1)

    it "Can push multiple crates" $ do
      coordinate (getSelectedRobot (move' (selectNextRobot level3) [right, right])) `shouldBe` (3, 2)

    it "Door should be opened => crate on button" $ do
      isOpened (head (doors (move level2 down))) `shouldBe` True

    it "Door should be opened => robot on button" $ do
      isOpened (head (doors (move' level2 [left, down, down, right]))) `shouldBe` True

    it "Cannot move left, up or down => robot on TileRight" $ do
      coordinate (getSelectedRobot (move' level4 [right, left, up, down])) `shouldBe` (2, 2)

    it "Can move right => robot on TileRight" $ do
      coordinate (getSelectedRobot (move' level4 [right, right])) `shouldBe` (3, 2)

    it "Red spot does not color robot => durabily was 1" $ do
      color (getSelectedRobot (move' level5 [right, right, left])) `shouldBe` Blue

level1, level2, level3, level4, level5 :: Level
level1 =
  Level
    { levelName = "trivial 1",
      layout =
        Layout
          { tiles =
              [ Tile {ttype = Wall, tcoordinate = (0, 0)},
                Tile {ttype = Wall, tcoordinate = (1, 0)},
                Tile {ttype = Wall, tcoordinate = (2, 0)},
                Tile {ttype = Wall, tcoordinate = (3, 0)},
                Tile {ttype = Wall, tcoordinate = (4, 0)},
                Tile {ttype = Wall, tcoordinate = (0, 1)},
                Tile {ttype = Normal, tcoordinate = (1, 1)},
                Tile {ttype = Normal, tcoordinate = (2, 1)},
                Tile {ttype = Normal, tcoordinate = (3, 1)},
                Tile {ttype = Wall, tcoordinate = (4, 1)},
                Tile {ttype = Wall, tcoordinate = (0, 2)},
                Tile {ttype = Normal, tcoordinate = (1, 2)},
                Tile {ttype = Normal, tcoordinate = (2, 2)},
                Tile {ttype = Normal, tcoordinate = (3, 2)},
                Tile {ttype = Wall, tcoordinate = (4, 2)},
                Tile {ttype = Wall, tcoordinate = (0, 3)},
                Tile {ttype = Wall, tcoordinate = (1, 3)},
                Tile {ttype = Wall, tcoordinate = (2, 3)},
                Tile {ttype = Wall, tcoordinate = (3, 3)},
                Tile {ttype = Wall, tcoordinate = (4, 3)}
              ],
            dimentions = (5, 4)
          },
      robots =
        [ Robot
            { rname = "Robot 1",
              rcoordinate = (1, 1),
              rcolor = Blue,
              selected = True,
              strength = 1 / 0
            }
        ],
      storages =
        [ Storage
            { sname = "Blauwe opslagplaats",
              scoordinate = (3, 1),
              scolor = Blue
            }
        ],
      crates =
        [ Crate
            { cname = "Blauwe kist",
              ccoordinate = (2, 1),
              ccolor = Blue,
              weight = 1.0
            }
        ],
      spots = [],
      doors = [],
      platforms = [],
      coins =
        [ Coin {coinName = "coin 1", coinCoordinate = (1, 2), value = 5},
          Coin {coinName = "coin 2", coinCoordinate = (2, 2), value = 2}
        ],
      requiredCoins = 5,
      collectedCoins = 0
    }
level2 =
  Level
    { levelName = "trivial 2",
      layout =
        Layout
          { tiles =
              [ Tile {ttype = Wall, tcoordinate = (0, 0)},
                Tile {ttype = Wall, tcoordinate = (1, 0)},
                Tile {ttype = Wall, tcoordinate = (2, 0)},
                Tile {ttype = Wall, tcoordinate = (3, 0)},
                Tile {ttype = Wall, tcoordinate = (4, 0)},
                Tile {ttype = Wall, tcoordinate = (5, 0)},
                Tile {ttype = Wall, tcoordinate = (6, 0)},
                Tile {ttype = Wall, tcoordinate = (7, 0)},
                Tile {ttype = Wall, tcoordinate = (8, 0)},
                Tile {ttype = Wall, tcoordinate = (0, 1)},
                Tile {ttype = Normal, tcoordinate = (1, 1)},
                Tile {ttype = Normal, tcoordinate = (2, 1)},
                Tile {ttype = Normal, tcoordinate = (3, 1)},
                Tile {ttype = Wall, tcoordinate = (4, 1)},
                Tile {ttype = Normal, tcoordinate = (5, 1)},
                Tile {ttype = Normal, tcoordinate = (6, 1)},
                Tile {ttype = Normal, tcoordinate = (7, 1)},
                Tile {ttype = Wall, tcoordinate = (8, 1)},
                Tile {ttype = Wall, tcoordinate = (0, 2)},
                Tile {ttype = Normal, tcoordinate = (1, 2)},
                Tile {ttype = Normal, tcoordinate = (2, 2)},
                Tile {ttype = Normal, tcoordinate = (3, 2)},
                Tile {ttype = Normal, tcoordinate = (4, 2)},
                Tile {ttype = Normal, tcoordinate = (5, 2)},
                Tile {ttype = Normal, tcoordinate = (6, 2)},
                Tile {ttype = Normal, tcoordinate = (7, 2)},
                Tile {ttype = Wall, tcoordinate = (8, 2)},
                Tile {ttype = Wall, tcoordinate = (0, 3)},
                Tile {ttype = Normal, tcoordinate = (1, 3)},
                Tile {ttype = Normal, tcoordinate = (2, 3)},
                Tile {ttype = Normal, tcoordinate = (3, 3)},
                Tile {ttype = Wall, tcoordinate = (4, 3)},
                Tile {ttype = Normal, tcoordinate = (5, 3)},
                Tile {ttype = Normal, tcoordinate = (6, 3)},
                Tile {ttype = Normal, tcoordinate = (7, 3)},
                Tile {ttype = Wall, tcoordinate = (8, 3)},
                Tile {ttype = Wall, tcoordinate = (0, 4)},
                Tile {ttype = Wall, tcoordinate = (1, 4)},
                Tile {ttype = Wall, tcoordinate = (2, 4)},
                Tile {ttype = Wall, tcoordinate = (3, 4)},
                Tile {ttype = Wall, tcoordinate = (4, 4)},
                Tile {ttype = Wall, tcoordinate = (5, 4)},
                Tile {ttype = Wall, tcoordinate = (6, 4)},
                Tile {ttype = Wall, tcoordinate = (7, 4)},
                Tile {ttype = Wall, tcoordinate = (8, 4)}
              ],
            dimentions = (9, 5)
          },
      robots =
        [ Robot
            { rname = "Robot 1",
              rcoordinate = (2, 1),
              rcolor = Purple,
              selected = True,
              strength = 1 / 0
            }
        ],
      storages =
        [ Storage
            { sname = "Blauwe opslagplaats",
              scoordinate = (7, 2),
              scolor = Purple
            },
          Storage
            { sname = "purple opslagplaats",
              scoordinate = (1, 3),
              scolor = Purple
            }
        ],
      crates =
        [ Crate
            { cname = "Blauwe kist",
              ccoordinate = (6, 2),
              ccolor = Purple,
              weight = 1.0
            },
          Crate
            { cname = "purple kist",
              ccoordinate = (2, 2),
              ccolor = Purple,
              weight = 1.0
            }
        ],
      spots = [],
      doors =
        [ Door
            { dname = "door 1",
              doorCoordinate = (4, 2),
              buttons = [Button {bcoordinate = (2, 3), isPressed = False}],
              isOpened = False
            }
        ],
      platforms = [],
      coins = [],
      requiredCoins = 0,
      collectedCoins = 0
    }
level3 =
  Level
    { levelName = "trivial 3",
      layout =
        Layout
          { tiles =
              [ Tile {ttype = Wall, tcoordinate = (0, 0)},
                Tile {ttype = Wall, tcoordinate = (1, 0)},
                Tile {ttype = Wall, tcoordinate = (2, 0)},
                Tile {ttype = Wall, tcoordinate = (3, 0)},
                Tile {ttype = Wall, tcoordinate = (4, 0)},
                Tile {ttype = Wall, tcoordinate = (5, 0)},
                Tile {ttype = Wall, tcoordinate = (6, 0)},
                Tile {ttype = Wall, tcoordinate = (7, 0)},
                Tile {ttype = Wall, tcoordinate = (0, 1)},
                Tile {ttype = Normal, tcoordinate = (1, 1)},
                Tile {ttype = Normal, tcoordinate = (2, 1)},
                Tile {ttype = Normal, tcoordinate = (3, 1)},
                Tile {ttype = Normal, tcoordinate = (4, 1)},
                Tile {ttype = Normal, tcoordinate = (5, 1)},
                Tile {ttype = Normal, tcoordinate = (6, 1)},
                Tile {ttype = Wall, tcoordinate = (7, 1)},
                Tile {ttype = Wall, tcoordinate = (0, 2)},
                Tile {ttype = Normal, tcoordinate = (1, 2)},
                Tile {ttype = Normal, tcoordinate = (2, 2)},
                Tile {ttype = Normal, tcoordinate = (3, 2)},
                Tile {ttype = Normal, tcoordinate = (4, 2)},
                Tile {ttype = Normal, tcoordinate = (5, 2)},
                Tile {ttype = Normal, tcoordinate = (6, 2)},
                Tile {ttype = Wall, tcoordinate = (7, 2)},
                Tile {ttype = Wall, tcoordinate = (0, 3)},
                Tile {ttype = Wall, tcoordinate = (1, 3)},
                Tile {ttype = Wall, tcoordinate = (2, 3)},
                Tile {ttype = Wall, tcoordinate = (3, 3)},
                Tile {ttype = Wall, tcoordinate = (4, 3)},
                Tile {ttype = Wall, tcoordinate = (5, 3)},
                Tile {ttype = Wall, tcoordinate = (6, 3)},
                Tile {ttype = Wall, tcoordinate = (7, 3)}
              ],
            dimentions = (8, 4)
          },
      robots =
        [ Robot
            { rname = "Robot 1",
              rcoordinate = (1, 1),
              rcolor = Blue,
              selected = True,
              strength = 2.0
            },
          Robot
            { rname = "Robot 2",
              rcoordinate = (1, 2),
              rcolor = Blue,
              selected = False,
              strength = 1 / 0
            }
        ],
      storages =
        [ Storage
            { sname = "Blauwe opslagplaats 1",
              scoordinate = (4, 1),
              scolor = Blue
            },
          Storage
            { sname = "Blauwe opslagplaats 2",
              scoordinate = (4, 2),
              scolor = Blue
            },
          Storage
            { sname = "Blauwe opslagplaats 3",
              scoordinate = (5, 2),
              scolor = Blue
            }
        ],
      crates =
        [ Crate
            { cname = "Blauwe kist 1",
              ccoordinate = (3, 1),
              ccolor = Blue,
              weight = 5.0
            },
          Crate
            { cname = "Blauwe kist 2",
              ccoordinate = (3, 2),
              ccolor = Blue,
              weight = 1.0
            },
          Crate
            { cname = "Blauwe kist 3",
              ccoordinate = (4, 2),
              ccolor = Blue,
              weight = 1.0
            }
        ],
      spots = [],
      doors = [],
      platforms = [],
      coins = [],
      requiredCoins = 0,
      collectedCoins = 0
    }
level4 =
  Level
    { levelName = "trivial 4",
      layout =
        Layout
          { tiles =
              [ Tile {ttype = Wall, tcoordinate = (0, 0)},
                Tile {ttype = Wall, tcoordinate = (1, 0)},
                Tile {ttype = Wall, tcoordinate = (2, 0)},
                Tile {ttype = Wall, tcoordinate = (3, 0)},
                Tile {ttype = Wall, tcoordinate = (4, 0)},
                Tile {ttype = Wall, tcoordinate = (0, 1)},
                Tile {ttype = Normal, tcoordinate = (1, 1)},
                Tile {ttype = Normal, tcoordinate = (2, 1)},
                Tile {ttype = Normal, tcoordinate = (3, 1)},
                Tile {ttype = Wall, tcoordinate = (4, 1)},
                Tile {ttype = Wall, tcoordinate = (0, 2)},
                Tile {ttype = Normal, tcoordinate = (1, 2)},
                Tile {ttype = TileRight, tcoordinate = (2, 2)},
                Tile {ttype = Normal, tcoordinate = (3, 2)},
                Tile {ttype = Wall, tcoordinate = (4, 2)},
                Tile {ttype = Wall, tcoordinate = (0, 3)},
                Tile {ttype = Normal, tcoordinate = (1, 3)},
                Tile {ttype = Normal, tcoordinate = (2, 3)},
                Tile {ttype = Normal, tcoordinate = (3, 3)},
                Tile {ttype = Wall, tcoordinate = (4, 3)},
                Tile {ttype = Wall, tcoordinate = (0, 4)},
                Tile {ttype = Wall, tcoordinate = (1, 4)},
                Tile {ttype = Wall, tcoordinate = (2, 4)},
                Tile {ttype = Wall, tcoordinate = (3, 4)},
                Tile {ttype = Wall, tcoordinate = (4, 4)}
              ],
            dimentions = (5, 5)
          },
      robots =
        [ Robot
            { rname = "Robot 1",
              rcoordinate = (1, 2),
              rcolor = Blue,
              selected = True,
              strength = 2.0
            }
        ],
      storages = [],
      crates = [],
      spots = [],
      doors = [],
      platforms = [],
      coins = [],
      requiredCoins = 0,
      collectedCoins = 0
    }
level5 =
  Level
    { levelName = "trivial 5",
      layout =
        Layout
          { tiles =
              [ Tile {ttype = Wall, tcoordinate = (0, 0)},
                Tile {ttype = Wall, tcoordinate = (1, 0)},
                Tile {ttype = Wall, tcoordinate = (2, 0)},
                Tile {ttype = Wall, tcoordinate = (3, 0)},
                Tile {ttype = Wall, tcoordinate = (4, 0)},
                Tile {ttype = Wall, tcoordinate = (5, 0)},
                Tile {ttype = Wall, tcoordinate = (0, 1)},
                Tile {ttype = Normal, tcoordinate = (1, 1)},
                Tile {ttype = Normal, tcoordinate = (2, 1)},
                Tile {ttype = Normal, tcoordinate = (3, 1)},
                Tile {ttype = Normal, tcoordinate = (4, 1)},
                Tile {ttype = Wall, tcoordinate = (5, 1)},
                Tile {ttype = Wall, tcoordinate = (0, 2)},
                Tile {ttype = Wall, tcoordinate = (1, 2)},
                Tile {ttype = Wall, tcoordinate = (2, 2)},
                Tile {ttype = Wall, tcoordinate = (3, 2)},
                Tile {ttype = Wall, tcoordinate = (4, 2)},
                Tile {ttype = Wall, tcoordinate = (5, 2)}
              ],
            dimentions = (6, 3)
          },
      robots =
        [ Robot
            { rname = "Robot 1",
              rcoordinate = (1, 1),
              rcolor = Blue,
              selected = True,
              strength = 2.0
            }
        ],
      storages = [],
      crates = [],
      spots =
        [ Spot
            { spname = "Rode vlek",
              spcoordinate = (2, 1),
              spcolor = Red,
              durability = Natural 1
            },
          Spot
            { spname = "Blauwe vlek",
              spcoordinate = (3, 1),
              spcolor = Blue,
              durability = Natural 1
            }
        ],
      doors = [],
      platforms = [],
      coins = [],
      requiredCoins = 0,
      collectedCoins = 0
    }
