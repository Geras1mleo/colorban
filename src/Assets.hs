module Assets (imagesPath, getImageIndex) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Drawable (Drawable (..))

imagesPath :: [FilePath]
imagesPath =
  [ {-0-} "assets/empty_50x50.bmp",
    {-1 robot-}
    {-1-} "assets/robots/robot_red_50x50.bmp",
    {-2-} "assets/robots/robot_blue_50x50.bmp",
    {-3-} "assets/robots/robot_green_50x50.bmp",
    {-4-} "assets/robots/robot_orange_50x50.bmp",
    {-5-} "assets/robots/robot_gray_50x50.bmp",
    {-6-} "assets/robots/robot_purple_50x50.bmp",
    {-7 crate-}
    {-7-} "assets/crates/crate_red_50x50.bmp",
    {-8-} "assets/crates/crate_blue_50x50.bmp",
    {-9-} "assets/crates/crate_green_50x50.bmp",
    {-10-} "assets/crates/crate_orange_50x50.bmp",
    {-11-} "assets/crates/crate_gray_50x50.bmp",
    {-12-} "assets/crates/crate_purple_50x50.bmp",
    {-13 storage-}
    {-13-} "assets/storages/storage_red_50x50.bmp",
    {-14-} "assets/storages/storage_blue_50x50.bmp",
    {-15-} "assets/storages/storage_green_50x50.bmp",
    {-16-} "assets/storages/storage_orange_50x50.bmp",
    {-17-} "assets/storages/storage_gray_50x50.bmp",
    {-18-} "assets/storages/storage_purple_50x50.bmp",
    {-19 paint-}
    {-19-} "assets/spots/spot_red_50x50.bmp",
    {-20-} "assets/spots/spot_blue_50x50.bmp",
    {-21-} "assets/spots/spot_green_50x50.bmp",
    {-22-} "assets/spots/spot_orange_50x50.bmp",
    {-23-} "assets/spots/spot_gray_50x50.bmp",
    {-24-} "assets/spots/spot_purple_50x50.bmp",
    {-25 tiles-}
    {-25-} "assets/tiles/wall_50x50.bmp",
    {-26-} "assets/tiles/tile_left_50x50.bmp",
    {-27-} "assets/tiles/tile_right_50x50.bmp",
    {-28-} "assets/tiles/tile_up_50x50.bmp",
    {-29-} "assets/tiles/tile_down_50x50.bmp",
    {-30 objects-}
    {-30-} "assets/objects/door_locked_50x50.bmp",
    {-31-} "assets/objects/door_opened_50x50.bmp",
    {-32-} "assets/objects/platform_50x50.bmp",
    {-33-} "assets/objects/button_50x50.bmp",
    {-34-} "assets/objects/coin_50x50.bmp",
    {-35-} "assets/objects/coin_2_50x50.bmp"
  ]

getImageIndex :: (Drawable a) => a -> Int
getImageIndex obj = fromMaybe (-1) (elemIndex ("assets/" ++ getImagePath obj) imagesPath)
