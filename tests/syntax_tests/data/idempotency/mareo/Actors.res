type dir_1d =
  | Left
  | Right

type dir_2d =
  | North
  | South
  | East
  | West

type xy = {
  mutable x: float,
  mutable y: float,
}

type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

type pl_typ =
  | BigM
  | SmallM

type item_typ =
  | Mushroom
  | FireFlower
  | Star
  | Coin

type enemy_typ =
  | Goomba
  | GKoopa
  | RKoopa
  | GKoopaShell
  | RKoopaShell

type block_typ =
  | QBlock(item_typ)
  | QBlockUsed
  | Brick
  | UnBBlock
  | Cloud
  | Panel
  | Ground

type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

type part_typ =
  | GoombaSquish
  | BrickChunkL
  | BrickChunkR
  | Score100
  | Score200
  | Score400
  | Score800
  | Score1000
  | Score2000
  | Score4000
  | Score8000

type spawn_typ =
  | SPlayer(pl_typ, player_typ)
  | SEnemy(enemy_typ)
  | SItem(item_typ)
  | SBlock(block_typ)
