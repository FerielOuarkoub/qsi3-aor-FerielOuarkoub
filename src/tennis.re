type player =
  | PlayerOne
  | PlayerTwo;

type point = int;

let other = player =>
  switch (player) {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

let string_of_player = player =>
  switch (player) {
  | PlayerOne => "Player 1"
  | PlayerTwo => "Player 2"
  };

let foo = PlayerOne;
