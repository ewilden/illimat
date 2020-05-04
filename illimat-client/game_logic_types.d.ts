type GameStateView = IGameStateView;

interface IGameStateView {
  illimatState: IllimatState;
  boardState: BoardStateView;
  playerState: [PlayerState, OtherPlayerView[]];
  deck: number;
  whoseTurn: WhoseTurn;
}

type IllimatState = IIllimatState;

interface IIllimatState {
  summerDir: Direction;
  numOkuses: number;
}

type Direction = "N" | "E" | "S" | "W";

type BoardStateView = IBoardStateView;

interface IBoardStateView {
  fieldN: FieldStateView;
  fieldE: FieldStateView;
  fieldS: FieldStateView;
  fieldW: FieldStateView;
}

type FieldStateView = IFieldStateView;

interface IFieldStateView {
  cards: CardStack[];
  luminary: LuminaryStateView;
}

type CardStack = ICardStack;

type ICardStack = [number[], Card[]];

type Card = ICard;

type ICard = [CardVal, CardSeason];

type CardVal = "Fool" | "Two" | "Three" | "Four" | "Five" | "Six" | "Seven" | "Eight" | "Nine" | "Ten" | "Knight" | "Queen" | "King";

type CardSeason = "CSummer" | "CSpring" | "CWinter" | "CAutumn" | "CStars";

type Season = "Summer" | "Spring" | "Winter" | "Autumn";

type LuminaryStateView = IFaceUpView | IFaceDownView | INoLuminaryView;

interface IFaceUpView {
  tag: "FaceUpView";
  contents: Luminary;
}

interface IFaceDownView {
  tag: "FaceDownView";
}

interface INoLuminaryView {
  tag: "NoLuminaryView";
}

type Luminary = "Union" | "Maiden" | "Rake" | "River" | "Changeling" | "Newborn" | "Forest_Queen" | "Children";

type PlayerState = IPlayerState;

interface IPlayerState {
  hand: Card[];
  numOkuses: number;
  luminaries: Luminary[];
  harvestPile: Card[];
}

type OtherPlayerView = IOtherPlayerView;

interface IOtherPlayerView {
  numCardsInHand: number;
  numOkuses: number;
  luminaries: Luminary[];
  harvestPileSize: number;
}

type WhoseTurn = IWhoseTurn;

type IWhoseTurn = [number, RakeTurn];

type RakeTurn = "NoRake" | "RakeBoth" | "RakeSow" | "RakeOtherMove";
