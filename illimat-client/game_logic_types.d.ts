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

type Move = IHarvest | ISow | IStockpile;

interface IHarvest {
  tag: "Harvest";
  contents: [Card[], Direction, CardStack[]];
}

interface ISow {
  tag: "Sow";
  contents: [Card, Direction];
}

interface IStockpile {
  tag: "Stockpile";
  contents: [Card, Direction, CardStack[], number];
}

type FinishedGame = IFinishedGame;

type IFinishedGame = void[];

type StartingGame = IStartingGame;

type IStartingGame = void[];

type GameViewData = IGameViewRunning | IGameViewStarting | IGameViewFinished;

interface IGameViewRunning {
  tag: "GameViewRunning";
  contents: GameStateView;
}

interface IGameViewStarting {
  tag: "GameViewStarting";
  contents: StartingGame;
}

interface IGameViewFinished {
  tag: "GameViewFinished";
  contents: FinishedGame;
}

type GameView = IGameView;

interface IGameView {
  data: GameViewData;
}

type CreateGameResponse = ICreateGameResponse;

interface ICreateGameResponse {
  gameView: GameView;
  gameId: string;
}

type JoinGameResponse = IJoinGameResponse;

interface IJoinGameResponse {
  gameView: GameView;
}

type StartGameResponse = IStartGameResponse;

interface IStartGameResponse {
  gameView: GameView;
}

type JoinGameError = "JoinGameErrorAlreadyInGame" | "JoinGameErrorNoSuchGame" | "JoinGameErrorGameAlreadyRunning" | "JoinGameErrorGameAlreadyFinished";

type StartGameError = IStartGameErrorNotGameOwner | IStartGameErrorNotEnoughPlayers | IStartGameErrorNoSuchGame | IStartGameErrorNotInGame | IStartGameErrorGameAlreadyStarted | IStartGameErrorOther;

interface IStartGameErrorNotGameOwner {
  tag: "StartGameErrorNotGameOwner";
}

interface IStartGameErrorNotEnoughPlayers {
  tag: "StartGameErrorNotEnoughPlayers";
}

interface IStartGameErrorNoSuchGame {
  tag: "StartGameErrorNoSuchGame";
}

interface IStartGameErrorNotInGame {
  tag: "StartGameErrorNotInGame";
}

interface IStartGameErrorGameAlreadyStarted {
  tag: "StartGameErrorGameAlreadyStarted";
}

interface IStartGameErrorOther {
  tag: "StartGameErrorOther";
  contents: string;
}

type MakeMoveError = IMakeMoveErrorInvalidMove | IMakeMoveErrorNoSuchGame | IMakeMoveErrorPlayerNotInGame | IMakeMoveErrorGameNotStartedYet | IMakeMoveErrorGameAlreadyFinished;

interface IMakeMoveErrorInvalidMove {
  tag: "MakeMoveErrorInvalidMove";
  contents: string;
}

interface IMakeMoveErrorNoSuchGame {
  tag: "MakeMoveErrorNoSuchGame";
}

interface IMakeMoveErrorPlayerNotInGame {
  tag: "MakeMoveErrorPlayerNotInGame";
}

interface IMakeMoveErrorGameNotStartedYet {
  tag: "MakeMoveErrorGameNotStartedYet";
}

interface IMakeMoveErrorGameAlreadyFinished {
  tag: "MakeMoveErrorGameAlreadyFinished";
}

type MakeMoveResponse = IMakeMoveResponse;

interface IMakeMoveResponse {
  gameStateView: GameView;
}

type GetGameViewResponse = IGetGameViewResponse;

interface IGetGameViewResponse {
  gameView: GameView;
}

type GetGameViewError = "GetGameViewErrorNoSuchGame" | "GetGameViewErrorUserNotInGame";

type GetGamesForUserResponse = IGetGamesForUserResponse;

interface IGetGamesForUserResponse {
  gameIds: string[];
}
