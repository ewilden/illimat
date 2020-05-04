import { Board } from '~/components/board';

export default function GameView({ gameView }: { gameView: GameStateView }) {
    return (<div>
        <p>
            This is the game state view.
        </p>
        <div className="gameViewContainer">
            <p>Board:</p>
            <Board boardState={gameView.boardState} illimatState={gameView.illimatState} />
        </div>
    </div>);
}

/*
{
illimatState: {
summerDir: "S",
numOkuses: 4
},
boardState: {
fieldN: {
cards: [
[
[
2
],
[
[
"Two",
"CSpring"
]
]
],
[
[
13
],
[
[
"King",
"CWinter"
]
]
],
[
[
5
],
[
[
"Five",
"CWinter"
]
]
]
],
luminary: {
tag: "FaceDownView"
}
},
fieldE: {
cards: [
[
[
11
],
[
[
"Knight",
"CAutumn"
]
]
],
[
[
13
],
[
[
"King",
"CStars"
]
]
],
[
[
7
],
[
[
"Seven",
"CSummer"
]
]
]
],
luminary: {
tag: "FaceDownView"
}
},
fieldS: {
cards: [
[
[
9
],
[
[
"Nine",
"CSpring"
]
]
],
[
[
6
],
[
[
"Six",
"CStars"
]
]
],
[
[
2
],
[
[
"Two",
"CWinter"
]
]
]
],
luminary: {
tag: "FaceDownView"
}
},
fieldW: {
cards: [
[
[
9
],
[
[
"Nine",
"CWinter"
]
]
],
[
[
11
],
[
[
"Knight",
"CSummer"
]
]
],
[
[
2
],
[
[
"Two",
"CSummer"
]
]
]
],
luminary: {
tag: "FaceDownView"
}
}
},
playerState: [
{
hand: [
[
"Nine",
"CSummer"
],
[
"Four",
"CSummer"
],
[
"Ten",
"CSpring"
],
[
"Ten",
"CStars"
]
],
numOkuses: 0,
luminaries: [ ],
harvestPile: [ ]
},
[
{
numCardsInHand: 4,
numOkuses: 0,
luminaries: [ ],
harvestPileSize: 0
},
{
numCardsInHand: 3,
numOkuses: 0,
luminaries: [ ],
harvestPileSize: 0
},
{
numCardsInHand: 4,
numOkuses: 0,
luminaries: [ ],
harvestPileSize: 0
}
]
],
deck: 38,
whoseTurn: [
2,
"NoRake"
]
}
*/