import { Board } from '~/components/board';
import { MyHand } from '../hand';

export default function GameView({ gameStateView }: { gameStateView: GameStateView }) {
    return (<div>
        <p>
            This is the game state view.
        </p>
        <div className="gameViewContainer">
            <p>Board:</p>
            <Board boardState={gameStateView.boardState} illimatState={gameStateView.illimatState} />
            <p>Your hand:</p>
            <MyHand hand={gameStateView.playerState[0].hand} />
        </div>
    </div>);
}
