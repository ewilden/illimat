import { Board } from '~/components/board';
import { MyHand } from '../hand';
import { moveTypes } from '~/common/game_logic';
import { useDispatch, useSelector } from 'react-redux';
import { createSelector } from '@reduxjs/toolkit';
import { RootState } from '~/common/reducers';
import { selectMoveType } from '~/common/features/selected_move_slice';
import { FieldCard } from '../card';

const moveButtonClasses = {
    always: " m-1 border border-solid border-1 border-indigo-300 bg-indigo-100 py-2 px-4 rounded ",
    selected: " bg-indigo-400 ",
    notSelected: " hover:bg-indigo-200 ",
};

const selectSelectedMove = createSelector((state: RootState) => state.selectedMoveType, x => x);
const selectSelectedCards = createSelector((state: RootState) => state.selectedCards,
    selectedCardsObj => [...Object.keys(selectedCardsObj)].map(x => selectedCardsObj[x])
);

export default function GameView({ gameStateView }: { gameStateView: GameStateView }) {
    const dispatch = useDispatch();
    const selectedMoveType = useSelector(selectSelectedMove);
    const selectedCards = useSelector(selectSelectedCards);

    return (<div>
        <p>
            This is the game state view.
        </p>
        <div className="gameViewContainer">
            <p>Board:</p>
            <Board boardState={gameStateView.boardState} illimatState={gameStateView.illimatState} />
            <div className="flex flex-row flex-wrap space-x-4 mt-4">
                <div>
                    <p>Your hand:</p>
                    <MyHand hand={gameStateView.playerState[0].hand} />
                </div>
                <div>
                    <p>Possible actions:</p>
                    <ul className="flex flex-row flex-wrap">
                        {moveTypes.map(moveType => (
                            <li key={moveType}>
                                <button
                                    onClick={() => {
                                        dispatch(selectMoveType({ moveType }));
                                    }}
                                    className={`${moveButtonClasses.always} ${moveType === selectedMoveType ? moveButtonClasses.selected : moveButtonClasses.notSelected}`}
                                >{moveType}
                                </button>
                            </li>
                        ))}
                    </ul>
                    {selectedMoveType && (
                        <div className="mt-4">
                            <p>Your pending move:</p>
                            <h3 className="text-lg">{selectedMoveType}</h3>
                            <p>using cards from your hand:{' '}
                                {selectedCards.map(card => <FieldCard card={card} />)}
                            </p>
                        </div>
                    )}
                </div>
            </div>
        </div>
    </div>);
}
