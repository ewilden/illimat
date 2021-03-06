import { Board } from '~/components/board';
import { MyHand } from '../hand';
import { moveTypes, cardToString, MoveType, longDirection, cardStackToString } from '~/common/game_logic';
import { useDispatch, useSelector } from 'react-redux';
import { createSelector } from '@reduxjs/toolkit';
import { RootState } from '~/common/reducers';
import { selectMoveType } from '~/common/features/selected_move_slice';
import { FieldCard, RenderCardStack } from '../card';
import { makeMove } from '~/common/features/game_state_view_slice';
import { useState } from 'react';

const moveButtonClasses = {
    always: " m-1 border border-solid border-1 border-indigo-300 bg-indigo-100 py-2 px-4 rounded ",
    selected: " bg-indigo-400 ",
    notSelected: " hover:bg-indigo-200 ",
};

const selectSelectedMove = createSelector((state: RootState) => state.selectedMoveType, x => x);
const selectSelectedCards = createSelector((state: RootState) => state.selectedCards,
    selectedCardsObj => Object.values(selectedCardsObj)
);
const selectSelectedCardStacks = createSelector((state: RootState) => state.selectedCardStacks,
    selectedStacksObj => Object.values(selectedStacksObj)
);
const selectSelectedField = createSelector((state: RootState) => state.selectedField,
    x => x);

function canTargetFieldCards(moveType: MoveType): boolean {
    return ['Harvest', 'Stockpile'].includes(moveType);
}

const selectGameId = createSelector((state: RootState) => state.game.gameId, _ => _);

function buildMove(selectedMoveType: MoveType, selectedCards: Card[], selectedCardStacks: CardStack[], selectedField: Direction, stockpileValue: number): Move {
    switch (selectedMoveType) {
        case "Harvest":
            return { tag: "Harvest", contents: [selectedCards, selectedField, selectedCardStacks] };
        case "Sow":
            return { tag: "Sow", contents: [selectedCards[0], selectedField] };
        case "Stockpile":
            return { tag: "Stockpile", contents: [selectedCards[0], selectedField, selectedCardStacks, stockpileValue] };
    }
    throw new Error('impossible');
}
function YourCurrentMove() {
    const dispatch = useDispatch();
    const selectedMoveType = useSelector(selectSelectedMove);
    const selectedCards = useSelector(selectSelectedCards);
    const selectedStacks = useSelector(selectSelectedCardStacks);
    const selectedField = useSelector(selectSelectedField);
    const gameId = useSelector(selectGameId);
    const isMoveSendable = selectedMoveType &&
        (canTargetFieldCards(selectedMoveType) ?
            selectedCards.length > 0 && selectedStacks.length > 0 :
            selectedCards.length > 0 && selectedField);
    const [stockpileValue, setStockpileValue] = useState<null | number>(null);

    return (selectedMoveType ?
        <div className="mt-4">
            <p>Your pending move:</p>
            <h3 className="text-lg inline">{selectedMoveType}</h3>
            {selectedMoveType === 'Stockpile' && <span><p className="inline">of value </p><input value={String(stockpileValue)} className="border border-solid border-1 border-indigo-100" onChange={e => setStockpileValue(Number(e.target.value))}></input></span>}
            {selectedCards.length > 0 && (
                <>
                    <p>
                        using cards from your hand:{' '}
                    </p>
                    <div>
                        {selectedCards.map(card => <FieldCard card={card} key={cardToString(card)} />)}
                    </div>
                </>
            )}
            {canTargetFieldCards(selectedMoveType) && selectedStacks.length > 0 && (
                <>
                    <p className="mt-2">on cards from the board:{' '}
                    </p>
                    <div>
                        {selectedStacks.map(cardStack => <div className="inline-block" key={cardStackToString(cardStack)}>
                            <RenderCardStack cardStack={cardStack} disableHighlighting={true} />
                        </div>)}
                    </div>
                </>
            )}
            {!canTargetFieldCards(selectedMoveType) && selectedField && (
                <p className="mt-2">into the field in the {longDirection(selectedField)}.</p>
            )}
            {isMoveSendable && (
                <button className={`mt-4 ${moveButtonClasses.always} ${
                    moveButtonClasses.notSelected}`}
                    onClick={e => {
                        dispatch(makeMove({ gameId, move: buildMove(selectedMoveType, selectedCards, selectedStacks, selectedField!, stockpileValue!) }));
                    }}>
                    Commit to your move.
                </button>
            )}
        </div> : <></>
    );
}

export default function GameStateView({ gameStateView }: { gameStateView: GameStateView }) {
    const dispatch = useDispatch();
    const selectedMoveType = useSelector(selectSelectedMove);
    const selectedCards = useSelector(selectSelectedCards);

    return (
        <div>
            <div className="gameViewContainer">
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
                            <YourCurrentMove />
                        )}
                    </div>
                </div>
            </div>
        </div>
    );
}
