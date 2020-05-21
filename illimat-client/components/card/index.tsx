import { match, cardToString, showCardVal } from "~/common/game_logic";
import { RootState } from '~/common/reducers';
import { cardStackToString } from '~/common/game_logic';
import { selectCardStack, deselectCardStack } from '~/common/features/selected_card_stacks_slice';

import { createSelector } from '@reduxjs/toolkit';
import { useSelector, useDispatch } from "react-redux";
import { selectCard, deselectCard } from "~/common/features/selected_cards_slice";


const selectIsMyCardSelected = createSelector(
    (state: RootState) => state.selectedCards,
    (_: unknown, myCard: Card) => myCard,
    (selectedCards, myCard) => Boolean(selectedCards[cardToString(myCard)]),
);

export function HandCard({ card, ...rest }: { card: Card }) {
    const isSelected = useSelector((state: RootState) => selectIsMyCardSelected(state, card));
    const dispatch = useDispatch();
    return (
        <FieldCard card={card} isSelected={isSelected} {...rest} onClick={() => {
            if (!isSelected) {
                dispatch(selectCard({ card }));
            } else {
                dispatch(deselectCard({ card }));
            }
        }} />
    );
}

const cardClasses = {
    selected: " border-4 border-yellow-400 bg-yellow-200 ",
    notSelected: " border-2 border-gray-800 hover:bg-yellow-100 ",
    always: " w-20 border border-solid rounded text-center "
};

export function FieldCard({ card, isSelected, onClick }: { card: Card, isSelected?: boolean, onClick?: (e: any) => void }) {
    const [val, season] = card;
    return (
        <button className={`${isSelected ? cardClasses.selected : cardClasses.notSelected} ${cardClasses.always}`}
            onClick={onClick} tabIndex={onClick ? 0 : -1}>
            <p>{showCardVal(val)}</p>
            <p className="text-2xl">{renderSeason(season)}</p>
            <p>{season.substring(1)}</p>
        </button>
    );
}

const selectIsMyStackSelected = createSelector(
    (state: RootState) => state.selectedCardStacks,
    (_: unknown, myCardStack: CardStack) => myCardStack,
    (selectedCardStacks, myCardStack) => Boolean(selectedCardStacks[cardStackToString(myCardStack)])
);

const classesForIsSelected = " border-solid border-4 border-yellow-400 bg-yellow-200 ";

export function RenderCardStack({ cardStack, disableHighlighting }: { cardStack: CardStack, disableHighlighting?: true }) {
    const [possibleValues, cards] = cardStack;
    const isMyStackSelected = useSelector((state: RootState) => selectIsMyStackSelected(state, cardStack));
    const shouldHighlight = !disableHighlighting && isMyStackSelected;
    const dispatch = useDispatch();

    if (cards.length < 1) {
        throw new Error("impossible");
    } else if (cards.length === 1) {
        return (
            <div className={shouldHighlight ? classesForIsSelected : ""}
                tabIndex={0}
                onClick={() => {
                    if (!isMyStackSelected) {
                        dispatch(selectCardStack({ cardStack }));
                    } else {
                        dispatch(deselectCardStack({ cardStack }));
                    }
                }}
            >
                {/* We apply isSelected at the stack level instead. */}
                <FieldCard card={cards[0]} isSelected={false} />
            </div>
        );
    } else {
        return (
            <div className={shouldHighlight ? classesForIsSelected : "border-dotted border-2 border-gray-400"}
                tabIndex={0}
                onClick={() => {
                    if (!isMyStackSelected) {
                        dispatch(selectCardStack({ cardStack }));
                    } else {
                        dispatch(deselectCardStack({ cardStack }));
                    }
                }}
            >
                <p>[{possibleValues.join(", ")}]</p>
                <div className="flex flex-col space-y-px">
                    {cards.map(card => (
                        <div >
                            {/* We apply isSelected at the stack level instead. */}
                            <FieldCard card={card} isSelected={false} />
                        </div>
                    ))}
                </div>
            </div>
        );
    }
}

function renderSeason(season: CardSeason): string {
    return match(season, {
        CSpring: () => "🌱",
        CWinter: () => "❄️",
        CSummer: () => "☀️",
        CAutumn: () => "🍂",
        CStars: () => "✨",
    });
}