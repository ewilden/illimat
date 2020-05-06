import { createSlice } from '@reduxjs/toolkit';
import { cardToString } from '~/common/game_logic';

const selectedCardsSlice = createSlice({
    name: 'selectedCards',
    initialState: {} as { [cardKey: string]: Card },
    reducers: {
        selectCard(state, action) {
            const { card }: { card: Card } = action.payload;
            state[cardToString(card)] = card;
        },
        deselectCard(state, action) {
            delete state[cardToString(action.payload.card)];
        },
    }
});

export const { selectCard, deselectCard } = selectedCardsSlice.actions;
export default selectedCardsSlice.reducer;