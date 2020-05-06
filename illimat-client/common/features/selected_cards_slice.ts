import { createSlice } from '@reduxjs/toolkit';
import { cardToString } from '~/common/game_logic';

const selectedCardsSlice = createSlice({
    name: 'selectedCards',
    initialState: {} as { [cardKey: string]: Card },
    reducers: {
        selectCard(state, action: { payload: { card: Card } }) {
            const { card } = action.payload;
            state[cardToString(card)] = card;
        },
        deselectCard(state, action: { payload: { card: Card } }) {
            const { card } = action.payload;
            delete state[cardToString(card)];
        },
    }
});

export const { selectCard, deselectCard } = selectedCardsSlice.actions;
export default selectedCardsSlice.reducer;