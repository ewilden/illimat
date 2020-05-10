import { createSlice } from '@reduxjs/toolkit';
import { cardToString } from '~/common/game_logic';
import { setGameStateView } from '~/common/features/game_state_view_slice';

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
    },
    extraReducers: builder => {
        builder.addCase(setGameStateView, (state, action) => {
            return {};
        });
    },
});

export const { selectCard, deselectCard } = selectedCardsSlice.actions;
export default selectedCardsSlice.reducer;