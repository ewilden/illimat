import { createSlice } from '@reduxjs/toolkit';
import { cardStackToString } from '~/common/game_logic';

const selectedCardStacksSlice = createSlice({
    name: 'selectedCardStacks',
    initialState: {} as { [stackKey: string]: CardStack },
    reducers: {
        selectCardStack(state, action: { payload: { cardStack: CardStack } }) {
            const { cardStack } = action.payload;
            state[cardStackToString(cardStack)] = cardStack;
        },
        deselectCardStack(state, action: { payload: { cardStack: CardStack } }) {
            const { cardStack } = action.payload;
            delete state[cardStackToString(cardStack)];
        },
    }
});

export const { selectCardStack, deselectCardStack } = selectedCardStacksSlice.actions;
export default selectedCardStacksSlice.reducer;