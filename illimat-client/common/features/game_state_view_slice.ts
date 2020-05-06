import { createSlice } from '@reduxjs/toolkit';

const gameStateView = createSlice({
    name: 'gameStateView',
    initialState: null as GameStateView | null,
    reducers: {
        setGameStateView(state, action) {
            const { gameStateView }: { gameStateView: GameStateView } = action.payload;
            return gameStateView;
        },
    }
});

export const { setGameStateView } = gameStateView.actions;
export default gameStateView.reducer;