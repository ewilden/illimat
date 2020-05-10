import { createSlice, Dispatch } from '@reduxjs/toolkit';
import axios from 'axios';

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



const commitMove = ({ move }: { move: Move }) => async (dispatch: Dispatch) => {
    const response = await axios.get('/api/move');
};

export const { setGameStateView } = gameStateView.actions;
export default gameStateView.reducer;