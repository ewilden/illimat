import { createSlice } from '@reduxjs/toolkit';
import { MoveType } from '~/common/game_logic';
import { setGameStateView } from './game_state_view_slice';

const selectedMoveSlice = createSlice({
    name: 'selectedMove',
    initialState: null as MoveType | null,
    reducers: {
        selectMoveType(state, action: { payload: { moveType: MoveType } }) {
            const { moveType }: { moveType: MoveType } = action.payload;
            return moveType;
        },
    },
    extraReducers: builder => {
        builder.addCase(setGameStateView, (state, action) => {
            return null;
        });
    },
});

export const { selectMoveType } = selectedMoveSlice.actions;
export default selectedMoveSlice.reducer;