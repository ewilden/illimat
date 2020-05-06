import { createSlice } from '@reduxjs/toolkit';
import { MoveType } from '~/common/game_logic';

const selectedMoveSlice = createSlice({
    name: 'selectedMove',
    initialState: null as MoveType | null,
    reducers: {
        selectMoveType(state, action: { payload: { moveType: MoveType } }) {
            const { moveType }: { moveType: MoveType } = action.payload;
            return moveType;
        },
    }
});

export const { selectMoveType } = selectedMoveSlice.actions;
export default selectedMoveSlice.reducer;