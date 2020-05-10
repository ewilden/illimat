import { createSlice } from '@reduxjs/toolkit';
import { setGameStateView } from './game_state_view_slice';

const selectedFieldSlice = createSlice({
    name: 'selectedField',
    initialState: null as Direction | null,
    reducers: {
        selectField(state, action: { payload: { field: Direction } }) {
            const { field }: { field: Direction } = action.payload;
            return field;
        },
    },
    extraReducers: builder => {
        builder.addCase(setGameStateView, (state, action) => {
            return null;
        });
    },
});

export const { selectField } = selectedFieldSlice.actions;
export default selectedFieldSlice.reducer;