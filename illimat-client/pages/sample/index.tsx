import GameView from '~/components/game_view';
import { useState, useEffect } from 'react';
import axios from 'axios';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { configureStore, createSelector } from '@reduxjs/toolkit';
import rootReducer, { RootState } from '~/common/reducers'
import { setGameStateView } from '~/common/features/game_state_view_slice';

const store = configureStore({
    reducer: rootReducer,
});

export default function Sample() {
    return (
        <Provider store={store}>
            <GameViewWithStore />
        </Provider>
    );
}

const selectGameStateView = createSelector(
    (state: RootState) => state.gameStateView,
    v => v,
);

function GameViewWithStore() {
    const gameStateView = useSelector(selectGameStateView);
    const dispatch = useDispatch();
    useEffect(() => {
        (async () => {
            const response = await axios.get('/api/sample');
            console.log('got response');
            console.log(response);
            dispatch(setGameStateView({ gameStateView: response.data }));
        })()
    }, []);
    return <>{gameStateView ? <GameView gameStateView={gameStateView} /> : <p>Loading...</p>}</>;
}