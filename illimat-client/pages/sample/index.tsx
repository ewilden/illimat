import GameStateView from '~/components/game_view';
import { useState, useEffect } from 'react';
import axios from 'axios';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { configureStore, createSelector } from '@reduxjs/toolkit';
import rootReducer, { RootState } from '~/common/reducers'
// import { setGameStateView } from '~/common/features/game_state_view_slice';
import * as gameApi from '~/common/game_logic/api';
import { match } from '~/common/game_logic';

const { matchEither } = gameApi;

const store = configureStore({
    reducer: rootReducer,
});

export default function Sample() {
    return (
        <Provider store={store}>
            <GameStateViewWithStore />
        </Provider>
    );
}

const selectGame = createSelector(
    (state: RootState) => state.game,
    g => g
);

function GameStateViewWithStore() {
    const game = useSelector(selectGame);
    // const dispatch = useDispatch();
    // useEffect(() => {
    //     (async () => {
    //         const response = await axios.get('/api/sample');
    //         console.log('got response');
    //         console.log(response);
    //         // dispatch(setGameStateView({ gameStateView: response.data }));
    //     })()
    // }, []);
    switch (game.gameView?.data.tag) {
        case "GameViewRunning": {
            const gameStateView = game.gameView.data.contents;
            return <GameStateView gameStateView={gameStateView} />;
        }
        default:
            return <p>Loading...</p>;
    }

}