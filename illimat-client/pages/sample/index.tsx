import GameStateView from '~/components/game_view';
import { useState, useEffect } from 'react';
import axios from 'axios';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { configureStore, createSelector } from '@reduxjs/toolkit';
import rootReducer, { RootState } from '~/common/reducers'
// import { setGameStateView } from '~/common/features/game_state_view_slice';
import * as gameApi from '~/common/game_logic/api';
import { match } from '~/common/game_logic';
import { setGame } from '~/common/features/game_state_view_slice';

const { matchEither } = gameApi;

const store = configureStore({
    reducer: rootReducer,
});

export default function Sample() {
    // const [sampleGame, setSampleGame] = useState<GameStateView | null>(null);
    useEffect(() => {
        fetch('/gameapi/sample').then(response => response.json()).then(data => store.dispatch(setGame({ gameId: 'sampleGame', gameView: { data: { tag: "GameViewRunning", contents: data } } })));
    });
    return (
        <Provider store={store}><GameStateViewWithStore /> </Provider>
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