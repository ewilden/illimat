import GameStateView from '~/components/game_view';
import { useEffect } from 'react';
import { useRouter } from 'next/router';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { configureStore, createSelector } from '@reduxjs/toolkit';
import rootReducer, { RootState } from '~/common/reducers'
import { joinGame, viewGame, startGame } from '~/common/features/game_state_view_slice';

const store = configureStore({
    reducer: rootReducer,
});

export default function GamePage() {
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
    const dispatch = useDispatch();
    const router = useRouter();
    const { gameId } = router.query;
    console.log('router query');
    console.log(router.query);
    useEffect(() => {
        // Try both, one will succeed if the game is real.
        if (gameId) {
            // dispatch(joinGame({ gameId: gameId as string }));
            dispatch(viewGame({ gameId: gameId as string }));
        }
    }, [gameId]);
    switch (game.gameView?.data?.tag) {
        case null:
        case undefined:
            return <p>Trying to join game...</p>
        case "GameViewStarting": {
            return (
                <div>
                    <p>Game hasn't started yet.</p>
                    <a onClick={() => {
                        console.log('clicked start game');
                        dispatch(startGame({ gameId: gameId as string }));
                    }}>Try to start the game</a>
                </div>);
        }
        case "GameViewRunning": {
            const gameStateView = game.gameView.data.contents;
            return <GameStateView gameStateView={gameStateView} />;
        }
        case "GameViewFinished": {
            return <p>Game's already finished.</p>
        }
        default:
            return <p>Trying to join game...</p>;
    }

}