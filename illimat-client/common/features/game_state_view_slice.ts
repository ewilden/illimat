import { createSlice, Dispatch } from '@reduxjs/toolkit';
import * as gameApi from '~/common/game_logic/api';

const { matchEither, onR } = gameApi;
const game = createSlice({
    name: 'game',
    initialState: {
        gameView: null as GameView | null,
        gameId: '',
    },
    reducers: {
        setGame(_, action: { payload: { gameView: GameView, gameId: string } }) {
            return action.payload;
        },
    },
});
export const { setGame } = game.actions;

const gameStateView = createSlice({
    name: 'gameStateView',
    initialState: null as GameStateView | null,
    reducers: {
        setGameStateView(_, action: { payload: { gameStateView: GameStateView } }) {
            const { gameStateView } = action.payload;
            return gameStateView;
        },
    },
});
export const { setGameStateView } = gameStateView.actions;

const createGame = () => async (dispatch: Dispatch) => {
    dispatch({ type: "game/createGame" });
    const response = await gameApi.creategame();
    dispatch(setGame(response.data));
}
const joinGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/joinGame", gameId });
    const response = await gameApi.joingame(gameId);
    onR(response.data, r => dispatch(setGame({ gameId, ...r })));
}
const startGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/startGame", gameId });
    const response = await gameApi.startgame(gameId);
    onR(response.data, r => dispatch(setGame({ gameId, ...r })));
}
const makeMove = ({ move, gameId }: { move: Move, gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/makeMove", move, gameId });
    const response = await gameApi.makemove(gameId, move);
    matchEither(response.data, {
        L: err => { console.log(err); },
        R: makeMoveResponse => {
            dispatch(setGame({ gameView: makeMoveResponse.gameStateView, gameId: gameId }));
            const gameViewData = makeMoveResponse.gameStateView.data;
            switch (gameViewData.tag) {
                case "GameViewRunning": {
                    const gameStateView = gameViewData.contents;
                    dispatch(setGameStateView({ gameStateView }));
                }
                case "GameViewFinished": {
                    console.warn('Game already finished.');
                    console.warn(response);
                }
                case "GameViewStarting": {
                    console.warn('Game not started yet.');
                    console.warn(response);
                }
            }
        }
    });
};
const viewGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/viewGame", gameId });
    const response = await gameApi.viewgame(gameId);
    onR(response.data, r => dispatch(setGame({ gameId, ...r })));
};
export { createGame, joinGame, startGame, makeMove, viewGame };

export default gameStateView.reducer;