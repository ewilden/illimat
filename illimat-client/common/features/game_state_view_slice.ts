import { createSlice, Dispatch } from '@reduxjs/toolkit';
import * as gameApi from '~/common/game_logic/api';
import gameApiService from '~/common/game_logic/api_service';

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

// const gameStateView = createSlice({
//     name: 'gameStateView',
//     initialState: null as GameStateView | null,
//     reducers: {
//         setGameStateView(_, action: { payload: { gameStateView: GameStateView } }) {
//             const { gameStateView } = action.payload;
//             return gameStateView;
//         },
//     },
// });
// export const { setGameStateView } = gameStateView.actions;

const createGame = () => async (dispatch: Dispatch) => {
    dispatch({ type: "game/createGame" });
    const response = await gameApiService.creategame();
    dispatch(setGame(response));
}
const joinGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/joinGame", gameId });
    const response = await gameApiService.joingame(gameId);
    dispatch(setGame({ gameId, ...response }));
}
const startGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/startGame", gameId });
    const response = await gameApiService.startgame(gameId);
    dispatch(setGame({ gameId, ...response }));
}
const makeMove = ({ move, gameId }: { move: Move, gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/makeMove", move, gameId });
    try {
        const response = await gameApiService.makemove(gameId, move);
        dispatch(setGame({ gameView: response.gameStateView, gameId: gameId }));
    } catch (e) {
        alert(e);
    }
};
const viewGame = ({ gameId }: { gameId: string }) => async (dispatch: Dispatch) => {
    dispatch({ type: "game/viewGame", gameId });
    try {
        const response = await gameApiService.viewgame(gameId);
        dispatch(setGame({ gameId, ...response }));
    } catch (e) {
        console.log('viewGame failed, trying joinGame instead');
        joinGame({ gameId })(dispatch);
    }
};
export { createGame, joinGame, startGame, makeMove, viewGame };
export default game.reducer;
