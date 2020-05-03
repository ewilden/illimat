import GameView from '~/components/game_view';
import {useState, useEffect } from 'react';
import axios from 'axios';

type UseStateOf<T> = [T, any];

export default function Sample() {
    const [gameView, setGameView]: UseStateOf<GameStateView|null> = useState(null);
    useEffect(() => {
        (async () => {
        const response = await axios.get('/api/sample');
        setGameView(response.data);
    })()}, []);
    return gameView ? <GameView gameView={gameView} /> : <p>Loading...</p>;
}