import axios, { AxiosResponse } from 'axios';


interface Left<A> {
    Left: A;
    Right: undefined;
}

interface Right<A> {
    Right: A;
    Left: undefined;
}

export type Either<L, R> = Left<L> | Right<R>;

export function matchEither<L, R, U>(either: Either<L, R>, matcher: { L: (l: L) => U, R: (r: R) => U }): U {
    console.log('in matchEither');
    console.log(either);
    console.log(matcher);
    if (either.Left) {
        console.log('taking left');
        return matcher.L(either.Left);
    } else {
        console.log('taking right');
        return matcher.R(either.Right!);
    }
}

export function onR<L, R, U>(either: Either<L, R>, fn: (r: R) => U | null): U | null {
    return matchEither(either, {
        L: _ => null,
        R: fn
    });
}

const apiPrefix = '/gameapi';
interface Ids {
    userId: string;
    gameId: string;
}

export async function creategame(userId: string): Promise<CreateGameResponse> {
    return myPost(`${apiPrefix}/creategame?user=${userId}`, {});
}

export async function joingame({ userId, gameId }: Ids): Promise<Either<JoinGameError, JoinGameResponse>> {
    return myPost(`${apiPrefix}/joingame/${gameId}?user=${userId}`, {});
}

export async function startgame({ userId, gameId }: Ids): Promise<Either<StartGameError, StartGameResponse>> {
    return myPost(`${apiPrefix}/startgame/${gameId}?user=${userId}`, {});
}

export async function makemove({ userId, gameId }: Ids, move: Move): Promise<Either<MakeMoveError, MakeMoveResponse>> {
    return myPost(`${apiPrefix}/makemove/${gameId}?user=${userId}`, move);
}

export async function viewgame({ userId, gameId }: Ids): Promise<Either<GetGameViewError, GetGameViewResponse>> {
    return myGet(`${apiPrefix}/viewgame/${gameId}?user=${userId}`);
}

export async function listgames(userId: string): Promise<GetGamesForUserResponse> {
    return myGet(`${apiPrefix}/listgames?user=${userId}`);
}

function myGet(url: string) {
    return fetch(url).then(async resp => {
        console.log(`resp for ${url}:`);
        const data = await resp.json();
        console.log(data);
        return data;
    });
}

function myPost(url: string, body: object) {
    return fetch(url, {
        method: 'POST',
        mode: 'no-cors',
        cache: 'no-cache',
        credentials: 'same-origin',
        headers: {
            'Content-Type': 'application/json'
        },
        redirect: 'follow',
        referrerPolicy: 'no-referrer',
        body: JSON.stringify(body),
    }).then(async resp => {
        console.log(`resp for ${url}:`);
        const data = await resp.json();
        console.log(data);
        return data;
    });
}