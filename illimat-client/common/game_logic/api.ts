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
    if (either.Left) {
        return matcher.L(either.Left);
    } else {
        return matcher.R(either.Right!);
    }
}

export function onR<L, R, U>(either: Either<L, R>, fn: (r: R) => U | null): U | null {
    return matchEither(either, {
        L: _ => null,
        R: fn
    });
}

const apiPrefix = 'http://localhost:3003';

export async function creategame(): Promise<AxiosResponse<CreateGameResponse>> {
    return axios.post(`${apiPrefix}/creategame`);
}

export async function joingame(gameId: string): Promise<AxiosResponse<Either<string, JoinGameResponse>>> {
    return axios.post(`${apiPrefix}/joingame/${gameId}`);
}

export async function startgame(gameId: string): Promise<AxiosResponse<Either<string, StartGameResponse>>> {
    return axios.post(`${apiPrefix}/startgame/${gameId}`);
}

export async function makemove(gameId: string, move: Move): Promise<AxiosResponse<Either<string, MakeMoveResponse>>> {
    return axios.post(`${apiPrefix}/makemove/${gameId}`, move);
}

export async function viewgame(gameId: string): Promise<AxiosResponse<Either<string, GetGameViewResponse>>> {
    return axios.get(`${apiPrefix}/viewgame/${gameId}`);
}

export async function listgames(): Promise<AxiosResponse<GetGamesForUserResponse>> {
    return axios.get(`${apiPrefix}/listgames`);
}
