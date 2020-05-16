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

export async function creategame(): Promise<CreateGameResponse> {
    // const resp = await axios.post(`${apiPrefix}/creategame`, {}, { withCredentials: true });
    // if (resp.headers.Cookie) {
    //     document.cookie = resp.headers.Cookie;
    // }
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return myPost(`${apiPrefix}/creategame`, {});
}

export async function joingame(gameId: string): Promise<Either<string, JoinGameResponse>> {
    // const resp = await axios.post(`${apiPrefix}/joingame/${gameId}`, {}, { withCredentials: true });
    // if (resp.headers.Cookie) {
    //     document.cookie = resp.headers.Cookie;
    // }
    // const resp = (await fetch(`${apiPrefix}/joingame/${gameId}`, {
    //     method: 'POST',
    //     mode: 'same-origin',
    //     cache: 'no-cache',
    //     credentials: 'same-origin',
    //     headers: {
    //         'Content-Type': 'application/json'
    //     },
    //     redirect: 'follow',
    //     referrerPolicy: 'no-referrer',
    //     body: '{}',
    // })).json();
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return myPost(`${apiPrefix}/joingame/${gameId}`, {});
}

export async function startgame(gameId: string): Promise<Either<string, StartGameResponse>> {
    // const resp = await axios.post(`${apiPrefix}/startgame/${gameId}`, {}, { withCredentials: true });
    // if (resp.headers.Cookie) {
    //     document.cookie = resp.headers.Cookie;
    // }
    // const resp = (await fetch(`${apiPrefix}/startgame/${gameId}`, {
    //     method: 'POST',
    //     mode: 'same-origin',
    //     cache: 'no-cache',
    //     credentials: 'same-origin',
    //     headers: {
    //         'Content-Type': 'application/json'
    //     },
    //     redirect: 'follow',
    //     referrerPolicy: 'no-referrer',
    //     body: '{}',
    // })).json();
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return myPost(`${apiPrefix}/startgame/${gameId}`, {});
}

export async function makemove(gameId: string, move: Move): Promise<Either<string, MakeMoveResponse>> {
    // const resp = await axios.post(`${apiPrefix}/makemove/${gameId}`, move, { withCredentials: true });
    // if (resp.headers.Cookie) {
    //     document.cookie = resp.headers.Cookie;
    // }
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return myPost(`${apiPrefix}/makemove/${gameId}`, move);
}

export async function viewgame(gameId: string): Promise<Either<string, GetGameViewResponse>> {
    // const resp = await axios.get(`${apiPrefix}/viewgame/${gameId}`, { withCredentials: true });
    // console.log(resp);
    // const resp = (await fetch(`${apiPrefix}/viewgame/${gameId}`, {
    //     method: 'POST',
    //     mode: 'same-origin',
    //     cache: 'no-cache',
    //     credentials: 'same-origin',
    //     headers: {
    //         'Content-Type': 'application/json'
    //     },
    //     redirect: 'follow',
    //     referrerPolicy: 'no-referrer',
    //     body: '{}',
    // })).json();
    // // if (resp.headers.Cookie) {
    // //     document.cookie = resp.headers.Cookie;
    // // }
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return myGet(`${apiPrefix}/viewgame/${gameId}`);
}

export async function listgames(): Promise<GetGamesForUserResponse> {
    // const resp = await axios.get(`${apiPrefix}/listgames`, { withCredentials: true });
    // if (resp.headers.Cookie) {
    //     document.cookie = resp.headers.Cookie;
    // }
    // console.log('looking for cookie');
    // console.log(resp);
    // return resp;
    return fetch(`${apiPrefix}/listgames`).then(resp => resp.json());
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