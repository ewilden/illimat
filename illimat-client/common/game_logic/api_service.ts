import * as gameApi from './api';
import { Either } from './api';

function assertRight<L, R>(either: Either<L, R>, errMsg: (l: L) => string): R {
    if (!either.Right) {
        throw new Error(errMsg(either.Left!));
    }
    return either.Right!;
}

function getPersistedUserId(): string | null {
    return window.localStorage.getItem('USER_ID');
}
function persistUserId(uid: string): void {
    window.localStorage.setItem('USER_ID', uid);
}

function orElse<A>(a: A | null, fallback: A): A {
    return a === null ? fallback : a;
}

class GameApiService {
    private userId: string = orElse(getPersistedUserId(), '');
    private prevTaskPromise: Promise<void> = Promise.resolve();

    private withQueue<A>(taskProducer: () => Promise<{ userId: string, result: A }>): Promise<A> {
        const oldPromise = this.prevTaskPromise;
        const myTask: Promise<A> = new Promise(async (resolve, reject) => {
            await oldPromise;
            try {
                const { userId, result } = await taskProducer();
                this.userId = userId;
                persistUserId(userId);
                resolve(result);
            } catch (e) {
                reject(e);
            }
        });
        this.prevTaskPromise = myTask.then(() => { }).catch(() => { });
        return myTask;
    }

    creategame(): Promise<CreateGameResponse> {
        return this.withQueue(async () => {
            const result = await gameApi.creategame(this.userId);
            return { result, userId: result.userId };
        });
    }
    joingame(gameId: string): Promise<JoinGameResponse> {
        return this.withQueue(async () => {
            const response = await gameApi.joingame({ userId: this.userId, gameId });
            const result = assertRight(response, err => `JoinGame failed: ${err}`);
            return { result, userId: result.userId };
        });
    }
    startgame(gameId: string): Promise<StartGameResponse> {
        return this.withQueue(async () => {
            const response = await gameApi.startgame({ userId: this.userId, gameId });
            const result = assertRight(response, err => `StartGame failed: ${err}`);
            return { result, userId: result.userId };
        });
    }
    makemove(gameId: string, move: Move): Promise<MakeMoveResponse> {
        return this.withQueue(async () => {
            const response = await gameApi.makemove({ userId: this.userId, gameId }, move);
            const result = assertRight(response, err => `MakeMove failed: ${err}`);
            return { result, userId: result.userId };
        });
    }
    viewgame(gameId: string): Promise<GetGameViewResponse> {
        return this.withQueue(async () => {
            const response = await gameApi.viewgame({ userId: this.userId, gameId });
            const result = assertRight(response, err => `ViewGame failed: ${err}`);
            return { result, userId: result.userId };
        });
    }
    listgames(): Promise<GetGamesForUserResponse> {
        return this.withQueue(async () => {
            const result = await gameApi.listgames(this.userId);
            return { result, userId: result.userId };
        });
    }
}

const instance = new GameApiService();

export default instance;
