
type IndexesTo<T extends string, U> = {
  [K in T]: U
};

export function match<T extends string, U, O extends IndexesTo<T, (t: T) => U>>
  (tag: T, obj: O): U {
  return obj[tag](tag);
}

const dirs: Direction[] = ["N", "E", "S", "W"];

export function dirsClockwise(dir: Direction, n: number): Direction {
  const index = dirs.indexOf(dir);
  return dirs[(index + n + 2 * dirs.length) % dirs.length];
}

const seasons: Season[] = ["Summer", "Spring", "Winter", "Autumn"];

export function seasonsClockwise(season: Season, n: number): Season {
  const index = seasons.indexOf(season);
  return seasons[(index + n + 2 * seasons.length) % seasons.length];
}

type SampleUnion = "A" | "B" | "C";
type SampleResult = "One" | "Two" | "Three";

const result: SampleResult = match("A" as SampleUnion, {
  A: () => "One",
  B: () => "Two",
  C: () => "Three",
  D: () => "whatever",
});

export function cardToString(card: Card): string {
  const [val, season] = card;
  return `Card(${val},${season})`;
}

export function cardStackToString(cardStack: CardStack) {
  return `CardStack(${cardStack[0].map(String).join(',')}|${cardStack[1].map(String).join(',')})`;
}