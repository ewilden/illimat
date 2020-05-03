
type IndexesTo<T extends string, U> = {
    [K in T]: U
};

export function match<T extends string, U, O extends IndexesTo<T, (t: T) => U>>
    (tag: T, obj: O): U {
  return obj[tag](tag);
}

type SampleUnion = "A" | "B" | "C";
type SampleResult = "One" | "Two" | "Three";

const result: SampleResult = match("A" as SampleUnion, {
  A: () => "One",
  B: () => "Two",
  C: () => "Three",
  D: () => "whatever",
});