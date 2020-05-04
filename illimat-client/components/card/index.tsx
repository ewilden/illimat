import { match } from "~/common/game_logic";

interface Props {
    card: Card;
}

export function HandCard({ card }: Props) {
    const [val, season] = card;
    return (
        <div className="w-20 border-solid border-2 border-gray-800 rounded">
            <p>{val}</p>
            <p className="text-2xl">{renderSeason(season)}</p>
            <p>{season.substring(1)}</p>
        </div>
    );
}

export function RenderCardStack({ cardStack }: { cardStack: CardStack }) {
    const [possibleValues, cards] = cardStack;
    if (cards.length < 1) {
        throw new Error("impossible");
    } else if (cards.length === 1) {
        return (
            <div>
                <HandCard card={cards[0]} />
            </div>
        );
    } else {
        return (
            <div className="border-dotted border-2 border-gray-400">
                <p>[{possibleValues.join(", ")}]</p>
                <div className="flex flex-col space-y-px">
                    {cards.map(card => (
                        <div >
                            <HandCard card={card} />
                        </div>
                    ))}
                </div>
            </div>
        );
    }
}

function renderSeason(season: CardSeason): string {
    return match(season, {
        CSpring: () => "🌱",
        CWinter: () => "❄️",
        CSummer: () => "☀️",
        CAutumn: () => "🍂",
        CStars: () => "✨",
    });
}