import { match } from "~/common/game_logic";

interface Props {
    card: Card;
}

export function HandCard({card}: Props) {
    const [val, season] = card;
    return <div>{val} of {renderSeason(season)}</div>
}

function renderSeason(season: CardSeason): string {
    return match(season, {
        CSpring: () => "Spring",
        CWinter: () => "Winter",
        CSummer: () => "Summer",
        CAutumn: () => "Autumn",
        CStars: () => "Stars",
    });
}