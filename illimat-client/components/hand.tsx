import { HandCard } from '~/components/card';
import { cardToString } from '~/common/game_logic';

export function MyHand({ hand }: { hand: Card[] }) {
    return <div className="flex flex-row space-x-1">{hand.map(card => <HandCard card={card} key={cardToString(card)} />)}</div>;
}