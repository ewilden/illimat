import { HandCard, RenderCardStack } from '~/components/card';
import { cardStackToString } from '~/common/game_logic';

interface Props {
    field: FieldStateView;
}

export default function Field(props: Props) {
    const { field } = props;
    const { cards } = field;
    return (
        <div className="flex flex-row flex-wrap h-full w-full">
            {cards.map(stack => (
                <div className="m-px"
                    key={cardStackToString(stack)}>
                    <RenderCardStack cardStack={stack} />
                </div>
            ))}
        </div>
    );
}