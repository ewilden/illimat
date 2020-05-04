import { HandCard, RenderCardStack } from '~/components/card';

interface Props {
    field: FieldStateView;
}

export default function Field(props: Props) {
    const { field } = props;
    const { cards } = field;
    return (
        <div className="flex flex-row flex-wrap h-full w-full">
            {cards.map(stack => (
                <div className="m-px">
                    <RenderCardStack key={stack.toString()} cardStack={stack} />
                </div>
            ))}
        </div>
    );
}