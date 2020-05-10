import { HandCard, RenderCardStack } from '~/components/card';
import { cardStackToString } from '~/common/game_logic';
import { createSelector } from '@reduxjs/toolkit';
import { RootState } from '~/common/reducers';
import { useSelector, useDispatch } from 'react-redux';
import { selectField } from '~/common/features/selected_field_slice';

interface Props {
    field: FieldStateView;
    direction: Direction;
}

const selectIsSowSelected = createSelector(
    (state: RootState) => state.selectedMoveType,
    selectedMoveType => selectedMoveType === 'Sow',
);

const selectSelectedField = createSelector(
    (state: RootState) => state.selectedField,
    selectedField => selectedField,
);

const moveButtonClasses = {
    always: " border border-solid border-1 border-indigo-300 bg-indigo-100 py-2 px-4 rounded ",
    selected: " bg-indigo-400 ",
    notSelected: " hover:bg-indigo-200 ",
};

export default function Field(props: Props) {
    const { field, direction } = props;
    const { cards } = field;
    const isSowSelected = useSelector(selectIsSowSelected);
    const selectedField = useSelector(selectSelectedField);
    const dispatch = useDispatch();
    return (
        <div className="h-full w-full flex flex-col justify-between">
            <div className="flex flex-row flex-wrap">
                {cards.map(stack => (
                    <div className="m-px"
                        key={cardStackToString(stack)}>
                        <RenderCardStack cardStack={stack} />
                    </div>
                ))}
            </div>
            {isSowSelected && (
                <button
                    onClick={() => {
                        dispatch(selectField({ field: direction }));
                    }}
                    className={
                        `block w-full ${moveButtonClasses.always} ${
                        selectedField === direction ? moveButtonClasses.selected : moveButtonClasses.notSelected
                        }`}
                >Select Field</button>
            )}
        </div>
    );
}