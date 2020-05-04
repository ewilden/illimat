import Field from '~/components/field';
import { dirsClockwise, seasonsClockwise } from '~/common/game_logic';

interface Props {
    boardState: BoardStateView;
    illimatState: IllimatState;
}

function atGridCell(row: number, col: number): string {
    return ` grid-row: ${row} / ${row + 1}; grid-column: ${col} / ${col + 1} `;
}

const commonFieldClasses = " h-full w-full border-dashed border-2 border-gray-600 ";

const commonCellClasses = " h-full w-full text-center flex flex-col content-center justify-center";

export function Board(props: Props) {
    const { fieldN, fieldE, fieldS, fieldW } = props.boardState;
    const { summerDir, numOkuses } = props.illimatState;
    const dirToSeason = (dir: Direction) => {
        const cwDistToSummerDir = [0, 1, 2, 3].find(rotations => dirsClockwise(dir, rotations) === summerDir);
        return seasonsClockwise("Summer", -1 * (cwDistToSummerDir || 0));
    };

    return <div className="boardContainer">
        <div className="grid grid-rows-3 grid-cols-3 text-center">
            <div className={`cellN ${commonFieldClasses}`}>
                <Field field={fieldN} />
            </div>
            <div className={`cellW ${commonFieldClasses}`}>
                <Field field={fieldW} />
            </div>
            <div className={`cellE ${commonFieldClasses}`}>
                <Field field={fieldE} />
            </div>
            <div className={`cellS ${commonFieldClasses}`}>
                <Field field={fieldS} />
            </div>
            <div className="cellCenter">
                <div className="grid grid-rows-3 grid-cols-3 text-center h-full">
                    <div className={`seasonLabel ${commonCellClasses} cellN`}><p>{dirToSeason("N")}</p></div>
                    <div className={`seasonLabel ${commonCellClasses} cellW`}><p>{dirToSeason("W")}</p></div>
                    <div className={`seasonLabel ${commonCellClasses} cellE`}><p>{dirToSeason("E")}</p></div>
                    <div className={`seasonLabel ${commonCellClasses} cellS`}><p>{dirToSeason("S")}</p></div>
                    <div className={` ${commonCellClasses} cellCenter`}>
                        {numOkuses} Okuses remain.
                    </div>
                </div>
            </div>
        </div>
        <style jsx>{`
            .boardContainer {
                min-width: 520px;
            }
            .seasonLabel {
                font-variant: small-caps;
            }
            .cellN {
                ${atGridCell(1, 2)};
            }
            .cellW {
                ${atGridCell(2, 1)};
            }
            .cellE {
                ${atGridCell(2, 3)};
            }
            .cellS {
                ${atGridCell(3, 2)};
            }
            .cellCenter {
                ${atGridCell(2, 2)};
            }
        `}</style>
    </div>
}