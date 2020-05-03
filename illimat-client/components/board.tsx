interface Props {
    boardState: BoardStateView;
}

function atGridCell(row: number, col: number): string {
    return ` grid-row: ${row} / ${row + 1}; grid-column: ${col} / ${col + 1}; `;
}

export function Board(props: Props) {
    const {fieldN, fieldE, fieldS, fieldW} = props.boardState;
    return <div>
        <div className="boardContainer">
            <div className="fieldN">N</div>
            <div className="fieldW">W</div>
            <div className="fieldE">E</div>
            <div className="fieldS">S</div>
        </div>
        <style jsx>{`
            .boardContainer {
                display: grid;
                grid-template-columns: 1fr 1fr 1fr;
                grid-template-rows: 1fr 1fr 1fr;
                min-height: 300px;
            }
            .fieldN {
                ${atGridCell(1, 2)}
            }
            .fieldW {
                ${atGridCell(2, 1)}
            }
            .fieldE {
                ${atGridCell(2, 3)}
            }
            .fieldS {
                ${atGridCell(3, 2)}
            }
        `}</style>
    </div>
}