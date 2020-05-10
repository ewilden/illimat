import gameStateViewReducer from '~/common/features/game_state_view_slice';
import selectedCardsReducer from '~/common/features/selected_cards_slice';
import selectedCardStacksReducer from '~/common/features/selected_card_stacks_slice';
import selectedMoveReducer from '~/common/features/selected_move_slice';
import selectedFieldReducer from '~/common/features/selected_field_slice';
import { combineReducers } from 'redux';

const rootReducer = combineReducers({
  gameStateView: gameStateViewReducer,
  selectedCards: selectedCardsReducer,
  selectedCardStacks: selectedCardStacksReducer,
  selectedMoveType: selectedMoveReducer,
  selectedField: selectedFieldReducer,
});

export default rootReducer;
export type RootState = ReturnType<typeof rootReducer>;

