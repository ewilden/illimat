import gameStateViewReducer from '~/common/features/game_state_view_slice';
import selectedCardsReducer from '~/common/features/selected_cards_slice';
import selectedCardStacksReducer from '~/common/features/selected_card_stacks_slice';
import { combineReducers } from 'redux';

const rootReducer = combineReducers({
  gameStateView: gameStateViewReducer,
  selectedCards: selectedCardsReducer,
  selectedCardStacks: selectedCardStacksReducer,
});

export default rootReducer;
export type RootState = ReturnType<typeof rootReducer>;

